package scalapb.core

import java.io.ByteArrayOutputStream

import com.google.protobuf.CodedOutputStream
import shapeless._

import scalapb.macros._

trait Msg

abstract class FieldSerializer[PT <: ProtoType, T] {
  def isDefault(t: T): Boolean

  def serializeOne(cos: CodedOutputStream, t: T): Unit

  def serialize(cos: CodedOutputStream, tag: Int, t: T): Unit

  def serializedSize(t: T): Int
}

object FieldSerializer {
  def apply[PT <: ProtoType, T](ser: (CodedOutputStream, T) => Unit, size: T => Int, default: T => Boolean): FieldSerializer[PT, T] =
    new FieldSerializer[PT, T] {
      override def serializeOne(cos: CodedOutputStream, t: T): Unit = ser(cos, t)

      override def serialize(cos: CodedOutputStream, tag: Int, t: T): Unit = ser(cos, t)

      override def isDefault(t: T): Boolean = default(t)

      override def serializedSize(t: T): Int = size(t)
    }

  implicit val int32Serializer = FieldSerializer[ProtoType.Int32.type, Int](_.writeInt32NoTag(_), CodedOutputStream.computeInt32SizeNoTag, _ == 0)

  implicit val stringSerializer = FieldSerializer[ProtoType.String.type, String](_.writeStringNoTag(_), CodedOutputStream.computeStringSizeNoTag, _.isEmpty)

  implicit def optionFieldSerializer[PT <: ProtoType, T](implicit ser: FieldSerializer[PT, T]): FieldSerializer[PT, Option[T]] =
    new FieldSerializer[PT, Option[T]] {
      override def isDefault(t: Option[T]): Boolean = t.forall(ser.isDefault)

      override def serialize(cos: CodedOutputStream, tag: Int, t: Option[T]): Unit = t.foreach(ser.serialize(cos, tag, _))

      override def serializeOne(cos: CodedOutputStream, t: Option[T]): Unit = t.foreach(ser.serializeOne(cos, _))

      override def serializedSize(t: Option[T]): Int = t.map(ser.serializedSize).getOrElse(0)
    }

  implicit def messageFieldSerializer[M](implicit ser: Serializer[M]) = FieldSerializer[ProtoType.Message.type, M](
    (cos, t) => {
      cos.writeUInt32NoTag(ser.serializedSize(t))
      ser.serialize(cos, t)
    }, {
      t =>
        val size = ser.serializedSize(t)
        CodedOutputStream.computeUInt32SizeNoTag(size) + size
    }, _ => false
  )
}

abstract class RepeatedFieldSerializer[PT <: ProtoType, T] {
  def isDefault(t: T): Boolean

  def serializedSize(tag: Int, t: T): Int

  def serializedPackedSize(tag: Int, t: T): Int

  def serialize(codedOutputStream: CodedOutputStream, tag: Int, t: T): Unit

  def serializePacked(codedOutputStream: CodedOutputStream, tag: Int, t: T): Unit

}

object RepeatedFieldSerializer {
  implicit def vectorFieldSerializer[PT <: ProtoType, T](implicit ser: FieldSerializer[PT, T]): RepeatedFieldSerializer[PT, Vector[T]] =
    new RepeatedFieldSerializer[PT, Vector[T]] {
      override def serialize(cos: CodedOutputStream, tag: Int, t: Vector[T]): Unit = {
        t.foreach(ser.serialize(cos, tag, _))
      }

      override def serializedSize(tag: Int, t: Vector[T]): Int = {
        val tagSize = CodedOutputStream.computeTagSize(tag)
        t.map(ser.serializedSize).sum + t.size * tagSize
      }

      override def serializedPackedSize(tag: Int, t: Vector[T]): Int = {
        val tagSize = CodedOutputStream.computeTagSize(tag)
        val totalSize = t.map(ser.serializedSize).sum
        tagSize + totalSize + CodedOutputStream.computeUInt32SizeNoTag(totalSize)
      }

      override def serializePacked(codedOutputStream: CodedOutputStream, tag: Int, t: Vector[T]): Unit = {
        codedOutputStream.writeTag(tag, 2)
        codedOutputStream.writeUInt32NoTag(t.map(ser.serializedSize).sum)
        t.foreach(ser.serializeOne(codedOutputStream, _))
      }

      override def isDefault(t: Vector[T]): Boolean = t.isEmpty
    }
}



abstract class Serializer[T] {
  type SCHEMA
  def serialize(cos: CodedOutputStream, m: T): Unit

  def toByteArray(t: T) = {
    val baos = new ByteArrayOutputStream
    val outputStream = CodedOutputStream.newInstance(baos)
    serialize(outputStream, t)
    outputStream.flush()
    baos.toByteArray
  }

  def serializedSize(t: T): Int
}

object Serializer {
  type Aux[T, SCHEMA0] = Serializer[T] { type SCHEMA = SCHEMA0 }

  def apply[T, SCHEMA0](ser: (CodedOutputStream, T) => Unit, size: T => Int): Serializer.Aux[T, SCHEMA0] =
    new Serializer[T] {
      type SCHEMA = SCHEMA0
      override def serialize(cos: CodedOutputStream, t: T) = ser(cos, t)

      override def serializedSize(t: T) = size(t)
    }

  implicit def SerializerHNil: Serializer.Aux[HNil, HNil] = Serializer[HNil, HNil]((_, _) => {}, _ => 0)

  implicit def HListSerializerOptional[PT <: ProtoType, TAG <: Int, SCHEMATAIL <: HList, TH, TT <: HList](
    implicit tagWitness: Witness.Aux[TAG],
    prototypeWitness: Witness.Aux[PT],
    fe: FieldSerializer[PT, TH],
    tail: Serializer.Aux[TT, SCHEMATAIL]
  ): Serializer.Aux[TH :: TT, OptionalField[PT, TAG] :: SCHEMATAIL] = {

    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    Serializer({ (cos, t) =>
      if (!fe.isDefault(t.head)) {
        cos.writeTag(tag, wiretype)
        fe.serializeOne(cos, t.head)
      }
      tail.serialize(cos, t.tail)
    }, { t => CodedOutputStream.computeTagSize(tag) + fe.serializedSize(t.head) + tail.serializedSize(t.tail) })
  }

  implicit def HListSerializerRepeated[PT <: ProtoType, TAG <: Int, SCHEMATAIL <: HList, TH, TT <: HList](
    implicit tagWitness: Witness.Aux[TAG],
    prototypeWitness: Witness.Aux[PT],
    fe: RepeatedFieldSerializer[PT, TH],
    tail: Serializer.Aux[TT, SCHEMATAIL]
  ): Serializer.Aux[TH :: TT, RepeatedField[PT, TAG, CFalse] :: SCHEMATAIL] = {

    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    Serializer({ (cos, t) =>
      fe.serialize(cos, tag, t.head)
      tail.serialize(cos, t.tail)
    }, { t => CodedOutputStream.computeTagSize(tag) + fe.serializedSize(tag, t.head) + tail.serializedSize(t.tail) })
  }

  implicit def HListSerializerPackedRepeated[PT <: ProtoType, TAG <: Int, SCHEMATAIL <: HList, TH, TT <: HList](
    implicit tagWitness: Witness.Aux[TAG],
    prototypeWitness: Witness.Aux[PT],
    fe: RepeatedFieldSerializer[PT, TH],
    tail: Serializer.Aux[TT, SCHEMATAIL]
  ): Serializer.Aux[TH :: TT, RepeatedField[PT, TAG, CTrue] :: SCHEMATAIL] = {

    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    Serializer({ (cos, t) =>
      cos.writeTag(tag, wiretype)
      fe.serializePacked(cos, tag, t.head)
      tail.serialize(cos, t.tail)
    }, { t => CodedOutputStream.computeTagSize(tag) + fe.serializedPackedSize(tag, t.head) + tail.serializedSize(t.tail) })
  }

  implicit def fromHelper[T <: Msg, SCHEMA <: HList, G](
    implicit helper: SchemaHolder.Aux[T, SCHEMA], gen: Generic.Aux[T, G], pr: Serializer.Aux[G, SCHEMA]) =
    Serializer[T, SCHEMA]((cos, t) => {
      pr.serialize(cos, gen.to(t))
    }, t => pr.serializedSize(gen.to(t)))
}

