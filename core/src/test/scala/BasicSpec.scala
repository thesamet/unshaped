import java.io.ByteArrayOutputStream

import com.google.protobuf.CodedOutputStream
import org.scalatest.FlatSpec
import shapeless._

import scalapb.macros.ProtoType.Int32
import scalapb.macros._

sealed trait Msg

case class Other(
  @Optional(Int32, 1) other: Int,
  @Optional(ProtoType.String, 2) bother: String,
  @Optional(ProtoType.String, 2) mager: String = ""
) extends Msg

case class Bar(
  @Optional(Int32, 44) x: Int,
  @Optional(ProtoType.String, 39) y: String,
  @Optional(ProtoType.String, 75) z: Option[String],
  @Optional(ProtoType.Message, 17) o: Other,
  @Repeated(ProtoType.Int32, 75) w: Vector[Int],
) extends Msg

case class Recursive(
  @Optional(Int32, 1) value: Option[Int],
  @Optional(ProtoType.Message, 2) next: Option[Recursive]
) extends Msg

abstract class FieldSerializer[PT <: ProtoType, T] {
  def isDefault(t: T): Boolean

  def serialize(cos: CodedOutputStream, t: T): Unit

  def serializedSize(t: T): Int
}

object FieldSerializer {
  def apply[PT <: ProtoType, T](ser: (CodedOutputStream, T) => Unit, size: T => Int, default: T => Boolean): FieldSerializer[PT, T] =
    new FieldSerializer[PT, T] {
      override def serialize(cos: CodedOutputStream, t: T): Unit = ser(cos, t)

      override def isDefault(t: T): Boolean = default(t)

      override def serializedSize(t: T): Int = size(t)
    }

  implicit val int32Serializer = FieldSerializer[ProtoType.Int32.type, Int](_.writeInt32NoTag(_), CodedOutputStream.computeInt32SizeNoTag, _ == 0)

  implicit val stringSerializer = FieldSerializer[ProtoType.String.type, String](_.writeStringNoTag(_), CodedOutputStream.computeStringSizeNoTag, _.isEmpty)

  implicit def optionFieldSerializer[PT <: ProtoType, T](implicit ser: FieldSerializer[PT, T]): FieldSerializer[PT, Option[T]] =
    new FieldSerializer[PT, Option[T]] {
      override def isDefault(t: Option[T]): Boolean = t.forall(ser.isDefault)

      override def serialize(cos: CodedOutputStream, t: Option[T]): Unit = t.foreach(ser.serialize(cos, _))

      override def serializedSize(t: Option[T]): Int = 0
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

abstract class RepeatedFieldSerializer[PT <: ProtoType, T] extends FieldSerializer[PT, T]

object RepeatedFieldSerializer {
  implicit def vectorFieldSerializer[PT <: ProtoType, T](implicit ser: FieldSerializer[PT, T]): RepeatedFieldSerializer[PT, Vector[T]] =
    new RepeatedFieldSerializer[PT, Vector[T]] {
      override def isDefault(t: Vector[T]): Boolean = t.isEmpty

      override def serialize(cos: CodedOutputStream, t: Vector[T]): Unit = {
        t.foreach(ser.serialize(cos, _))
      }

      override def serializedSize(t: Vector[T]): Int = 0
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
  ): Serializer.Aux[TH :: TT, Field[Optional, PT, TAG] :: SCHEMATAIL] = {

    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    Serializer({ (cos, t) =>
      cos.writeTag(tag, wiretype)
      fe.serialize(cos, t.head)
      tail.serialize(cos, t.tail)
    }, { t => CodedOutputStream.computeTagSize(tag) + fe.serializedSize(t.head) + tail.serializedSize(t.tail) })
  }

  implicit def HListSerializerRepeated[PT <: ProtoType, TAG <: Int, SCHEMATAIL <: HList, TH, TT <: HList](
    implicit tagWitness: Witness.Aux[TAG],
    prototypeWitness: Witness.Aux[PT],
    fe: RepeatedFieldSerializer[PT, TH],
    tail: Serializer.Aux[TT, SCHEMATAIL]
  ): Serializer.Aux[TH :: TT, Field[Repeated, PT, TAG] :: SCHEMATAIL] = {

    val tag = tagWitness.value
    val pt = prototypeWitness.value
    val wiretype = pt.wireType

    Serializer({ (cos, t) =>
      cos.writeTag(tag, wiretype)
      fe.serialize(cos, t.head)
      tail.serialize(cos, t.tail)
    }, { t => CodedOutputStream.computeTagSize(tag) + fe.serializedSize(t.head) + tail.serializedSize(t.tail) })
  }

  implicit def fromHelper[T <: Msg, SCHEMA <: HList, G](
    implicit helper: SchemaHolder.Aux[T, SCHEMA], gen: Generic.Aux[T, G], pr: Serializer.Aux[G, SCHEMA]) =
    Serializer[T, SCHEMA]((cos, t) => {
      pr.serialize(cos, gen.to(t))
    }, t => pr.serializedSize(gen.to(t)))
}

class BasicSpec extends FlatSpec {
  "foo" should "bar" in {
    val helper = SchemaHolder[Bar]
    val j: Serializer[Other] = implicitly[Serializer[Other]]
    val i: Serializer[Bar] = implicitly[Serializer[Bar]]
    println(i.toByteArray(Bar(35, "foo", Some("koo"),
      Other(243, ""),
      Vector(1, 2, 3)
    )).toVector)
  }


}
