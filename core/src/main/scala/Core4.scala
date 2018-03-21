package scalapb.core4

import com.google.protobuf.{CodedOutputStream, WireFormat}
import shapeless.ops.nat.ToInt
import shapeless.{::, HList, HNil, Lazy, Nat, Succ}

sealed trait ProtoType

sealed trait Int32 extends ProtoType

sealed trait StringType extends ProtoType

sealed trait Message extends ProtoType

abstract class FieldSerializer[PT <: ProtoType, T] {
  def serializedSize(value: T): Int

  def serialize(cos: CodedOutputStream, tag: Int, value: T): Unit

  def serializeNoTag(cos: CodedOutputStream, value: T): Unit
}

object FieldSerializer {
  def message[T](implicit e: FieldSerializer[Message, T]) = e

  implicit val intSerializer: FieldSerializer[Int32, Int] = new FieldSerializer[Int32, Int] {
    override def serializedSize(value: Int): Int = CodedOutputStream.computeInt32SizeNoTag(value)

    override def serializeNoTag(cos: CodedOutputStream, value: Int): Unit = cos.writeInt32NoTag(value)

    override def serialize(cos: CodedOutputStream, tag: Int, value: Int): Unit = cos.writeInt32(tag, value)
  }

  implicit val stringSerializer: FieldSerializer[StringType, String] = new FieldSerializer[StringType, String] {
    override def serializedSize(value: String): Int = CodedOutputStream.computeStringSizeNoTag(value)

    override def serializeNoTag(cos: CodedOutputStream, value: String): Unit = cos.writeStringNoTag(value)

    override def serialize(cos: CodedOutputStream, tag: Int, value: String): Unit = cos.writeString(tag, value)
  }

  implicit def optionSerializer[PT <: ProtoType, T](implicit fs: FieldSerializer[PT, T]): FieldSerializer[Message, Option[T]] =
    new FieldSerializer[Message, Option[T]] {
      override def serializedSize(value: Option[T]): Int = value match {
        case None => 0
        case Some(v) => fs.serializedSize(v)
      }

      override def serialize(cos: CodedOutputStream, tag: Int, value: Option[T]): Unit = value match {
        case None =>
        case Some(v) => fs.serialize(cos, tag, v)
      }

      override def serializeNoTag(cos: CodedOutputStream, value: Option[T]): Unit = ???
    }

  implicit def messageSerializer[T](implicit ms: Lazy[MessageSerializer[T]]): FieldSerializer[Message, T] = new FieldSerializer[Message, T] {
    override def serializedSize(value: T): Int = {
      val size = ms.value.serializedSize(value)
      size + CodedOutputStream.computeUInt32SizeNoTag(size)
    }

    override def serializeNoTag(cos: CodedOutputStream, value: T): Unit = {
      cos.writeUInt32NoTag(ms.value.serializedSize(value))
      ms.value.serialize(cos, value)
    }

    override def serialize(cos: CodedOutputStream, tag: Int, value: T): Unit = {
      cos.writeTag(tag, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      serializeNoTag(cos, value)
    }
  }
}

abstract class MessageSerializer[T] {
  def serialize(cos: CodedOutputStream, value: T): Unit

  def serializedSize(value: T): Int

  def toByteArray(t: T): Array[Byte] = {
    val a = new Array[Byte](serializedSize(t))
    val outputStream = CodedOutputStream.newInstance(a)
    serialize(outputStream, t)
    outputStream.checkNoSpaceLeft()
    a
  }
}

abstract class HListSerializer[N <: Nat, R] extends MessageSerializer[R]

object HListSerializer {

  implicit def hlistNilSerializer[N <: Nat, R] = new HListSerializer[N, HNil] {
    override def serializedSize(value: HNil): Int = 0

    override def serialize(cos: CodedOutputStream, value: HNil): Unit = {}
  }

  implicit def hlistSerializer[N <: Nat, PT <: ProtoType, H, T <: HList](
    implicit fieldSer: FieldSerializer[PT, H], tail: HListSerializer[Succ[N], T], tag: ToInt[N]): HListSerializer[N, H :: T] =
    new HListSerializer[N, H :: T] {

      override def serializedSize(value: H :: T): Int = {
        val sz = fieldSer.serializedSize(value.head)
        val r = (if (sz > 0) (sz + CodedOutputStream.computeTagSize(tag())) else 0) + tail.serializedSize(value.tail)
        r
      }

      override def serialize(cos: CodedOutputStream, t: H :: T): Unit = {
        fieldSer.serialize(cos, tag(), t.head)
        tail.serialize(cos, t.tail)
      }
    }

}

object MessageSerializer {
  def apply[T](implicit e: MessageSerializer[T]) = e

  implicit def messageSerializer[T, R](implicit aux: shapeless.Generic.Aux[T, R], vs: Lazy[HListSerializer[Nat._1, R]]): MessageSerializer[T] =
    new MessageSerializer[T] {
      override def serializedSize(value: T): Int = vs.value.serializedSize(aux.to(value))

      override def serialize(cos: CodedOutputStream, t: T): Unit = {
        vs.value.serialize(cos, aux.to(t))
      }
    }

}
