package scalapb.generic

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
}

abstract class MessageSerializer[T] {
  def serialize(cos: CodedOutputStream, value: T): Unit
}

abstract class HListSerializer[R](val tag: Int) extends MessageSerializer[R]

object HListSerializer {

  implicit def hlistNilSerializer[R] = new HListSerializer[HNil](0) {
    override def serialize(cos: CodedOutputStream, value: HNil): Unit = {}
  }

  implicit def hlistSerializer[PT <: ProtoType, H, T <: HList](
    implicit fieldSer: FieldSerializer[PT, H], tail: HListSerializer[T]): HListSerializer[H :: T] =
    new HListSerializer[H :: T](tail.tag + 1) {

      override def serialize(cos: CodedOutputStream, t: H :: T): Unit = {
        fieldSer.serialize(cos, tag, t.head)
        tail.serialize(cos, t.tail)
      }
    }

}

object MessageSerializer {
  def apply[T](implicit e: MessageSerializer[T]) = e

  implicit def messageSerializer[T, R](implicit aux: shapeless.Generic.Aux[T, R], vs: HListSerializer[R]): MessageSerializer[T] =
    new MessageSerializer[T] {
      override def serialize(cos: CodedOutputStream, t: T): Unit = {
        vs.serialize(cos, aux.to(t))
      }
    }

}
