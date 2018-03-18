package scalapb.core3

import com.google.protobuf.CodedOutputStream
import scalapb.macros.ProtoType

import scala.language.experimental.macros

abstract class Msg[T <: Msg[T]] {
  self: T =>

  var __cachedSerializedSize = 0
}

abstract class Serializer[T <: Msg[T]] {
  def serializedSize(t: T): Int

  def serialize(codedOutputStream: CodedOutputStream, t: T): Unit

  def toByteArray(t: T): Array[Byte] = {
    val a = new Array[Byte](serializedSize(t))
    val outputStream = CodedOutputStream.newInstance(a)
    serialize(outputStream, t)
    outputStream.checkNoSpaceLeft()
    a
  }
}

abstract class FieldSerializer[PT <: ProtoType, @specialized R] {
  def serializeNoTag(cos: CodedOutputStream, r: R): Unit

  def serializedSizeNoTag(r: R): Int

  def serialize(cos: CodedOutputStream, tag: Int, r: R): Unit
}

abstract class PackedRepeatedFieldSerializer[PT <: ProtoType, T] extends FieldSerializer[PT, T] {
  def serializeWithKnownSize(cos: CodedOutputStream, tag: Int, knownSize: Int, t: T)

  def contentSize(t: T): Int
}

object FieldSerializer {
  implicit val stringSerializer: FieldSerializer[ProtoType.String.type, String] = new StringSerializer

  implicit val intSerializer: FieldSerializer[ProtoType.Int32.type, Int] = new Int32Serializer

  implicit def vectorSerializer[PT <: ProtoType, T](implicit
    fieldSerializer: FieldSerializer[PT, T]): PackedRepeatedFieldSerializer[PT, Vector[T]] = new VectorSerializer[PT, T]

  implicit def optionSerializer[PT <: ProtoType, T](implicit
    fieldSerializer: FieldSerializer[PT, T]): FieldSerializer[PT, Option[T]] =
    new FieldSerializer[PT, Option[T]] {
      override def serializedSizeNoTag(r: Option[T]): Int =
        if (r.isDefined) fieldSerializer.serializedSizeNoTag(r.get)
        else 0

      override def serialize(cos: CodedOutputStream, tag: Int, r: Option[T]): Unit = r match {
        case Some(v) => fieldSerializer.serialize(cos, tag, v)
        case None =>
      }

      override def serializeNoTag(cos: CodedOutputStream, r: Option[T]): Unit = r match {
        case Some(v) => fieldSerializer.serializeNoTag(cos, v)
        case None =>
      }
    }

  implicit def messageSerializer[T <: Msg[T]](implicit sr: Serializer[T]): FieldSerializer[ProtoType.Message.type, T] =
    new FieldSerializer[ProtoType.Message.type, T] {
      override def serializeNoTag(cos: CodedOutputStream, r: T): Unit = {
        cos.writeUInt32NoTag(sr.serializedSize(r))
        sr.serialize(cos, r)
      }

      override def serializedSizeNoTag(r: T): Int = sr.serializedSize(r) + CodedOutputStream.computeUInt32SizeNoTag(sr.serializedSize(r))

      override def serialize(cos: CodedOutputStream, tag: Int, r: T): Unit = {
        cos.writeTag(tag, 2)
        serializeNoTag(cos, r)
      }
    }
}

final class VectorSerializer[PT <: ProtoType, T](implicit fieldSerializer: FieldSerializer[PT, T]) extends PackedRepeatedFieldSerializer[PT, Vector[T]] {
  final private def writeContent(cos: CodedOutputStream, r: Vector[T]): Unit = {
    var i = 0
    while (i < r.size) {
      fieldSerializer.serializeNoTag(cos, r(i))
      i += 1
    }
  }

  final override def serializeNoTag(cos: CodedOutputStream, r: Vector[T]): Unit = {
    cos.writeUInt32NoTag(contentSize(r))
    writeContent(cos, r)
  }

  final override def serializeWithKnownSize(cos: CodedOutputStream, tag: Int, knownSize: Int, r: Vector[T]): Unit = {
    cos.writeTag(tag, 2)
    cos.writeUInt32NoTag(knownSize)
    var i = 0
    while (i < r.size) {
      fieldSerializer.serializeNoTag(cos, r(i))
      i += 1
    }
  }

  final override def contentSize(r: Vector[T]): Int = {
    var size = 0
    var i = 0
    while (i < r.size) {
      size += fieldSerializer.serializedSizeNoTag(r(i))
      i += 1
    }
    size
  }

  final override def serializedSizeNoTag(r: Vector[T]): Int = {
    var size = contentSize(r)
    size += CodedOutputStream.computeUInt32SizeNoTag(size)
    size
  }

  final override def serialize(cos: CodedOutputStream, tag: Int, r: Vector[T]): Unit = {
    cos.writeTag(tag, 2)
    serializeNoTag(cos, r)
  }
}

final class StringSerializer extends FieldSerializer[ProtoType.String.type, String] {
  final override def serializeNoTag(cos: CodedOutputStream, r: String): Unit = {
    cos.writeStringNoTag(r)
  }

  final override def serialize(cos: CodedOutputStream, tag: Int, r: String): Unit = {
    cos.writeString(tag, r)
  }

  final override def serializedSizeNoTag(r: String): Int = CodedOutputStream.computeStringSizeNoTag(r)
}

final class Int32Serializer extends FieldSerializer[ProtoType.Int32.type,  Int] {
  final override def serializeNoTag(cos: CodedOutputStream, r: Int): Unit = {
    cos.writeInt32NoTag(r)
  }

  final override def serializedSizeNoTag(r: Int): Int = CodedOutputStream.computeInt32SizeNoTag(r)

  final override def serialize(cos: CodedOutputStream, tag: Int, r: Int): Unit = {
    cos.writeInt32(tag, r)
  }
}

object Serializer {
  implicit def makeSerializer[T <: Msg[T]]: Serializer[T] = macro scalapb.macros.Macros.mkSerializer[T]
}
