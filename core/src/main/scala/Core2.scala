package scalapb.core2

import com.google.protobuf.CodedOutputStream

import scalapb.core.FieldExtractors
import shapeless._

import scala.collection.mutable
import scalapb.core3.Msg
import scalapb.macros.{OptionalField, ProtoType, SchemaHolder}

abstract sealed class Serializer[T <: Msg[T]] {
  def serialize(codedOutputStream: CodedOutputStream, t: T)

  def serializedSize(t: T): Int

}

object Serializer {
  implicit def buildSerializer[T <: Msg[T], R <: HList, SCHEMA <: HList](
    implicit ce: FieldExtractors.Aux[T, R],
    schema: SchemaHolder.Aux[T, SCHEMA],
    psb: SerializerBuilder[T, R, SCHEMA]): Serializer[T] = {
    val extractors = ce.extractors
    val x = psb.withFetcher(extractors)
    x
  }
}

abstract class SerializerBuilder[T <: Msg[T], R, SCHEMA] {
  def withFetcher(f: R): Serializer[T]
}

abstract class FieldSerializerBuilder[T, R, TAG] {
  def build(tag: Int, extractor: T => R): FieldSerializer[T]
}

object FieldSerializerBuilder {
  implicit def stringSerializerBuilder[T, TAG <: Int](implicit t: Witness.Aux[TAG]): FieldSerializerBuilder[T, String, TAG] = new FieldSerializerBuilder[T, String, TAG] {
    override def build(tag: Int, extractor: T => String): FieldSerializer[T] = new StringSerializer[T](tag, extractor)
  }
}

abstract class FieldSerializer[R] {
  def serialize(cos: CodedOutputStream,  v: R): Unit
}

final class StringSerializer[T](tag: Int, extractor: T => String) extends FieldSerializer[T] {
  final override def serialize(cos: CodedOutputStream,  v: T): Unit = cos.writeString(tag, extractor(v))
}

object SerializerBuilder {
  implicit def buildBuilderHnil[T <: Msg[T]] = new SerializerBuilder[T, HNil, HNil] {
    override def withFetcher(f: HNil): Serializer[T] = new Serializer[T] {
      final override def serialize(codedOutputStream: CodedOutputStream, t: T): Unit = {}

      final override def serializedSize(t: T): Int = 0
    }
  }

  implicit def buildBuilderGen[T <: Msg[T], RH, RT <: HList, SH, ST <: HList, PT <: ProtoType, TAG <: Int](implicit
    tail: SerializerBuilder[T, RT, ST], tagWt: Witness.Aux[TAG], fsb: FieldSerializerBuilder[T, RH, TAG]): SerializerBuilder[T, (T => RH) :: RT, OptionalField[PT, TAG]:: ST] =
    new SerializerBuilder[T, (T => RH) :: RT, OptionalField[PT, TAG] :: ST] {

      final override def withFetcher(f: (T => RH) :: RT): Serializer[T] = {
        val tailFh = tail.withFetcher(f.tail)
        val extr: T => RH = f.head
//        val fs = fsb.build(tagWt.value, extr).asInstanceOf[StringSerializer[T]]
        val tg = tagWt.value

        new Serializer[T] {

          final override def serialize(codedOutputStream: CodedOutputStream, t: T): Unit = {
            codedOutputStream.writeString(tg, extr(t).asInstanceOf[String])
//            fs.serialize(codedOutputStream, t)
            tailFh.serialize(codedOutputStream, t)
          }

          final override def serializedSize(t: T): Int = {
            7 + tailFh.serializedSize(t)
          }
        }
      }
    }
}
