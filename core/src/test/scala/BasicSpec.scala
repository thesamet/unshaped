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
) extends Msg

case class Bar(
  @Optional(Int32, 44) x: Int,
  @Optional(ProtoType.String, 39) y: String,
  @Optional(ProtoType.String, 75) z: String,
  @Optional(ProtoType.Message, 75) o: Other
) extends Msg

abstract class FieldSerializer[PT <: ProtoType, T] {
  def serialize(cos: CodedOutputStream, t: T): Unit

  def serializedSize(t: T): Int
}

object FieldSerializer {
  def apply[PT <: ProtoType, T](ser: (CodedOutputStream, T) => Unit, size: T => Int): FieldSerializer[PT, T] =
    new FieldSerializer[PT, T] {
      override def serialize(cos: CodedOutputStream, t: T): Unit = ser(cos, t)

      override def serializedSize(t: T): Int = size(t)
    }

  implicit val int32Serializer = FieldSerializer[ProtoType.Int32.type, Int](_.writeInt32NoTag(_), CodedOutputStream.computeInt32SizeNoTag)

  implicit val stringSerializer = FieldSerializer[ProtoType.String.type, String](_.writeStringNoTag(_), CodedOutputStream.computeStringSizeNoTag)

  implicit def messageFieldSerializer[M](implicit ser: Serializer[M]) = FieldSerializer[ProtoType.Message.type, M](
    (cos, t) => {
      cos.writeUInt32NoTag(ser.serializedSize(t))
      ser.serialize(cos, t)
    }, {
      t =>
        val size = ser.serializedSize(t)
        CodedOutputStream.computeUInt32SizeNoTag(size) + size
    }
  )
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

//
//    println(ev.value)
//    println("  OPTIONAL " + et.value)
//    tail.print
//    Serializer[Field[Optional, PT, TAG] :: SCHEMATAIL]((cos, t) => {
//      tail.serialize(cos, tail)
//    }
//  }

  /*

  implicit def HListSerializerRequired[PT <: ProtoType, TAG, TAIL <: HList](
    implicit ev: Witness.Aux[TAG], et: Witness.Aux[PT], tail: Serializer[TAIL]): Serializer[Field[Required, PT, TAG] :: TAIL] =
    new Serializer[Field[Required, PT, TAG] :: TAIL] {
      override def print: Unit = {
        println(ev.value)
        println("  REQUIRED " + et.value)
        tail.print
      }
    }

  implicit def HListSerializerRepeated[PT <: ProtoType, TAG, TAIL <: HList](
    implicit ev: Witness.Aux[TAG], et: Witness.Aux[PT], tail: Serializer[TAIL]): Serializer[Field[Repeated, PT, TAG] :: TAIL] =
    new Serializer[Field[Repeated, PT, TAG] :: TAIL] {
      override def print: Unit = {
        println(ev.value)
        println("  REPEATED " + et.value)
        tail.print
      }
    }
    */

  implicit def fromHelper[T <: Msg, SCHEMA <: HList, G](
    implicit helper: SchemaHolder.Aux[T, SCHEMA], gen: Generic.Aux[T, G], pr: Serializer.Aux[G, SCHEMA]) =
    Serializer[T, SCHEMA]((cos, t) => {
      pr.serialize(cos, gen.to(t))
    }, t => pr.serializedSize(gen.to(t)))
}

class BasicSpec extends FlatSpec {
  "foo" should "bar" in {
    val helper = SchemaHolder[Bar]
    val i: Serializer[Bar] = implicitly[Serializer[Bar]]
    println(i.toByteArray(Bar(35, "foo", "koo",
      Other(243, "foo")
    )).toVector.map(_.toChar))
  }


}
