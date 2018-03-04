package scalapb.core

import java.io.ByteArrayOutputStream

import com.google.protobuf.CodedOutputStream
import org.scalatest.FlatSpec
import shapeless._

import scalapb.macros.ProtoType.Int32
import scalapb.macros._

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
  @Repeated(ProtoType.Int32, 75, true) w: Vector[Int],
) extends Msg

case class Recursive(
  @Optional(Int32, 1) value: Option[Int],
  @Optional(ProtoType.Message, 2) next: Option[Recursive]
) extends Msg

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
