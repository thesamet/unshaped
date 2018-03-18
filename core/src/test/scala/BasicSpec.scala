package scalapb.core3

import java.io.ByteArrayOutputStream

import org.scalatest.FlatSpec
import scalapb.macros._
import shapeless.Lazy

case class Other(
  @Optional(ProtoType.Int32, 1) other: Int,
  @Optional(ProtoType.String, 2) bother: String,
//  @Optional(ProtoType.Message, 2) inner: Other,
  @Optional(ProtoType.String, 2) mager: String = ""
) extends Msg[Other]

case class Bar(
  @Optional(ProtoType.Int32, 44) x: Int,
  @Optional(ProtoType.String, 39) y: String,
  @Optional(ProtoType.String, 75) z: Option[String],
//  @Optional(ProtoType.Message, 17) o: Other,
//  @Repeated(ProtoType.Int32, 75, false) w: Vector[Int] = Vector.empty,
//  @Repeated(ProtoType.Message, 77, false) rm: Vector[Other] = Vector.empty
) extends Msg[Bar] {
  var __cached_w = 0
  var __cached_rm = 0
}

case class Recursive(
  @Optional(ProtoType.Int32, 1) value: Option[Int],
  @Optional(ProtoType.Message, 2) next: Option[Recursive]
) extends Msg[Recursive]

class BasicSpec extends FlatSpec {
  "foo" should "bar" in {
//    implicit val i = Serializer[Other]
    val i: Serializer[Bar] = Serializer.makeSerializer[Bar]
    /*
    val k: Serializer[Recursive] = Serializer.makeSerializer[Recursive]
//    println(j.toByteArray(Other(34, "foo", "bar")))
    println(i.toByteArray(Bar(35, "foo", Some("koo"),
      Other(243, ""),
      Vector(1, 2, 3),
      rm = Vector(Other(35, "foo"))
    )).toVector)
    */
  }


}
