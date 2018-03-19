package scalapb.core4

import org.scalatest.FlatSpec

case class TestMessage(x: Int, y: String, z: String)

case class TestMessageRef(x: Int, other: TestMessage)

case class TestMessageRec(x: Int, next: Option[TestMessageRec])

case class TestMessageOutRec(rec: TestMessageRec)

class Core4Spec extends FlatSpec {
  def serialize[T](message: T)(implicit ser: MessageSerializer[T]) = ser.toByteArray(message)

  val rec = TestMessageRec(3, None)
  val outRec = TestMessageOutRec(rec)

  "core4" should "work" in {
    println(serialize(TestMessage(4, "foo", "bar")).toVector)
  }

  "ref" should "work" in {
    println(serialize(TestMessageRef(4, other = TestMessage(3, "foo", "bar"))).toVector)
  }

  "recursive" should "work" in {
    println(serialize(rec))
  }

  "out recursive" should "work" in {
    println(serialize(outRec))
  }
}
