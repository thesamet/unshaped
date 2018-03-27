package scalapb.magnolia

import com.google.protobuf.CodedOutputStream
import org.scalatest.FlatSpec

case class MyMsg(a: String, b: Int)

case class RefX(a: String, y: RefY)

case class RefY(a: Int, b: RefX)

class MagnoliaSpec extends FlatSpec {
  val d = implicitly[MagnoliaSerializer[MyMsg]]
  val refD = implicitly[MagnoliaSerializer[RefX]]
  val msg = MyMsg("x", 4)

  "simple derivation" should "work" in {
    val arr = new Array[Byte](5)
    val cs = CodedOutputStream.newInstance(arr)
    d.serialize(cs, msg)
    cs.checkNoSpaceLeft()
    println(arr.toVector)
  }


}
