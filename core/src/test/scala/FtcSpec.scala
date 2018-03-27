import com.google.protobuf.CodedOutputStream
import org.scalatest.FlatSpec

case class MyMsg(x: String, y: Int)

case class Other(a: Int, msg: MyMsg)

case class Ref(a: Int, ref: Ref)

class FtcSpec extends FlatSpec {
  "simple derivcation" should "work" in {
    val mymsgSer = ftc.Serializer.gen[MyMsg]
    val otherSer = ftc.Serializer.gen[Other]
//    val refSer = ftc.Serializer.gen[Ref]

  }


}
