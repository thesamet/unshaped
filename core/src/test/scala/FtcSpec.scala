import com.google.protobuf.CodedOutputStream
import ftc.FieldSerializer
import org.scalatest.FlatSpec
import ftc.MessageSerializer
import scalapb.core3.Msg

case class MyMsg(x: String, y: Int)

case class Other(a: Int, msg: MyMsg)

case class Ref(a: Int, ref: Option[Ref]) extends Msg[Ref]

class FtcSpec extends FlatSpec {
  val refSer = ftc.MessageSerializer.gen[Ref]
  "simple derivcation" should "work" in {
//    val mymsgSer = ftc.Serializer.gen[MyMsg]
//    val otherSer = ftc.Serializer.gen[Other]

    println("XXX", refSer.toByteArray(Ref(4, Some(Ref(7, None)))).toVector)
  }
}
