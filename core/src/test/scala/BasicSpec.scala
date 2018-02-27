import org.scalatest.FlatSpec
import shapeless._

import scalapb.macros.ProtoType.Int32
import scalapb.macros._

sealed trait Msg

case class Bar(
  @Optional(Int32, 44) x: Int,
  @Optional(ProtoType.String, 39) y: String
) extends Msg

sealed trait Printer[M] {
  def print
}

object Printer {
  implicit val PrinterHNil: Printer[HNil] = new Printer[HNil] {
    override def print: Unit = {}
  }

  implicit def HListPrinter[PT <: ProtoType, TAG, TAIL <: HList](implicit ev: Witness.Aux[TAG], et: Witness.Aux[PT], tail: Printer[TAIL]): Printer[OptionalAtt[PT, TAG] :: TAIL] = new Printer[OptionalAtt[PT, TAG] :: TAIL] {
    override def print: Unit = {
      println(ev.value)
        println("  " + et.value)
      tail.print
    }
  }

  implicit def fromHelper[T <: Msg, R <: HList](implicit helper: Helper.Aux[T, R], pr: Printer[R]) = new Printer[T] {
    override def print: Unit = {
      pr.print
    }
  }
}

class BasicSpec extends FlatSpec {

  "foo" should "bar" in {
    val helper = Helper[Bar]
    val i = implicitly[Printer[Bar]].print
  }


}
