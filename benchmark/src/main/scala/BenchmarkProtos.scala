package scalapb.benchmark

import scalapb.core._
import scalapb.macros._
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import shapeless._

import scalapb.macros.ProtoType.Int32

case class SimpleMessage(
//  @Optional(Int32, 44) x: Int,
  @Optional(ProtoType.String, 21) x: String,
  @Optional(ProtoType.String, 39) y: String,
  @Optional(ProtoType.String, 47) z: String
) extends Msg[SimpleMessage] {
  self =>

  /*
  */
}


object SimpleMessage {
  final val cst = "poo" :: "foobar" :: "pak" :: HNil

  implicit val fg: FakeGeneric[SimpleMessage, String :: String :: String :: HNil] = new FakeGeneric[SimpleMessage, String :: String :: String :: HNil] {
    override def to(t: SimpleMessage) = cst
  }
}

@State(Scope.Benchmark)
class UnshapedState {
  val s = implicitly[scalapb.core.MessageSerializer[SimpleMessage]]
  val msg = SimpleMessage(x = "poo", y = "foobar", z = "pak")
  val genMsg = scalapb.gen.gen.SimpleMessageGen(x = "poo", y = "foobar", z = "pak")
  println("XXX" ,s.serializedSize(msg))
  println("YYY", genMsg.serializedSize)
}

class BenchmarkProtos {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeUnshapedSimple(state: UnshapedState): Unit = {
    state.s.toByteArray(state.msg)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeGenSimple(state: UnshapedState): Unit = {
    state.genMsg.toByteArray
  }

}
