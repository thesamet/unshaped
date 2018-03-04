package scalapb.benchmark

import scalapb.core._
import scalapb.macros._
import scalapb.macros.ProtoType._
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

case class SimpleMessage(
  @Optional(Int32, 44) x: Int,
  @Optional(ProtoType.String, 39) y: String
) extends Msg

@State(Scope.Benchmark)
class UnshapedState {
  val s = implicitly[scalapb.core.Serializer[SimpleMessage]]
  val msg = SimpleMessage(125, "foobar")
  val genMsg = scalapb.gen.gen.SimpleMessageGen(x = 125, y = "foobar")

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
