package scalapb.benchmark

import java.util.concurrent.TimeUnit

import com.google.protobuf.CodedOutputStream
import org.openjdk.jmh.annotations._
import scalapb.{GeneratedMessage, core4}
import scalapb.gen.gen._
import scalapb.generic


@State(Scope.Benchmark)
class Protos {
  val message20 = Message20(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message19 = Message19(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message18 = Message18(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message17 = Message17(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message16 = Message16(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message15 = Message15(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message14 = Message14(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message13 = Message13(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message12 = Message12(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message11 = Message11(114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message10 = Message10(114, 114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message9  =  Message9(114, 114, 114, 114, 114, 114, 114, 114, 114)
  val message8  =  Message8(114, 114, 114, 114, 114, 114, 114, 114)
  val message7  =  Message7(114, 114, 114, 114, 114, 114, 114)
  val message6  =  Message6(114, 114, 114, 114, 114, 114)
  val message5  =  Message5(114, 114, 114, 114, 114)
  val message4  =  Message4(114, 114, 114, 114)
  val message3  =  Message3(114, 114, 114)
  val message2  =  Message2(114, 114)
  val message1  =  Message1(114)

  val all = Array(
    message1,
    message2,
    message3,
    message4,
    message5,
    message6,
    message7,
    message8,
    message9,
    message10,
    message11,
    message12,
    message13,
    message14,
    message15,
    message16,
    message17,
    message18,
    message19,
    message20
  )

  val sizes: Array[Int] = all.map(_.serializedSize)

  val byteArrays: Array[Array[Byte]] = sizes.map(new Array[Byte](_))

  @Param(Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  var messageIndex: Int = _

  case class SerializerAndMessage[T <: GeneratedMessage](instance: T)(
    implicit
    generatedSerializer: GeneratedSerializer[T],
    reflectionSerializer: scalapb.reflection.MessageSerializer[T],
    genericSerializer: generic.MessageSerializer[T],
    ftcSerializer: ftc.MessageSerializer[T]
  ) {
    def reflectionSerialize(cos: CodedOutputStream): Unit = { reflectionSerializer.writeTo(cos, instance) }
    def generatedSerialize(cos: CodedOutputStream): Unit = { generatedSerializer.serialize(cos, instance) }
    def genericSerialize(cos: CodedOutputStream): Unit = { genericSerializer.serialize(cos, instance) }
    def ftcSerialize(cos: CodedOutputStream): Unit = { ftcSerializer.serialize(cos, instance) }
  }


  val generics = Array(
    SerializerAndMessage(message1),
    SerializerAndMessage(message2),
    SerializerAndMessage(message3),
    SerializerAndMessage(message4),
    SerializerAndMessage(message5),
    SerializerAndMessage(message6),
    SerializerAndMessage(message7),
    SerializerAndMessage(message8),
    SerializerAndMessage(message9),
    SerializerAndMessage(message10),
    SerializerAndMessage(message11),
    SerializerAndMessage(message12),
    SerializerAndMessage(message13),
    SerializerAndMessage(message14),
    SerializerAndMessage(message15),
    SerializerAndMessage(message16),
    SerializerAndMessage(message17),
    SerializerAndMessage(message18),
    SerializerAndMessage(message19),
    SerializerAndMessage(message20)
  )

}

class SizeBenchmark {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeGeneratedSimple(state: Protos): Unit = {
    val gs = state.generics(state.messageIndex)
    val ba = state.byteArrays(state.messageIndex)
    val cos = CodedOutputStream.newInstance(ba)
    gs.generatedSerialize(cos)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeGenericSimple(state: Protos): Unit = {
    val gs = state.generics(state.messageIndex)
    val ba = state.byteArrays(state.messageIndex)
    val cos = CodedOutputStream.newInstance(ba)
    gs.genericSerialize(cos)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeFtcSimple(state: Protos): Unit = {
    val gs = state.generics(state.messageIndex)
    val ba = state.byteArrays(state.messageIndex)
    val cos = CodedOutputStream.newInstance(ba)
    gs.ftcSerialize(cos)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeReflectionSimple(state: Protos): Unit = {
    val gs = state.generics(state.messageIndex)
    val ba = state.byteArrays(state.messageIndex)
    val cos = CodedOutputStream.newInstance(ba)
    gs.reflectionSerialize(cos)
  }
}

class GeneratedSerializer[T <: GeneratedMessage] {
  def serialize(cos: CodedOutputStream, instance: T): Unit = instance.writeTo(cos)
}

object GeneratedSerializer {
  implicit def apply[T <: GeneratedMessage]: GeneratedSerializer[T] = new GeneratedSerializer[T]
}
