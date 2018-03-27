package scalapb.benchmark

import scalapb.core._
import scalapb.macros._
import java.util.concurrent.TimeUnit

import com.google.protobuf.CodedOutputStream
import org.openjdk.jmh.annotations._
import shapeless._
import scalapb.core3.{Int32Serializer, Msg}
import scalapb.macros.ProtoType.Int32
import scalapb.magnolia.MagnoliaSerializer

case class SimpleMessage(
//  @Optional(Int32, 44) x: Int,
  @Optional(ProtoType.String, 21) x: String,
  @Optional(ProtoType.String, 39) y: String,
  @Optional(ProtoType.String, 47) z: String,
  @Optional(ProtoType.Int32, 15) i: Int,
  @Repeated(ProtoType.Int32, 19, false) r: Vector[Int] = Vector.empty
) extends Msg[SimpleMessage] {
  self =>

  var __cached_r: Int = 0

//  val serializer = implicitly[scalapb.core2.Serializer[SimpleMessage]]

  /*
  def toByteArray = {
    if (__cachedSerializedSize == 0) {
      __cachedSerializedSize = serializer.serializedSize(this)
    }
    val a = new Array[Byte](21)
    val outputStream = CodedOutputStream.newInstance(a)
    serializer.serialize(outputStream, this)
    outputStream.checkNoSpaceLeft()
    a
  }
  */
}

case class SII(
  @Optional(ProtoType.String, 1) x: String,
  @Optional(ProtoType.String, 2) y: String,
  @Optional(ProtoType.Int32, 3) z: Int,
  @Optional(ProtoType.Int32, 4) w: Int) extends Msg[SII] {

}

/*
class MyManualSerializer(
  implicit
  fs1: scalapb.core3.FieldSerializer[String],
  fs2: scalapb.core3.FieldSerializer[String],
  fs3: scalapb.core3.FieldSerializer[String],
  fi: scalapb.core3.FieldSerializer[Int]) extends scalapb.core3.Serializer[SimpleMessage] {


  final def serializedSize(m: SimpleMessage): Int = {
    var r = m.__cachedSerializedSize
    if (r == 0) {
      r = fs1.serializedSize(17, m.x) + fs2.serializedSize(18, m.y) + fs3.serializedSize(19, m.z) + fi.serializedSize(20, m.i)
      m.__cachedSerializedSize = r
    }
    r
  }

  final def serialize(cos: CodedOutputStream, m: SimpleMessage) = {
    /*
    sf(cos, 16, m.i)
    sf(cos, 21, m.x)
    sf(cos, 35, m.y)
    sf(cos, 17, m.z)
    */
    fi.serialize(cos, 16, m.i)
    fs1.serialize(cos, 21, m.x)
    fs2.serialize(cos, 35, m.y)
    fs3.serialize(cos, 17, m.z)
  }

  def sf[@specialized T](cos: CodedOutputStream, tag: Int, v: T)(implicit fe: scalapb.core3.FieldSerializer[T]) = fe.serialize(cos, tag, v)
}

final class SpecificSerializer extends MyManualSerializer()(
  new scalapb.core3.StringSerializer,
  new scalapb.core3.StringSerializer,
  new scalapb.core3.StringSerializer,
  new scalapb.core3.Int32Serializer
) {
//  override final def f1: scalapb.core3.FieldSerializer[String] = new scalapb.core3.StringSerializer(35)
//  override final def f2: scalapb.core3.FieldSerializer[String] = new scalapb.core3.StringSerializer(21)
//  override final def f3: scalapb.core3.FieldSerializer[String] = new scalapb.core3.StringSerializer(17)
}
*/


object SimpleMessage {
  final val cst = "poo" :: "foobar" :: "pak" :: 57 :: HNil

  implicit val fg: FakeGeneric[SimpleMessage, String :: String :: String :: Int :: HNil] = new FakeGeneric[SimpleMessage, String :: String :: String :: Int :: HNil] {
    override def to(t: SimpleMessage) = cst
  }

  type SimpleMessageType = (SimpleMessage => String) :: (SimpleMessage => String) :: (SimpleMessage => String) :: (SimpleMessage => Int) :: HNil

  val res: SimpleMessageType = ((f:SimpleMessage) => f.x) ::((f:SimpleMessage) => f.y) ::((f:SimpleMessage) => f.z) ::((f:SimpleMessage) => f.i) :: HNil

  implicit val fieldExtractors:FieldExtractors.Aux[SimpleMessage, SimpleMessageType] = new FieldExtractors[SimpleMessage] {
    override def extractors: SimpleMessageType = res

    type G = SimpleMessageType
  }
}

@State(Scope.Benchmark)
class UnshapedState {
  val s = implicitly[scalapb.core.MessageSerializer[SimpleMessage]]
//  val s2 = implicitly[scalapb.core2.Serializer[SimpleMessage]]
//  val s3: scalapb.core3.Serializer[SimpleMessage] = new SpecificSerializer
//  scalapb.core2.SerializerBuilder.buildBuilder[SimpleMessage]
  val r = Vector(2,1,9,17,35,47,16,8)
  val msg = SimpleMessage(x = "poo", y = "foobar", z = "pak", i = 17, r = r)
  val genMsg = scalapb.gen.gen.SimpleMessageGen(x = "poo", y = "foobar", z = "pak", i = 17, r = r)
  val macroSer = scalapb.core3.Serializer.makeSerializer[SimpleMessage]
  val macroSII = scalapb.core3.Serializer.makeSerializer[SII]
  val shapelessSII = scalapb.core4.MessageSerializer[SII]
  val magnoliaSII= MagnoliaSerializer.gen[SII]
  val sii = SII("foo", "bar", 35, 17)
  println("XXX S3", macroSer.serializedSize(msg))
  println("XXX",   genMsg.toByteArray.map(_.toInt).toVector)
  println("MSR",   macroSer.toByteArray(msg).map(_.toInt).toVector)
  println("YYY", genMsg.serializedSize)
  println("MSR csize",   msg.__cachedSerializedSize.toVector)
}

class BenchmarkProtos {

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeGenSimple(state: UnshapedState): Unit = {
    state.genMsg.toByteArray
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeGenSimpleNew(state: UnshapedState): Unit = {
    val genMsg = scalapb.gen.gen.SimpleMessageGen(x = "poo", y = "foobar", z = "pak", i = 17, r = state.r)
    genMsg.toByteArray
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeUnshaped4(state: UnshapedState): Unit = {
    state.macroSer.toByteArray(state.msg)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeUnshaped4New(state: UnshapedState): Unit = {
    //    val msg = SimpleMessage(x = "poo", y = "foobar", z = "pak")
    val msg = SimpleMessage(x = "poo", y = "foobar", z = "pak", i = 17, r = state.r)

    state.macroSer.toByteArray(msg)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeMacroSII(state: UnshapedState): Unit = {

    val sii = SII("foo", "bar", 35, 17)
    state.macroSII.toByteArray(sii)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeShapelessSII(state: UnshapedState): Unit = {
    state.shapelessSII.toByteArray(state.sii)

//    state.macroSII.toByteArray(msg)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.NANOSECONDS)
  def serializeMagnoliaSII(state: UnshapedState): Unit = {
    val arr = new Array[Byte](14)
    val cs = CodedOutputStream.newInstance(arr)
    state.magnoliaSII.serialize(cs, state.sii)
    cs.checkNoSpaceLeft()
  }
}
