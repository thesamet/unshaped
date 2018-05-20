package scalapb.reflection

import com.google.protobuf.CodedOutputStream

import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

class MessageSerializer[T : TypeTag : ClassTag] {
  val methods: Array[universe.MethodSymbol] = typeOf[T].decls.collect {
    case m: MethodSymbol if m.isCaseAccessor => m
  }.toArray
  val serializers: Array[ftc.FieldSerializer[Any]] = methods.map(t => ftc.FieldSerializer.IntSerializer.asInstanceOf[ftc.FieldSerializer[Any]]).toArray
  val mirror = runtimeMirror(getClass.getClassLoader)

  def writeTo(cos: CodedOutputStream, instance: T): Unit = {
    val instanceMirror = mirror.reflect(instance)
    val sz = methods.size
    var i = 0
    while (i < sz) {
      serializers(i).serialize(cos, i + 1, instanceMirror.reflectMethod(methods(i)).apply())
      i += 1
    }
  }
}

object MessageSerializer {
  implicit def gen[T : TypeTag : ClassTag] = new MessageSerializer[T]
}

object TestReflection extends App {
  case class Person(x: Int, y: Int)

  val m = implicitly[MessageSerializer[Person]]

  m.writeTo(null, Person(35, 27))

}
