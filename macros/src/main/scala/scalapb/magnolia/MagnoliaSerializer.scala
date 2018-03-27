package scalapb.magnolia

import com.google.protobuf.CodedOutputStream
import magnolia.{CaseClass, Magnolia, SealedTrait, debug}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox


trait MagnoliaSerializer[@specialized T] {
  def serialize(cos: CodedOutputStream, value: T): Unit
}

object MagnoliaSerializer {
  type Typeclass[T] = MagnoliaSerializer[T]

  def combine[T](ctx: CaseClass[MagnoliaSerializer, T]): MagnoliaSerializer[T] = {
    println("in combine")
    val paramsLength = ctx.parameters.length
    val params = ctx.parameters
    new MagnoliaSerializer[T] {
      def serialize(cos: CodedOutputStream, value: T): Unit = {
        var i = 0
        while (i < paramsLength) {
          val p = params(i)
          p.typeclass.serialize(cos, p.dereference(value))
          i += 1
        }
      }
    }
  }


  def dispatch[T](ctx: SealedTrait[Typeclass, T]): MagnoliaSerializer[T] = new Typeclass[T] {
    override def serialize(cos: CodedOutputStream, value: T): Unit = {}
  }

  @debug() implicit def gen[T]: MagnoliaSerializer[T] = macro hack[T]

  implicit val intSerializer: MagnoliaSerializer[Int] = new Typeclass[Int] {
    override def serialize(cos: CodedOutputStream, value: Int): Unit = {
      cos.writeInt32(1, value)
    }
  }

  implicit val strSerializer: MagnoliaSerializer[String] = new Typeclass[String] {
    override def serialize(cos: CodedOutputStream,
      value: String): Unit = cos.writeString(2, value)
  }

  def hack[T : c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    val z = Magnolia.gen(c)
    println(z.toString().replace("$6", "***$6***"))
    z
  }
}
