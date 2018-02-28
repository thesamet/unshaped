package scalapb.macros

import shapeless.{CaseClassMacros, HNil, Witness}

import scala.language.experimental.macros
import scala.annotation.{Annotation, ClassfileAnnotation, StaticAnnotation}
import scala.reflect.macros.{blackbox, whitebox}
import macrocompat.bundle
import shapeless._

trait SchemaHolder[T] {
  type FD
}

object SchemaHolder {

  type Aux[T, FD0] = SchemaHolder[T] { type FD = FD0 }

  def apply[T](implicit helper: SchemaHolder[T]): Aux[T, helper.FD] = helper

  implicit def makeHelper[T, R]: SchemaHolder[T] = macro Macros.mkHelper[T, R]
}

@bundle
class Macros(val c: whitebox.Context) extends CaseClassMacros with SingletonTypeUtils {
  import c.universe._

  def mkHelper[T : WeakTypeTag, R : WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val labelTpe: c.universe.Type = weakTypeOf[Label]
    val fieldTpe: c.universe.Type = weakTypeOf[Field[_, _, _]]

    val tags =
      tpe.member(termNames.CONSTRUCTOR).asMethod.paramLists.flatten.flatMap {
        sym =>
          sym.annotations.collect {
            case a if a.tree.tpe <:< labelTpe =>
              val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
              val tag = internal.constantType(c)
              appliedType(fieldTpe, a.tree.tpe, protoType.tpe, tag)
          }
      }

    val m = mkHListTpe(tags)

    val clsName = TermName(c.freshName("anon$"))
    q"""
      object $clsName extends SchemaHolder[$tpe] {
        type FD = $m
      }
      $clsName: SchemaHolder.Aux[$tpe, $clsName.FD]
    """
  }
}

sealed trait ProtoType {
  def wireType: Int
}

object ProtoType {
  case object Int32 extends ProtoType {
    override def wireType: Int = 1
  }

  case object Int64 extends ProtoType {
    override def wireType: Int = 2
  }

  case object String extends ProtoType {
    override def wireType: Int = 3
  }

  case object Message extends ProtoType {
    override def wireType: Int = 4
  }
}

sealed trait Label

case class Optional(protoType: ProtoType, tag: Int) extends StaticAnnotation with Label

case class Required(protoType: ProtoType, tag: Int) extends StaticAnnotation with Label

case class Repeated(protoType: ProtoType, tag: Int) extends StaticAnnotation with Label

sealed trait Field[LABEL <: Label, PT<:ProtoType, Tag]