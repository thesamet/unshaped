package scalapb.macros

import shapeless.{CaseClassMacros, HNil, Witness}

import scala.language.experimental.macros
import scala.annotation.{Annotation, ClassfileAnnotation, StaticAnnotation}
import scala.reflect.macros.{blackbox, whitebox}
import macrocompat.bundle
import shapeless._

trait Helper[T] {
  type FD
}

object Helper {

  type Aux[T, FD0] = Helper[T] { type FD = FD0 }

  def apply[T](implicit helper: Helper[T]): Aux[T, helper.FD] = helper

  implicit def makeHelper[T, R]: Helper[T] = macro Macros.mkHelper[T, R]
}

@bundle
class Macros(val c: whitebox.Context) extends CaseClassMacros with SingletonTypeUtils {
  import c.universe._

  def mkHelper[T : WeakTypeTag, R : WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val fieldTpe: c.universe.Type = weakTypeOf[Optional]
    val optionalAtt: c.universe.Type = weakTypeOf[OptionalAtt[_, _]]

    val tags =
      tpe.member(termNames.CONSTRUCTOR).asMethod.paramLists.flatten.flatMap {
        sym =>
          sym.annotations.collect {
            case a if a.tree.tpe =:= fieldTpe =>
              val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
              val tag = internal.constantType(c)
              appliedType(optionalAtt, protoType.tpe, tag)
          }
      }

    val m = mkHListTpe(tags)

    val clsName = TermName(c.freshName("anon$"))
    q"""
      object $clsName extends Helper[$tpe] {
        type FD = $m
      }
      $clsName: Helper.Aux[$tpe, $clsName.FD]
    """
  }
}

sealed trait ProtoType

object ProtoType {
  case object Int32 extends ProtoType

  case object Int64 extends ProtoType

  case object String extends ProtoType
}

case class Optional(protoType: ProtoType, tag: Int) extends StaticAnnotation

sealed trait OptionalAtt[PT<:ProtoType, Tag]