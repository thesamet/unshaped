package scalapb.macros

import com.google.protobuf.CodedOutputStream
import shapeless.{CaseClassMacros, HNil, Witness}

import scala.language.experimental.macros
import scala.annotation.{ClassfileAnnotation, StaticAnnotation}
import scala.reflect.macros.{blackbox, whitebox}
import macrocompat.bundle
import shapeless._
import scalapb.core3.{FieldSerializer, PackedRepeatedFieldSerializer}

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

  def mkSerializer[T : WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    val fs = weakTypeOf[FieldSerializer[_, _]]
    val packedfs = weakTypeOf[PackedRepeatedFieldSerializer[_, _]]
    val iTpe  = weakTypeOf[Int]
    val members = tpe.member(termNames.CONSTRUCTOR).asMethod.paramLists.flatten.sortBy(
      m => extractTag(m.annotations.head)
    )
    val messageType = weakTypeOf[ProtoType.Message.type]

    val types = members
      .map {
        sym =>
          val innerType = sym.typeSignatureIn(tpe).finalResultType
          val pt = extractProtoType(sym.annotations.head)
          val ap = {
            val p = if (isRepeated(sym.annotations.head))
              appliedType(packedfs, pt, innerType)
            else
              appliedType(fs, pt, innerType)
            p
          }
          q"${TermName(sym.name.decodedName.toString)}: $ap"
      }

    def serializerSym(sym: Symbol): Tree = {
      val termName = TermName(sym.name.decodedName.toString)
      val pt = extractProtoType(sym.annotations.head)
      q"$termName"
    }

    def getOrUpdateCache(obj: Tree, sym: Symbol): Tree =  {
      val termName = TermName(sym.name.decodedName.toString)
      val tag = extractTag(sym.annotations.head)

        q"""{
              var __c = $obj
              if (__c == 0) {
                  __c = ${com.google.protobuf.CodedOutputStream.computeTagSize(tag)} + ${serializerSym(sym)}.serializedSizeNoTag(__v.${termName})
                  $obj = __c
              }
              __c
           }"""
    }

    val serStatements = members.map {
        sym =>
          val tag = extractTag(sym.annotations.head)
          val termName = TermName(sym.name.decodedName.toString)

          if (isRepeated(sym.annotations.head)) {
            q"${serializerSym(sym)}.serializeWithKnownSize(__cos, $tag, __size(1), __v.$termName)"
          } else {
            q"${serializerSym(sym)}.serialize(__cos, $tag, __v.$termName)"
          }
      }

    val serSize = members.map {
        sym =>
          val tag = extractTag(sym.annotations.head)
          val termName = TermName(sym.name.decodedName.toString)
          val cacheName = TermName("__cached_" + sym.name.decodedName.toString)
          if (isRepeated(sym.annotations.head)) {
            q"""__size(0) += ${getOrUpdateCache(q"__size(1)", sym)}"""
          } else {
            q"__size(0) += ${com.google.protobuf.CodedOutputStream.computeTagSize(tag)} + ${serializerSym(sym)}.serializedSizeNoTag(__v.$termName)"
          }
      }

    val clsName = TypeName(c.freshName("anon$"))
    val f = q"""
       class $clsName(implicit ..$types) extends _root_.scalapb.core3.Serializer[$tpe] {
         def serialize(__cos: _root_.com.google.protobuf.CodedOutputStream, __v: $tpe): Unit = {
           val __size = __v.__cachedSerializedSize
           ..$serStatements
         }

         def serializedSize(__v: $tpe): Int = {
           var __size = __v.__cachedSerializedSize
           if (__size == null) {
             __size = new Array[Int](3)
             ..$serSize
             __v.__cachedSerializedSize = __size
           }
           __size(0)
         }
       }
       new $clsName
     """
    println(f)
    f
  }

  val repeatedTpe: c.universe.Type = weakTypeOf[Repeated]
  val optionalTpe: c.universe.Type = weakTypeOf[Optional]
  val requiredTpe: c.universe.Type = weakTypeOf[Required]

  def isRepeated(x: Annotation): Boolean = x match {
    case a if a.tree.tpe =:= repeatedTpe =>
      true
    case a if a.tree.tpe <:< optionalTpe =>
      false
    case a if a.tree.tpe <:< requiredTpe =>
      false
  }

  def extractTag(x: Annotation): Int = x match {
    case a if a.tree.tpe =:= repeatedTpe =>
      val List(protoType, Literal(Constant(n: Int)), Literal(Constant(packed: Boolean))) = a.tree.children.tail
      n
    case a if a.tree.tpe <:< optionalTpe =>
      val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
      n
    case a if a.tree.tpe <:< requiredTpe =>
      val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
      n
  }

  def extractProtoType(x: Annotation): Type = x match {
    case a if a.tree.tpe =:= repeatedTpe =>
      val List(protoType, Literal(Constant(n: Int)), Literal(Constant(packed: Boolean))) = a.tree.children.tail
      protoType.tpe
    case a if a.tree.tpe <:< optionalTpe =>
      val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
      protoType.tpe
    case a if a.tree.tpe <:< requiredTpe =>
      val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
      protoType.tpe
  }

  def mkHelper[T : WeakTypeTag, R : WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]

    val optionalTypeTpe: c.universe.Type = weakTypeOf[OptionalField[_, _]]
    val requiredTypeTpe: c.universe.Type = weakTypeOf[RequiredField[_, _]]
    val repeatedTypeTpe: c.universe.Type = weakTypeOf[RepeatedField[_, _, _]]
    val cTrue: c.universe.Type = weakTypeOf[CTrue]
    val cFalse: c.universe.Type = weakTypeOf[CFalse]

    val tags =
      tpe.member(termNames.CONSTRUCTOR).asMethod.paramLists.flatten.flatMap {
        sym =>
          sym.annotations.collect {
            case a if a.tree.tpe =:= repeatedTpe =>
              val List(protoType, Literal(c@ Constant(_)), Literal(Constant(packed: Boolean))) = a.tree.children.tail
              val tag = internal.constantType(c)
              appliedType(repeatedTypeTpe, protoType.tpe, tag, if (packed) cTrue else cFalse)
            case a if a.tree.tpe <:< optionalTpe =>
              val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
              val tag = internal.constantType(c)
              appliedType(optionalTypeTpe, protoType.tpe, tag)
            case a if a.tree.tpe <:< requiredTpe =>
              val List(protoType, Literal(c@ Constant(n: Int))) = a.tree.children.tail
              val tag = internal.constantType(c)
              appliedType(requiredTypeTpe, protoType.tpe, tag)
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
    override def wireType: Int = 2
  }

  case object Message extends ProtoType {
    override def wireType: Int = 4
  }
}

sealed trait CBool

sealed trait CTrue extends CBool

sealed trait CFalse extends CBool

sealed trait Label

case class Optional(protoType: ProtoType, tag: Int) extends StaticAnnotation with Label

case class Required(protoType: ProtoType, tag: Int) extends StaticAnnotation with Label

case class Repeated(protoType: ProtoType, tag: Int, isPacked: Boolean) extends StaticAnnotation with Label


sealed trait FieldType

sealed trait RequiredField[PT <: ProtoType, Tag <: Int] extends FieldType

sealed trait OptionalField[PT <: ProtoType, Tag <: Int] extends FieldType

sealed trait RepeatedField[PT <: ProtoType, Tag <: Int, Packed <: CBool] extends FieldType
