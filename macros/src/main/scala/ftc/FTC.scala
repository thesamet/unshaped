package ftc

import java.io.ByteArrayOutputStream

import com.google.protobuf.CodedOutputStream

import scala.collection.mutable
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

object Deferred { def apply[T](method: String): T = ??? }

class FTC(val c: whitebox.Context) {
  import c.universe._

  val removeDeferred = new Transformer {
    override def transform(tree: Tree) = tree match {
      case q"ftc.Deferred.apply[$_](${Literal(Constant(method: String))})" =>
        println("TRASNFORMED!")
        q"this" // ${TermName(method)}"
      case _ =>
        super.transform(tree)
    }
  }


  def generateTree(genericType: c.Type, outTypeclass: c.Type, fieldTypeclass: c.Type): c.Tree = {
    import c.universe._
    val classType = genericType.typeSymbol.asClass

    if (classType.isCaseClass) {
      println(s"Case class for $genericType")
      val params: List[Symbol] = classType.primaryConstructor.asMethod.typeSignature.paramLists.head
      val inits = params.map {
        sym =>
          val innerType = sym.typeSignatureIn(genericType).finalResultType
          val searchType = appliedType(fieldTypeclass, innerType)
          println(s"----> for $searchType")
          val foundImplicit = c.inferImplicitValue(searchType)
          println(s"    foundImpliict: $foundImplicit")
          if (foundImplicit.isEmpty) {
            c.abort(c.enclosingPosition, s"failed for $searchType")
          }
          if (innerType =:= weakTypeOf[Int]) {
            q"val ${sym.name.toTermName}: ${searchType} = ${foundImplicit} // foo"
          } else {
            q"val ${sym.name.toTermName}: ${searchType} = ${foundImplicit} // foo"
          }
      }
      val clsName = c.universe.TypeName(c.freshName("anon"))
      val serStatements = params.map {
        p =>
          val innerType = p.typeSignatureIn(genericType).finalResultType
          val ser = TermName(p.name.decodedName.toString)
          println(s"$ser ${innerType} ${innerType =:= weakTypeOf[Int]}")
          q"${ser}.serialize(__cos, 1, __t.$ser)"
      }
      val serialize =
        q"""final def serialize(__cos: CodedOutputStream, __t: $genericType): Unit = {
           ..$serStatements
           }"""

      val serSizeStatements = params.map {
        p =>
          val ser = TermName(p.name.decodedName.toString)
          q"__size += $ser.serializedSize(1, __t.$ser)"
      }

      val serializedSize = q"""
         final def serializedSize(__t: $genericType): Int = {
           var __sizeArray = __t.__cachedSerializedSize
           if (__sizeArray == null) {
             __sizeArray = new Array[Int](3)
             var __size = 0
             ..$serSizeStatements
             __sizeArray(0) = __size
             __t.__cachedSerializedSize = __sizeArray
           }
           __sizeArray(0)
         }"""

      val r = q"""{
          final class $clsName extends ${appliedType(outTypeclass, genericType)} {
            ..$inits
            $serialize
            $serializedSize
          }
          new $clsName
         }
          """

      r
    } else {
      ???
    }
  }

  def deriveOrGet(
    stack: GlobalContext.Stack[c.type], termName: TermName, searchType: Type)(fn: => Tree): Tree = {
    stack.find(searchType) match {
      case Some(termName) =>
        println(s"Match for $searchType: $termName")
        val z = q"_root_.ftc.Deferred.apply[${searchType}](${termName.decodedName.toString})"
        z

//        if (stack.isEmpty) c.untypecheck(removeDeferred.transform(z))
//        else z
      //        q"${termName.decodedName.toString}"
      case None =>
        println(s"No match for $searchType, we have ${stack.cache}")
        stack.cache += searchType -> termName
        val tree = stack.enter(fn)

        val r = q"""{
          val ${termName}: $searchType = ${tree}
          ${termName}
        }"""
        r
    }
  }

  def findType(typeField: String): c.Type = {
    val prefixType = c.prefix.tree.tpe
    prefixType.baseClasses.flatMap { cls =>
      cls.asType.toType.decls.filter(_.isType).find(_.name.toString == typeField).map { tpe =>
        tpe.asType.toType.asSeenFrom(prefixType, cls)
      }
    }.headOption.getOrElse(c.abort(c.enclosingPosition, s"FTC: unable to find $typeField type in ${prefixType.typeSymbol}")).typeConstructor
  }



  def gen[T: c.WeakTypeTag]: c.Tree = GlobalContext.withContext(c) {
    stack =>
      import c.universe._
      val callsite = c.macroApplication.symbol

      val fieldTypeClass = findType("FieldTypeclass")
      val outTypeclass = findType("Typeclass")
      val genericType = c.weakTypeOf[T]
      val searchType = appliedType(outTypeclass, genericType)
      val z = deriveOrGet(stack, TermName(c.freshName("ftc")), searchType) {
        generateTree(genericType, outTypeclass, fieldTypeClass)
      }
      if (stack.isEmpty) {
        val t = c.untypecheck(removeDeferred.transform(z))
        println(t)
        t
      } else z
  }
}

abstract class FieldSerializer[@specialized T] {
  def serialize(cos: CodedOutputStream, tag: Int, value: T): Unit

  def serializedSize(tag: Int, value: T): Int

  def serializeNoTag(cos: CodedOutputStream, value: T): Unit

  def serializedSizeNoTag(value: T): Int
}

abstract class MessageSerializer[T] {
  def serializedSize(value: T): Int

  def serialize(cos: CodedOutputStream, value: T): Unit

  final def toByteArray(t: T): Array[Byte] = {
    val bytes = new Array[Byte](serializedSize(t))
    val cos = CodedOutputStream.newInstance(bytes)
    serialize(cos, t)
    bytes
  }
}

object MessageSerializer {
  type Typeclass[T] = MessageSerializer[T]

  type FieldTypeclass[T] = FieldSerializer[T]

  implicit def gen[T]: MessageSerializer[T] = macro FTC.gen[T]
}

object FieldSerializer {
  implicit val IntSerializer: FieldSerializer[Int] = new FieldSerializer[Int] {
    final override def serializedSize(tag: Int, value: Int): Int = CodedOutputStream.computeInt32Size(tag, value)

    final override def serializeNoTag(cos: CodedOutputStream, value: Int): Unit =
      cos.writeInt32NoTag(value)

    final override def serializedSizeNoTag(value: Int): Int = CodedOutputStream.computeInt32SizeNoTag(value)

    final override def serialize(cos: CodedOutputStream, tag: Int, value: Int): Unit =
      cos.writeInt32(tag, value)
  }

  implicit val StringSerializer: FieldSerializer[String] = new FieldSerializer[String] {
    final override def serializedSize(tag: Int, value: String): Int = CodedOutputStream.computeStringSize(tag, value)

    final override def serializeNoTag(cos: CodedOutputStream, value: String): Unit =
      cos.writeStringNoTag(value)

    final override def serializedSizeNoTag(value: String): Int = CodedOutputStream.computeStringSizeNoTag(value)

    final override def serialize(cos: CodedOutputStream, tag: Int, value: String): Unit =
      cos.writeString(tag, value)
  }

  implicit def optionalFieldSerializer[T](implicit fser: FieldSerializer[T]): FieldSerializer[Option[T]] =
    new FieldSerializer[Option[T]] {
      final override def serializedSize(tag: Int, value: Option[T]): Int =
        if (value.isEmpty) 0 else fser.serializedSize(tag, value.get)

      final override def serializeNoTag(cos: CodedOutputStream, value: Option[T]): Unit = ???

      final override def serializedSizeNoTag(value: Option[T]): Int = ???

      final override def serialize(cos: CodedOutputStream, tag: Int, value: Option[T]): Unit = {
        if (!value.isEmpty) {
          fser.serialize(cos, tag, value.get)
        } else {}
      }
    }

  implicit def messageFieldSerializer[T](implicit messageSer: MessageSerializer[T]): FieldSerializer[T] =
    new FieldSerializer[T] {
      final override def serializedSize(tag: Int, value: T): Int = {
        val sz = messageSer.serializedSize(value)
        CodedOutputStream.computeInt32Size(tag, sz) + sz
      }

      final override def serializeNoTag(cos: CodedOutputStream, value: T): Unit = {
        val sz = messageSer.serializedSize(value)
        cos.writeInt32NoTag(sz)
        messageSer.serialize(cos, value)
      }

      final override def serializedSizeNoTag(value: T): Int = {
        val sz = messageSer.serializedSize(value)
        CodedOutputStream.computeInt32SizeNoTag(sz) + sz
      }

      final override def serialize(cos: CodedOutputStream, tag: Int, value: T): Unit = {
        cos.writeTag(tag, 2)
        cos.writeInt32NoTag(messageSer.serializedSize(value))
        messageSer.serialize(cos, value)
      }
    }
}

object GlobalContext {
  class Stack[C <: whitebox.Context] {
    var depth = 0
    val cache = mutable.Map.empty[C#Type, C#TermName]

    def isEmpty = depth == 0

    def find(searchType: C#Type): Option[C#TermName] = cache.find(_._1 =:= searchType).map(_._2)

    def enter[T](fn: => T): T = {
      depth += 1
      try fn
      finally {
        depth -= 1
      }
    }
  }

  val stack = new Stack

  private val workSet = mutable.Set.empty[whitebox.Context#Symbol]

  def withContext[U](c: whitebox.Context)(fn: Stack[c.type] => U): U = {
    workSet.add(c.macroApplication.symbol)
    val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
    try {
      fn(stack.asInstanceOf[Stack[c.type]])
    } finally if (depth <= 1) {
      workSet.clear()
    }
  }
}
