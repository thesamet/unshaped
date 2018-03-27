package ftc

import com.google.protobuf.CodedOutputStream

import scala.collection.mutable
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

class FTC(val c: whitebox.Context) {
  import c.universe._

  class Stack[C <: whitebox.Context] {
    var depth = 0
    val cache = mutable.Map.empty[C#Type, C#TermName]

    /*
    def deriveOrGet[T <: C#Tree](searchType: C#Type)(fn: => T): T = {
      try {
        cache.get(searchType) match {
          case Some(result) => result
          case None => val derived = searchType(derived)
        }
        if (cache.contains(searchType))
        val result = cache.getOrElseUpdate(searchType, fn)
        result.asInstanceOf[T]
      } finally {
      }
    }
    */
  }

  val stack = new Stack[whitebox.Context]

  private val workSet = mutable.Set.empty[whitebox.Context#Symbol]

  def withContext[U](fn: Stack[c.type] => U): U = {
    workSet.add(c.macroApplication.symbol)
    val depth = c.enclosingMacros.count(m => workSet(m.macroApplication.symbol))
    try {
      fn(stack.asInstanceOf[Stack[c.type]])
    } finally if (depth <= 1) {
      workSet.clear()
    }
  }

  def deriveOrGet(genericType: c.Type, typeClass: c.Type): Option[c.Tree] = {
    val searchType = appliedType(typeClass, genericType)
    stack.cache.get(searchType) match {
      case Some(termName) => Some(q"${termName.toString}")
      case None =>
        val tn = TermName(c.freshName("ftc"))
        stack.cache += searchType -> tn

        val tcInstance =
          Option(c.inferImplicitValue(searchType))
            .filterNot(_.isEmpty)
            .orElse(generateTree(genericType, typeClass))
            .getOrElse(c.abort(c.enclosingPosition, s"ftc: could not find an implicit value for $searchType"))
        val r = q"""{
          lazy val ${tn} = $tcInstance
          ${tn}
        }"""
        println(r)
        Some(r)
    }
  }

  def generateTree(genericType: c.Type, typeClass: c.Type): Option[c.Tree] = {
    import c.universe._
    val classType = genericType.typeSymbol.asClass

    if (classType.isCaseClass) {
      println("Case class")
      val params: List[Symbol] = classType.primaryConstructor.asMethod.typeSignature.paramLists.head
      val inits = params.map {
        sym =>
          val innerType = sym.typeSignatureIn(genericType).finalResultType
          val searchType = appliedType(typeClass, innerType)
          val r = deriveOrGet(innerType, typeClass)
          q"lazy val ${sym.name.toTermName} = ${r}"
      }
      val clsName = c.universe.TypeName(c.freshName("anon"))
      val r = q"""{
          ..$inits
          class $clsName extends ${appliedType(typeClass, genericType)} {
          }
          new $clsName
         }
          """

      println(show(r))
      Some(r)
    } else {
      ???
    }
  }

  def gen[T: c.WeakTypeTag]: c.Tree = withContext {
    stack =>
      import c.universe._
      val callsite = c.macroApplication.symbol
      val prefixType = c.prefix.tree.tpe

      val typeClass = prefixType.baseClasses.flatMap { cls =>
        cls.asType.toType.decls.filter(_.isType).find(_.name.toString == "Typeclass").map { tpe =>
          tpe.asType.toType.asSeenFrom(prefixType, cls)
        }
      }.headOption.getOrElse(c.abort(c.enclosingPosition, s"FTC: unable to find Typeclass type in ${prefixType.typeSymbol}")).typeConstructor

      val genericType = c.weakTypeOf[T]
      deriveOrGet(genericType, typeClass).get
  }
}

class Serializer[T] {
  def serialize(cos: CodedOutputStream, tag: Int, value: T): Unit = {}
}

object Serializer {
  type Typeclass[T] = Serializer[T]
  implicit def gen[T]: Serializer[T] = macro FTC.gen[T]

  implicit val IntSerializer: Serializer[Int] = new Serializer[Int] {
    override def serialize(cos: CodedOutputStream, tag: Int, value: Int): Unit = {
      cos.writeInt32(tag, value)
    }
  }

  implicit def StringSerializer: Serializer[String] = new Serializer[String] {
    override def serialize(cos: CodedOutputStream, tag: Int,
      value: String): Unit = {
      cos.writeString(tag, value)
    }
  }
}
