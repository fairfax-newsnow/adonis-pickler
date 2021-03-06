package au.com.fairfax.pickler.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import au.com.fairfax.pickler.types._

/**
 * Singleton to keep track if the parser/formatter are already generated
 */
object ParserFormatterTracker {
  var registeredTypes: Set[String] = Set.empty

  def add(t: String): Unit =
    registeredTypes += t

  def contains(t: String): Boolean =
    registeredTypes contains t
}

case class RegistrarDoesNothing[T]() extends TraversableRegistrar[T] {
  def traversableRegister: List[(String, JsonParser[_], JsonFormatter[_])] = Nil
}

trait TraversableRegistrar[T] {
  def traversableRegister: List[(String, JsonParser[_], JsonFormatter[_])]
}

object TraversableRegistrar {
  implicit def registerHelperMacros[T]: TraversableRegistrar[T] = macro materialize[T]


  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[TraversableRegistrar[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val tpeStr = toMapKey(tpe.toString)
    
    val result =
      if (ParserFormatterTracker contains tpeStr)
        alreadyRegisteredQuote(c)(tpe)
      else {
        ParserFormatterTracker add tpeStr
        yetToRegisterQuote[T](c)(tpeStr)
      }

    c.Expr[TraversableRegistrar[T]](result)
  }

  private def alreadyRegisteredQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"au.com.fairfax.pickler.macros.RegistrarDoesNothing[$tpe]()"
  }

  private def parserFormatterQuote(c: Context)(tpe: c.universe.Type): (c.universe.Tree, c.universe.Tree) =
    (ParserMaterializerImpl.parserQuote(c)(tpe), FormatterMaterializerImpl.formatterQuote(c)(tpe))

  private def yetToRegisterQuote[T: c.WeakTypeTag](c: Context)(keyBeAdded: String) = {
    import c.universe._
    val tpe = weakTypeOf[T]

    q"""
      import au.com.fairfax.pickler.macros.{TraversableRegistrar, JsonParser, JsonFormatter}

      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe}] {
        val traversableRegister: List[(String, JsonParser[_], JsonFormatter[_])] =
          ${traverseQuote(c)(tpe)(keyBeAdded)}
      }

      GenTraversableRegistrar
    """
  }

  private def traverseQuote(c: Context)(tpe: c.universe.Type)(keyBeAdded: String): c.universe.Tree = {
    import c.universe._

    lazy val accessors: List[MethodSymbol] = getAccessors(c)(tpe)
    lazy val (parser, formatter) = parserFormatterQuote(c)(tpe)

    tpe match {
      case t: Type if t <:< c.mirror.typeOf[Enum] =>
        q"List(($keyBeAdded, $parser, $formatter))"

      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        q"List(($keyBeAdded, $parser, $formatter))"

      // string type
      case t: Type if deliasTpeName[String](c) == t.dealias.toString =>
        q"List(($keyBeAdded, $parser, $formatter))"

      // boolean type
      case t: Type if deliasTpeName[Boolean](c) == t.dealias.toString =>
        q"List(($keyBeAdded, $parser, $formatter))"

      // a collection type
      case collectionTpe: Type if collTypes(c) contains tpeClassNm(c)(collectionTpe) =>
        val itemTpe = collectionTpe.typeArgs.head
        q"($keyBeAdded, $parser, $formatter) :: ${childParserFormatter(c)(itemTpe)}"

      // a map type
      case mapTpe: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(mapTpe) =>
        val List(keyTpe, valTpe) = mapTpe.dealias.typeArgs
        q"($keyBeAdded, $parser, $formatter) :: ${childParserFormatter(c)(keyTpe)} ::: ${childParserFormatter(c)(valTpe)}"

      // an option type
      case optionTpe: Type if tpeClassNm(c)(typeOf[Option[_]]) == tpeClassNm(c)(optionTpe) =>
        val itemTpe = optionTpe.typeArgs.head
        q"($keyBeAdded, $parser, $formatter) :: ${childParserFormatter(c)(itemTpe)}"

      // an either type
      case eitherTpe: Type if tpeClassNm(c)(typeOf[Either[_, _]]) == tpeClassNm(c)(eitherTpe) =>
        val leftTpe = eitherTpe.dealias.typeArgs.head
        val rightTpe = eitherTpe.dealias.typeArgs.last
        q"($keyBeAdded, $parser, $formatter) :: ${childParserFormatter(c)(leftTpe)} ::: ${childParserFormatter(c)(rightTpe)}"

      // a sealed trait
      case traitTpe: Type if traitTpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        val childrenQuote = getSealedTraitChildren(c)(traitTpe).map {
          childParserFormatter(c)(_)
        }.foldLeft[Tree](q"Nil") {
          (z, accessorQuote) => q"$z ::: $accessorQuote"
        }
        q"($keyBeAdded, $parser, $formatter) :: $childrenQuote"

      // a structured type
      case t: Type if isCaseClass(c)(t) && accessors.nonEmpty =>
        val accessorsQuote = accessors.map {
          accessor =>
            val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
            childParserFormatter(c)(accessorTpe)
        }.foldLeft[Tree](q"Nil") {
          (z, accessorQuote) => q"$z ::: $accessorQuote"
        }
        q"($keyBeAdded, $parser, $formatter) :: $accessorsQuote"

      // empty case class
      case t: Type if isCaseClass(c)(t) =>
        q"List(($keyBeAdded, $parser, $formatter))"

      // a case object
      case caseObjTpe: Type if caseObjTpe.typeSymbol.isModuleClass =>
        q"List(($keyBeAdded, $parser, $formatter))"

      // Any, AnyRef, non-sealed trait, abstract class
      case _ =>
        q"Nil"
    }
  }

  private def childParserFormatter(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val tpeName = toMapKey(tpe.toString)
    if (ParserFormatterTracker contains tpeName)
      q"Nil"
    else {
      ParserFormatterTracker add tpeName
      traverseQuote(c)(tpe)(tpeName)
    }
  }
}