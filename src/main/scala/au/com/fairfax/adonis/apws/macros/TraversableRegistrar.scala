package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

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


  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[TraversableRegistrar[T]] = {
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

//    println(
//      s"""
//             |TraversableRegistrar.materialize[$tpe], result =
//             |$result
//           """.stripMargin)
        println(s"\n\n------------------------  stopping TraversableRegistrar: $tpe ---------------------------- t-length: ${result.toString.length}\n\n")
    c.Expr[TraversableRegistrar[T]](result)
  }

  private def alreadyRegisteredQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"au.com.fairfax.adonis.apws.macros.RegistrarDoesNothing[$tpe]()"
  }

  private def parserFormatterQuote(c: Context)(tpe: c.universe.Type): (c.universe.Tree, c.universe.Tree) =
    (ParserMaterializerImpl.parserQuote(c)(tpe), FormatterMaterializerImpl.formatterQuote(c)(tpe))

  private def yetToRegisterQuote[T: c.universe.WeakTypeTag](c: Context)(keyBeAdded: String) = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val (parser, formatter) = parserFormatterQuote(c)(tpe)

    q"""
      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar
      import au.com.fairfax.adonis.apws.macros.JsonRegistry
      import au.com.fairfax.adonis.apws.macros.JsonParser
      import au.com.fairfax.adonis.apws.macros.JsonFormatter
      
      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe}] {
        val traversableRegister: List[(String, JsonParser[_], JsonFormatter[_])] =
          ($keyBeAdded, $parser, $formatter) :: ${traverseChildrenQuote(c)(tpe)}
      }

      GenTraversableRegistrar
    """
  }

  private def traverseChildrenQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._

    lazy val accessors: List[MethodSymbol] = getAccessors(c)(tpe)

    tpe match {
      // a collection type
      case collectionTpe: Type if collTypes(c) contains tpeClassNm(c)(collectionTpe) =>
        val itemTpe = collectionTpe.typeArgs.head
        childParserFormatter(c)(itemTpe)

      // a map type
      case mapTpe: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(mapTpe) =>
        val List(keyTpe, valTpe) = mapTpe.dealias.typeArgs
        q"${childParserFormatter(c)(keyTpe)} ::: ${childParserFormatter(c)(valTpe)}"

      // an option type
      case optionTpe: Type if tpeClassNm(c)(typeOf[Option[_]]) == tpeClassNm(c)(optionTpe) =>
        val itemTpe = optionTpe.typeArgs.head
        childParserFormatter(c)(itemTpe)

      // an either type
      case eitherTpe: Type if tpeClassNm(c)(typeOf[Either[_, _]]) == tpeClassNm(c)(eitherTpe) =>
        val leftTpe = eitherTpe.dealias.typeArgs.head
        val rightTpe = eitherTpe.dealias.typeArgs.last
        q"${childParserFormatter(c)(leftTpe)} ::: ${childParserFormatter(c)(rightTpe)}"

      // a sealed trait
      case traitTpe: Type if traitTpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        getSealedTraitChildren(c)(traitTpe).withFilter(ct => !ct.typeSymbol.isModuleClass && !noAccessor(c)(ct)).map {
          childParserFormatter(c)(_)
        }.foldLeft[Tree](q"Nil") {
          (z, accessorQuote) => q"$z ::: $accessorQuote"
        }

      // a structured type
      case _ if accessors.nonEmpty =>
        accessors.map {
          accessor =>
            val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
            childParserFormatter(c)(accessorTpe)
        }.foldLeft[Tree](q"Nil") {
          (z, accessorQuote) => q"$z ::: $accessorQuote"
        }

      case _ => q"Nil"
    }
  }

  private def childParserFormatter(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val tpeName = toMapKey(tpe.toString)
    if (ParserFormatterTracker contains tpeName)
      q"Nil"
    else {
      ParserFormatterTracker add tpeName
      val (parser, formatter) = parserFormatterQuote(c)(tpe)
      q"($tpeName, $parser, $formatter) :: ${traverseChildrenQuote(c)(tpe)}"
    }
  }
}