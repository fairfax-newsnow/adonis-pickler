
package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds
import au.com.fairfax.adonis.apws.base.JsSerialisable
import au.com.fairfax.adonis.utils.simpleTypeNm
import au.com.fairfax.adonis.apws.types.Enum

object Materializer {
  def itemMethNm(typeName: String): String =
    ("handle" :: List(typeName).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(simpleTypeNm).flatMap(_ split "\\.")).mkString("_")

}

trait Materializer[FP[_] <: FormatterParser[_]] {

  import Materializer._

  /**
   * @return "reader" if this is parser, o.w. "builder"
   */
  def jsonIo(c: Context): c.universe.TermName

  def tpeClassNm(c: Context): c.universe.Type => String = _.dealias.typeSymbol.asClass.name.toString

  def collTypes(c: Context) = {
    import c.universe._
    List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map tpeClassNm(c)
  }

  def deliasTpeName[T: c.universe.TypeTag](c: Context): String =
    c.universe.typeOf[T].dealias.toString

  def numDealisTpeNms(c: Context) =
    List(deliasTpeName[Double](c), deliasTpeName[Float](c), deliasTpeName[Short](c), deliasTpeName[Int](c), deliasTpeName[Long](c))

  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: c.universe.TermName): c.universe.Tree

  /**
   * Quote for a method definition that handle a map, it will be something like
   * def handleMap(...) = {...}
   */
  def handleMapQuote(c: Context)(handleMapMeth: c.universe.TermName)(kvTpes: (c.universe.Type, c.universe.Type))(kvMeths: (c.universe.TermName, c.universe.TermName))(itemQuotes: List[c.universe.Tree]): c.universe.Tree

  def getAccessors(c: Context)(tpe: c.universe.Type): List[c.universe.MethodSymbol] = {
    import c.universe._
    tpe.decls.collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }.toList
  }

  def hasNoAccessor(c: Context)(tpe: c.universe.Type): Boolean =
    getAccessors(c)(tpe).isEmpty

  def collectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree

  def optionQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree

  def eitherQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(fieldNm: String): c.universe.Tree

  /**
   * Quote to handle a numeric value
   */
  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree

  /**
   * Quote to handle a boolean value
   */
  def booleanQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree

  /**
   * Quote to handle a string value
   */
  def stringQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree

  /**
   * The quote for checking if varOfNullCheck is null.
   */
  def quoteForNullCheck(c: Context)(varOfNullCheck: c.universe.TermName): c.universe.Tree

  /**
   * The quote for the condition when the involved variable is null.
   */
  def quoteForNullVar(c: Context): c.universe.Tree

  /**
   * A func template that creates a quote to do null check on a var, and executes correspondingly upon different condition.
   */
  def quoteWithNullCheck(c: Context)(varOfNullCheck: c.universe.TermName)(quoteForNonNullVar: => c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""
      if (${ quoteForNullCheck(c)(varOfNullCheck) })
        ${quoteForNullVar(c)}
      else
        $quoteForNonNullVar
    """
  }

  /**
   * Quote to handle fieldNm on objNm
   */
  def fieldQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree

  def caseObjQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree

  def caseClassItemQuote(c: Context)(method: String)(ct: c.universe.Type)(fieldNm: String): c.universe.Tree

  def caseClassHandlerQuote(c: Context)(method: String)(objNm: String): c.universe.Tree

  def ptnToHandlerQuote(c: Context)(ct: c.universe.Type)(handlerQuote: c.universe.Tree)(pattern: String): c.universe.Tree

  def ptnMatchQuote(c: Context)(onlyCaseObjects: Boolean)(ptnToHandlerQuotes: Set[c.universe.Tree])(objNm: String): c.universe.Tree

  def nullHandlerQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(methodNm: String)(quote: c.universe.Tree): c.universe.Tree

  def sealedTraitQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(methodNm: String): c.universe.Tree = {
    import c.universe._

    val childTypes = tpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sealedDescendants.filterNot {
      des => des.isSealed || tpeClassNm(c)(tpe) == tpeClassNm(c)(des.asInstanceOf[Symbol].asType.toType)
    }.map(_.asInstanceOf[Symbol].asType.toType)

    val onlyCaseObjects = childTypes forall hasNoAccessor(c)

    val (itemQuotes, ptnToHandlerQuotes) = childTypes.map {
      ct =>
        val pattern = simpleTypeNm(ct.toString)
        val method = itemMethNm(pattern)
        val (iQuote, handlerQuote) =
          if (hasNoAccessor(c)(ct))
            (caseObjQuote(c)(ct)(method)(onlyCaseObjects), q"${TermName(method)}")
          else
            (caseClassItemQuote(c)(method)(ct)(fieldNm), caseClassHandlerQuote(c)(method)(objNm))
        (iQuote, ptnToHandlerQuote(c)(ct)(handlerQuote)(pattern))
    }.unzip

    val nonNullQuote =
      q"""
        ..$itemQuotes
        ${ptnMatchQuote(c)(onlyCaseObjects)(ptnToHandlerQuotes)(objNm)}
      """

    nullHandlerQuote(c)(tpe)(objNm)(methodNm)(q"${quoteWithNullCheck(c)(varOfNullCheck = objNm)(nonNullQuote)}")
  }

  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree

  /**
   * Quote to handle an enum object
   */
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree

  def recurQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(firstRun: Boolean): c.universe.Tree = {
    import c.universe._
    val jsSerialisable: c.universe.Type = c.mirror.typeOf[JsSerialisable]

    tpe match {
      // an enum type represented by au.com.fairfax.adonis.apws.types.Enum
      case t: Type if t <:< c.mirror.typeOf[Enum] =>
        enumObjQuote(c)(t)(objNm)(fieldNm)

      case t: Type if t <:< jsSerialisable && !firstRun =>
        jsSerialisableQuote(c)(t)(objNm)(fieldNm)

      // a numeric type
      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        numericValQuote(c)(t)(objNm)(fieldNm)

      // string type
      case t: Type if deliasTpeName[String](c) == t.dealias.toString =>
        stringQuote(c)(objNm)(fieldNm)

      // boolean type
      case t: Type if deliasTpeName[Boolean](c) == t.dealias.toString =>
        booleanQuote(c)(objNm)(fieldNm)

      // a collection type
      case t: Type if collTypes(c) contains tpeClassNm(c)(t) =>
        val handleMeth = "handleCollection"
        q"""
          ${collectionQuote(c)(t.typeArgs.head)(tpeClassNm(c)(t))(handleMeth)}
          ${TermName(handleMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a map type
      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        val (List(keyTpe, valTpe), List(keyMeth, valMeth)) = t.dealias.typeArgs.map {
          t => (t, itemMethNm(t.toString))
        }.unzip
        val itemQuotes = itemQuote(c)(keyTpe)(keyMeth) :: {
          if (keyTpe != valTpe) List(itemQuote(c)(valTpe)(valMeth))
          else Nil
        }
        val methName = TermName("handleMap")
        val defMethQuote = handleMapQuote(c)(methName)((keyTpe, valTpe))((keyMeth, valMeth))(itemQuotes)
        q"""
          $defMethQuote
          $methName(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // an option type
      case t: Type if tpeClassNm(c)(typeOf[Option[_]]) == tpeClassNm(c)(t) =>
        val handleMeth = "handleOption"
        q"""
          ${optionQuote(c)(t.typeArgs.head)(handleMeth)}
          ${TermName(handleMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // an either type
      case t: Type if tpeClassNm(c)(typeOf[Either[_, _]]) == tpeClassNm(c)(t) =>
        val handleMeth = "handleEither"
        q"""
           ${eitherQuote(c)(t)(handleMeth)(fieldNm: String)}
           ${TermName(handleMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a sealed trait
      case t: Type if t.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        val handleMeth = itemMethNm(t.toString + "_traitFamily")
        q"""
          ${sealedTraitQuote(c)(t)(objNm)(fieldNm)(handleMeth)}
          ${TermName(handleMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a structured type
      case _ =>
        val accessors = getAccessors(c)(tpe)
        accessors match {
          case x :: _ =>
            val accessorQuotes = accessors map {
              accessor =>
                val accessorField = accessor.name.toString
                val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
                eachAccessorQuote(c)(accessorTpe)(objNm)(fieldNm)(accessorField)
            }
            structuredTypeQuote(c)(tpe)(objNm)(fieldNm)(accessorQuotes)
          case other =>
            throw new Error("Can't match: " + tpe)
        }
    }
  }

}

trait ParserMaterializer {
  implicit def jsonParserMacro[T]: JsonParser[T] = macro ParserMaterializerImpl.materialize[T]
}

trait FormatterMaterializer {
  implicit def jsonFormatterMacro[T]: JsonFormatter[T] = macro FormatterMaterializerImpl.materialize[T]
}
