
package au.com.fairfax.pickler.macros

import scala.language.experimental.macros
import scala.language.{existentials, higherKinds}
import scala.reflect.macros.blackbox.Context
import au.com.fairfax.pickler.types._
import au.com.fairfax.pickler.simpleTypeNm

object Materializer {
  /**
   * name of method that handle a item of a particular type, the name will include the item type,
   * e.g. for a string, it will be "handle_String"
   */
  def methdNameOfHandleItem(typeName: String): String =
    ("handle" :: List(typeName).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(simpleTypeNm).flatMap(_ split "\\.")).mkString("_")

}

trait Materializer[FP[_] <: FormatterParser[_]] {

  /**
   * @return "reader" if this is parser, o.w. "builder"
   */
  def jsonIo(c: Context): c.universe.TermName

  /**
   * Quote to handle a map
   */
  def mapQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(mapTpe: c.universe.Type): c.universe.Tree

  /**
   * Quote to handle a collection
   */
  def collectionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type)(collType: c.universe.TypeName): c.universe.Tree

  /**
   * Quote to handle an option
   */
  def optionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type): c.universe.Tree

  /**
   * Quote to handle an either
   */
  def eitherQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(tpe: c.universe.Type): c.universe.Tree

  /**
   * Quote to handle a numeric value
   */
  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree

  /**
   * Quote to handle a boolean value
   */
  def booleanQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree

  /**
   * Quote to handle a string value
   */
  def stringQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree

  /**
   * The quote for checking if varOfNullCheck is null.
   */
  def quoteForNullCheck(c: Context)(varOfNullCheck: c.universe.TermName): c.universe.Tree

  /**
   * The quote for the condition when the involved variable is null.
   */
  def quoteForNullVar(c: Context): c.universe.Tree

  /**
   * A func template that creates a quote to do null check on a var, and executes correspondingly upon different condition, the pseudo code will be something like,
   * if (varOfNullCheck found to be null) <- this checking is subclass specific
   *   quote for the logic be executed for varOfNullCheck found to be null
   * else
   *  quote for the logic be executed for varOfNullCheck found to be not null
   */
  final def quoteWithNullCheck(c: Context)(varOfNullCheck: c.universe.TermName)(quoteForNonNullVar: => c.universe.Tree): c.universe.Tree = {
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
  def fieldQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree

  /**
   * Quote to handle an accessor, aka a field of a structured type
   */
  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorField: c.universe.TermName): c.universe.Tree

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree

  def handleCaseObjectAndCallQuote(c: Context)(tpe: c.universe.Type)(tpeInJson: String): (c.universe.Tree, c.universe.Tree)

  def handleCaseClassAndCallQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): (c.universe.Tree, c.universe.Tree)
  
  def handleEmptyCaseClassAndCallQuote(c: Context)(tpe: c.universe.Type)(tpeInJson: String): (c.universe.Tree, c.universe.Tree)
  
  def ptnMatchQuoteForTraitFamily(c: Context)(patternToHandlerQuotes: Set[c.universe.Tree])(objNm: c.universe.TermName): c.universe.Tree

  def traitFamilyMethDefAndCallQuote(c: Context)(traitTpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(quote: c.universe.Tree): c.universe.Tree

  def caseObjQuote(c: Context)(caseObjTpe: c.universe.Type): c.universe.Tree

  def emptyCaseClassQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree

  /**
   * Quote to handle an enum object
   */
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree

  final def matchObjTpeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    
    lazy val accessors = getAccessors(c)(tpe)

    tpe match {
      // an enum type represented by au.com.fairfax.pickler.macros.Enum
      case t: Type if t <:< c.mirror.typeOf[Enum] =>
        enumObjQuote(c)(t)(objNm)(fieldNm)

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
        val itemTpe = t.typeArgs.head
        val collTpe = tpeClassNm(c)(t)
        collectionQuote(c)(objNm)(fieldNm)(itemTpe)(collTpe)

      // a map type
      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        mapQuote(c)(objNm)(fieldNm)(t)

      // an option type
      case t: Type if tpeClassNm(c)(typeOf[Option[_]]) == tpeClassNm(c)(t) =>
        optionQuote(c)(objNm)(fieldNm)(t.typeArgs.head)

      // an either type
      case t: Type if tpeClassNm(c)(typeOf[Either[_, _]]) == tpeClassNm(c)(t) =>
        eitherQuote(c)(objNm)(fieldNm)(t)

      // a sealed trait
      case traitTpe: Type if traitTpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        val childTypes = getSealedTraitChildren(c)(traitTpe)

        if (childTypes.isEmpty) {
          println(s"JsonMaterializers(), $traitTpe has no child")
          throw new Error("JsonMaterializers(), $traitTpe has no child")
        }
        require(childTypes.nonEmpty)

        val (handleChildQuote, patternToCallChildQuotes) = childTypes.map {
          ct =>
            // if ct is a case object, the type name will end with ".type" and should be trimmed off
            val ctString = simpleTypeNm(ct.toString).replace(".type", "")
            ct match {
              case tpe if tpe.typeSymbol.isModuleClass =>
                handleCaseObjectAndCallQuote(c)(tpe)(ctString)
              case tpe if noAccessor(c)(tpe) =>
                handleEmptyCaseClassAndCallQuote(c)(tpe)(ctString)
              case _ =>
                handleCaseClassAndCallQuote(c)(ct)(objNm)(fieldNm)
            }
        }.unzip

        val traitMethImplQuote =
          quoteWithNullCheck(c)(varOfNullCheck = objNm) {
            q"""
              ..$handleChildQuote
              ${ptnMatchQuoteForTraitFamily(c)(patternToCallChildQuotes)(objNm)}
            """
          }

        traitFamilyMethDefAndCallQuote(c)(traitTpe)(objNm)(fieldNm)(traitMethImplQuote)

      // a structured type
      case t: Type if isCaseClass(c)(t) && accessors.nonEmpty =>
        val accessorQuotes = accessors map {
          accessor =>
            val accessorField = accessor.name.toString
            val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
            eachAccessorQuote(c)(accessorTpe)(objNm)(fieldNm)(accessorField)
        }
        structuredTypeQuote(c)(tpe)(objNm)(fieldNm)(accessorQuotes)

      // empty case class
      case t: Type if isCaseClass(c)(t) =>
        emptyCaseClassQuote(c)(t)

      // a case object
      case t: Type if t.typeSymbol.isModuleClass =>
        caseObjQuote(c)(t)

      // Any, AnyRef, non-sealed trait, abstract class, just class
      case _ =>
        throw new IllegalArgumentException(s"Can't match $tpe")
    }
  }
  
}

//trait ParserMaterializer {
//  implicit def jsonParserMacro[T]: JsonParser[T] = macro ParserMaterializerImpl.materialize[T]
//}
//
//trait FormatterMaterializer {
//  implicit def jsonFormatterMacro[T]: JsonFormatter[T] = macro FormatterMaterializerImpl.materialize[T]
//}
