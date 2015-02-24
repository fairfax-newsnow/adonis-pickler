
package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds
import au.com.fairfax.adonis.utils.simpleTypeNm
import au.com.fairfax.adonis.apws.types.Enum

object Materializer {
  /**
   * name of method that handle a item of a particular type, the name will include the item type,
   * e.g. for a string, it will be "handle_String"
   */
  def methdNameOfHandleItem(typeName: String): String =
    ("handle" :: List(typeName).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(simpleTypeNm).flatMap(_ split "\\.")).mkString("_")

}

trait Materializer[FP[_] <: FormatterParser[_]] {

  import Materializer._

  /**
   * @return "reader" if this is parser, o.w. "builder"
   */
  def jsonIo(c: Context): c.universe.TermName

  def deliasTpeName[T: c.universe.TypeTag](c: Context): String =
    c.universe.typeOf[T].dealias.toString

  def numDealisTpeNms(c: Context) =
    List(deliasTpeName[Double](c), deliasTpeName[Float](c), deliasTpeName[Short](c), deliasTpeName[Int](c), deliasTpeName[Long](c))

  /**
   * Qutoe of method definition that handle an Item
   */
//  def quoteOfHandleItemDef(c: Context)(itemTpe: c.universe.Type)(methodNm: c.universe.TermName): c.universe.Tree

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

  /**
   * Quote of method definition that handle a "case object" of tpe
   */
  def handleCaseObjDefQuote(c: Context)(tpe: c.universe.Type)(methodNm: c.universe.TermName)(areSiblingCaseObjs: Boolean): c.universe.Tree

  /**
   * Quote of method definition that parse an case class object of type ct
   */
  def handleCaseClassDefQuote(c: Context)(method: c.universe.TermName)(ct: c.universe.Type)(fieldNm: c.universe.TermName): c.universe.Tree

  /**
   * Quote of call to method which is the method that handle the case class of a trait
   */
  def handleCaseClassCallQuote(c: Context)(handleCaseClassMeth: c.universe.TermName)(objNm: c.universe.TermName): c.universe.Tree

  /**
   * Quote that maps a pattern to the corresponding handler, it will be someething like
   * pattern => handler
   */
  def patternToHandlerQuote(c: Context)(ct: c.universe.Type)(pattern: String)(handlerQuote: c.universe.Tree): c.universe.Tree

  def ptnMatchQuoteForTraitFamily(c: Context)(onlyCaseObjects: Boolean)(patternToHandlerQuotes: Set[c.universe.Tree])(objNm: c.universe.TermName): c.universe.Tree

  def traitFamilyMethDefAndCallQuote(c: Context)(traitTpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(quote: c.universe.Tree): c.universe.Tree

  /**
   * Quote to handle an enum object
   */
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree

  final def matchObjTpeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._

    tpe match {
      // an enum type represented by au.com.fairfax.adonis.apws.types.Enum
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

        val onlyCaseObjects = childTypes forall hasNoAccessor(c)

        // handleChildTypeDefQuotes is list of handleChildTypeDefQuote
        // patternToHandleChildTypeCallQuotes are the list of quotes where each defines something like ChildType => handleChildTypeCall
        val (handleChildTypeDefQuotes, patternToHandleChildTypeCallQuotes) = childTypes.map {
          ct =>
            val childType = simpleTypeNm(ct.toString)
            val handleChildTypeMeth = TermName(methdNameOfHandleItem(childType))
            // handleChildTypeDefQuote defines the method that handles the case object or case class
            // handleChildTypeCallQuote is a call to method defined by handleChildTypeDefQuote
            val (handleChildTypeDefQuote, handleChildTypeCallQuote) =
              if (hasNoAccessor(c)(ct))  // case "object"
                (handleCaseObjDefQuote(c)(ct)(handleChildTypeMeth)(onlyCaseObjects), q"$handleChildTypeMeth")
              else  // case class
                (handleCaseClassDefQuote(c)(handleChildTypeMeth)(ct)(fieldNm), handleCaseClassCallQuote(c)(handleChildTypeMeth)(objNm))

            (handleChildTypeDefQuote, patternToHandlerQuote(c)(ct)(childType)(handleChildTypeCallQuote))
        }.unzip

        val traitMethImplQuote =
          quoteWithNullCheck(c)(varOfNullCheck = objNm) {
            q"""
              ..$handleChildTypeDefQuotes
              ${ptnMatchQuoteForTraitFamily(c)(onlyCaseObjects)(patternToHandleChildTypeCallQuotes)(objNm)}
            """
          }

        traitFamilyMethDefAndCallQuote(c)(traitTpe)(objNm)(fieldNm)(traitMethImplQuote)

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
