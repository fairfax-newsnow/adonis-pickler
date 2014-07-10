package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.language.higherKinds
import au.com.fairfax.adonis.utils.simpleTypeNm

object Materializer {
  def itemMethNm(typeName: String): String =
    ("handle" :: List(typeName).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(simpleTypeNm).flatMap(_ split "\\.")).mkString("_")

}

trait Materializer[FP[_] <: FormatterParser[_]] {

  import Materializer._

  val jsonIO: String

  def tpeClassNm(c: Context): c.universe.Type => String = _.typeSymbol.asClass.name.toString

  def collTypes(c: Context) = {
    import c.universe._
    List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map tpeClassNm(c)
  }

  def deliasTpeName[T: c.universe.TypeTag](c: Context): String =
    c.universe.typeOf[T].dealias.toString

  def numDealisTpeNms(c: Context) =
    List(deliasTpeName[Double](c), deliasTpeName[Float](c), deliasTpeName[Short](c), deliasTpeName[Int](c), deliasTpeName[Long](c))

  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree

  def mapTemplateQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(quoteFunc: (String, String, List[c.universe.Tree]) => c.universe.Tree) = {
    val List(keyMeth, valMeth) = List(keyTpe, valTpe) map (t => itemMethNm(t.toString))
    val itemQuotes = itemQuote(c)(keyTpe)(keyMeth) :: {
      if (keyTpe != valTpe) List(itemQuote(c)(valTpe)(valMeth))
      else Nil
    }
    quoteFunc(keyMeth, valMeth, itemQuotes)
  }

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

  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree

  def stringQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree

  def stringQuoteTemplate(c: Context)(preQuote: c.universe.Tree)(varBeChecked: String): c.universe.Tree = {
    import c.universe._
    val nonNullQuote = q"${TermName(jsonIO)}.${TermName(ioActionString + "String")}(${TermName(varBeChecked)})"
    q"""
      $preQuote
      ${nullHandlerTemplate(c)(nullCheckQuote(c)(varBeChecked))(nonNullQuote)}
    """
  }

  def nullCheckQuote(c: Context)(varBeChecked: String): c.universe.Tree

  def nullQuote(c: Context): c.universe.Tree

  def nullHandlerTemplate(c: Context)(nullCheckQuote: c.universe.Tree)(nonNullQuote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""
      if ($nullCheckQuote)
        ${nullQuote(c)}
      else
        $nonNullQuote
    """
  }

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree

  def ioActionString: String

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

    nullHandlerQuote(c)(tpe)(objNm)(methodNm)(q"${nullHandlerTemplate(c)(nullCheckQuote(c)(objNm))(nonNullQuote)}")
  }

  def recurQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    tpe match {
      // a numeric type
      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        numericValQuote(c)(t)(objNm)(fieldNm)

      // string type
      case t: Type if deliasTpeName[String](c) == t.dealias.toString =>
        stringQuote(c)(objNm)(fieldNm)

      // boolean type
      case t: Type if deliasTpeName[Boolean](c) == t.dealias.toString =>
        q"${TermName(jsonIO)}.${TermName(ioActionString + "Boolean")}(${fieldQuote(c)(objNm)(fieldNm)})"

      // a collection type
      case t: Type if collTypes(c) contains tpeClassNm(c)(t) =>
        val handleMeth = "handleCollection"
        q"""
          ${collectionQuote(c)(t.typeArgs.head)(tpeClassNm(c)(t))(handleMeth)}
          ${TermName(handleMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a map type
      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        val handleMeth = "handleMap"
        val List(key, value) = t.typeArgs
        q"""
          ${mapQuote(c)(key)(value)(handleMeth)}
          ${TermName(handleMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // an option type
      case t: Type if tpeClassNm(c)(typeOf[Option[_]]) == tpeClassNm(c)(t) =>
        val handleMeth = "handleOption"
        q"""
          ${optionQuote(c)(t.typeArgs.head)(handleMeth)}
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
        }
    }
  }

  def materializeTemplate[T: c.WeakTypeTag](c: Context)(quoteFunc: c.universe.Type => c.universe.Tree): c.Expr[FP[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result = quoteFunc(tpe)
//    println(result)
    c.Expr[FP[T]](result)
  }
}

trait ParserMaterializer {
  implicit def jsonParserMacro[T]: JsonParser[T] = macro ParserMaterializerImpl.materialize[T]
}

trait FormatterMaterializer {
  implicit def jsonFormatterMacro[T]: JsonFormatter[T] = macro FormatterMaterializerImpl.materialize[T]
}