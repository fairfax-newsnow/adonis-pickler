package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
//import au.com.fairfax.adonis.utils._
import au.com.fairfax.adonis.apws.macros.json._
import scala.language.higherKinds

object Materializer {
  def itemMethNm(typeName: String): String =
    List("handle", typeName).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(simpleTypeNm).flatMap(_ split "\\.").mkString("_")

  def simpleTypeNm(typeName: String): String =
    typeName.split("\\.").toList.dropWhile(typeName => typeName(0) < 'A' || typeName(0) > 'Z').mkString(".")
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

  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree

  def ioActionString: String

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree

  def caseObjQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree

  def caseClassItemQuote(c: Context)(method: String)(ct: c.universe.Type)(fieldNm: String): c.universe.Tree

  def caseClassHandlerQuote(c: Context)(method: String)(objNm: String): c.universe.Tree

  def ptnToHandlerQuote(c: Context)(ct: c.universe.Type)(handlerQuote: c.universe.Tree)(pattern: String): c.universe.Tree

  def ptnMatchQuote(c: Context)(onlyCaseObjects: Boolean)(ptnToHandlerQuotes: Set[c.universe.Tree])(objNm: String): c.universe.Tree

  def traitMethodQuote(c: Context)(tpe: c.universe.Type)(method: String)(itemQuotes: Set[c.universe.Tree])(objNm: String)(matchQuote: c.universe.Tree): c.universe.Tree

  def sealedTraitQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    val childTypes = tpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sealedDescendants.filterNot {
      des => des.isSealed || tpeClassNm(c)(tpe) == tpeClassNm(c)(des.asInstanceOf[Symbol].asType.toType)
    }.map(_.asInstanceOf[Symbol].asType.toType)

    val onlyCaseObjects = childTypes forall hasNoAccessor(c)

    val (itemQuotes, ptnToHandlerQuotes) = {
      childTypes.map {
        ct =>
          val pattern = simpleTypeNm(ct.toString)
          val method = itemMethNm(pattern)
          val (iQuote, handlerQuote) =
            if (hasNoAccessor(c)(ct))
              (caseObjQuote(c)(ct)(method)(onlyCaseObjects), q"${TermName(method)}")
            else
              (caseClassItemQuote(c)(method)(ct)(fieldNm), caseClassHandlerQuote(c)(method)(objNm))
          (iQuote, ptnToHandlerQuote(c)(ct)(handlerQuote)(pattern))
      }
    }.unzip

    val traitMeth = itemMethNm(tpe.toString + "_family")
    q"""
      ${traitMethodQuote(c)(tpe)(traitMeth)(itemQuotes)(objNm)(ptnMatchQuote(c)(onlyCaseObjects)(ptnToHandlerQuotes)(objNm))}
      ${TermName(traitMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  def recurQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

//    println(s"recurQuote, tpe = $tpe, tpe.typeSymbol = ${tpe.typeSymbol}, tpe.companion = ${tpe.companion}")

    tpe match {
      // a numeric type
      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        numericValQuote(c)(t)(objNm)(fieldNm)

      // boolean or string type
      case t: Type if List(deliasTpeName[Boolean](c), deliasTpeName[String](c)) contains t.dealias.toString =>
        q"${TermName(jsonIO)}.${TermName(ioActionString + t.dealias.toString)}(${fieldQuote(c)(objNm)(fieldNm)})"

      // a collection type
      case t: Type if collTypes(c) contains tpeClassNm(c)(t) =>
        val handleCollection = "handleCollection"
        q"""
            ${collectionQuote(c)(t.typeArgs.head)(tpeClassNm(c)(t))(handleCollection)}
            ${TermName(handleCollection)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a map type
      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        val handleMap = "handleMap"
        val List(key, value) = t.typeArgs
        q"""
            ${mapQuote(c)(key)(value)(handleMap)}
            ${TermName(handleMap)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a sealed trait
      case t: Type if t.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        sealedTraitQuote(c)(t)(objNm)(fieldNm)

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
    println(result)
    c.Expr[FP[T]](result)
  }
}

trait ParserMaterializer {
  implicit def jsonParserMacro[T]: JsonParser[T] = macro ParserMaterializerImpl.materialize[T]
}

trait FormatterMaterializer {
  implicit def jsonFormatterMacro[T]: JsonFormatter[T] = macro FormatterMaterializerImpl.materialize[T]
}