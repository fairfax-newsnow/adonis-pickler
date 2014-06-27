package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import au.com.fairfax.adonis.utils._
import au.com.fairfax.adonis.apws.macros.json._
import scala.language.higherKinds

object Materializer {
  def itemMeth(inStr: String): String =
    List("handle", inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(removePkgName).mkString("_")
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

  def itemQuote(c: Context)(tpe: c.universe.Type)(method: String): c.universe.Tree

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree

  def mapTemplateQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(quoteFunc: (String, String, List[c.universe.Tree]) => c.universe.Tree) = {
    import c.universe._
    val List(keyMeth, valMeth) = List(keyTpe, valTpe) map (t => itemMeth(t.toString))
    val itemQuotes = itemQuote(c)(keyTpe)(keyMeth) :: {
      if (keyTpe != valTpe) List(itemQuote(c)(valTpe)(valMeth))
      else Nil
    }
    quoteFunc(keyMeth,valMeth, itemQuotes)
  }

  def collectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree

  def doubleValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree

  def ioActionString: String

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree

  def recurQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    val accessors = (tpe.decls collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }).toList

//    println(s"recurQuote, tpe = $tpe, tpe.dealias = ${tpe.dealias}, accessors = $accessors")
    tpe match {
      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        doubleValQuote(c)(t)(objNm)(fieldNm)

      case t: Type if List(deliasTpeName[Boolean](c), deliasTpeName[String](c)) contains t.dealias.toString =>
        q"${TermName(jsonIO)}.${TermName(ioActionString + t.dealias.toString)}(${fieldQuote(c)(objNm)(fieldNm)})"

      case t: Type if collTypes(c) contains tpeClassNm(c)(t) =>
        val handleCollection = "handleCollection"
        q"""
            ${collectionQuote(c)(t.typeArgs.head)(tpeClassNm(c)(t))(handleCollection)}
            ${TermName(handleCollection)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        val handleMap = "handleMap"
        val List(key, value) = t.typeArgs
        q"""
            ${mapQuote(c)(key)(value)(handleMap)}
            ${TermName(handleMap)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      case _ => accessors match {
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