package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import au.com.fairfax.adonis.utils._
import au.com.fairfax.adonis.apws.macros.json.{JsonParser, FormatterParser}
import scala.language.higherKinds

object Materializer {
  def handleItemMeth(inStr: String): String =
    List("handle", inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(removePkgName).mkString("_")
}

trait Materializer {

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

  def handleItemQuote(c: Context)(tpe: c.universe.Type)(method: String): c.universe.Tree

  def handleMapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree

  def handleCollectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree

  def handleDoubleQuote(c: Context)(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree

  def recurQuote(c: Context)(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree = {
    ???
  }

  def materialize[T: c.WeakTypeTag, FP[_] <: FormatterParser[_]](c: Context): c.Expr[FP[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result = ???
    ???
  }
}


//trait JsonMaterializers {
//  implicit def jsonParserMacro[T]: JsonParser[T] = macro ParserMaterializerImpl.materialize[T, JsonParser]

//  implicit def jsonFormatterMacro[T]: JsonFormatter[T] = macro MaterializersImpl.materializeFormatter[T]
//}