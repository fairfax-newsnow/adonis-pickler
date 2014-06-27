package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.apws.macros.json.JsonParserRefactored

object ParserMaterializerImpl extends Materializer[JsonParserRefactored] {
  val jsonIO: String = "reader"

  def handleItemQuote(c: Context)(tpe: c.universe.Type)(method: String): c.universe.Tree =
    ???

  def handleMapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree =
    ???

  def handleCollectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree =
    ???

  def handleDoubleQuote(c: Context)(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree =
    ???

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonParserRefactored[T]] = {
    import c.universe._
    materializeTemplate(c) {
      tpe =>
        q"""
          object GenJsonParser extends au.com.fairfax.adonis.apws.macros.json.JsonParser[$tpe] {
            import org.scalajs.spickling._
            override def parse[J](json: J)(implicit ${TermName(jsonIO)}: PReader[J]) = {
              ${recurQuote(c)(tpe)("json")("args")}
            }
          }
          GenJsonParser
      """
    }
  }
}
