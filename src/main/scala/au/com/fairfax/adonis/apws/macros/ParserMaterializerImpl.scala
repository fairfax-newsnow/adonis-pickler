package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context

object ParserMaterializerImpl extends Materializer {
  val jsonIO: String = ???

  def handleItemQuote(c: Context)(tpe: c.universe.Type)(method: String): c.universe.Tree =
    ???

  def handleMapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree =
    ???

  def handleCollectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree =
    ???

  def handleDoubleQuote(c: Context)(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree =
    ???

}
