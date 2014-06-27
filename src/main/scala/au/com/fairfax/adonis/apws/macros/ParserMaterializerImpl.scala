package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.apws.macros.json.JsonParserRefactored

object ParserMaterializerImpl extends Materializer[JsonParserRefactored] {
  val jsonIO: String = ???

  def handleItemQuote(c: Context)(tpe: c.universe.Type)(method: String): c.universe.Tree =
    ???

  def handleMapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree =
    ???

  def handleCollectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree =
    ???

  def handleDoubleQuote(c: Context)(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree =
    ???

  override def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonParserRefactored[T]] = {
    super.materialize(c)
  }
}
