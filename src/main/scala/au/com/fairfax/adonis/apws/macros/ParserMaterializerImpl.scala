package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.apws.macros.json._
import Materializer._
import au.com.fairfax.adonis.utils._

object ParserMaterializerImpl extends Materializer[JsonParser] {
  val jsonIO: String = "reader"

  // don't declare return type after def ${TermName(createItemMeth)}(item: J), o.w. will get meaningless error of type XXX not found in macro call
  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    // if this is a singleton object
    if (tpe.companion == NoType)
      q"""
        def ${TermName(methodNm)} =
          ${recurQuote(c)(tpe)("")("")}
      """
    else
      q"""
        def ${TermName(methodNm)}(item: ${TypeName("J")}) =
          ${recurQuote(c)(tpe)("item")("")}
      """
  }

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    mapTemplateQuote(c)(keyTpe)(valTpe) {
      (keyMeth, valMeth, itemQuotes) =>
        q"""
          def ${TermName(methodNm)}(map: J) = {
            ..$itemQuotes
            val mapSize = ${TermName(jsonIO)}.readArrayLength(map)
            (0 until mapSize).toList.map { idx =>
              val tuple = ${TermName(jsonIO)}.readArrayElem(map, idx)
              val key = ${TermName(jsonIO)}.readArrayElem(tuple, 0)
              val value = ${TermName(jsonIO)}.readArrayElem(tuple, 1)
              ${TermName(keyMeth)}(key) -> ${TermName(valMeth)}(value)
            }.toMap
          }
       """
    }
  }

  def collectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val createMeth = itemMethNm(tpe.toString)
    val intsToItemsQuote =
      q"""
          (0 until arraySize).map {
            idx => ${TermName(createMeth)}(${TermName(jsonIO)}.readArrayElem(array, idx))
          }
        """
    val toCollQuote =
      if (collType == tpeClassNm(c)(typeOf[Seq[_]]))
        intsToItemsQuote
      else q"""
            ${intsToItemsQuote}.${TermName("to" + collType)}
           """
    q"""
        def ${TermName(methodNm)}(array: J) = {
          ${itemQuote(c)(tpe)(createMeth)}
          val arraySize = ${TermName(jsonIO)}.readArrayLength(array)
          $toCollQuote
        }
      """
  }

  def doubleValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val quote = q"${TermName(jsonIO)}.readNumber(${fieldQuote(c)(objNm)(fieldNm)})"
    if (tpe == typeOf[Double]) quote
    else q"${quote}.asInstanceOf[$tpe]"
  }

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    if (fieldNm == "")
      q"${TermName(objNm)}"
    else
      q"${TermName(jsonIO)}.readObjectField(${TermName(objNm)}, $fieldNm)"
  }

  def ioActionString: String = "read"

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree =
    recurQuote(c)(accessorTpe)(objNm + "_" + fieldNm)(accessorField)

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    q"""
        val ${TermName(objNm + "_" + fieldNm)} = ${fieldQuote(c)(objNm)(fieldNm)}
        new $tpe(..$accessorQuotes)
    """
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonParser[T]] = {
    import c.universe._
    materializeTemplate(c) {
      tpe =>
        q"""
          object GenJsonParser extends au.com.fairfax.adonis.apws.macros.json.JsonParser[$tpe] {
            override def parse[J](json: J)(implicit ${TermName(jsonIO)}: au.com.fairfax.adonis.utils.json.JReader[J]) = {
              ${recurQuote(c)(tpe)("json")("args")}
            }
          }
          GenJsonParser
      """
    }
  }
}
