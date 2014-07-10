package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import Materializer._

object ParserMaterializerImpl extends Materializer[JsonParser] {
  lazy val jsonIO: String = "reader"

  lazy val ioActionString: String = "read"

  // don't declare return type after def ${TermName(createItemMeth)}(item: J), o.w. will get meaningless error of type XXX not found in macro call
  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(methodNm)}(item: ${TypeName("J")}) =
        ${recurQuote(c)(tpe)("item")("")}
    """
  }

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._

    mapTemplateQuote(c)(keyTpe)(valTpe) {
      (keyMeth, valMeth, itemQuotes) =>
        val nonNullQuote =
          q"""
              ..$itemQuotes
              val mapSize = ${TermName(jsonIO)}.readArrayLength(map)
              (0 until mapSize).toList.map { idx =>
                val tuple = ${TermName(jsonIO)}.readArrayElem(map, idx)
                val key = ${TermName(jsonIO)}.readArrayElem(tuple, 0)
                val value = ${TermName(jsonIO)}.readArrayElem(tuple, 1)
                ${TermName(keyMeth)}(key) -> ${TermName(valMeth)}(value)
              }.toMap
          """

        q"""
          def ${TermName(methodNm)}(map: J) = {
            ${nullHandlerTemplate(c)(nullCheckQuote(c)(varBeChecked = "map"))(nonNullQuote)}
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
      else
        q"""${intsToItemsQuote}.${TermName("to" + collType)}"""

    val nonNullQuote =
      q"""
        ${itemQuote(c)(tpe)(createMeth)}
        val arraySize = ${TermName(jsonIO)}.readArrayLength(array)
        $toCollQuote
      """

    q"""
      def ${TermName(methodNm)}(array: J) = {
        ${nullHandlerTemplate(c)(nullCheckQuote(c)(varBeChecked = "array"))(nonNullQuote)}
      }
    """
  }

  def optionQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val createMeth = itemMethNm(tpe.toString)
    q"""
      def ${TermName(methodNm)}(json: J): Option[$tpe] = {
        ${itemQuote(c)(tpe)(createMeth)}
        if (${TermName(jsonIO)}.isNull(json))
          None
        else
          Some(${TermName(createMeth)}(json))
      }
    """
  }

  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val quote = q"${TermName(jsonIO)}.readNumber(${fieldQuote(c)(objNm)(fieldNm)})"
    if (tpe == typeOf[Double])
      quote
    else
      q"${quote}.asInstanceOf[$tpe]"
  }

  def stringQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    val varBeChecked = objNm + "_" + fieldNm
    val preQuote = q"val ${TermName(varBeChecked)} = ${fieldQuote(c)(objNm)(fieldNm)}"
    stringQuoteTemplate(c)(preQuote)(varBeChecked)
  }

  def nullCheckQuote(c: Context)(varBeChecked: String): c.universe.Tree = {
    import c.universe._
    q"${TermName(jsonIO)}.${TermName("isNull")}(${TermName(varBeChecked)})"
  }

  def nullQuote(c: Context): c.universe.Tree = {
    import c.universe._
    q"null"
  }

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    if (fieldNm == "")
      q"${TermName(objNm)}"
    else
      q"${TermName(jsonIO)}.readObjectField(${TermName(objNm)}, $fieldNm)"
  }

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree =
    recurQuote(c)(accessorTpe)(objNm + "_" + fieldNm)(accessorField)

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val nonNullQuote = q"new $tpe(..$accessorQuotes)"
    val assignedVar = objNm + "_" + fieldNm
    q"""
      val ${TermName(assignedVar)} = ${fieldQuote(c)(objNm)(fieldNm)}
      ${nullHandlerTemplate(c)(nullCheckQuote(c)(varBeChecked = assignedVar))(nonNullQuote)}
    """
  }

  def caseObjQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    import c.universe._
    q"def ${TermName(methodNm)} = new $tpe"
  }

  def caseClassItemQuote(c: Context)(method: String)(ct: c.universe.Type)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    itemQuote(c)(ct)(method)
  }

  def caseClassHandlerQuote(c: Context)(method: String)(objNm: String): c.universe.Tree = {
    import c.universe._
    q"""${TermName(method)}(${TermName(jsonIO)}.readObjectField(${TermName(objNm)}, "v"))"""
  }

  def ptnToHandlerQuote(c: Context)(ct: c.universe.Type)(handlerQuote: c.universe.Tree)(pattern: String): c.universe.Tree = {
    import c.universe._
    cq"$pattern => $handlerQuote"
  }

  def ptnMatchQuote(c: Context)(onlyCaseObjects: Boolean)(ptnToHandlerQuotes: Set[c.universe.Tree])(objNm: String): c.universe.Tree = {
    import c.universe._
    if (onlyCaseObjects)
      q"""
        ${TermName(jsonIO)}.readString(${TermName(objNm)}) match {
          case ..$ptnToHandlerQuotes
        }
      """
    else
      q"""
        ${TermName(jsonIO)}.readString(${TermName(jsonIO)}.readObjectField(${TermName(objNm)}, "t")) match {
          case ..$ptnToHandlerQuotes
        }
      """
  }

  def nullHandlerQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(methodNm: String)(quote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(methodNm)}(${TermName(objNm)}: ${TypeName("J")}) = {
        $quote
      }
    """
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonParser[T]] = {
    import c.universe._
    materializeTemplate(c) {
      tpe =>
        q"""
          implicit object GenJsonParser extends au.com.fairfax.adonis.apws.macros.JsonParser[$tpe] {
            override def parse[J](json: J)(implicit ${TermName(jsonIO)}: au.com.fairfax.adonis.apws.macros.JReader[J]) = {
              ${recurQuote(c)(tpe)("json")("args")}
            }
          }
          GenJsonParser
        """
    }
  }
}
