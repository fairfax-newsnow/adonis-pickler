package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import Materializer._
import au.com.fairfax.adonis.utils.simpleTypeNm

object ParserMaterializerImpl extends Materializer[JsonParser] {
  def jsonIo(c: Context): c.universe.TermName = c.universe.TermName("reader")

  lazy val ioActionString: String = "read"

  def concatVarNms(varNms: String*): String =
    varNms mkString "_"

  // don't declare return type after def ${TermName(createItemMeth)}(item: J), o.w. will get meaningless error of type XXX not found in macro call
  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(methodNm)}(item: ${TypeName("J")}) =
        ${recurQuote(c)(tpe)("item")("")(false)}
    """
  }

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._

    mapTemplateQuote(c)(keyTpe)(valTpe) {
      (keyMeth, valMeth, itemQuotes) =>
        val nonNullQuote =
          q"""
              ..$itemQuotes
              val mapSize = ${jsonIo(c)}.readArrayLength(map)
              (0 until mapSize).toList.map { idx =>
                val tuple = ${jsonIo(c)}.readArrayElem(map, idx)
                val key = ${jsonIo(c)}.readArrayElem(tuple, 0)
                val value = ${jsonIo(c)}.readArrayElem(tuple, 1)
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
            idx => ${TermName(createMeth)}(${jsonIo(c)}.readArrayElem(array, idx))
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
        val arraySize = ${jsonIo(c)}.readArrayLength(array)
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
        if (${jsonIo(c)}.isNull(json))
          None
        else
          Some(${TermName(createMeth)}(json))
      }
    """
  }

  def eitherQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val leftTpe = tpe.dealias.typeArgs.head
    val rightTpe = tpe.dealias.typeArgs.last
    val simpleLeftTpe = simpleTypeNm(leftTpe.toString)
    val simpleRightTpe = simpleTypeNm(rightTpe.toString)
    val leftMeth = itemMethNm(leftTpe.toString)
    val rightMeth = itemMethNm(rightTpe.toString)

    q"""
      def ${TermName(methodNm)}(json: J): Either[$leftTpe, $rightTpe] = {
        ${caseClassItemQuote(c)(leftMeth)(leftTpe)("")}
        ${caseClassItemQuote(c)(rightMeth)(rightTpe)("")}
        val value = ${jsonIo(c)}.readObjectField(json, "v")
        val providedTypeName = ${jsonIo(c)}.readString(${jsonIo(c)}.readObjectField(json, "t"))
        providedTypeName match {
          case $simpleLeftTpe => Left(${TermName(leftMeth)}(value))
          case $simpleRightTpe => Right(${TermName(rightMeth)}(value))
          case missed => throw new Error("Can't match: " + missed)
        }
      }
    """
  }

  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val quote = q"${jsonIo(c)}.readNumber(${fieldQuote(c)(objNm)(fieldNm)})"
    if (tpe == typeOf[Double])
      quote
    else
      q"${quote}.asInstanceOf[$tpe]"
  }

  def stringQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    val varBeChecked = concatVarNms(objNm, fieldNm)
    val preQuote = q"val ${TermName(varBeChecked)} = ${fieldQuote(c)(objNm)(fieldNm)}"
    stringQuoteTemplate(c)(preQuote)(varBeChecked)
  }

  def nullCheckQuote(c: Context)(varBeChecked: String): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.${TermName("isNull")}(${TermName(varBeChecked)})"
  }

  def nullQuote(c: Context): c.universe.Tree = {
    import c.universe._
    q""" throw new IllegalArgumentException("The json data contains a null attribute which is not mapped to an Option[_] attribute") """
  }

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    if (fieldNm == "")
      q"${TermName(objNm)}"
    else
      q"${jsonIo(c)}.readObjectField(${TermName(objNm)}, $fieldNm)"
  }

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree =
    recurQuote(c)(accessorTpe)(concatVarNms(objNm, fieldNm))(accessorField)(false)

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val nonNullQuote = q"new $tpe(..$accessorQuotes)"
    val assignedVar = concatVarNms(objNm, fieldNm)
    q"""
      val ${TermName(assignedVar)} = ${fieldQuote(c)(objNm)(fieldNm)}
      ${nullHandlerTemplate(c)(nullCheckQuote(c)(varBeChecked = assignedVar))(nonNullQuote)}
    """
  }

  def caseObjQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    this.synchronized {
      import c.universe._
      q"def ${TermName(methodNm)} = new $tpe"
    }
  }

  def caseClassItemQuote(c: Context)(method: String)(ct: c.universe.Type)(fieldNm: String): c.universe.Tree =
    itemQuote(c)(ct)(method)

  def caseClassHandlerQuote(c: Context)(method: String)(objNm: String): c.universe.Tree = {
    import c.universe._
    q"""${TermName(method)}(${jsonIo(c)}.readObjectField(${TermName(objNm)}, "v"))"""
  }

  def ptnToHandlerQuote(c: Context)(ct: c.universe.Type)(handlerQuote: c.universe.Tree)(pattern: String): c.universe.Tree = {
    import c.universe._
    cq"$pattern => $handlerQuote"
  }

  def ptnMatchQuote(c: Context)(onlyCaseObjects: Boolean)(ptnToHandlerQuotes: Set[c.universe.Tree])(objNm: String): c.universe.Tree = {
    import c.universe._
    if (onlyCaseObjects)
      q"""
        ${jsonIo(c)}.readString(${TermName(objNm)}) match {
          case ..$ptnToHandlerQuotes
        }
      """
    else
      q"""
        ${jsonIo(c)}.readString(${jsonIo(c)}.readObjectField(${TermName(objNm)}, "t")) match {
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

  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"parseJsSerialised(${fieldQuote(c)(objNm)(fieldNm)}).asInstanceOf[$tpe]"
  }

  def getCompanion(c: Context)(tpe: c.Type) = {
    import c.universe._
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    c.universe.treeBuild.mkAttributedRef(pre, tpe.typeSymbol.companionSymbol)
  }

  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    val companion = getCompanion(c)(tpe)

    q"""
      val caseEnumName = ${stringQuote(c)(objNm)(fieldNm)}
      au.com.fairfax.adonis.apws.types.CaseEnum.makeEnum($companion, caseEnumName)
    """
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonParser[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
        q"""
          implicit object GenJsonParser extends au.com.fairfax.adonis.apws.macros.JsonParser[${tpe.dealias}] {
            override def parse[J](json: J)(implicit ${jsonIo(c)}: au.com.fairfax.adonis.apws.macros.JReader[J]) = {
              def parseJsSerialised(jsSerialised: J) =
                au.com.fairfax.adonis.apws.macros.JsonRegistry.parse[J](jsSerialised)

              ${recurQuote(c)(tpe.dealias)("json")("args")(true)}
            }
          }
          GenJsonParser
        """
    c.Expr[JsonParser[T]](result)
  }
}
