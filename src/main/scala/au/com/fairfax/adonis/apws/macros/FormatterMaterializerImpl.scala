package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.utils.simpleTypeNm
import Materializer._

object FormatterMaterializerImpl extends Materializer[JsonFormatter] {
  def jsonIo(c: Context): c.universe.TermName = c.universe.TermName("builder")

  private def toJsonStringQuote(c: Context)(s: String): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.makeString($s)"
  }

  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: c.universe.TermName): c.universe.Tree =
    itemQuoteTemplate(c)(tpe)(methodNm) {
      recurQuote(c)(tpe)(_)("")(false)
    }

  private def itemQuoteTemplate(c: Context)(tpe: c.universe.Type)(methodNm: c.universe.TermName)(quoteFunc: String => c.universe.Tree): c.universe.Tree = {
    import c.universe._
    val varName = "obj"
    q"""
      def $methodNm(${TermName(varName)}: $tpe) =
        ${quoteFunc(varName)}
    """
  }

  def handleMapQuote(c: Context)(handleMapMeth: c.universe.TermName)(kvTpes: (c.universe.Type, c.universe.Type))(kvMeths: (c.universe.TermName, c.universe.TermName))(itemQuotes: List[c.universe.Tree]): c.universe.Tree = {    import c.universe._
    import c.universe._
    val (keyTpe, valTpe) = kvTpes
    val (keyMeth, valMeth) = kvMeths
    val methodImplQuote =
      quoteWithNullCheck(c)(varOfNullCheck = "map") {
        q"""
            ..$itemQuotes
            val elems =
              map.map { t =>
                val (k, v) = t
                ${jsonIo(c)}.makeArray($keyMeth(k), $valMeth(v))
              }.toList
            ${jsonIo(c)}.makeArray(elems: _*)
        """
      }
    q"def $handleMapMeth(map: $keyTpe Map $valTpe) = $methodImplQuote"
  }

  def collectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val formatMeth = itemMethNm(tpe.toString)
    val nonNullQuote =
      q"""
        ${itemQuote(c)(tpe)(formatMeth)}
        val jsonList = objList.map {
          obj => ${TermName(formatMeth)}(obj)
        }
        ${jsonIo(c)}.makeArray(jsonList: _*)
      """

    q"""
      def ${TermName(methodNm)}(objList: ${TypeName(collType)}[$tpe]) = {
        ${quoteWithNullCheck(c)(varOfNullCheck = "objList")(nonNullQuote)}
      }
    """
  }

  def optionQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val formatMeth = itemMethNm(tpe.toString)
    val caseQuotes = List(
      cq"Some(v) => ${TermName(formatMeth)}(v)",
      cq"None => ${jsonIo(c)}.makeNull()")

    q"""
      def ${TermName(methodNm)}(opt: Option[$tpe]) = {
        ${itemQuote(c)(tpe)(formatMeth)}
        opt match {
          case ..$caseQuotes
        }
      }
    """
  }

  def eitherQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val leftTpe = tpe.dealias.typeArgs.head
    val rightTpe = tpe.dealias.typeArgs.last
    val leftFormatMeth = itemMethNm(leftTpe.toString)
    val rightFormatMeth = itemMethNm(rightTpe.toString)
    //caseClassItemQuote(c: Context)(method: String)(ct: c.universe.Type)(fieldNm: String)
    val caseQuotes = List(
      cq"""Left(v) => ${TermName(leftFormatMeth)}(v)""",
      cq"""Right(v) => ${TermName(rightFormatMeth)}(v)"""
    )

    q"""
      def ${TermName(methodNm)}(either: Either[$leftTpe, $rightTpe]) = {
        ${caseClassItemQuote(c)(leftFormatMeth)(leftTpe)(fieldNm)}
        ${caseClassItemQuote(c)(rightFormatMeth)(rightTpe)(fieldNm)}
        either match {
          case ..$caseQuotes
        }
      }
    """
  }

  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val numQuote =
      if (tpe == typeOf[Double]) q"${TermName(objNm)}"
      else q"${TermName(objNm)}.asInstanceOf[Double]"
    q"${jsonIo(c)}.makeNumber($numQuote)"
  }

  /**
   * Quote for formatting a string value, it will be something like, e.g.
   * if (reader.isNull(objNm)) {
   *   throw new IllegalArgumentException
   * } else {
   *  builder.makeString(objNm)
   * }
   */
  def stringQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      ${
        quoteWithNullCheck(c)(objNm) {
          q"${jsonIo(c)}.makeString($objNm)"
        }
      }
    """
  }

  def booleanQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.makeBoolean(${fieldQuote(c)(objNm)(fieldNm)})"
  }

  def quoteForNullCheck(c: Context)(varOfNullCheck: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"$varOfNullCheck == null"
  }

  def quoteForNullVar(c: Context): c.universe.Tree = {
    import c.universe._
    q""" throw new IllegalArgumentException("The data object has a null attribute") """
  }

  /**
   * Quote that format the field on that object.
   * return objNm
   */
  def fieldQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"$objNm"
  }

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree = {
    import c.universe._
    q"""
      val ${TermName(accessorField)} = ${TermName(objNm)}.${TermName(accessorField)}
      $accessorField -> ${recurQuote(c)(accessorTpe)(accessorField)("")(false)}
    """
  }

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val nonNullQuote = q"${jsonIo(c)}.makeObject(..$accessorQuotes)"
    q"${quoteWithNullCheck(c)(varOfNullCheck = objNm)(nonNullQuote)}"
  }

  def caseObjQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    import c.universe._
    val typeName = simpleTypeNm(tpe.toString)
    val buildQuote =
      if (areSiblingCaseObjs)
        toJsonStringQuote(c)(typeName)
      else
        q"""${jsonIo(c)}.makeObject("t" -> ${toJsonStringQuote(c)(typeName)}, "v" -> ${toJsonStringQuote(c)("")})"""
    q"""
      def ${TermName(methodNm)} =
        $buildQuote
    """
  }

  def caseClassItemQuote(c: Context)(method: String)(ct: c.universe.Type)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    itemQuoteTemplate(c)(ct)(method) {
      varName =>
        val ctsTypeName = simpleTypeNm(ct.toString)
        val accessorQuotes =
          List( q""" "t" -> ${toJsonStringQuote(c)(ctsTypeName)} """,
            q""" "v" -> ${recurQuote(c)(ct)(varName)(fieldNm)(false)} """)
        structuredTypeQuote(c)(ct)(varName)(fieldNm)(accessorQuotes)
    }
  }

  private lazy val varBeMatched = "obj"

  def caseClassHandlerQuote(c: Context)(method: String)(objNm: String): c.universe.Tree = {
    import c.universe._
    q"${TermName(method)}(${TermName(varBeMatched)})"
  }

  def ptnToHandlerQuote(c: Context)(ct: c.universe.Type)(handlerQuote: c.universe.Tree)(pattern: String): c.universe.Tree = {
    import c.universe._
    cq"${TermName(varBeMatched)} : ${ct} => $handlerQuote"
  }

  def ptnMatchQuote(c: Context)(onlyCaseObjects: Boolean)(ptnToHandlerQuotes: Set[c.universe.Tree])(objNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      ${TermName(objNm)} match {
        case ..$ptnToHandlerQuotes
      }
    """
  }

  def nullHandlerQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(methodNm: String)(quote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(methodNm)}(${TermName(objNm)}: $tpe) = {
        $quote
      }
    """
  }

  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"au.com.fairfax.adonis.apws.macros.JsonRegistry.format[J, $tpe](${fieldQuote(c)(objNm)(fieldNm)})"
  }

  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      ${jsonIo(c)}.makeString(${TermName(objNm)}.toString)
    """
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatter[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
        q"""
          implicit object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.JsonFormatter[$tpe] {
            override def format[J](obj: Any)(implicit ${jsonIo(c)}: au.com.fairfax.adonis.apws.macros.JBuilder[J]) = {
              val typedObj = obj.asInstanceOf[$tpe]
              ${jsonIo(c)}.makeObject(
                "t" -> ${jsonIo(c)}.makeString(${tpe.toString}),
                "args" -> ${recurQuote(c)(tpe)("typedObj")("")(true)}
              )
            }
          }
          GenJsonFormatter
        """
    c.Expr[JsonFormatter[T]](result)
  }
}
