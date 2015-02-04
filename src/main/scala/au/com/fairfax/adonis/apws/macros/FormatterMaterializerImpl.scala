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

  def quoteOfHandleItemDef(c: Context)(itemTpe: c.universe.Type)(methodNm: c.universe.TermName): c.universe.Tree = {
    itemQuoteTemplate(c)(itemTpe)(methodNm) {
      recurQuote(c)(itemTpe)(_)("")(false)
    }
  }

  private def itemQuoteTemplate(c: Context)(tpe: c.universe.Type)(methodNm: c.universe.TermName)(quoteFunc: String => c.universe.Tree): c.universe.Tree = {
    import c.universe._
    val varName = "obj"
    q"""
      def $methodNm(${TermName(varName)}: $tpe) =
        ${quoteFunc(varName)}
    """
  }

  /**
   * Quote to parse collection, it will be something like
   * def formatMap(map: K Map V) = ???
   * formatMap(objNm)
   */
  def mapQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(kvTpes: (c.universe.Type, c.universe.Type))(kvMeths: (c.universe.TermName, c.universe.TermName))(itemQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val (keyTpe, valTpe) = kvTpes
    val (keyMeth, valMeth) = kvMeths

    val formatMapMethNm = TermName("formatMap")
    q"""
      def $formatMapMethNm(map: $keyTpe Map $valTpe) = ${
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
      }
      $formatMapMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to parse collection, it will be something like
   * def formatCollection(objList: Seq[ITEM]) = ???
   * formatCollection(objNm)
   */
  def collectionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(itemTpe: c.universe.Type)(collType: c.universe.TypeName): c.universe.Tree = {
    import c.universe._
    val formatItemMeth = TermName(methdNameOfHandleItem(itemTpe.toString))
    val formatCollMethNm = TermName("formatCollection")
    q"""
      def $formatCollMethNm(objList: $collType[$itemTpe]) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "objList") {
          q"""
            ${quoteOfHandleItemDef(c)(itemTpe)(formatItemMeth)}
            val jsonList = objList.map { obj => $formatItemMeth(obj) }
            ${jsonIo(c)}.makeArray(jsonList: _*)
          """
        }
      }
      $formatCollMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to format an option, it will be something like
   * def formatOption(opt: Option[ITEM]) = ???
   * formatOption(objNm)
   */
  def optionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(itemTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val formatItemMeth = TermName(methdNameOfHandleItem(itemTpe.toString))
    val caseQuotes = List(
      cq"Some(v) => $formatItemMeth(v)",
      cq"None => ${jsonIo(c)}.makeNull()")

    val formatOptionMethNm = TermName("formatOption")
    q"""
      def $formatOptionMethNm(opt: Option[$itemTpe]) = {
        ${quoteOfHandleItemDef(c)(itemTpe)(formatItemMeth)}
        opt match {
          case ..$caseQuotes
        }
      }
      $formatOptionMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to format an either, it will be something like
   * def formatEither(either: Either[LEFT, RIGHT]) = ???
   * formatEither(objNm)
   */
  def eitherQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val leftTpe = tpe.dealias.typeArgs.head
    val rightTpe = tpe.dealias.typeArgs.last
    val leftFormatMeth = methdNameOfHandleItem(leftTpe.toString)
    val rightFormatMeth = methdNameOfHandleItem(rightTpe.toString)
    val formatMethNm = TermName("formatEither")
    //caseClassItemQuote(c: Context)(method: String)(ct: c.universe.Type)(fieldNm: String)
    val caseQuotes = List(
      cq"""Left(v) => ${TermName(leftFormatMeth)}(v)""",
      cq"""Right(v) => ${TermName(rightFormatMeth)}(v)"""
    )

    q"""
      def $formatMethNm(either: Either[$leftTpe, $rightTpe]) = {
        ${caseClassItemQuote(c)(leftFormatMeth)(leftTpe)(fieldNm)}
        ${caseClassItemQuote(c)(rightFormatMeth)(rightTpe)(fieldNm)}
        either match {
          case ..$caseQuotes
        }
      }
      $formatMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote for formatting a numeric value, it will be something like, e.g.
   * builder.makeNumber(objNm)
   *
   * N.B. unlike stringQuote, it doesn't do null check because an expression of type Null is ineligible for implicit conversion for numeric value
   */
  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val numQuote =
      if (tpe == typeOf[Double]) q"$objNm"
      else q"$objNm.asInstanceOf[Double]"
    q"${jsonIo(c)}.makeNumber($numQuote)"
  }

  /**
   * Quote for formatting a string value, it will be something like, e.g.
   * if (objNm == null) {
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

  /**
   * Quote for formatting a boolean value, it will be something like, e.g.
   * builder.makeBoolean(objNm)
   *
   * N.B. unlike stringQuote, it doesn't do null check because an expression of type Null is ineligible for implicit conversion for boolean
   */
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

  def caseClassItemQuote(c: Context)(method: c.universe.TermName)(ct: c.universe.Type)(fieldNm: String): c.universe.Tree = {
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

  def nullHandlerQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(methodNm: c.universe.TermName)(quote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""
      def $methodNm($objNm: $tpe) = {
        $quote
      }
    """
  }

  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"au.com.fairfax.adonis.apws.macros.JsonRegistry.format[J, $tpe](${fieldQuote(c)(objNm)(fieldNm)})"
  }

  /**
   * Quote to format an enum object, it should be something like
   * builder.makeString(objNm.toString)
   */
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.makeString($objNm.toString)"
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
//    println(
//      s"""
//         |formatter
//         |$result
//       """.stripMargin)
    c.Expr[JsonFormatter[T]](result)
  }
}
