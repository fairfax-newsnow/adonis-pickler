package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import Materializer._

object FormatterMaterializerImpl extends Materializer[JsonFormatter] {
  lazy val jsonIO: String = "builder"

  lazy val ioActionString: String = "make"

  private def toJsonStringQuote(c: Context)(s: String): c.universe.Tree = {
    import c.universe._
    q"${TermName(jsonIO)}.makeString($s)"
  }

  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree =
    itemQuoteTemplate(c)(tpe)(methodNm) {
      recurQuote(c)(tpe)(_)("")(false)
    }

  private def itemQuoteTemplate(c: Context)(tpe: c.universe.Type)(methodNm: String)(quoteFunc: String => c.universe.Tree): c.universe.Tree = {
    import c.universe._
    val varName = "obj"
    q"""
      def ${TermName(methodNm)}(${TermName(varName)}: $tpe) =
        ${quoteFunc(varName)}
    """
  }

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    mapTemplateQuote(c)(keyTpe)(valTpe) {
      (keyMeth, valMeth, itemQuotes) =>
        q"""
          def ${TermName(methodNm)}(map: $keyTpe Map $valTpe) = {
            ..$itemQuotes
            val elems =
              map.map { t =>
                val (k, v) = t
                ${TermName(jsonIO)}.makeArray(${TermName(keyMeth)}(k), ${TermName(valMeth)}(v))
              }.toList
            ${TermName(jsonIO)}.makeArray(elems: _*)
          }
        """
    }
  }

  def collectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val formatMeth = itemMethNm(tpe.toString)
    q"""
        def ${TermName(methodNm)}(objList: ${TypeName(collType)}[$tpe]) = {
          ${itemQuote(c)(tpe)(formatMeth)}
          val jsonList = objList.map {
            obj => ${TermName(formatMeth)}(obj)
          }
          ${TermName(jsonIO)}.makeArray(jsonList: _*)
        }
      """
  }

  def optionQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val formatMeth = itemMethNm(tpe.toString)
    val caseQuotes = List(
      cq"Some(v) => ${TermName(formatMeth)}(v)",
      cq"None => ${TermName(jsonIO)}.makeNull()")

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
    val leftTpe = tpe.typeArgs.head
    val rightTpe = tpe.typeArgs.last
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
    q"${TermName(jsonIO)}.makeNumber($numQuote)"
  }

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"${TermName(objNm)}"
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
    q"${TermName(jsonIO)}.makeObject(..$accessorQuotes)"
  }

  def caseObjQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    import c.universe._
    val typeName = simpleTypeNm(tpe.toString)
    val buildQuote =
      if (areSiblingCaseObjs)
        toJsonStringQuote(c)(typeName)
      else
        q"""${TermName(jsonIO)}.makeObject("t" -> ${toJsonStringQuote(c)(typeName)}, "v" -> ${toJsonStringQuote(c)("")})"""
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

  def traitMethodQuote(c: Context)(tpe: c.universe.Type)(method: String)(itemQuotes: Set[c.universe.Tree])(objNm: String)(matchQuote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(method)}(${TermName(objNm)}: $tpe) = {
        ..$itemQuotes
        $matchQuote
      }
    """
  }

  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"formatJsSerialisable(${fieldQuote(c)(objNm)(fieldNm)})"
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatter[T]] = {
    import c.universe._
    materializeTemplate(c) {
      tpe =>
        q"""
          implicit object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.JsonFormatter[$tpe] {
            override def format[J](obj: Any)(implicit ${TermName(jsonIO)}: au.com.fairfax.adonis.apws.macros.JBuilder[J]) = {
              val typedObj = obj.asInstanceOf[$tpe]
              def formatJsSerialisable[T: scala.reflect.ClassTag](jsSerialisable: T) = {
                au.com.fairfax.adonis.apws.macros.JsonRegistry.format[J, T](jsSerialisable)
              }
              ${TermName(jsonIO)}.makeObject(
                "t" -> ${TermName(jsonIO)}.makeString(${tpe.toString}),
                "args" -> ${recurQuote(c)(tpe)("typedObj")("")(true)}
              )
            }
          }
          GenJsonFormatter
        """
    }
  }
}
