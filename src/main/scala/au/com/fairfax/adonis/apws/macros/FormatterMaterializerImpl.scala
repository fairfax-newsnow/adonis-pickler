package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.apws.macros.json._
import Materializer._
import au.com.fairfax.adonis.utils._

object FormatterMaterializerImpl extends Materializer[JsonFormatter] {
  lazy val jsonIO: String = "builder"

  lazy val ioActionString: String = "make"

  private def toJsonStringQuote(c: Context)(s: String): c.universe.Tree = {
    import c.universe._
    q"${TermName(jsonIO)}.makeString($s)"
  }

  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree =
    itemQuoteTemplate(c)(tpe)(methodNm) {
      recurQuote(c)(tpe)(_)("")
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
      $accessorField -> ${recurQuote(c)(accessorTpe)(accessorField)("")}
    """
  }

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    q"${TermName(jsonIO)}.makeObject(..$accessorQuotes)"
  }

  def caseObjQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    import c.universe._
    val typeName = removePkgName(tpeClassNm(c)(tpe))
    val buildQuote =
      if (areSiblingCaseObjs)
        q"${TermName(jsonIO)}.makeString($typeName)"
      else
        q"""${TermName(jsonIO)}.makeObject("t" -> ${toJsonStringQuote(c)(typeName)}, "v" -> ${toJsonStringQuote(c)("")})"""
    q"""
      def ${TermName(methodNm)} =
        $buildQuote
    """
  }

  def sealedTraitQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val classItemQuote: (String, Type) => Tree =
      (method, ct) =>
        itemQuoteTemplate(c)(ct)(method) {
          varName =>
            val ctsTypeName = removePkgName(tpeClassNm(c)(ct))
            val accessorQuotes =
              List( q""" "t" -> ${toJsonStringQuote(c)(ctsTypeName)} """,
                q""" "v" -> ${recurQuote(c)(ct)(varName)(fieldNm)} """)
            structuredTypeQuote(c)(ct)(varName)(fieldNm)(accessorQuotes)
        }
    val varBeMatched = "obj"
    val classHandlerQuote: String => Tree =
      method => q"${TermName(method)}(${TermName(varBeMatched)})"
    val ptnToHandlerQuote: (Type, Tree, String) => Tree =
      (ct, handlerQuote, _) => cq"${TermName(varBeMatched)} : ${ct} => $handlerQuote"
    val matchQuote: (Boolean, Set[Tree]) => Tree =
      (onlyCaseObjects, ptnToHandlerQuotes) =>
        q"""
          ${TermName(objNm)} match {
            case ..$ptnToHandlerQuotes
          }
        """
    val traitMethodQuote: (String, Set[Tree], Tree) => Tree =
      (method, itemQuotes, matchQuote) =>
        q"""
          def ${TermName(method)}(${TermName(objNm)}: $tpe) = {
            ..$itemQuotes
            $matchQuote
          }
        """
    sealedTraitQuoteTemplate(c)(tpe)(objNm)(fieldNm)(classItemQuote)(classHandlerQuote)(ptnToHandlerQuote)(matchQuote)(traitMethodQuote)
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatter[T]] = {
    import c.universe._
    materializeTemplate(c) {
      tpe =>
        q"""
          object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.json.JsonFormatter[$tpe] {
            override def format[J](obj: Any)(implicit ${TermName(jsonIO)}: au.com.fairfax.adonis.utils.json.JBuilder[J]) = {
              val typedObj = obj.asInstanceOf[$tpe]
              ${TermName(jsonIO)}.makeObject(
                "cmd" -> ${TermName(jsonIO)}.makeString(${tpe.toString}),
                "args" -> ${recurQuote(c)(tpe)("typedObj")("")}
              )
            }
          }
          GenJsonFormatter
        """
    }
  }
}
