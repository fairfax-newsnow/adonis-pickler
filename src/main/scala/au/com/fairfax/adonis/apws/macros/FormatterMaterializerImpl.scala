package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.apws.macros.json._
import Materializer._
import au.com.fairfax.adonis.utils._

object FormatterMaterializerImpl extends Materializer[JsonFormatter] {
  lazy val jsonIO: String = "builder"

  lazy val ioActionString: String = "make"

  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(methodNm)}(obj: $tpe) =
        ${recurQuote(c)(tpe)("obj")("")}
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

  def objectQuote(c: Context)(tpe: c.universe.Type)(methodNm: String)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    import c.universe._
    val typeName = removePkgName(tpe.typeSymbol.asClass.name.toString)
    val makeJQuote: String => c.universe.Tree = v => q"${TermName(jsonIO)}.makeString($v)"
    val buildQuote =
      if (areSiblingCaseObjs)
        q"""${TermName(jsonIO)}.makeString($typeName)"""
      else
        q"""${TermName(jsonIO)}.makeObject("t" -> ${makeJQuote(typeName)}, "v" -> ${makeJQuote("")})"""
    q"""
      def ${TermName(methodNm)} =
        $buildQuote
    """
  }

  private lazy val varBeMatched = "obj"

  def sealedTraitQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    val childTypeSyms = tpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sealedDescendants.filterNot {
      des => des.isSealed || tpeClassNm(c)(tpe) == tpeClassNm(c)(des.asInstanceOf[Symbol].asType.toType)
    }.map(_.asInstanceOf[Symbol].asType)

    val onlyCaseObjects = childTypeSyms forall (cts => hasNoAccessor(c)(cts.toType))

    val (itemQuotes, caseQuotes) = {
      childTypeSyms.map {
        cts =>
          val pattern = removePkgName(cts.asClass.name.toString)
          val method = itemMethNm(pattern)
          val (iQuote, caseHandler) =
            if (hasNoAccessor(c)(cts.toType))
              (objectQuote(c)(cts.toType)(method)(onlyCaseObjects), q"${TermName(method)}")
            else
              (itemQuote(c)(cts.toType)(method), q"""${TermName(method)}(${TermName(varBeMatched)})""")
          (iQuote, cq"""${TermName(varBeMatched)} : ${cts.toType} => $caseHandler""")
      }
    }.unzip

    val matchQuote =
      q"""
           ${TermName(objNm)} match {
             case ..$caseQuotes
           }
         """

    val traitFamilyMeth = itemMethNm(tpe.toString + "_family")
    // TODO try to use another namei instead of objNm inside TermName(objNm)
    q"""
      def ${TermName(traitFamilyMeth)}(${TermName(objNm)}: $tpe) = {
        ..$itemQuotes
        $matchQuote
      }
      ${TermName(traitFamilyMeth)}(${fieldQuote(c)(objNm)(fieldNm)})
    """
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
