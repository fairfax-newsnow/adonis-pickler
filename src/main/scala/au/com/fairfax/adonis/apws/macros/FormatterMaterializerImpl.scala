package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.apws.macros.json._
import Materializer._

object FormatterMaterializerImpl extends Materializer[JsonFormatter] {
  val jsonIO: String = "builder"

  def itemQuote(c: Context)(tpe: c.universe.Type)(method: String): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(method)}(obj: $tpe) =
        ${recurQuote(c)(tpe)("obj")("")}
    """
  }

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val List(formatKeyMeth, formatValMeth) = List(keyTpe, valTpe) map (t => itemMeth(t.toString))
    var formatQuote = List(itemQuote(c)(keyTpe)(formatKeyMeth))
    if (keyTpe != valTpe)
      formatQuote = itemQuote(c)(valTpe)(formatValMeth) :: formatQuote
    q"""
        def ${TermName(methodNm)}(map: $keyTpe Map $valTpe) = {
          ..$formatQuote
          val elems =
            map.map { t =>
              val (k, v) = t
              ${TermName(jsonIO)}.makeArray(${TermName(formatKeyMeth)}(k), ${TermName(formatValMeth)}(v))
            }.toList
          ${TermName(jsonIO)}.makeArray(elems: _*)
        }
      """
  }

  def collectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree = {
    import c.universe._
    val formatMeth = itemMeth(tpe.toString)
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

  def doubleValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
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

  def ioActionString: String = "make"

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree = {
    import c.universe._
    q"""
      val ${TermName(accessorField)} = ${TermName(objNm)}.${TermName(accessorField)}
      $accessorField -> ${recurQuote(c)(accessorTpe)(accessorField)("")}
    """
  }

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    q"""
      ${TermName(jsonIO)}.makeObject(..$accessorQuotes)
    """
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatter[T]] = {
    import c.universe._
    materializeTemplate(c) {
      tpe =>
        q"""
          object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.json.JsonFormatter[$tpe] {
            import org.scalajs.spickling._
            override def format[J](obj: Any)(implicit ${TermName(jsonIO)}: PBuilder[J]) = {
              val typedObj = obj.asInstanceOf[$tpe]
              ${TermName(jsonIO)}.makeObject(
                "cmd" -> ${TermName(jsonIO)}.makeString(${tpe.toString}),
                "args" -> ${recurQuote(c)(tpe)(s"typedObj")("")}
              )
            }
          }
          GenJsonFormatter
      """
    }
  }
}
