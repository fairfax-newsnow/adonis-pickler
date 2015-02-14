package au.com.fairfax.adonis.apws

import au.com.fairfax.adonis.utils._

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context

package object macros {
  def getAccessors(c: Context)(tpe: c.universe.Type): List[c.universe.MethodSymbol] = {
    import c.universe._
    tpe.decls.collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }.toList
  }

  def tpeClassNm(c: Context): c.universe.Type => String = _.dealias.typeSymbol.asClass.name.toString

  def collTypes(c: Context) = {
    import c.universe._
    List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map tpeClassNm(c)
  }

  lazy val strConversion: String Map String =
    (List(className[Short], className[Int], className[Long], className[Double], className[Float], className[Boolean]).map {
      s => s -> s.capitalize
    } ::: List(className[String]).map{
      s => s -> simpleTypeNm(s)
    }).toMap
  
  lazy val strReplacement: String Map String =
    List(className[Map[_, _]]).map {
      s => s -> simpleTypeNm(s)
    }.toMap

  def className[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass.getName

  def toMapKey(s: String): String =
    strReplacement.foldLeft(strConversion.getOrElse(s, s).replace('$', '.')) {
      (origStr, tuple) =>
        val (key, value) = tuple
        origStr.replaceAll(key,value)
    }
}
