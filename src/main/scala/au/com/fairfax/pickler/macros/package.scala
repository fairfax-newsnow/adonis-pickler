package au.com.fairfax.pickler

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context

package object macros {
  def hasNoAccessor(c: Context)(tpe: c.universe.Type): Boolean =
    getAccessors(c)(tpe).isEmpty

  def getAccessors(c: Context)(tpe: c.universe.Type): List[c.universe.MethodSymbol] = {
    import c.universe._
    tpe.decls.collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }.toList
  }

  def deliasTpeName[T: c.universe.TypeTag](c: Context): String =
    c.universe.typeOf[T].dealias.toString

  def numDealisTpeNms(c: Context) =
    List(deliasTpeName[Double](c), deliasTpeName[Float](c), deliasTpeName[Short](c), deliasTpeName[Int](c), deliasTpeName[Long](c))

  def noAccessor(c: Context)(tpe: c.universe.Type): Boolean =
    getAccessors(c)(tpe).isEmpty

  def tpeClassNm(c: Context): c.universe.Type => String = _.dealias.typeSymbol.asClass.name.toString

  def collTypes(c: Context) = {
    import c.universe._
    List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map tpeClassNm(c)
  }
  
  def isCaseClass(c: Context)(tpe: c.universe.Type): Boolean =
    tpe.typeSymbol.isClass && 
      tpe.typeSymbol.asClass.isCaseClass &&
        !tpe.typeSymbol.isModuleClass

  private lazy val strConversion: String Map String =
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
  
  def getSealedTraitChildren(c: Context)(traitTpe: c.universe.Type): Set[c.universe.Type] = {
    import c.universe._
    traitTpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sealedDescendants.filterNot {
      des => des.isSealed || tpeClassNm(c)(traitTpe) == tpeClassNm(c)(des.asInstanceOf[Symbol].asType.toType)
    }.map(_.asInstanceOf[Symbol].asType.toType)
  }

}
