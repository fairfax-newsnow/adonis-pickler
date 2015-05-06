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

  private val name2SimpleTypeName: String => (String, String) = s => s -> simpleTypeNm(s)
  /*
   * A primitive type, such as classOf[Float].getName != typeOf[Float].getName.  But the type of a 
   * data type, i.e. "Float" is used as the key to its corresponding formatter/parser in the JsonRegistry, 
   * therefore strConversion is defined to convert a primitive type class name to the corresponding type name 
   * to find out the key in the JsonRegistry.
   * strConversion is
   * Map(java.lang.Boolean -> Boolean, java.lang.Float -> Float, float -> Float, short -> Short, java.lang.Integer -> Int, java.lang.Double -> Double, double -> Double, 
   * long -> Long, boolean -> Boolean, java.lang.Long -> Long, int -> Int, java.lang.Short -> Short, java.lang.String -> String)
   * A number of java.lang.XXX -> XXX is needed because, e.g. Sample(a: Any), if a is integer,
   * JsonRegistry.internalFormat will find its runtime to be java.lang.Integer
   */
  private lazy val strConversion: String Map String =
    (
      className[java.lang.Integer] -> className[Int].capitalize ::
      List(className[java.lang.Float], className[java.lang.Short], className[java.lang.Double], className[java.lang.Long], className[java.lang.Boolean], className[String]).map{
        name2SimpleTypeName
      } :::
      List(className[Short], className[Int], className[Long], className[Double], className[Float], className[Boolean]).map {
        s => s -> s.capitalize
      }
    ).toMap
  
  lazy val strReplacement: String Map String =
    Map(
      name2SimpleTypeName(className[Map[_, _]])
    )
      
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
