package au.com.fairfax.adonis.apws

import scala.reflect.macros.blackbox.Context

package object macros {
  def getAccessors(c: Context)(tpe: c.universe.Type): List[c.universe.MethodSymbol] = {
    import c.universe._
    tpe.decls.collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }.toList
  }

}
