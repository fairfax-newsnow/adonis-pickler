package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object RegistrarMacroTracker {
  var registeredTypes: Set[String] = Set()

  def add(t: String): Unit = {
    registeredTypes += t
    println(s"$RegistrarMacroTracker.add(), registeredTypes = $registeredTypes")
  }
  
  def contains(t: String): Boolean = {
    println(s"$RegistrarMacroTracker.contains($t), registeredTypes = $registeredTypes")
    registeredTypes contains t
  }
}

trait TraversableRegistrar[T] {
  def traversableRegister: Unit
}

object TraversableRegistrar {
  implicit def registerHelperMacros[T]: TraversableRegistrar[T] = macro materialize[T]

  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[TraversableRegistrar[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val tpeStr = tpe.toString

    val result =
      if (RegistrarMacroTracker contains tpeStr) {
        q"""
          import au.com.fairfax.adonis.apws.macros.TraversableRegistrar

          implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe.dealias}] {
            def traversableRegister: Unit = {}
          }

          GenTraversableRegistrar
        """
      }
      else {
        RegistrarMacroTracker add tpeStr
        q"""
          import au.com.fairfax.adonis.apws.macros.TraversableRegistrar
          import au.com.fairfax.adonis.apws.macros.JsonParser
          import au.com.fairfax.adonis.apws.macros.JsonFormatter
          import au.com.fairfax.adonis.apws.macros.JsonRegistry

          implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe.dealias}] {
            def traversableRegister: Unit = register

            private def register(implicit parser: JsonParser[${tpe.dealias}], formatter: JsonFormatter[${tpe.dealias}]) {
              add((${tpe.dealias.toString}, parser))((${tpe.dealias.toString}, formatter))
            }
          }

          GenTraversableRegistrar
        """
      }
    
    println(
      s"""
         |TraversableRegistrar.materialize(), result =
         |$result
       """.stripMargin)
    
    c.Expr[TraversableRegistrar[T]](result)
  }
 }
