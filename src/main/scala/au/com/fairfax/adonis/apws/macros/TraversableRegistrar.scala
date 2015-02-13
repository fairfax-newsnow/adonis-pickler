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
        alreadyRegisteredQuote(c)(tpe)
      }
      else {
        RegistrarMacroTracker add tpeStr
        yetToRegisterQuote(c)(tpe)
      }

    println(
      s"""
         |TraversableRegistrar.materialize(), result =
         |$result
       """.stripMargin)

    c.Expr[TraversableRegistrar[T]](result)
  }

  private def alreadyRegisteredQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"""
      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar

      implicit object RegistrarDoesNothing extends TraversableRegistrar[${tpe}] {
        def traversableRegister: Unit = {}
      }

      RegistrarDoesNothing
    """
  }

  private def yetToRegisterQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"""
      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar
      import au.com.fairfax.adonis.apws.macros.JsonParser
      import au.com.fairfax.adonis.apws.macros.JsonFormatter
      import au.com.fairfax.adonis.apws.macros.JsonRegistry
      
      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe}] {
        def traversableRegister: Unit = registerParserFormatter

        private def registerParserFormatter(implicit parser: JsonParser[${tpe}], formatter: JsonFormatter[${tpe}]) {
          add((${tpe.toString}, parser))((${tpe.toString}, formatter))
          ${traverseChildren(c)(tpe)}
        }
      }

      GenTraversableRegistrar
    """
  }

  private def traverseChildren(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._

    lazy val accessors: List[MethodSymbol] = getAccessors(c)(tpe)

    tpe match {
      case _ if accessors.nonEmpty =>
        val accessorQuotes =
          accessors map {
            accessor =>
              val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
              q"JsonRegistry.registerNew[$accessorTpe]"
          }
        q"..$accessorQuotes"
      case _ => q"()"

    }

  }


}
