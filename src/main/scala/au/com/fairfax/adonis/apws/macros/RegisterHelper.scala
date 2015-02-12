package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait TraversableRegistrar[T] {
  def traversableRegister: Unit
}

object TraversableRegistrar {
  implicit def registerHelperMacros[T]: TraversableRegistrar[T] = macro materialize[T]

  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[TraversableRegistrar[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val doubleTpeStr = "Double"
    val result =
//      if (JsonRegistry alreadyRegistered tpe.toString) {
//        println(s"$tpe already registered, GenTraversableRegistrar is a simple generated code ")
//        quoteForAlreadyRegistered(c)(tpe)
//      }
//      else {
//        println(s"$tpe yet to register, GenTraversableRegistrar will trigger generation of parser/formatter and register them")
        quoteForYetToRegister(c)(tpe)(doubleTpeStr)
//      }
    
    println(
      s"""
         |TraversableRegistrar.materialize(), result =
         |$result
       """.stripMargin)
    
    c.Expr[TraversableRegistrar[T]](result)
  }
  
  private def quoteForAlreadyRegistered(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"""
      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar

      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe.dealias}] {
        def traversableRegister: Unit = {}
      }
        
      GenTraversableRegistrar
    """    
  } 
  
  private def quoteForYetToRegister(c: Context)(tpe: c.universe.Type)(anotherTpeStr: String): c.universe.Tree = {
    import c.universe._
    q"""
      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar
      import au.com.fairfax.adonis.apws.macros.JsonParser
      import au.com.fairfax.adonis.apws.macros.JsonFormatter
      import au.com.fairfax.adonis.apws.macros.JsonRegistry

      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe.dealias}] {
        def traversableRegister: Unit = {
          println("GenTraversableRegistrar.traversableRegister() is triggered")
          register
        }
          
        private def register(implicit parser: JsonParser[${TypeName(anotherTpeStr)}], formatter: JsonFormatter[${TypeName(anotherTpeStr)}]) {
          add((${anotherTpeStr}, parser))((${anotherTpeStr}, formatter))
        }
          
      }
        
      GenTraversableRegistrar
      """
  }

}
