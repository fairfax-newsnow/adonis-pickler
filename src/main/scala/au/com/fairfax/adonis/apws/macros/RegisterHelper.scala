package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait RegisterHelper[T] {
  def traversableRegister: (String Map JsonParser[_], String Map JsonFormatter[_])
}

object RegisterHelper {
  implicit def registerHelperMacros[T]: RegisterHelper[T] = macro materialize[T]

  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[RegisterHelper[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
      q"""
        implicit object GenRegisterHelper extends au.com.fairfax.adonis.apws.macros.RegisterHelper[${tpe.dealias}] {
          def traversableRegister: (String Map au.com.fairfax.adonis.apws.macros.JsonParser[_], String Map au.com.fairfax.adonis.apws.macros.JsonFormatter[_]) = {
            (Map[String, au.com.fairfax.adonis.apws.macros.JsonParser[_]](), Map[String, au.com.fairfax.adonis.apws.macros.JsonFormatter[_]]())
          }
        }
        
        GenRegisterHelper
      """
    
    println(
      s"""
         |ReigsterHelper.materialze(), result = 
         |$result
         |""".stripMargin)
    
    c.Expr[RegisterHelper[T]](result)
  }

}
