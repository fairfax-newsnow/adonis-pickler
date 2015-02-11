package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait RegisterHelper[T] {
  def traversableRegister(implicit parser: JsonParser[T], formatter: JsonFormatter[T], keyProvider: TypeKeyProvider[T]): (String Map JsonParser[_], String Map JsonFormatter[_])
}

object RegisterHelper {
  implicit def registerHelperMacros[T]: RegisterHelper[T] = macro materialize[T]

  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[RegisterHelper[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
      q"""
        implicit object GenRegisterHelper extends au.com.fairfax.adonis.apws.macros.RegisterHelper[${tpe.dealias}] {
          def traversableRegister(implicit parser: au.com.fairfax.adonis.apws.macros.JsonParser[${tpe.dealias}], formatter: au.com.fairfax.adonis.apws.macros.JsonFormatter[${tpe.dealias}], keyProvider: au.com.fairfax.adonis.apws.macros.TypeKeyProvider[${tpe.dealias}]): (String Map au.com.fairfax.adonis.apws.macros.JsonParser[_], String Map au.com.fairfax.adonis.apws.macros.JsonFormatter[_]) = {

            (Map[String, au.com.fairfax.adonis.apws.macros.JsonParser[_]](), Map[String, au.com.fairfax.adonis.apws.macros.JsonFormatter[_]]())
          }
          
          private def ownRegisterForTest()
        }
        
        GenRegisterHelper
      """
    
    println(
      s"""
         |RegisterHelper.materialze(), result =
         |$result
         |""".stripMargin)
    
    c.Expr[RegisterHelper[T]](result)
  }

}
