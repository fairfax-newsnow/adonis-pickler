package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait RegisterHelper[T] {
  //def traversableRegisterTest(b: Boolean)(implicit parser: JsonParser[T], formatter: JsonFormatter[T], keyProvider: TypeKeyProvider[T]): (String Map JsonParser[_], String Map JsonFormatter[_])
  
  def traversableRegister: Unit
}

object RegisterHelper {
  implicit def registerHelperMacros[T]: RegisterHelper[T] = macro materialize[T]
  
  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[RegisterHelper[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val intStr = "Int"
    val result =
      q"""
        import au.com.fairfax.adonis.apws.macros.RegisterHelper
        import au.com.fairfax.adonis.apws.macros.JsonParser
        import au.com.fairfax.adonis.apws.macros.JsonFormatter
        import au.com.fairfax.adonis.apws.macros.TypeKeyProvider
        import au.com.fairfax.adonis.apws.macros.JsonRegistry

        implicit object GenRegisterHelper extends RegisterHelper[${tpe.dealias}] {
          def traversableRegister: Unit = {
            /*register*/
          }
          
          private def register(implicit parser: JsonParser[${tpe.dealias}], formatter: JsonFormatter[${tpe.dealias}]) {
            add((${tpe.dealias.toString}, parser))((${tpe.dealias.toString}, formatter))
          }
          
        }
        
        GenRegisterHelper
      """
    
    println(
      s"""
         |RegisterHelper.materialize(), result =
         |$result
       """.stripMargin)
    
    c.Expr[RegisterHelper[T]](result)
  }

//  def materializeFor[T: c.universe.WeakTypeTag](c: Context): c.Expr[RegisterHelper[T]] = {
//    import c.universe._
//    val tpe = weakTypeOf[T]
//    val result =
//      q"""
//        implicit object GenRegisterHelper extends au.com.fairfax.adonis.apws.macros.RegisterHelper[${tpe.dealias}] {
//          def traversableRegisterTest(b: Boolean)(implicit parser: au.com.fairfax.adonis.apws.macros.JsonParser[${tpe.dealias}], formatter: au.com.fairfax.adonis.apws.macros.JsonFormatter[${tpe.dealias}], keyProvider: au.com.fairfax.adonis.apws.macros.TypeKeyProvider[${tpe.dealias}]): (String Map au.com.fairfax.adonis.apws.macros.JsonParser[_], String Map au.com.fairfax.adonis.apws.macros.JsonFormatter[_]) = {
//            val (parsers, formatters): (String Map au.com.fairfax.adonis.apws.macros.JsonParser[_], String Map au.com.fairfax.adonis.apws.macros.JsonFormatter[_]) = {
//              if (b) {
//                println("generate int as well!!")
//                registerIntForTest
//              } else {
//                println("no need to generate int!!")
//                (Map(), Map())
//              }
//            }
//
//            (parsers + (${tpe.dealias.toString} -> parser), formatters + (${tpe.dealias.toString} -> formatter))
//          }
//
//          private def registerIntForTest(implicit parser: au.com.fairfax.adonis.apws.macros.JsonParser[Int], formatter: au.com.fairfax.adonis.apws.macros.JsonFormatter[Int], keyProvider: au.com.fairfax.adonis.apws.macros.TypeKeyProvider[Int]): (String Map au.com.fairfax.adonis.apws.macros.JsonParser[_], String Map au.com.fairfax.adonis.apws.macros.JsonFormatter[_]) = {
//            (Map("Int" -> parser), Map("Int" -> formatter))
//          }
//        }
//
//        GenRegisterHelper
//      """
//
//    println(
//      s"""
//         |RegisterHelper.materialze(), result =
//         |$result
//         |""".stripMargin)
//
//    c.Expr[RegisterHelper[T]](result)
//  }

}
