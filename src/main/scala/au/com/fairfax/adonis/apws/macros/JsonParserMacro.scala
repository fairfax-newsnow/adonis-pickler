package au.com.fairfax.adonis.apws.macros

import play.api.libs.json._
import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.reflect.runtime.universe
import au.com.fairfax.adonis.json.SampleUnpickler

object JsonParserMacro {
  type ParserType = JsValue => Any
  val prefix = "jsVal"

  def materializeJsonParser[T: c.WeakTypeTag](c: Context): c.Expr[ParserType] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass

    if (!sym.isCaseClass) {
      c.error(c.enclosingPosition,
        "Cannot materialize JsonParser for non-case class")
      return c.Expr[ParserType](q"null")
    }

    //    val result = q"""
    //      implicit object GenUnpickler extends au.com.fairfax.adonis.json.SampleUnpickler[$tpe] {
    //        import org.scalajs.spickling._
    //        import au.com.fairfax.adonis.json._
    //        override def unpickle[P](pickle: P)(
    //            implicit registry: SamplePicklerRegistry,
    //      reader: PReader[P]): $tpe = $unpickleLogic
    //      }
    //      GenUnpickler
    //    """

    val result = q"""
                     def parse(jsValue: play.api.libs.json.JsValue): Any = "testing"
                     parse _
                     """
    println(
      s"""result =
           $result
       """.stripMargin)
    c.Expr[ParserType](result)
  }

}

object JsonMaterializers {

  import JsonParserMacro._

  def materializeJsonParser[T]: ParserType = macro JsonParserMacro.materializeJsonParser[T]

}