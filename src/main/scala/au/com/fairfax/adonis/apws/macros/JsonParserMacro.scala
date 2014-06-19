package au.com.fairfax.adonis.apws.macros

import play.api.libs.json._
import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.reflect.runtime.universe

object JsonParserMacro {
  type ParserType = JsValue => Any
  val prefix = "jsVal"

  def materializeJsonParser[T: c.WeakTypeTag](c: Context): c.Expr[ParserType] = {
    import c.universe._

    def subExpr(tpe: c.universe.Type)(jsValueVar: String)(fieldName: String): c.universe.Tree = {
      println(s"tpe = $tpe, jsValueVar = $jsValueVar, fieldName = $fieldName")

      val newJsValueVar = s"${jsValueVar}_$fieldName"
      val jsonExtract = s"val $newJsValueVar = ($jsValueVar \\ ${"\""}$fieldName${"\""}).asInstanceOf"
      println(s"jsonExtract = $jsonExtract")

      val accessors = (tpe.declarations collect {
        case acc: MethodSymbol if acc.isCaseAccessor => acc
      }).toList

      accessors match {
        case x :: _ =>
          if (!tpe.typeSymbol.asClass.isCaseClass) {
            c.error(c.enclosingPosition, "Cannot materialize JsonParser for non-case class")
            return q"null"
          }

          val constrArgs = accessors map {
            accessor =>
              val fieldName = accessor.name.toString
              val fieldTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
              subExpr(fieldTpe)(newJsValueVar)(fieldName)
          }
          println(s"constrArgs = $constrArgs")

          q"""
            {
              $jsonExtract[JsObject]
              $tpe(..$constrArgs)
            }
          """

        case _ =>
          q"""
              {
                val ${newTermName(newJsValueVar)} = (${newTermName(jsValueVar)} \ $fieldName).asInstanceOf[JsNumber].value.toDouble
                ${newTermName(newJsValueVar)}
              }
          """
      }
    }

    val quote = subExpr(weakTypeOf[T])("rootJsValue")("args")

    val result =
      q"""
          import play.api.libs.json._
          def parse(rootJsValue: JsValue): Any = {
            $quote
          }
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