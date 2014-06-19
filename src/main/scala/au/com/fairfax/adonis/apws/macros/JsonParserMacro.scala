package au.com.fairfax.adonis.apws.macros

import play.api.libs.json._
import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.reflect.runtime.universe

object JsonParserMacro {
  type ParserType = JsValue => Any

  def materializeJsonParser[T: c.WeakTypeTag](c: Context): c.Expr[ParserType] = {
    import c.universe._

    val rootJsValue = "rootJsValue"

    def subExpr(tpe: c.universe.Type)(jsValueVar: String)(fieldName: String): c.universe.Tree = {
      //      println(s"tpe = $tpe, jsValueVar = $jsValueVar, fieldName = $fieldName")

      val accessors = (tpe.declarations collect {
        case acc: MethodSymbol if acc.isCaseAccessor => acc
      }).toList

      accessors match {
        case x :: _ =>
          if (!tpe.typeSymbol.asClass.isCaseClass) {
            c.error(c.enclosingPosition, "Cannot materialize JsonParser for non-case class")
            return q"null"
          }

          val newJsValueVar = s"${jsValueVar}_$fieldName"

          val constrArgs = accessors map {
            accessor =>
              val fieldName = accessor.name.toString
              val fieldTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
              subExpr(fieldTpe)(newJsValueVar)(fieldName)
          }
          //          println(s"constrArgs = $constrArgs")

          q"""
            {
              val ${TermName(newJsValueVar)} = (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsObject]
              new $tpe(..$constrArgs)
            }
          """

        case _ =>
          println(s"tpe.dealias = ${tpe.dealias}")
          q"""
              {
                (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsNumber].value.toDouble
              }
          """
      }
    }

    val result =
      q"""
          import play.api.libs.json._
          def parse(${TermName(rootJsValue)}: JsValue): Any = {
            ${subExpr(weakTypeOf[T])(rootJsValue)("args")}
          }
          parse _
      """

    println( s"""result =
           $result
       """.stripMargin)
    c.Expr[ParserType](result)
  }

}

import JsonParserMacro._

object JsonMaterializers {
  def jsonParserMacro[T]: ParserType = macro materializeJsonParser[T]
}