package au.com.fairfax.adonis.apws.macros

import play.api.libs.json._
import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.reflect.runtime.universe._

object JsonParserMacro {
  type ParserType = JsValue => Any

  def materializeJsonParser[T: c.WeakTypeTag](c: Context): c.Expr[ParserType] = {
    import c.universe._

    val rootJsValue = "rootJsValue"

    lazy val numTypes = List(typeOf[Double], typeOf[Float], typeOf[Short], typeOf[Int], typeOf[Long]) map typeToString

    def typeToString(tpe: c.universe.Type): String = tpe.toString

    def numQuote(jsValueVar: String)(fieldName: String)(typeStr: String) =
      q"""
          {
            (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsNumber].value.${TermName("to" + typeStr)}
          }
      """

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
          val typeStr = tpe.toString
          typeStr match {
            case t: String if numTypes contains t => numQuote(jsValueVar)(fieldName)(typeStr)
            case t: String if t == typeToString(typeOf[Boolean]) =>
              q"""
                  {
                    (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsBoolean].value
                  }
              """
            case _ =>
              q"""
                  {
                    (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsString].value
                  }
              """
          }
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

    println(
      s"""result =
           $result
       """.stripMargin)

    c.Expr[ParserType](result)
  }

}

import JsonParserMacro._

object JsonMaterializers {
  def jsonParserMacro[T]: ParserType = macro materializeJsonParser[T]
}