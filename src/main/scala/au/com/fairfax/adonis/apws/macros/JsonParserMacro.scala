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

    lazy val numTypes = List(typeOf[Double], typeOf[Float], typeOf[Short], typeOf[Int], typeOf[Long])

    def parseCollectionQuote(tpe: c.universe.Type)(collType: String) = {
      val quote = q"""
        def parseCollection(jsArray: JsArray) = {
          def createItem(item: JsValue) = {
            ${subExpr(tpe)("item")("")}
          }
          jsArray.value.map(createItem).to[${TypeName(collType)}]
        }
      """
      quote
    }

    def extractFieldString(jsValueVar: String)(fieldName: String) =
      if (fieldName == "")
        q"${TermName(jsValueVar)}"
      else
        q"(${TermName(jsValueVar)} \ $fieldName)"

    def numQuote(jsValueVar: String)(fieldName: String)(typeStr: String) =
      q"""
          {
            ${extractFieldString(jsValueVar)(fieldName)}.asInstanceOf[JsNumber].value.${TermName("to" + typeStr)}
          }
      """

    def subExpr(tpe: c.universe.Type)(jsValueVar: String)(fieldName: String): c.universe.Tree = {
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
          q"""
            {
              val ${TermName(newJsValueVar)} = ${extractFieldString(jsValueVar)(fieldName)}.asInstanceOf[JsObject]
              new $tpe(..$constrArgs)
            }
          """

        case _ =>
          tpe match {
            case t: Type if numTypes contains t => numQuote(jsValueVar)(fieldName)(t.toString)
            case t: Type if t == typeOf[Boolean] =>
              q"""
                  {
                    ${extractFieldString(jsValueVar)(fieldName)}.asInstanceOf[JsBoolean].value
                  }
              """
            case t: Type if t == typeOf[String] =>
              q"""
                  {
                    ${extractFieldString(jsValueVar)(fieldName)}.asInstanceOf[JsString].value
                  }
              """
            case t: Type if t.typeSymbol.asClass.fullName == typeOf[List[_]].typeSymbol.asClass.fullName =>
              assert(t.typeArgs.size == 1)
              q"""
                  {
                    ${parseCollectionQuote(t.typeArgs.head)("List")}
                    parseCollection(${extractFieldString(jsValueVar)(fieldName)}.asInstanceOf[JsArray])
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