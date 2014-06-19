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
          tpe match {
            case t: Type if numTypes contains t => numQuote(jsValueVar)(fieldName)(t.toString)
            case t: Type if t == typeOf[Boolean] =>
              q"""
                  {
                    (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsBoolean].value
                  }
              """
            case t: Type if t == typeOf[String] =>
              q"""
                  {
                    (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsString].value
                  }
              """
//            case t: Type if t.typeSymbol.asClass.getClass == classOf[List[_]].getName =>
//              q"""
//                  {
//                    (${TermName(jsValueVar)} \ $fieldName).asInstanceOf[JsArray].value
//                  }
//              """
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

  def testMaterialize[T: c.WeakTypeTag](c: Context): c.Expr[ParserType] = {
    import c.universe._

    val tpe = weakTypeOf[T]

    println(s"tpe = $tpe, tpe.typeSymbol.asClass.getName = ${tpe.typeSymbol.asClass.fullName}")
    println(s"tpe.typeArgs = ${tpe.typeArgs}")

    val accessors = (tpe.declarations collect {
      case acc: MethodSymbol if acc.isCaseAccessor => acc
    }).toList

    val accessor = accessors.head
    println(s"fieldName = ${accessor.name.toString}")
    val accessorType = accessor.returnType
    println(s"accessorType without type params substitution = ${accessorType}")
    val subsitutedAccType = accessorType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
    println(s"accessorType with type params substitution = $subsitutedAccType")

    val result =
      q"""
          import play.api.libs.json._
          def parser(jsValue: JsValue): Any = "testing"
          parser _
      """
    c.Expr[ParserType](result)
  }
}

import JsonParserMacro._

object JsonMaterializers {
  def jsonParserMacro[T]: ParserType = macro materializeJsonParser[T]

  def test[T]: ParserType = macro testMaterialize[T]
}