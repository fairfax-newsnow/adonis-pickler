package au.com.fairfax.adonis.apws.macros

import play.api.libs.json._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox._

import scala.reflect.runtime.universe._

object JsonParserMacro {
  type ParserType = JsValue => Any

  val rootJsValue = "rootJsValue"
  val parseCollectionMeth = "parseCollection"
  val parseMapMeth = "parseMap"

  def createItemMeth(inStr: String): String =
    List("create", inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map {
      s =>
        val idx = s.lastIndexOf(".")
        if (idx < 0) s
        else s.substring(idx + 1, s.length)
    }.mkString("_")

  def materializeJsonParser[T: c.WeakTypeTag](c: Context): c.Expr[ParserType] = {
    import c.universe._

    lazy val numTypes = List(typeOf[Double], typeOf[Float], typeOf[Short], typeOf[Int], typeOf[Long])

    // don't declare return type after def ${TermName("create" + simplified)}(item: JsValue), o.w. will get meaningless error of type ... not found in macro call
    def createItemQuote(tpe: c.universe.Type)(createItemMeth: String) =
      q"""
        def ${TermName(createItemMeth)}(item: JsValue) =
          ${subExpr(tpe)("item")("")}
      """

    def parseCollectionQuote(tpe: c.universe.Type)(collType: String) = {
      val create = createItemMeth(tpe.toString)
      q"""
        def ${TermName(parseCollectionMeth)}(jsArray: JsArray) = {
          ${createItemQuote(tpe)(create)}
          jsArray.value.map(${TermName(create)}).to[${TypeName(collType)}]
        }
      """
    }

    def parseMapQuote(keyTpe: c.universe.Type)(valTpe: c.universe.Type) = {
      val List(createKey, createVal) = List(keyTpe, valTpe) map (t => createItemMeth(t.toString))
      var createQuote = List(createItemQuote(keyTpe)(createKey))
      if (keyTpe != valTpe)
        createQuote = createItemQuote(valTpe)(createVal) :: createQuote
      q"""
        def ${TermName(parseMapMeth)}(jsArray: JsArray) = {
          ..$createQuote
          jsArray.value.map { item =>
            val seq = item.asInstanceOf[JsArray].value
            val key = seq(0)
            val value = seq(1)
            ${TermName(createKey)}(key) -> ${TermName(createVal)}(value)
          }.toMap
        }
      """
    }

    def extractJsonField(jsValueVar: String)(fieldName: String) =
      if (fieldName == "")
        q"${TermName(jsValueVar)}"
      else
        q"(${TermName(jsValueVar)} \ $fieldName)"

    def numQuote(jsValueVar: String)(fieldName: String)(typeStr: String) =
      q"${extractJsonField(jsValueVar)(fieldName)}.asInstanceOf[JsNumber].value.${TermName("to" + typeStr)}"

    def subExpr(tpe: c.universe.Type)(jsValueVar: String)(fieldName: String): c.universe.Tree = {
      val accessors = (tpe.decls collect {
        case acc: MethodSymbol if acc.isCaseAccessor => acc
      }).toList

      val tpeSymClass: Type => String = _.typeSymbol.asClass.fullName

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
            val ${TermName(newJsValueVar)} = ${extractJsonField(jsValueVar)(fieldName)}.asInstanceOf[JsObject]
            new $tpe(..$constrArgs)
          """

        case _ =>
          tpe match {
            case t: Type if numTypes contains t => numQuote(jsValueVar)(fieldName)(t.toString)
            case t: Type if t == typeOf[Boolean] => q"${extractJsonField(jsValueVar)(fieldName)}.asInstanceOf[JsBoolean].value"
            case t: Type if t == typeOf[String] => q"${extractJsonField(jsValueVar)(fieldName)}.asInstanceOf[JsString].value"
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[List[_]]) =>
              q"""
                ${parseCollectionQuote(t.typeArgs.head)("List")}
                ${TermName(parseCollectionMeth)}(${extractJsonField(jsValueVar)(fieldName)}.asInstanceOf[JsArray])
              """
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[Map[_, _]]) =>
              val List(key, value) = t.typeArgs
              q"""
                ${parseMapQuote(key)(value)}
                ${TermName(parseMapMeth)}(${extractJsonField(jsValueVar)(fieldName)}.asInstanceOf[JsArray])
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