package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import au.com.fairfax.adonis.utils.json._

import scala.reflect.runtime.universe._

object JsonParserMacro {
  val parseCollectionMeth = "parseCollection"
  val parseMapMeth = "parseMap"

  def createItemMeth(inStr: String): String =
    List("create", inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map {
      s =>
        val idx = s.lastIndexOf(".")
        if (idx < 0) s
        else s.substring(idx + 1, s.length)
    }.mkString("_")

  def materializeJsonParser[T: c.WeakTypeTag](c: Context): c.Expr[JsonParser[T]] = {
    import c.universe._

    // don't declare return type after def ${TermName("create" + simplified)}(item: JsValue), o.w. will get meaningless error of type ... not found in macro call
    def createItemQuote(tpe: c.universe.Type)(createItemMeth: String) =
      q"""
        def ${TermName(createItemMeth)}(item: P) =
          ${subExpr(tpe)("item")("")}
      """

    def parseCollectionQuote(tpe: c.universe.Type)(collType: String) = {
      val create = createItemMeth(tpe.toString)
      q"""
        def ${TermName(parseCollectionMeth)}(array: P) = {
          ${createItemQuote(tpe)(create)}
          val arraySize = reader.readArrayLength(array)
          (0 until arraySize).to[${TypeName(collType)}].map {
            idx => ${TermName(create)}(reader.readArrayElem(array, idx))
          }
        }
      """
    }

    def parseMapQuote(keyTpe: c.universe.Type)(valTpe: c.universe.Type) = {
      val List(createKey, createVal) = List(keyTpe, valTpe) map (t => createItemMeth(t.toString))
      var createQuote = List(createItemQuote(keyTpe)(createKey))
      if (keyTpe != valTpe)
        createQuote = createItemQuote(valTpe)(createVal) :: createQuote
      q"""
        def ${TermName(parseMapMeth)}(map: P) = {
          ..$createQuote
          val mapSize = reader.readArrayLength(map)
          (0 until mapSize).toList.map { idx =>
            val tuple = reader.readArrayElem(map, idx)
            val key = reader.readArrayElem(tuple, 0)
            val value = reader.readArrayElem(tuple, 1)
            ${TermName(createKey)}(key) -> ${TermName(createVal)}(value)
          }.toMap
        }
      """
    }

    def extractJsonField(jsValueVar: String)(fieldName: String) =
      if (fieldName == "")
        q"${TermName(jsValueVar)}"
      else
        q"reader.readObjectField(${TermName(jsValueVar)}, $fieldName)"

    def readDouble(jsValueVar: String)(fieldName: String) =
      q"reader.readNumber(${extractJsonField(jsValueVar)(fieldName)})"

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
            val ${TermName(newJsValueVar)} = ${extractJsonField(jsValueVar)(fieldName)}
            new $tpe(..$constrArgs)
          """

        case _ =>
          tpe match {
            case t: Type if t == typeOf[Double] => readDouble(jsValueVar)(fieldName)
            case t: Type if t == typeOf[Float] => q"${readDouble(jsValueVar)(fieldName)}.asInstanceOf[Float]"
            case t: Type if t == typeOf[Short] => q"${readDouble(jsValueVar)(fieldName)}.asInstanceOf[Short]"
            case t: Type if t == typeOf[Int] => q"${readDouble(jsValueVar)(fieldName)}.asInstanceOf[Int]"
            case t: Type if t == typeOf[Long] => q"${readDouble(jsValueVar)(fieldName)}.asInstanceOf[Long]"
            case t: Type if t == typeOf[Boolean] => q"reader.readBoolean(${extractJsonField(jsValueVar)(fieldName)})"
            case t: Type if t == typeOf[String] => q"reader.readString(${extractJsonField(jsValueVar)(fieldName)})"
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[List[_]]) =>
              q"""
                ${parseCollectionQuote(t.typeArgs.head)("List")}
                ${TermName(parseCollectionMeth)}(${extractJsonField(jsValueVar)(fieldName)})
              """
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[Map[_, _]]) =>
              val List(key, value) = t.typeArgs
              q"""
                ${parseMapQuote(key)(value)}
                ${TermName(parseMapMeth)}(${extractJsonField(jsValueVar)(fieldName)})
              """
          }
      }
    }

    val tpe = weakTypeOf[T]
    val result =
      q"""
          object GenJsonParser extends au.com.fairfax.adonis.utils.json.JsonParser[$tpe] {
            import org.scalajs.spickling._
            override def parse[P](json: P)(implicit reader: PReader[P]) = {
              ${subExpr(tpe)("json")("args")}
            }
          }
          GenJsonParser
      """

    println(
      s"""result =
           $result
       """.stripMargin)

    c.Expr[JsonParser[T]](result)
  }
}

import JsonParserMacro._

object JsonMaterializers {
  def jsonParserMacro[T]: JsonParser[T] = macro materializeJsonParser[T]
}