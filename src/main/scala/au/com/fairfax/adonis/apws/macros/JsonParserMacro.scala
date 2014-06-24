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

    // don't declare return type after def ${TermName(createItemMeth)}(item: P), o.w. will get meaningless error of type ... not found in macro call
    def createItemQuote(tpe: c.universe.Type)(createItemMeth: String) =
      q"""
        def ${TermName(createItemMeth)}(item: P) =
          ${subExpr(tpe)("item")("")}
      """

    def parseCollectionQuote(tpe: c.universe.Type)(collType: String) = {
      val createMeth = createItemMeth(tpe.toString)
      q"""
        def ${TermName(parseCollectionMeth)}(array: P) = {
          ${createItemQuote(tpe)(createMeth)}
          val arraySize = reader.readArrayLength(array)
          (0 until arraySize).to[${TypeName(collType)}].map {
            idx => ${TermName(createMeth)}(reader.readArrayElem(array, idx))
          }
        }
      """
    }

    def parseMapQuote(keyTpe: c.universe.Type)(valTpe: c.universe.Type) = {
      val List(createKeyMeth, createValMeth) = List(keyTpe, valTpe) map (t => createItemMeth(t.toString))
      var createQuote = List(createItemQuote(keyTpe)(createKeyMeth))
      if (keyTpe != valTpe)
        createQuote = createItemQuote(valTpe)(createValMeth) :: createQuote
      q"""
        def ${TermName(parseMapMeth)}(map: P) = {
          ..$createQuote
          val mapSize = reader.readArrayLength(map)
          (0 until mapSize).toList.map { idx =>
            val tuple = reader.readArrayElem(map, idx)
            val key = reader.readArrayElem(tuple, 0)
            val value = reader.readArrayElem(tuple, 1)
            ${TermName(createKeyMeth)}(key) -> ${TermName(createValMeth)}(value)
          }.toMap
        }
      """
    }

    def extractJsonField(jsonVarNm: String)(fieldNm: String) =
      if (fieldNm == "")
        q"${TermName(jsonVarNm)}"
      else
        q"reader.readObjectField(${TermName(jsonVarNm)}, $fieldNm)"

    def readDouble(jsonVarNm: String)(fieldNm: String) =
      q"reader.readNumber(${extractJsonField(jsonVarNm)(fieldNm)})"

    def subExpr(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree = {
      val accessors = (tpe.decls collect {
        case acc: MethodSymbol if acc.isCaseAccessor => acc
      }).toList

      accessors match {
        case x :: _ =>
          if (!tpe.typeSymbol.asClass.isCaseClass) {
            c.error(c.enclosingPosition, "Cannot materialize JsonParser for non-case class")
            return q"null"
          }
          val newJsValueVar = s"${jsonVarNm}_$fieldNm"
          val constrArgs = accessors map {
            accessor =>
              val fieldName = accessor.name.toString
              val fieldTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
              subExpr(fieldTpe)(newJsValueVar)(fieldName)
          }
          q"""
            val ${TermName(newJsValueVar)} = ${extractJsonField(jsonVarNm)(fieldNm)}
            new $tpe(..$constrArgs)
          """

        case _ =>
          lazy val tpeSymClass: Type => String = _.typeSymbol.asClass.fullName
          tpe match {
            case t: Type if t == typeOf[Double] => readDouble(jsonVarNm)(fieldNm)
            case t: Type if t == typeOf[Float] => q"${readDouble(jsonVarNm)(fieldNm)}.asInstanceOf[Float]"
            case t: Type if t == typeOf[Short] => q"${readDouble(jsonVarNm)(fieldNm)}.asInstanceOf[Short]"
            case t: Type if t == typeOf[Int] => q"${readDouble(jsonVarNm)(fieldNm)}.asInstanceOf[Int]"
            case t: Type if t == typeOf[Long] => q"${readDouble(jsonVarNm)(fieldNm)}.asInstanceOf[Long]"
            case t: Type if t == typeOf[Boolean] => q"reader.readBoolean(${extractJsonField(jsonVarNm)(fieldNm)})"
            case t: Type if t == typeOf[String] => q"reader.readString(${extractJsonField(jsonVarNm)(fieldNm)})"
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[List[_]]) =>
              q"""
                ${parseCollectionQuote(t.typeArgs.head)("List")}
                ${TermName(parseCollectionMeth)}(${extractJsonField(jsonVarNm)(fieldNm)})
              """
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[Map[_, _]]) =>
              val List(key, value) = t.typeArgs
              q"""
                ${parseMapQuote(key)(value)}
                ${TermName(parseMapMeth)}(${extractJsonField(jsonVarNm)(fieldNm)})
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

object JsonMaterializers {

  import JsonParserMacro._

  def jsonParserMacro[T]: JsonParser[T] = macro materializeJsonParser[T]
}