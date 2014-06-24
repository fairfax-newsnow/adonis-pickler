package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import au.com.fairfax.adonis.utils.json._

object MaterializersImpl {
  def simplifiedMeth(action: String)(inStr: String): String =
    List(action, inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map {
      s =>
        val idx = s.lastIndexOf(".")
        if (idx < 0) s
        else s.substring(idx + 1, s.length)
    }.mkString("_")

  val createItemMeth = simplifiedMeth("create") _

  val formatItemMeth = simplifiedMeth("format") _

  def materializeParser[T: c.WeakTypeTag](c: Context): c.Expr[JsonParser[T]] = {
    import c.universe._

    val parseCollectionMeth = "parseCollection"
    val parseMapMeth = "parseMap"
    val jreader = "reader"

    // don't declare return type after def ${TermName(createItemMeth)}(item: J), o.w. will get meaningless error of type XXX not found in macro call
    def createItemQuote(tpe: c.universe.Type)(method: String) =
      q"""
        def ${TermName(method)}(item: J) =
          ${recurParseQuote(tpe)("item")("")}
      """

    def parseCollectionQuote(tpe: c.universe.Type)(collType: String) = {
      val createMeth = createItemMeth(tpe.toString)
      q"""
        def ${TermName(parseCollectionMeth)}(array: J) = {
          ${createItemQuote(tpe)(createMeth)}
          val arraySize = ${TermName(jreader)}.readArrayLength(array)
          (0 until arraySize).to[${TypeName(collType)}].map {
            idx => ${TermName(createMeth)}(${TermName(jreader)}.readArrayElem(array, idx))
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
        def ${TermName(parseMapMeth)}(map: J) = {
          ..$createQuote
          val mapSize = ${TermName(jreader)}.readArrayLength(map)
          (0 until mapSize).toList.map { idx =>
            val tuple = ${TermName(jreader)}.readArrayElem(map, idx)
            val key = ${TermName(jreader)}.readArrayElem(tuple, 0)
            val value = ${TermName(jreader)}.readArrayElem(tuple, 1)
            ${TermName(createKeyMeth)}(key) -> ${TermName(createValMeth)}(value)
          }.toMap
        }
      """
    }

    def readJsonFieldQuote(jsonVarNm: String)(fieldNm: String) =
      if (fieldNm == "")
        q"${TermName(jsonVarNm)}"
      else
        q"${TermName(jreader)}.readObjectField(${TermName(jsonVarNm)}, $fieldNm)"

    def readDoubleQuote(jsonVarNm: String)(fieldNm: String) =
      q"${TermName(jreader)}.readNumber(${readJsonFieldQuote(jsonVarNm)(fieldNm)})"

    def recurParseQuote(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree = {
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
              recurParseQuote(fieldTpe)(newJsValueVar)(fieldName)
          }
          q"""
            val ${TermName(newJsValueVar)} = ${readJsonFieldQuote(jsonVarNm)(fieldNm)}
            new $tpe(..$constrArgs)
          """

        case _ =>
          lazy val tpeSymClass: Type => String = _.typeSymbol.asClass.fullName
          tpe match {
            case t: Type if t == typeOf[Double] => readDoubleQuote(jsonVarNm)(fieldNm)
            case t: Type if t == typeOf[Float] => q"${readDoubleQuote(jsonVarNm)(fieldNm)}.asInstanceOf[Float]"
            case t: Type if t == typeOf[Short] => q"${readDoubleQuote(jsonVarNm)(fieldNm)}.asInstanceOf[Short]"
            case t: Type if t == typeOf[Int] => q"${readDoubleQuote(jsonVarNm)(fieldNm)}.asInstanceOf[Int]"
            case t: Type if t == typeOf[Long] => q"${readDoubleQuote(jsonVarNm)(fieldNm)}.asInstanceOf[Long]"
            case t: Type if t == typeOf[Boolean] => q"${TermName(jreader)}.readBoolean(${readJsonFieldQuote(jsonVarNm)(fieldNm)})"
            case t: Type if t == typeOf[String] => q"${TermName(jreader)}.readString(${readJsonFieldQuote(jsonVarNm)(fieldNm)})"
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[List[_]]) =>
              q"""
                ${parseCollectionQuote(t.typeArgs.head)("List")}
                ${TermName(parseCollectionMeth)}(${readJsonFieldQuote(jsonVarNm)(fieldNm)})
              """
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[Map[_, _]]) =>
              val List(key, value) = t.typeArgs
              q"""
                ${parseMapQuote(key)(value)}
                ${TermName(parseMapMeth)}(${readJsonFieldQuote(jsonVarNm)(fieldNm)})
              """
          }
      }
    }

    val tpe = weakTypeOf[T]
    val result =
      q"""
          object GenJsonParser extends au.com.fairfax.adonis.utils.json.JsonParser[$tpe] {
            import org.scalajs.spickling._
            override def parse[J](json: J)(implicit ${TermName(jreader)}: PReader[J]) = {
              ${recurParseQuote(tpe)("json")("args")}
            }
          }
          GenJsonParser
      """
    println(result)
    c.Expr[JsonParser[T]](result)
  }

  def materializeFormatter[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatter[T]] = {
    import c.universe._

    val formatCollectionMeth = "formatCollection"
    val formatMapMeth = "formatMap"
    val jbuilder = "builder"

    def formatItemQuote(tpe: c.universe.Type)(method: String) =
      q"""
        def ${TermName(method)}(obj: $tpe) =
          ${recurFormatQuote(tpe)("obj")}
      """

    def formatCollectionQuote(tpe: c.universe.Type)(collType: String) = {
      val formatMeth = formatItemMeth(tpe.toString)
      q"""
        def ${TermName(formatCollectionMeth)}(objList: ${TypeName(collType)}[$tpe]) = {
          ${formatItemQuote(tpe)(formatMeth)}
          val jsonList = objList.map {
            obj => ${TermName(formatMeth)}(obj)
          }
          ${TermName(jbuilder)}.makeArray(jsonList: _*)
        }
      """
    }

    def formatMapQuote(keyTpe: c.universe.Type)(valTpe: c.universe.Type) = {
      val List(formatKeyMeth, formatValMeth) = List(keyTpe, valTpe) map (t => formatItemMeth(t.toString))
      var formatQuote = List(formatItemQuote(keyTpe)(formatKeyMeth))
      if (keyTpe != valTpe)
        formatQuote = formatItemQuote(valTpe)(formatValMeth) :: formatQuote
      q"""
        def ${TermName(formatMapMeth)}(map: $keyTpe Map $valTpe) = {
          ..$formatQuote
          val elems =
            map.map { t =>
              val (k, v) = t
              ${TermName(jbuilder)}.makeArray(${TermName(formatKeyMeth)}(k), ${TermName(formatValMeth)}(v))
            }.toList
          ${TermName(jbuilder)}.makeArray(elems: _*)
        }
      """
    }

    def formatDoubleQuote(tpe: c.universe.Type)(objNm: String) = {
      val numQuote =
        if (tpe == typeOf[Double]) q"${TermName(objNm)}"
        else q"${TermName(objNm)}.asInstanceOf[Double]"
      q"${TermName(jbuilder)}.makeNumber($numQuote)"
    }

    def recurFormatQuote(tpe: c.universe.Type)(objNm: String): c.universe.Tree = {
      val accessors = (tpe.decls collect {
        case acc: MethodSymbol if acc.isCaseAccessor => acc
      }).toList

      accessors match {
        case x :: _ =>
          if (!tpe.typeSymbol.asClass.isCaseClass) {
            c.error(c.enclosingPosition, "Cannot materialize JsonFormatter for non-case class")
            return q"null"
          }
          val jsonFields = accessors map {
            accessor =>
              val fieldName = accessor.name.toString
              val fieldTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
              q"""
                val ${TermName(fieldName)} = ${TermName(objNm)}.${TermName(fieldName)}
                $fieldName -> ${recurFormatQuote(fieldTpe)(fieldName)}
              """
          }
          q"""
            ${TermName(jbuilder)}.makeObject(..$jsonFields)
          """

        case _ =>
          lazy val tpeSymClass: Type => String = _.typeSymbol.asClass.fullName
          tpe match {
            case t: Type if t == typeOf[Double] => formatDoubleQuote(t)(objNm)
            case t: Type if t == typeOf[Float] => formatDoubleQuote(t)(objNm)
            case t: Type if t == typeOf[Short] => formatDoubleQuote(t)(objNm)
            case t: Type if t == typeOf[Int] => formatDoubleQuote(t)(objNm)
            case t: Type if t == typeOf[Long] => formatDoubleQuote(t)(objNm)
            case t: Type if t == typeOf[Boolean] => q"${TermName(jbuilder)}.makeBoolean(${TermName(objNm)})"
            case t: Type if t == typeOf[String] => q"${TermName(jbuilder)}.makeString(${TermName(objNm)})"
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[List[_]]) =>
              q"""
                ${formatCollectionQuote(t.typeArgs.head)("List")}
                ${TermName(formatCollectionMeth)}(${TermName(objNm)})
              """
            case t: Type if tpeSymClass(t) == tpeSymClass(typeOf[Map[_, _]]) =>
              val List(key, value) = t.typeArgs
              q"""
                ${formatMapQuote(key)(value)}
                ${TermName(formatMapMeth)}(${TermName(objNm)})
              """
          }
      }
    }

    val tpe = weakTypeOf[T]
    val result =
      q"""
          object GenJsonFormatter extends au.com.fairfax.adonis.utils.json.JsonFormatter[$tpe] {
            import org.scalajs.spickling._
            override def format[J](obj: $tpe)(implicit ${TermName(jbuilder)}: PBuilder[J]) = {
              ${TermName(jbuilder)}.makeObject(
                "cmd" -> ${TermName(jbuilder)}.makeString(${tpe.toString}),
                "args" -> ${recurFormatQuote(tpe)("obj")}
              )
            }
          }
          GenJsonFormatter
      """
    println(result)
    c.Expr[JsonFormatter[T]](result)
  }
}

object JsonMaterializerMacro {
  def jsonParserMacro[T]: JsonParser[T] = macro MaterializersImpl.materializeParser[T]

  def jsonFormatterMacro[T]: JsonFormatter[T] = macro MaterializersImpl.materializeFormatter[T]
}