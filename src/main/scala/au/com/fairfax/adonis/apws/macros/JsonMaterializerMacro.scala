package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import au.com.fairfax.adonis.apws.macros.json._
import au.com.fairfax.adonis.utils.removePkgName

object MaterializersImpl {
  def simplifiedMeth(action: String)(inStr: String): String =
    List(action, inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(removePkgName).mkString("_")

  def materializeParser[T: c.WeakTypeTag](c: Context): c.Expr[JsonParser[T]] = {
    import c.universe._

    val jreader = "reader"

    lazy val createItemMeth = simplifiedMeth("create") _

    lazy val numTypes = List(typeOf[Double], typeOf[Float], typeOf[Short], typeOf[Int], typeOf[Long])

    lazy val typeName: Type => String = _.typeSymbol.asClass.name.toString

    lazy val collTypes = List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map typeName

    // don't declare return type after def ${TermName(createItemMeth)}(item: J), o.w. will get meaningless error of type XXX not found in macro call
    def createItemQuote(tpe: c.universe.Type)(method: String) =
      q"""
        def ${TermName(method)}(item: J) =
          ${recurParseQuote(tpe)("item")("")}
      """

    def parseCollectionQuote(tpe: c.universe.Type)(collType: String)(methodNm: String) = {
      val createMeth = createItemMeth(tpe.toString)
      val mapQuote =
        q"""
          (0 until arraySize).map {
            idx => ${TermName(createMeth)}(${TermName(jreader)}.readArrayElem(array, idx))
          }
        """
      val toCollQuote =
        if (collType == typeName(typeOf[Seq[_]])) mapQuote
        else q"""
          ${mapQuote}.${TermName("to" + collType)}
        """

      q"""
        def ${TermName(methodNm)}(array: J) = {
          ${createItemQuote(tpe)(createMeth)}
          val arraySize = ${TermName(jreader)}.readArrayLength(array)
          $toCollQuote
        }
      """
    }

    def parseMapQuote(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String) = {
      val List(createKeyMeth, createValMeth) = List(keyTpe, valTpe) map (t => createItemMeth(t.toString))
      var createQuote = List(createItemQuote(keyTpe)(createKeyMeth))
      if (keyTpe != valTpe)
        createQuote = createItemQuote(valTpe)(createValMeth) :: createQuote
      q"""
        def ${TermName(methodNm)}(map: J) = {
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

    def readDoubleQuote(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String) = {
      val quote = q"${TermName(jreader)}.readNumber(${readJsonFieldQuote(jsonVarNm)(fieldNm)})"
      if (tpe == typeOf[Double]) quote
      else q"${quote}.asInstanceOf[$tpe]"
    }

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
          tpe match {
            case t: Type if numTypes contains t => readDoubleQuote(t)(jsonVarNm)(fieldNm)
            case t: Type if t == typeOf[Boolean] => q"${TermName(jreader)}.readBoolean(${readJsonFieldQuote(jsonVarNm)(fieldNm)})"
            case t: Type if t == typeOf[String] => q"${TermName(jreader)}.readString(${readJsonFieldQuote(jsonVarNm)(fieldNm)})"
            case t: Type if collTypes contains typeName(t) =>
              val parseCollection = "parseCollection"
              q"""
                ${parseCollectionQuote(t.typeArgs.head)(typeName(t))(parseCollection)}
                ${TermName(parseCollection)}(${readJsonFieldQuote(jsonVarNm)(fieldNm)})
              """
            case t: Type if typeName(t) == typeName(typeOf[Map[_, _]]) =>
              val parseMap = "parseMap"
              val List(key, value) = t.typeArgs
              q"""
                ${parseMapQuote(key)(value)(parseMap)}
                ${TermName(parseMap)}(${readJsonFieldQuote(jsonVarNm)(fieldNm)})
              """
          }
      }
    }

    val tpe = weakTypeOf[T]
    val result =
      q"""
          object GenJsonParser extends au.com.fairfax.adonis.apws.macros.json.JsonParser[$tpe] {
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

    val jbuilder = "builder"

    lazy val formatItemMeth = simplifiedMeth("format") _

    lazy val numTypes = List(typeOf[Double], typeOf[Float], typeOf[Short], typeOf[Int], typeOf[Long])

    lazy val typeName: Type => String = _.typeSymbol.asClass.name.toString

    lazy val collTypes = List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map typeName

    def formatItemQuote(tpe: c.universe.Type)(method: String) =
      q"""
        def ${TermName(method)}(obj: $tpe) =
          ${recurFormatQuote(tpe)("obj")}
      """

    def formatCollectionQuote(tpe: c.universe.Type)(collType: String)(methodNm: String) = {
      val formatMeth = formatItemMeth(tpe.toString)
      q"""
        def ${TermName(methodNm)}(objList: ${TypeName(collType)}[$tpe]) = {
          ${formatItemQuote(tpe)(formatMeth)}
          val jsonList = objList.map {
            obj => ${TermName(formatMeth)}(obj)
          }
          ${TermName(jbuilder)}.makeArray(jsonList: _*)
        }
      """
    }

    def formatMapQuote(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String) = {
      val List(formatKeyMeth, formatValMeth) = List(keyTpe, valTpe) map (t => formatItemMeth(t.toString))
      var formatQuote = List(formatItemQuote(keyTpe)(formatKeyMeth))
      if (keyTpe != valTpe)
        formatQuote = formatItemQuote(valTpe)(formatValMeth) :: formatQuote
      q"""
        def ${TermName(methodNm)}(map: $keyTpe Map $valTpe) = {
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
          tpe match {
            case t: Type if numTypes contains t => formatDoubleQuote(t)(objNm)
            case t: Type if t == typeOf[Boolean] => q"${TermName(jbuilder)}.makeBoolean(${TermName(objNm)})"
            case t: Type if t == typeOf[String] => q"${TermName(jbuilder)}.makeString(${TermName(objNm)})"
            case t: Type if collTypes contains typeName(t) =>
              val formatCollection = "formatCollection"
              q"""
                ${formatCollectionQuote(t.typeArgs.head)(typeName(t))(formatCollection)}
                ${TermName(formatCollection)}(${TermName(objNm)})
              """
            case t: Type if typeName(t) == typeName(typeOf[Map[_, _]]) =>
              val formatMap = "formatMap"
              val List(key, value) = t.typeArgs
              q"""
                ${formatMapQuote(key)(value)(formatMap)}
                ${TermName(formatMap)}(${TermName(objNm)})
              """
          }
      }
    }

    val tpe = weakTypeOf[T]
    val result =
      q"""
          object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.json.JsonFormatter[$tpe] {
            import org.scalajs.spickling._
            override def format[J](obj: Any)(implicit ${TermName(jbuilder)}: PBuilder[J]) = {
              val typedObj = obj.asInstanceOf[$tpe]
              ${TermName(jbuilder)}.makeObject(
                "cmd" -> ${TermName(jbuilder)}.makeString(${tpe.toString}),
                "args" -> ${recurFormatQuote(tpe)(s"typedObj")}
              )
            }
          }
          GenJsonFormatter
      """
    println(result)
    c.Expr[JsonFormatter[T]](result)
  }
}

trait JsonMaterializers {
  implicit def jsonParserMacro[T]: JsonParser[T] = macro MaterializersImpl.materializeParser[T]

  implicit def jsonFormatterMacro[T]: JsonFormatter[T] = macro MaterializersImpl.materializeFormatter[T]
}