package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import au.com.fairfax.adonis.apws.macros.json._
import au.com.fairfax.adonis.utils.removePkgName

object MaterializersImpl {
  def simplifiedMeth(action: String)(inStr: String): String =
    List(action, inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(removePkgName).mkString("_")

  val jreader = "reader"
  val jbuilder = "builder"

  lazy val createItemMeth = simplifiedMeth("create") _
  lazy val formatItemMeth = simplifiedMeth("format") _

  def tpeClassNm(c: Context): c.universe.Type => String = _.typeSymbol.asClass.name.toString

  def collTypes(c: Context) = {
    import c.universe._
    List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map tpeClassNm(c)
  }

  def deliasTpeName[T: c.universe.TypeTag](c: Context): String =
    c.universe.typeOf[T].dealias.toString

  def numDealisTpeNms(c: Context) =
    List(deliasTpeName[Double](c), deliasTpeName[Float](c), deliasTpeName[Short](c), deliasTpeName[Int](c), deliasTpeName[Long](c))

  // don't declare return type after def ${TermName(createItemMeth)}(item: J), o.w. will get meaningless error of type XXX not found in macro call
  def parseItemQuote(c: Context)(tpe: c.universe.Type)(method: String) = {
    import c.universe._
    q"""
        def ${TermName(method)}(item: ${TypeName("J")}) =
          ${recurParseQuote(c)(tpe)("item")("")}
      """
  }

  def parseCollectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String) = {
    import c.universe._
    val createMeth = createItemMeth(tpe.toString)
    val mapQuote =
      q"""
          (0 until arraySize).map {
            idx => ${TermName(createMeth)}(${TermName(jreader)}.readArrayElem(array, idx))
          }
        """
    val toCollQuote =
      if (collType == tpeClassNm(c)(typeOf[Seq[_]])) mapQuote
      else q"""
          ${mapQuote}.${TermName("to" + collType)}
        """

    q"""
        def ${TermName(methodNm)}(array: J) = {
          ${parseItemQuote(c)(tpe)(createMeth)}
          val arraySize = ${TermName(jreader)}.readArrayLength(array)
          $toCollQuote
        }
      """
  }

  def parseMapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String) = {
    import c.universe._
    val List(createKeyMeth, createValMeth) = List(keyTpe, valTpe) map (t => createItemMeth(t.toString))
    var createQuote = List(parseItemQuote(c)(keyTpe)(createKeyMeth))
    if (keyTpe != valTpe)
      createQuote = parseItemQuote(c)(valTpe)(createValMeth) :: createQuote
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

  def readJsonFieldQuote(c: Context)(jsonVarNm: String)(fieldNm: String) = {
    import c.universe._
    if (fieldNm == "")
      q"${TermName(jsonVarNm)}"
    else
      q"${TermName(jreader)}.readObjectField(${TermName(jsonVarNm)}, $fieldNm)"
  }

  def readDoubleQuote(c: Context)(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String) = {
    import c.universe._
    val quote = q"${TermName(jreader)}.readNumber(${readJsonFieldQuote(c)(jsonVarNm)(fieldNm)})"
    if (tpe == typeOf[Double]) quote
    else q"${quote}.asInstanceOf[$tpe]"
  }

  def recurParseQuote(c: Context)(tpe: c.universe.Type)(jsonVarNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val accessors = (tpe.decls collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }).toList

    println(s"recurParseQuote, tpe = $tpe, tpe.dealias = ${tpe.dealias}, accessors = $accessors")
    tpe match {
      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        readDoubleQuote(c)(t)(jsonVarNm)(fieldNm)
      case t: Type if deliasTpeName[Boolean](c) == t.dealias.toString =>
        q"${TermName(jreader)}.readBoolean(${readJsonFieldQuote(c)(jsonVarNm)(fieldNm)})"
      case t: Type if deliasTpeName[String](c) == t.dealias.toString =>
        q"${TermName(jreader)}.readString(${readJsonFieldQuote(c)(jsonVarNm)(fieldNm)})"
      case t: Type if collTypes(c) contains tpeClassNm(c)(t) =>
        val parseCollection = "parseCollection"
        q"""
            ${parseCollectionQuote(c)(t.typeArgs.head)(tpeClassNm(c)(t))(parseCollection)}
            ${TermName(parseCollection)}(${readJsonFieldQuote(c)(jsonVarNm)(fieldNm)})
        """
      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        val parseMap = "parseMap"
        val List(key, value) = t.typeArgs
        q"""
            ${parseMapQuote(c)(key)(value)(parseMap)}
            ${TermName(parseMap)}(${readJsonFieldQuote(c)(jsonVarNm)(fieldNm)})
        """
      case _ => accessors match {
        case x :: _ =>
          val newJsValueVar = s"${jsonVarNm}_$fieldNm"
          val constrArgs = accessors map {
            accessor =>
              val fieldName = accessor.name.toString
              val fieldTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
              recurParseQuote(c)(fieldTpe)(newJsValueVar)(fieldName)
          }
          q"""
            val ${TermName(newJsValueVar)} = ${readJsonFieldQuote(c)(jsonVarNm)(fieldNm)}
            new $tpe(..$constrArgs)
          """
      }
    }
  }

  def materializeParser[T: c.WeakTypeTag](c: Context): c.Expr[JsonParserBACKUP[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
      q"""
          object GenJsonParser extends au.com.fairfax.adonis.apws.macros.json.JsonParserBACKUP[$tpe] {
            import org.scalajs.spickling._
            override def parse[J](json: J)(implicit ${TermName(jreader)}: PReader[J]) = {
              ${recurParseQuote(c)(tpe)("json")("args")}
            }
          }
          GenJsonParser
      """
    println(result)
    c.Expr[JsonParserBACKUP[T]](result)
  }


  def formatItemQuote(c: Context)(tpe: c.universe.Type)(method: String) = {
    import c.universe._
    q"""
        def ${TermName(method)}(obj: $tpe) =
          ${recurFormatQuote(c)(tpe)("obj")}
      """
  }

  def formatCollectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String) = {
    import c.universe._
    val formatMeth = formatItemMeth(tpe.toString)
    q"""
        def ${TermName(methodNm)}(objList: ${TypeName(collType)}[$tpe]) = {
          ${formatItemQuote(c)(tpe)(formatMeth)}
          val jsonList = objList.map {
            obj => ${TermName(formatMeth)}(obj)
          }
          ${TermName(jbuilder)}.makeArray(jsonList: _*)
        }
      """
  }

  def formatMapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String) = {
    import c.universe._
    val List(formatKeyMeth, formatValMeth) = List(keyTpe, valTpe) map (t => formatItemMeth(t.toString))
    var formatQuote = List(formatItemQuote(c)(keyTpe)(formatKeyMeth))
    if (keyTpe != valTpe)
      formatQuote = formatItemQuote(c)(valTpe)(formatValMeth) :: formatQuote
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

  def formatDoubleQuote(c: Context)(tpe: c.universe.Type)(objNm: String) = {
    import c.universe._
    val numQuote =
      if (tpe == typeOf[Double]) q"${TermName(objNm)}"
      else q"${TermName(objNm)}.asInstanceOf[Double]"
    q"${TermName(jbuilder)}.makeNumber($numQuote)"
  }

  def recurFormatQuote(c: Context)(tpe: c.universe.Type)(objNm: String): c.universe.Tree = {
    import c.universe._
    val accessors = (tpe.decls collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }).toList

    println(s"recurFormatQuote, tpe = $tpe, tpe.dealias = ${tpe.dealias}, accessors = $accessors")
    tpe match {
      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        formatDoubleQuote(c)(t)(objNm)
      case t: Type if deliasTpeName[Boolean](c) == t.dealias.toString =>
        q"${TermName(jbuilder)}.makeBoolean(${TermName(objNm)})"
      case t: Type if deliasTpeName[String](c) == t.dealias.toString =>
        q"${TermName(jbuilder)}.makeString(${TermName(objNm)})"
      case t: Type if collTypes(c) contains tpeClassNm(c)(t) =>
        val formatCollection = "formatCollection"
        q"""
            ${formatCollectionQuote(c)(t.typeArgs.head)(tpeClassNm(c)(t))(formatCollection)}
            ${TermName(formatCollection)}(${TermName(objNm)})
        """
      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        val formatMap = "formatMap"
        val List(key, value) = t.typeArgs
        q"""
            ${formatMapQuote(c)(key)(value)(formatMap)}
            ${TermName(formatMap)}(${TermName(objNm)})
        """
      case _ => accessors match {
        case x :: _ =>
          val jsonFields = accessors map {
            accessor =>
              val fieldName = accessor.name.toString
              val fieldTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
              q"""
                val ${TermName(fieldName)} = ${TermName(objNm)}.${TermName(fieldName)}
                $fieldName -> ${recurFormatQuote(c)(fieldTpe)(fieldName)}
              """
          }
          q"""
            ${TermName(jbuilder)}.makeObject(..$jsonFields)
          """
      }
    }
  }

  def materializeFormatter[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatterBACKUP[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
      q"""
          object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.json.JsonFormatterBACKUP[$tpe] {
            import org.scalajs.spickling._
            override def format[J](obj: Any)(implicit ${TermName(jbuilder)}: PBuilder[J]) = {
              val typedObj = obj.asInstanceOf[$tpe]
              ${TermName(jbuilder)}.makeObject(
                "cmd" -> ${TermName(jbuilder)}.makeString(${tpe.toString}),
                "args" -> ${recurFormatQuote(c)(tpe)(s"typedObj")}
              )
            }
          }
          GenJsonFormatter
      """
    println(result)
    c.Expr[JsonFormatterBACKUP[T]](result)
  }
}

trait JsonMaterializersBACKUP {
  implicit def jsonParserMacro[T]: JsonParserBACKUP[T] = macro MaterializersImpl.materializeParser[T]

  implicit def jsonFormatterMacro[T]: JsonFormatterBACKUP[T] = macro MaterializersImpl.materializeFormatter[T]
}