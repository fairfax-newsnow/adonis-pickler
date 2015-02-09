package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import Materializer._
import au.com.fairfax.adonis.utils.simpleTypeNm

/**
 * e.g. case class IntWrapper(i: Int), this is a structure class having an integer field, therefore the code will be generated as  
 * calls recurQuote, the type is matched to a structured type having only 1 accessorField.  The generated code will be:
 *  
 * implicit object GenJsonParser extends au.com.fairfax.adonis.apws.macros.JsonParser[IntWrapper] {
 *   override def parse[J](json: J)(implicit reader: au.com.fairfax.adonis.apws.macros.JReader[J]) = {
 *     def parseJsSerialised(jsSerialised: J) = au.com.fairfax.adonis.apws.macros.JsonRegistry.parse[J](jsSerialised);
 *          
 *     {
 *       val json_args = reader.readObjectField(json, "args");  // from structuredTypeQuote()
 *       if (reader.isNull(json_args))                          // from structuredTypeQuote()
 *         throw new IllegalArgumentException("The json data contains a null attribute which is not mapped to an Option[_] attribute")  // from structuredTypeQuote() 
 *       else                     // from structuredTypeQuote()
 *         new IntWrapper(        // from structuredTypeQuote()
 *           reader.readNumber(   // from eachAccessorQuote and then numericValQuote()
 *             reader.readObjectField(json_args, "i")).asInstanceOf[Int]  // numericValQuote() and then fieldQuote()
 *         )
 *     }
 *   };
 * };
 * GenJsonParser
 */
object ParserMaterializerImpl extends Materializer[JsonParser] {
  def jsonIo(c: Context): c.universe.TermName = c.universe.TermName("reader")

  def concatVarNms(varNms: String*): String =
    varNms mkString "_"

  /**
   * Qutoe of method definition that parse an Item, it will be something like
   * def $methodNm(item: J) = ???
   * 
   * Note: don't declare return type after def ${TermName(createItemMeth)}(item: J), o.w. will get meaningless error of type XXX not found in macro call
   */
  def quoteOfHandleItemDef(c: Context)(itemTpe: c.universe.Type)(methodNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      def $methodNm(item: ${TypeName("J")}) =
        ${recurQuote(c)(itemTpe)("item")("")(false)}
    """
  }

  /**
   * Quote to parse a map, it will be something like
   * def parseMap(map: J) = ???
   * parseMap(reader.readObjectField(objNm, s"$fieldNm"))
   */
  def mapQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(kvTpes: (c.universe.Type, c.universe.Type))(kvMeths: (c.universe.TermName, c.universe.TermName))(itemQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val(keyMeth, valMeth) = kvMeths
    val parseMapMethNm = TermName("parseMap")
    q"""
      def $parseMapMethNm(map: J) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "map") {
          q"""
            ..$itemQuotes
            val mapSize = ${jsonIo(c)}.readArrayLength(map)
            (0 until mapSize).toList.map { idx =>
              val tuple = ${jsonIo(c)}.readArrayElem(map, idx)
              val key = ${jsonIo(c)}.readArrayElem(tuple, 0)
              val value = ${jsonIo(c)}.readArrayElem(tuple, 1)
              $keyMeth(key) -> $valMeth(value)
            }.toMap
          """
        }
      }
      $parseMapMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to parse a collection, it will be something like
   * def parseCollection(array: J) = ???
   * parseCollection(reader.readObjectField(objNm, s"$fieldNm"))
   */
  def collectionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(itemTpe: c.universe.Type)(collType: c.universe.TypeName): c.universe.Tree = {
    import c.universe._
    val parseItemMeth = TermName(methdNameOfHandleItem(itemTpe.toString))
    val intsToItemsQuote =
      q"""
          (0 until arraySize).map {
            idx => $parseItemMeth(${jsonIo(c)}.readArrayElem(array, idx))
          }
      """

    // if it's not Seq but, say, List, append .toList to the quote
    val toCollQuote =
      if (collType == tpeClassNm(c)(typeOf[Seq[_]]))
        intsToItemsQuote
      else
        q"""${intsToItemsQuote}.${TermName("to" + collType)}"""

    val parseCollMethNm = TermName("parseCollection")
    q"""
      def $parseCollMethNm(array: J) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "array") {
          q"""
            ${quoteOfHandleItemDef(c)(itemTpe)(parseItemMeth)}
            val arraySize = ${jsonIo(c)}.readArrayLength(array)
            $toCollQuote
          """
        }
      }
      $parseCollMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to parse an option, it will be something like
   * def parseOption(json: J): Option[ITEM] = ???
   * parseOption(reader.readObjectField(objNm, s"$fieldNm"))
   */
  def optionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(itemTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val parseItemMeth = TermName(methdNameOfHandleItem(itemTpe.toString))
    val parseOptionMethdNm = TermName("parseOption")
    q"""
      def $parseOptionMethdNm(json: J): Option[$itemTpe] = {
        ${quoteOfHandleItemDef(c)(itemTpe)(parseItemMeth)}
        if (${jsonIo(c)}.isNull(json))
          None
        else
          Some($parseItemMeth(json))
      }
      $parseOptionMethdNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to parse an either, it will be something like
   * def parseEither(json: J): Either[LEFT, RIGHT] = ???
   * parseEither(reader.readObjectField(objNm, s"$fieldNm"))
   */
  def eitherQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val leftTpe = tpe.dealias.typeArgs.head
    val rightTpe = tpe.dealias.typeArgs.last
    val simpleLeftTpe = simpleTypeNm(leftTpe.toString)
    val simpleRightTpe = simpleTypeNm(rightTpe.toString)
    val leftMeth = TermName(methdNameOfHandleItem(leftTpe.toString))
    val rightMeth = TermName(methdNameOfHandleItem(rightTpe.toString))
    val parseEitherMethNm = TermName("parseEither")

    q"""
      def $parseEitherMethNm(json: J): Either[$leftTpe, $rightTpe] = {
        ${handleCaseClassDefQuote(c)(leftMeth)(leftTpe)("")}
        ${handleCaseClassDefQuote(c)(rightMeth)(rightTpe)("")}
        val value = ${jsonIo(c)}.readObjectField(json, "v")
        val providedTypeName = ${jsonIo(c)}.readString(${jsonIo(c)}.readObjectField(json, "t"))
        providedTypeName match {
          case $simpleLeftTpe => Left($leftMeth(value))
          case $simpleRightTpe => Right($rightMeth(value))
          case missed => throw new Error("Can't match: " + missed)
        }
      }
      $parseEitherMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to parse a numeric value, it will be something like
   * reader.readNumber(
   *   reader.readObjectField(objNm, s"$fieldNm")
   * )
   *
   * N.B. unlike stringQuote, it doesn't do null check because an expression of type Null is ineligible for implicit conversion for numeric value
   */
  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val quote = q"${jsonIo(c)}.readNumber(${fieldQuote(c)(objNm)(fieldNm)})"
    if (tpe == typeOf[Double])
      quote
    else
      q"${quote}.asInstanceOf[$tpe]"
  }

  /**
   * Quote to parse a string value, it will be something like, e.g.
   * val objNm_fieldNm = reader.readObjectField(objNm, s"$fieldNm")
   * if (reader.isNull(objNm_fieldNm)) {
   *   throw new IllegalArgumentException
   * } else {
   *  reader.readString(objNm_fieldNm)
   * }
   */
  def stringQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    val varBeChecked = TermName(concatVarNms(objNm.toString, fieldNm))
    q"""
      val $varBeChecked = ${fieldQuote(c)(objNm)(fieldNm)}
      ${
        quoteWithNullCheck(c)(varBeChecked) {
          q"${jsonIo(c)}.readString($varBeChecked)"
        }
      }
    """
  }

  /**
   * Quote to parse a boolean value, it will be something like, e.g.
   * val objNm_fieldNm = reader.readObjectField(objNm, s"$fieldNm")
   * reader.readBoolean(objNm_fieldNm)
   *
   * N.B. unlike stringQuote, it doesn't do null check because an expression of type Null is ineligible for implicit conversion for boolean
   */
  def booleanQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.readBoolean(${fieldQuote(c)(objNm)(fieldNm)})"
  }

  def quoteForNullCheck(c: Context)(varOfNullCheck: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.${TermName("isNull")}($varOfNullCheck)"
  }

  def quoteForNullVar(c: Context): c.universe.Tree = {
    import c.universe._
    q""" throw new IllegalArgumentException("The json data contains a null attribute which is not mapped to an Option[_] attribute") """
  }

  /**
   * Quote that reads the field on that object.
   * if the field name is "", objNm is the field, therefore return objNm
   * o.w. return reader.readObjectField(objNm, s"$fieldNm")
   */
  def fieldQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    if (fieldNm == "")
      q"$objNm"
    else
      q"${jsonIo(c)}.readObjectField($objNm, $fieldNm)"
  }

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree =
    recurQuote(c)(accessorTpe)(concatVarNms(objNm, fieldNm))(accessorField)(false)

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val assignedVar = concatVarNms(objNm, fieldNm)
    q"""
      val ${TermName(assignedVar)} = ${fieldQuote(c)(objNm)(fieldNm)}
      ${
        quoteWithNullCheck(c)(varOfNullCheck = assignedVar) {
          q"new $tpe(..$accessorQuotes)"
        }
      }
    """
  }

  /**
   * Quote of method definition that creates a "case object" of tpe, it will be something like
   */
  def handleCaseObjDefQuote(c: Context)(tpe: c.universe.Type)(methodNm: c.universe.TermName)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    this.synchronized {
      import c.universe._
      q"def $methodNm = new $tpe"
    }
  }

  /**
   * Quote of method definition that creates an case class object of type ct, it will be seomthing like
   * def $methodNm(item: J) = ???
   */
  def handleCaseClassDefQuote(c: Context)(method: c.universe.TermName)(ct: c.universe.Type)(fieldNm: String): c.universe.Tree =
    quoteOfHandleItemDef(c)(ct)(method)

  /**
   * Quote of call to method which is the method that parse the case class of a trait
   */
  def handleCaseClassCallQuote(c: Context)(handleCaseClassMeth: c.universe.TermName)(objNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""$handleCaseClassMeth(${jsonIo(c)}.readObjectField($objNm, "v"))"""
  }

  /**
   * Quote that maps a pattern to the corresponding handler, it will be someething like
   * s"$pattern" => handler
   */
  def patternToHandlerQuote(c: Context)(ct: c.universe.Type)(pattern: String)(handlerQuote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    cq"$pattern => $handlerQuote"
  }

  /**
   * Quote that defines the case pattern on a list of the sealed trait family, if all the members are case "objects", it will be
   * reader.readString(objNm) match {
   *   case "CaseObject1" => handle_CaseObject1
   *   case "CaseObject2" => handle_CaseObject2
   *   ...
   * }
   * o.w.
   * reader.readString(reader.readObjectField(objNm, "t")) match {
   *  case "CaseObject1" => handle_CaseObject1
   *  case "CaseClass2" => handle_CaseClass2(reader.readObjectField(objNm, "v"))
   * }
   */
  def ptnMatchQuoteForTraitFamily(c: Context)(onlyCaseObjects: Boolean)(patternToHandlerQuotes: Set[c.universe.Tree])(objNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    if (onlyCaseObjects)
      q"""
        ${jsonIo(c)}.readString($objNm) match {
          case ..$patternToHandlerQuotes
        }
      """
    else
      q"""
        ${jsonIo(c)}.readString(${jsonIo(c)}.readObjectField($objNm, "t")) match {
          case ..$patternToHandlerQuotes
        }
      """
  }

  /**
   * Quote to create a method definition of handle_TRAITTYPE_traitFamily and a call to it, it will be something like
   * def handle_TRAITTYPE_traitFamily(objNm: J) = ???
   * handle_TRAITTYPE_traitFamily(reader.readObjectField(objNm, s"$fieldNm"))
   */
  def traitFamilyMethDefAndCallQuote(c: Context)(traitTpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: String)(quote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    val handleTraitMethNm = TermName(methdNameOfHandleItem(traitTpe.toString + "_traitFamily"))
    q"""
      def $handleTraitMethNm($objNm: ${TypeName("J")}) = $quote
      $handleTraitMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"parseJsSerialised(${fieldQuote(c)(objNm)(fieldNm)}).asInstanceOf[$tpe]"
  }

  /**
   * Quote to parse an enum object, it should be something like, e.g.
   * val caseEnumName = {
   *   val objNm_fieldNm = reader.readObjectField(objNm, s"$fieldNm")
   *   if (reader.isNull(objNm_fieldNm)) {
   *     throw new IllegalArgumentException
   *   } else {
   *    reader.readString(objNm_fieldNm)
   *   }
   * }
   * au.com.fairfax.adonis.apws.types.CaseEnum.makeEnum(values.this.StoryStatus.Value, caseEnumName)
   */
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    val companion = {
      import c.universe._
      val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
      val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
      c.universe.treeBuild.mkAttributedRef(pre, tpe.typeSymbol.companionSymbol)
    }

    q"""
      val caseEnumName = ${stringQuote(c)(objNm)(fieldNm)}
      au.com.fairfax.adonis.apws.types.CaseEnum.makeEnum($companion, caseEnumName)
    """
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonParser[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result = jsonParserQuote(c)(tpe)("json")("args")
    println(
      s"""
         |parser
         |$result
       """.stripMargin)
    c.Expr[JsonParser[T]](result)
  }
  
  private def jsonParserQuote(c: Context)(tpeBeParsed: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      import au.com.fairfax.adonis.apws.macros.JsonParser
      import au.com.fairfax.adonis.apws.macros.JsonRegistry
          
      implicit object GenJsonParser extends JsonParser[${tpeBeParsed.dealias}] {
        override def parse[J](json: J)(implicit ${jsonIo(c)}: au.com.fairfax.adonis.apws.macros.JReader[J]) = {
          def parseJsSerialised(jsSerialised: J) = JsonRegistry.parse[J](jsSerialised)
          ${recurQuote(c)(tpeBeParsed.dealias)(objNm)(fieldNm)(true)}
        }
            
        override def buildChildParsers: String Map JsonParser[_] = {
          ${childParsersQuote(c)(tpeBeParsed.dealias)(objNm)(fieldNm)}
        }
      }
          
      GenJsonParser
    """
  }
}
