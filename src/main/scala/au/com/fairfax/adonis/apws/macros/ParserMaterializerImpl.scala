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
  def mapQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(mapTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val List(keyTpe, valTpe) = mapTpe.dealias.typeArgs
    q"""
      def parseMap(map: J) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "map") {
          q"""
            val mapSize = ${jsonIo(c)}.readArrayLength(map)
            (0 until mapSize).toList.map { idx =>
              val tuple = ${jsonIo(c)}.readArrayElem(map, idx)
              val key = ${jsonIo(c)}.readArrayElem(tuple, 0)
              val value = ${jsonIo(c)}.readArrayElem(tuple, 1)
              JsonRegistry.parse(key, "", Some(${keyTpe.toString})).asInstanceOf[$keyTpe] -> JsonRegistry.parse(value, "", Some(${valTpe.toString})).asInstanceOf[$valTpe]
            }.toMap
          """
        }
      }
      parseMap(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to parse a collection, it will be something like
   * def parseCollection(array: J) = ???
   * parseCollection(reader.readObjectField(objNm, s"$fieldNm"))
   */
  def collectionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type)(collType: c.universe.TypeName): c.universe.Tree = {
    import c.universe._

    val intsToItemsQuote =
      q"""
          (0 until arraySize).map {
            idx => 
              val jsonItem = ${jsonIo(c)}.readArrayElem(array, idx)
              JsonRegistry.parse(jsonItem, "", Some(${itemTpe.toString})).asInstanceOf[$itemTpe]
          }
      """
    // if it's not Seq but, say, List, append .toList to the quote
    val toCollQuote =
      if (collType == tpeClassNm(c)(typeOf[Seq[_]]))
        intsToItemsQuote
      else
        q"""${intsToItemsQuote}.${TermName("to" + collType)}"""

    q"""
      def parseCollection(array: J) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "array") {
          q"""
            val arraySize = ${jsonIo(c)}.readArrayLength(array)
            $toCollQuote
          """
        }
      }
      
      parseCollection(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to parse an option, because this is an option field, 
   * it implies this might be a new field whose data not in the old json data yet, 
   * therefore it needs to check if its json field is undefined, 
   * if so, it will be set to None, 
   * o.w. it will proceed to parsing the data,
   * it will be something like
   *  
   * def parseOption(json: J): Option[ITEM] = {
   *  def handleItem(item: J) = {
   *    ... recursive parsing on item
   *  }  
   *       
   *  if (reader.isNull(json))
   *    None
   *  else
   *    Some(handleItem(json))
   * } // parseOption()
   * 
   * val obj_field = reader.readObjectField(obj, s"$field")
   * if (reader.isUndefined(obj_field)
   *  None
   * else  
   *  parseOption(obj_field)
   */
  def optionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val optField = TermName(concatVarNms(objNm.toString, fieldNm.toString))
    q"""
      def parseOption(json: J): Option[$itemTpe] = {
        if (${jsonIo(c)}.isNull(json))
          None
        else
          Some( JsonRegistry.parse(json, "", Some(${itemTpe.toString})).asInstanceOf[$itemTpe] )
      }

      val $optField = ${fieldQuote(c)(objNm)(fieldNm)}
      if (${jsonIo(c)}.isUndefined($optField))
        None
      else
        parseOption($optField)
    """
  }

  /**
   * Quote to parse an either, it will be something like
   * def parseEither(json: J): Either[LEFT, RIGHT] = ???
   * parseEither(reader.readObjectField(objNm, s"$fieldNm"))
   */
  def eitherQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val leftTpe = tpe.dealias.typeArgs.head
    val rightTpe = tpe.dealias.typeArgs.last
    val simpleLeftTpe = simpleTypeNm(leftTpe.toString)
    val simpleRightTpe = simpleTypeNm(rightTpe.toString)

    q"""
      def parseEither(json: J): Either[$leftTpe, $rightTpe] = {
        val value = ${jsonIo(c)}.readObjectField(json, "v")
        val providedTypeName = ${jsonIo(c)}.readString(${jsonIo(c)}.readObjectField(json, "t"))
        providedTypeName match {
          case $simpleLeftTpe => Left(JsonRegistry.parse(value, "", Some(${leftTpe.toString})).asInstanceOf[$leftTpe])
          case $simpleRightTpe => Right(JsonRegistry.parse(value, "", Some(${rightTpe.toString})).asInstanceOf[$rightTpe])
          case missed => throw new Error("Can't match: " + missed)
        }
      }
      
      parseEither(${fieldQuote(c)(objNm)(fieldNm)})
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
  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
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
  def stringQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    val assignedVar = TermName(concatVarNms(objNm.toString, fieldNm.toString))
    q"""
      val $assignedVar = ${fieldQuote(c)(objNm)(fieldNm)}
      ${
        quoteWithNullCheck(c)(assignedVar) {
          q"${jsonIo(c)}.readString($assignedVar)"
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
  def booleanQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
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
  def fieldQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      if ($fieldNm == "")
        $objNm
      else
        ${jsonIo(c)}.readObjectField($objNm, $fieldNm)
    """
  }

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorField: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      JsonRegistry.parse(
        ${TermName(concatVarNms(objNm, fieldNm.toString))}, ${accessorField.toString}, Some(${accessorTpe.toString})
      ).asInstanceOf[$accessorTpe]
    """
  }

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val assignedVar = concatVarNms(objNm, fieldNm.toString)
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
  def handleCaseClassDefQuote(c: Context)(method: c.universe.TermName)(ct: c.universe.Type)(fieldNm: c.universe.TermName): c.universe.Tree =
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
  def traitFamilyMethDefAndCallQuote(c: Context)(traitTpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(quote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    val handleTraitMethNm = TermName(methdNameOfHandleItem(traitTpe.toString + "_traitFamily"))
    q"""
      def $handleTraitMethNm($objNm: ${TypeName("J")}) = $quote
      $handleTraitMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName): c.universe.Tree = {
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
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
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
    val result =
      q"""
        implicit object GenJsonParser extends au.com.fairfax.adonis.apws.macros.JsonParser[${tpe.dealias}] {
          import au.com.fairfax.adonis.apws.macros.JsonRegistry
          import au.com.fairfax.adonis.apws.macros.JReader
          
          override def parse[J](json: J)(nameOfParsedField: String)(implicit ${jsonIo(c)}: JReader[J]) = {
            def parseJsSerialised(jsSerialised: J) = JsonRegistry.parse[J](jsSerialised)
            ${recurQuote(c)(tpe.dealias)("json")(TermName("nameOfParsedField"))(true)}
          }
        }
  
        GenJsonParser
      """
    println(
      s"""
         |ParserMaterializerImpl.materialize()
         |$result
       """.stripMargin)
    c.Expr[JsonParser[T]](result)
  }
}
