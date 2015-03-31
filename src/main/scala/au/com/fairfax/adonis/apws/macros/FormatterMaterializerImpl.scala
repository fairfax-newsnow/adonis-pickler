package au.com.fairfax.adonis.apws.macros

import scala.reflect.macros.blackbox.Context
import au.com.fairfax.adonis.utils.simpleTypeNm
import Materializer._

/**
 * e.g. case class IntWrapper(i: Int), this is a structure class having an integer field, therefore the code will be generated as  
 * calls recurQuote, the type is matched to a structured type having only 1 accessorField.  The generated code will be:
 *
 * implicit object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.JsonFormatter[IntWrapper] {
 *   override def format[J](obj: Any)(implicit builder: au.com.fairfax.adonis.apws.macros.JBuilder[J]) = {
 *     val typedObj = obj.asInstanceOf[IntWrapper];
 *     builder.makeObject(
 *       "t" -> builder.makeString("IntWrapper"),
 *       "args" ->
 *       (
 *         if (typedObj.$eq$eq(null)) // from structuredTypeQuote()
 *           throw new IllegalArgumentException("The data object has a null attribute") // from structuredTypeQuote()
 *         else // from structuredTypeQuote()
 *           builder.makeObject({ // from structuredTypeQuote()
 *             val i = typedObj.i;  // from eachAccessorQuote()
 *             "i" -> builder.makeNumber(i.asInstanceOf[Double])  // from eachAccessorQuote() and then numericValQuote()
 *           })
 *       )
 *     )
 *   }
 * };
 * GenJsonFormatter
 */
object FormatterMaterializerImpl extends Materializer[JsonFormatter] {
  def jsonIo(c: Context): c.universe.TermName = c.universe.TermName("builder")

  /**
   * Quote to format a map, it will be something like
   * 
   * def formatMap(map: K Map V) = 
   *   if (map == null)
   *     throw new IllegalArgumentException("The data object has a null attribute")
   *   else {
   *     val elems = 
   *       map.map { t =>
   *         val (k, v) = t
   *         builder.makeArray(
   *           JsonRegistry.internalFormat(k, "", false, k's Type), JsonRegistry.internalFormat(v, "", false, v's Type)          
   *         )         
   *       }.toList         
   *     builder.makeArray(elems: _*)    
   *   }   
   * formatMap(objNm)
   */
  def mapQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(mapTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val List(keyTpe, valTpe) = mapTpe.dealias.typeArgs

    q"""
      def formatMap(map: $keyTpe Map $valTpe) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "map") {
          q"""
              val elems =
                map.map { t =>
                  val (k, v) = t
                  ${jsonIo(c)}.makeArray(
                    JsonRegistry.internalFormat(k, "", false, ${keyTpe.toString}), JsonRegistry.internalFormat(v, "", false, ${valTpe.toString})
                  )
                }.toList
              ${jsonIo(c)}.makeArray(elems: _*)
          """
        }
      }
      formatMap(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to format a collection, it will be something like
   *  
   * def formatCollection(itemList: Seq[ITEM]) <- not necessarily Seq but any collection type 
   *   if (itemList == null)
   *     throw new IllegalArgumentException("The data object has a null attribute")
   *   else {
   *     val jsonList = itemList.map (
   *       item => JsonRegistry.internalFormat(item, "", false, item's type)       
   *     )     
   *     builder.makeArray(jsonList: _*)     
   *   }   
   * formatCollection(objNm)
   */
  def collectionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type)(collType: c.universe.TypeName): c.universe.Tree = {
    import c.universe._
    q"""
      def formatCollection(itemList: $collType[$itemTpe]) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "itemList") {
          q"""
            val jsonList = itemList.map {
              item => JsonRegistry.internalFormat(item, "", false, ${itemTpe.toString})
            }
            ${jsonIo(c)}.makeArray(jsonList: _*)
          """
        }
      }
      formatCollection(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to format an option, it will be something like
   * def formatOption(opt: Option[ITEM]) = 
   *   opt match {
   *     case Some(v) => JsonRegistry.internalFormat(v, "", false, item's type)
   *     case None => builder.makeNull()     
   *   }   
   * formatOption(objNm)
   */
  def optionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val caseQuotes = List(
      cq"""Some(v) => JsonRegistry.internalFormat(v, "", false, ${itemTpe.toString})""",
      cq"None => ${jsonIo(c)}.makeNull()")

    q"""
      def formatOption(opt: Option[$itemTpe]) = 
        opt match {
          case ..$caseQuotes
        }
      
      formatOption(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to format an either, it will be something like
   * def formatEither(either: Either[LEFT, RIGHT]) = 
   *   either match {
   *     case Left(v) => JsonRegistry.internalFormat(v, "v", true, left item's type)
   *     case Right(v) => JsonRegistry.internalFormat(v, "v", true, right item's type)     
   *   }   
   * formatEither(objNm)
   */
  def eitherQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val leftTpe = tpe.dealias.typeArgs.head
    val rightTpe = tpe.dealias.typeArgs.last
    q"""
      def formatEither(either: Either[$leftTpe, $rightTpe]) =
        either match {
           case Left(v) => JsonRegistry.internalFormat(v, "v", true, ${leftTpe.toString})
           case Right(v) => JsonRegistry.internalFormat(v, "v", true, ${rightTpe.toString})
        }
      
      formatEither(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote for formatting a numeric value, it will be something like, 
   * builder.makeNumber(objNm) or builder.makeNumber(objNm.asInstanceOf[Double]) depends on whether the field is double
   *
   * N.B. unlike stringQuote, it doesn't do null check because an expression of type Null is ineligible for implicit conversion for numeric value
   */
  def numericValQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    val numQuote =
      if (tpe == typeOf[Double]) q"$objNm"
      else q"$objNm.asInstanceOf[Double]"
    q"${jsonIo(c)}.makeNumber($numQuote)"
  }

  /**
   * Quote for formatting a string value, it will be something like
   * if (objNm == null)
   *   throw new IllegalArgumentException
   * else
   *  builder.makeString(objNm)
   */
  def stringQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      ${
        quoteWithNullCheck(c)(objNm) {
          q"${jsonIo(c)}.makeString($objNm)"
        }
      }
    """
  }

  /**
   * Quote for formatting a boolean value, it will be something like
   * builder.makeBoolean(objNm)
   *
   * N.B. unlike stringQuote, it doesn't do null check because an expression of type Null is ineligible for implicit conversion for boolean
   */
  def booleanQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.makeBoolean(${fieldQuote(c)(objNm)(fieldNm)})"
  }

  def quoteForNullCheck(c: Context)(varOfNullCheck: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"$varOfNullCheck == null"
  }

  def quoteForNullVar(c: Context): c.universe.Tree = {
    import c.universe._
    q""" throw new IllegalArgumentException("The data object has a null attribute") """
  }

  /**
   * Quote that format the field on that object.
   * return objNm
   */
  def fieldQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"$objNm"
  }

  /**
   * Quote that formats the field of a case class object, it will be something like
   * val caseField = obj.caseField
   * "caseField" -> JsonRegistry.internalFormat(caseField, "", false, caseField's type)
   */
  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorField: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      val $accessorField = ${TermName(objNm)}.$accessorField
      ${accessorField.toString} -> JsonRegistry.internalFormat($accessorField, "", false, ${accessorTpe.toString})
    """
  }

  /**
   * Quote that formats a case class object, it will be something like
   * if (obj == null)
   *   throw new IllegalArgumentException("The data object has a null attribute")
   * else
   *   builder.makeObject(caseField1Quote, caseField2Quote, ...)
   */
  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    quoteWithNullCheck(c)(varOfNullCheck = objNm) {
      q"${jsonIo(c)}.makeObject(..$accessorQuotes)"
    }
  }

  def handleEmptyCaseClassAndCallQuote(c: Context)(tpe: c.universe.Type)(tpeInJson: String): (c.universe.Tree, c.universe.Tree) =
    handleCaseObjectAndCallQuote(c)(tpe)(tpeInJson)

  def handleCaseObjectAndCallQuote(c: Context)(tpe: c.universe.Type)(tpeInJson: String): (c.universe.Tree, c.universe.Tree) = {
    import c.universe._
    val method: TermName = methdNameOfHandleItem(tpeInJson)
    val methodImplQuote =
        q"""${jsonIo(c)}.makeObject("t" -> ${jsonIo(c)}.makeString($tpeInJson), "v" -> ${jsonIo(c)}.makeString(""))"""
    
    val handleCaseObjMethQuote = q"def $method = $methodImplQuote"
    val patternToCallCaseObj = cq"_: $tpe => $method"
    (handleCaseObjMethQuote, patternToCallCaseObj)
  }
  
  def handleCaseClassAndCallQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): (c.universe.Tree, c.universe.Tree) = {
    import c.universe._
    val objNm: TermName = "obj"
    val tpeString = tpe.toString
    val method: TermName = methdNameOfHandleItem(tpeString)
    val methImplQuote = structuredTypeQuote(c)(tpe)(objNm.toString)(fieldNm) {
      List(
        q""" "t" -> ${jsonIo(c)}.makeString(${simpleTypeNm(tpeString)}) """,
        q""" "v" -> JsonRegistry.internalFormat($objNm, ${fieldNm.toString}, false, ${tpeString}) """)
    }

    val handleCaseClassMethQuote = q"def $method($objNm: $tpe) = $methImplQuote"
    val patternToCallCaseClass = cq"$objNm : $tpe => $method($objNm)"
    (handleCaseClassMethQuote, patternToCallCaseClass)
  }

  /**
   * Quote that defines the case pattern on a list of the sealed trait family, it will be
   * objNm match {
   *   case (obj @ (_: CaseObject1)) => handle_CaseObject1
   *   case (obj @ (_: CaseClass2)) => handle_CaseClass(obj)
   *   ...
   * }
   */
  def ptnMatchQuoteForTraitFamily(c: Context)(patternToHandlerQuotes: Set[c.universe.Tree])(objNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      $objNm match {
        case ..$patternToHandlerQuotes
      }
    """
  }

  /**
   * Quote to create a method definition of handle_TRAITTYPE_traitFamily and a call to it, it will be something like
   * def handle_TRAITTYPE_traitFamily(objNm: TRAITTYPE) = ???
   * handle_TRAITTYPE_traitFamily(objNm)
   */
  def traitFamilyMethDefAndCallQuote(c: Context)(traitTpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(quote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    val handleTraitMethNm = TermName(methdNameOfHandleItem(traitTpe.toString + "_traitFamily"))
    q"""
      def $handleTraitMethNm($objNm: $traitTpe) = $quote
      $handleTraitMethNm(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to format an enum object, it should be something like
   * builder.makeString(objNm.toString)
   */
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      val objStr = $objNm.toString
      ${jsonIo(c)}.makeString {
        if (objStr endsWith ${"$"})
          objStr.substring(0, objStr.length - 1)
        else 
          objStr
      }
    """
  }

  def caseObjQuote(c: Context)(caseObjTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"""${jsonIo(c)}.makeString("")"""
  }

  def emptyCaseClassQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree =
    caseObjQuote(c)(tpe)

  def formatterQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
      q"""
      implicit object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.JsonFormatter[$tpe] {
        import au.com.fairfax.adonis.apws.macros.JsonRegistry

        override def format[J](obj: Any)(nameOfFormattedField: String)(includeTpeInJson: Boolean)(implicit ${jsonIo(c)}: au.com.fairfax.adonis.apws.macros.JBuilder[J]) = {
          val typedObj = obj.asInstanceOf[$tpe]
          val formattedField = ${matchObjTpeQuote(c)(tpe)("typedObj")("")}
          if (includeTpeInJson)
            ${jsonIo(c)}.makeObject( "t" -> ${jsonIo(c)}.makeString(${tpe.toString}), nameOfFormattedField -> formattedField )
          else
            formattedField
        }
      }

      GenJsonFormatter
      """
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatter[T]] = {
    import c.universe._
    c.Expr[JsonFormatter[T]](formatterQuote(c)(weakTypeOf[T]))
  }
}
