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

  private def toJsonStringQuote(c: Context)(s: String): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.makeString($s)"
  }

  /**
   * Quote to parse collection, it will be something like
   * def formatMap(map: K Map V) = ???
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
   * Quote to parse collection, it will be something like
   * def formatCollection(objList: Seq[ITEM]) = ???
   * formatCollection(objNm)
   */
  def collectionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type)(collType: c.universe.TypeName): c.universe.Tree = {
    import c.universe._
    q"""
      def formatCollection(objList: $collType[$itemTpe]) = ${
        quoteWithNullCheck(c)(varOfNullCheck = "objList") {
          q"""
            val jsonList = objList.map {
              obj => JsonRegistry.internalFormat(obj, "", false, ${itemTpe.toString})
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
   * def formatOption(opt: Option[ITEM]) = ???
   * formatOption(objNm)
   */
  def optionQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(itemTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val caseQuotes = List(
      cq"""Some(v) => JsonRegistry.internalFormat(v, "", false, ${itemTpe.toString})""",
      cq"None => ${jsonIo(c)}.makeNull()")

    q"""
      def formatOption(opt: Option[$itemTpe]) = {
        opt match {
          case ..$caseQuotes
        }
      }
      
      formatOption(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote to format an either, it will be something like
   * def formatEither(either: Either[LEFT, RIGHT]) = ???
   * formatEither(objNm)
   */
  def eitherQuote(c: Context)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val leftTpe = tpe.dealias.typeArgs.head
    val rightTpe = tpe.dealias.typeArgs.last
    q"""
      def formatEither(either: Either[$leftTpe, $rightTpe]) = {
        either match {
           case Left(v) => JsonRegistry.internalFormat(v, "v", true, ${leftTpe.toString})
           case Right(v) => JsonRegistry.internalFormat(v, "v", true, ${rightTpe.toString})
        }
      }
      
      formatEither(${fieldQuote(c)(objNm)(fieldNm)})
    """
  }

  /**
   * Quote for formatting a numeric value, it will be something like, e.g.
   * builder.makeNumber(objNm)
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
   * Quote for formatting a string value, it will be something like, e.g.
   * if (objNm == null) {
   *   throw new IllegalArgumentException
   * } else {
   *  builder.makeString(objNm)
   * }
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
   * Quote for formatting a boolean value, it will be something like, e.g.
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

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorField: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"""
      val $accessorField = ${TermName(objNm)}.$accessorField
      ${accessorField.toString} -> JsonRegistry.internalFormat(${TermName(accessorField.toString)}, "", false, ${accessorTpe.toString})
    """
  }

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree = {
    import c.universe._
    val nonNullQuote = q"${jsonIo(c)}.makeObject(..$accessorQuotes)"
    q"${quoteWithNullCheck(c)(varOfNullCheck = objNm)(nonNullQuote)}"
  }

  /**
   * Quote of method definition that formats the "case object" of tpe
   */
  def handleCaseObjDefQuote(c: Context)(tpe: c.universe.Type)(methodNm: c.universe.TermName)(areSiblingCaseObjs: Boolean): c.universe.Tree = {
    import c.universe._
    val typeName = simpleTypeNm(tpe.toString)
    val methodImplQuote =
      if (areSiblingCaseObjs)
        toJsonStringQuote(c)(typeName)
      else
        q"""${jsonIo(c)}.makeObject("t" -> ${toJsonStringQuote(c)(typeName)}, "v" -> ${toJsonStringQuote(c)("")})"""

    q"def $methodNm = $methodImplQuote"
  }

  /**
   * Quote of method definition that creates an case class object of type ct, it will be seomthing like
   * def $methodNm(item: ITEMTYPE) = ???
   */
  def handleCaseClassDefQuote(c: Context)(method: c.universe.TermName)(ct: c.universe.Type)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    val objNm = TermName("obj")
    val methImplQuote = structuredTypeQuote(c)(ct)(objNm.toString)(fieldNm) {
      List(
        q""" "t" -> ${toJsonStringQuote(c)(simpleTypeNm(ct.toString))} """,
        q""" "v" -> JsonRegistry.internalFormat($objNm, ${fieldNm.toString}, false, ${ct.toString}) """)
    }
    
    q"def $method($objNm: $ct) = $methImplQuote"
  }

  private lazy val varBeMatched = "obj"

  /**
   * Quote of call to method which is the method that formats the case class of a trait
   */
  def handleCaseClassCallQuote(c: Context)(handleCaseClassMeth: c.universe.TermName)(objNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"$handleCaseClassMeth(${TermName(varBeMatched)})"
  }

  /**
   * Quote that maps a pattern to the corresponding handler, it will be something like
   * obj @ (_: Pattern) => handler
   */
  def patternToHandlerQuote(c: Context)(ct: c.universe.Type)(pattern: String)(handlerQuote: c.universe.Tree): c.universe.Tree = {
    import c.universe._
    cq"${TermName(varBeMatched)} : $ct => $handlerQuote"
  }

  /**
   * Quote that defines the case pattern on a list of the sealed trait family, it will be
   * objNm match {
   *   case (obj @ (_: CaseObject1)) => handle_CaseObject1
   *   case (obj @ (_: CaseClass2)) => handle_CaseClass(obj)
   *   ...
   * }
   */
  def ptnMatchQuoteForTraitFamily(c: Context)(onlyCaseObjects: Boolean)(patternToHandlerQuotes: Set[c.universe.Tree])(objNm: c.universe.TermName): c.universe.Tree = {
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

//  def jsSerialisableQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: c.universe.TermName): c.universe.Tree = {
//    import c.universe._
//    q"au.com.fairfax.adonis.apws.macros.JsonRegistry.internalFormat[J, $tpe](${fieldQuote(c)(objNm)(fieldNm)})"
//  }

  /**
   * Quote to format an enum object, it should be something like
   * builder.makeString(objNm.toString)
   */
  def enumObjQuote(c: Context)(tpe: c.universe.Type)(objNm: c.universe.TermName)(fieldNm: c.universe.TermName): c.universe.Tree = {
    import c.universe._
    q"${jsonIo(c)}.makeString($objNm.toString)"
  }

  def materialiseDirect(c: Context)(tpe: c.Type): c.Expr[JsonFormatter[_]] = {
    import c.universe._
    val result =
      q"""
      implicit object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.JsonFormatter[$tpe] {
        import au.com.fairfax.adonis.apws.macros.JsonRegistry

        override def format[J](obj: Any)(nameOfFormattedField: String)(topObj: Boolean)(implicit ${jsonIo(c)}: au.com.fairfax.adonis.apws.macros.JBuilder[J]) = {
          val typedObj = obj.asInstanceOf[$tpe]
          val formattedField = ${matchAndHandleObjTpeQuote(c)(tpe)("typedObj")("")}
          if (topObj)
            ${jsonIo(c)}.makeObject( "t" -> ${jsonIo(c)}.makeString(${tpe.toString}), nameOfFormattedField -> formattedField )
          else
            formattedField
        }
      }

      GenJsonFormatter
      """
    println(
      s"""
         |FormatterMaterializerImpl.materialize()
         |$result
       """.stripMargin)
    println(s"\n\n------------------------  stopping materializeDirect Formatter: $tpe ---------------------------- t-length: ${result.toString.length}\n\n")
    c.Expr[JsonFormatter[_]](result)
  }

  def materialize[T: c.WeakTypeTag](c: Context): c.Expr[JsonFormatter[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
      q"""
      implicit object GenJsonFormatter extends au.com.fairfax.adonis.apws.macros.JsonFormatter[$tpe] {
        import au.com.fairfax.adonis.apws.macros.JsonRegistry
        
        override def format[J](obj: Any)(nameOfFormattedField: String)(topObj: Boolean)(implicit ${jsonIo(c)}: au.com.fairfax.adonis.apws.macros.JBuilder[J]) = {
          val typedObj = obj.asInstanceOf[$tpe]
          val formattedField = ${matchAndHandleObjTpeQuote(c)(tpe)("typedObj")("")}
          if (topObj)
            ${jsonIo(c)}.makeObject( "t" -> ${jsonIo(c)}.makeString(${tpe.toString}), nameOfFormattedField -> formattedField )
          else
            formattedField
        }
      }

      GenJsonFormatter
      """
    println(
      s"""
         |FormatterMaterializerImpl.materialize()
         |$result
       """.stripMargin)
    println(s"\n\n------------------------  stopping materialize Formatter: $tpe ---------------------------- t-length: ${result.toString.length}\n\n")
    c.Expr[JsonFormatter[T]](result)
  }
}
