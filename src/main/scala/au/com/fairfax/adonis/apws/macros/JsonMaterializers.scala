package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import au.com.fairfax.adonis.utils._
import au.com.fairfax.adonis.apws.macros.json._
import scala.language.higherKinds

object Materializer {
  def itemMethNm(inStr: String): String =
    List("handle", inStr).flatMap(_ split "\\[").flatMap(_ split ",").flatMap(_ split "\\]").map(removePkgName).mkString("_")
}

trait Materializer[FP[_] <: FormatterParser[_]] {

  import Materializer._

  val jsonIO: String

  def tpeClassNm(c: Context): c.universe.Type => String = _.typeSymbol.asClass.name.toString

  def collTypes(c: Context) = {
    import c.universe._
    List(typeOf[List[_]], typeOf[Vector[_]], typeOf[Seq[_]]) map tpeClassNm(c)
  }

  def deliasTpeName[T: c.universe.TypeTag](c: Context): String =
    c.universe.typeOf[T].dealias.toString

  def numDealisTpeNms(c: Context) =
    List(deliasTpeName[Double](c), deliasTpeName[Float](c), deliasTpeName[Short](c), deliasTpeName[Int](c), deliasTpeName[Long](c))

  def itemQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree

  def objectQuote(c: Context)(tpe: c.universe.Type)(methodNm: String): c.universe.Tree = {
    import c.universe._
    q"""
      def ${TermName(methodNm)} =
        new $tpe
    """
  }

  def mapQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(methodNm: String): c.universe.Tree

  def mapTemplateQuote(c: Context)(keyTpe: c.universe.Type)(valTpe: c.universe.Type)(quoteFunc: (String, String, List[c.universe.Tree]) => c.universe.Tree) = {
    import c.universe._
    val List(keyMeth, valMeth) = List(keyTpe, valTpe) map (t => itemMethNm(t.toString))
    val itemQuotes = itemQuote(c)(keyTpe)(keyMeth) :: {
      if (keyTpe != valTpe) List(itemQuote(c)(valTpe)(valMeth))
      else Nil
    }
    quoteFunc(keyMeth, valMeth, itemQuotes)
  }

  def collectionQuote(c: Context)(tpe: c.universe.Type)(collType: String)(methodNm: String): c.universe.Tree

  def doubleValQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree

  def fieldQuote(c: Context)(objNm: String)(fieldNm: String): c.universe.Tree

  def ioActionString: String

  def eachAccessorQuote(c: Context)(accessorTpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorField: String): c.universe.Tree

  def structuredTypeQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String)(accessorQuotes: List[c.universe.Tree]): c.universe.Tree

  def getAccessors(c: Context)(tpe: c.universe.Type): List[c.universe.MethodSymbol] = {
    import c.universe._
    tpe.decls.collect {
      case acc: MethodSymbol if tpe.typeSymbol.asClass.isCaseClass && acc.isCaseAccessor || !tpe.typeSymbol.asClass.isCaseClass && acc.isParamAccessor => acc
    }.toList
  }

  def hasNoAccessor(c: Context)(tpe: c.universe.Type): Boolean =
    getAccessors(c)(tpe).isEmpty

  def sealedTraitQuote(c: Context)(tpe: c.universe.Type)(objNm: String): (Set[c.universe.Tree], c.universe.Tree) = {
    import c.universe._

    val childTypeSyms = tpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sealedDescendants.filterNot {
      des => des.isSealed || tpeClassNm(c)(tpe) == tpeClassNm(c)(des.asInstanceOf[Symbol].asType.toType)
    }.map(_.asInstanceOf[Symbol].asType)

    println(s"sealedTraitQuote, childTypeSyms.size = ${childTypeSyms.size}")

    val itemQuotes = childTypeSyms.map {
      cts =>
        val classNm = cts.asClass.name.toString
        if (hasNoAccessor(c)(cts.toType)) objectQuote(c)(cts.toType)(itemMethNm(classNm))
        else itemQuote(c)(cts.toType)(itemMethNm(classNm))
    }

    val caseQuotes = childTypeSyms.map {
      cts =>
        val pattern = removePkgName(cts.asClass.name.toString)
        val patternHandler =
          if (hasNoAccessor(c)(cts.toType))
            q"${TermName(itemMethNm(pattern))}"
          else
            q"""
              ${TermName(itemMethNm(pattern))}(${TermName(jsonIO)}.readObjectField(${TermName(objNm)}, "v"))
            """
        cq"""$pattern => $patternHandler"""
    }

    val matchQuote =
      if (childTypeSyms forall (cts => hasNoAccessor(c)(cts.toType)))
        q"""
          ${TermName(jsonIO)}.readString(${TermName(objNm)}) match {
            case ..$caseQuotes
          }
        """
      else
        q"""
          ${TermName(jsonIO)}.readString(${TermName(jsonIO)}.readObjectField(${TermName(objNm)}, "t")) match {
            case ..$caseQuotes
          }
        """

    (itemQuotes, matchQuote)
  }

  def recurQuote(c: Context)(tpe: c.universe.Type)(objNm: String)(fieldNm: String): c.universe.Tree = {
    import c.universe._

    println(s"recurQuote, tpe = $tpe, tpe.typeSymbol = ${tpe.typeSymbol}, tpe.companion = ${tpe.companion}")

    //    println(s"recurQuote, tpe = $tpe, tpe.dealias = ${tpe.dealias}, accessors = $accessors")
    tpe match {
      // a numeric type
      case t: Type if numDealisTpeNms(c) contains t.dealias.toString =>
        doubleValQuote(c)(t)(objNm)(fieldNm)

      // boolean or string type
      case t: Type if List(deliasTpeName[Boolean](c), deliasTpeName[String](c)) contains t.dealias.toString =>
        q"${TermName(jsonIO)}.${TermName(ioActionString + t.dealias.toString)}(${fieldQuote(c)(objNm)(fieldNm)})"

      // a collection type
      case t: Type if collTypes(c) contains tpeClassNm(c)(t) =>
        val handleCollection = "handleCollection"
        q"""
            ${collectionQuote(c)(t.typeArgs.head)(tpeClassNm(c)(t))(handleCollection)}
            ${TermName(handleCollection)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a map type
      case t: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(t) =>
        val handleMap = "handleMap"
        val List(key, value) = t.typeArgs
        q"""
            ${mapQuote(c)(key)(value)(handleMap)}
            ${TermName(handleMap)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a sealed trait
      case t: Type if t.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        val (itemQuotes, matchQuote) = sealedTraitQuote(c)(t)("item")
        val handleSealedTrait = itemMethNm(t.toString + "_family")
        q"""
          def ${TermName(handleSealedTrait)}(item: ${TypeName("J")}) = {
            ..$itemQuotes
            $matchQuote
          }
          ${TermName(handleSealedTrait)}(${fieldQuote(c)(objNm)(fieldNm)})
        """

      // a structured type
      case _ =>
        val accessors = getAccessors(c)(tpe)
        accessors match {
          case x :: _ =>
            val accessorQuotes = accessors map {
              accessor =>
                val accessorField = accessor.name.toString
                val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
                eachAccessorQuote(c)(accessorTpe)(objNm)(fieldNm)(accessorField)
            }
            structuredTypeQuote(c)(tpe)(objNm)(fieldNm)(accessorQuotes)
          case _ => q"new $tpe"
        }
    }
  }

  def materializeTemplate[T: c.WeakTypeTag](c: Context)(quoteFunc: c.universe.Type => c.universe.Tree): c.Expr[FP[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val result = quoteFunc(tpe)
    println(result)
    c.Expr[FP[T]](result)
  }
}

trait ParserMaterializer {
  implicit def jsonParserMacro[T]: JsonParser[T] = macro ParserMaterializerImpl.materialize[T]
}

trait FormatterMaterializer {
  implicit def jsonFormatterMacro[T]: JsonFormatter[T] = macro FormatterMaterializerImpl.materialize[T]
}