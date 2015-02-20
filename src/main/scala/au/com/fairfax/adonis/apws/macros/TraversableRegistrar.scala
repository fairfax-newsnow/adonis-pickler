package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object ParserRegistrarMacroTracker {
  var registeredTypes: Set[String] = Set()

  def add(t: String): Unit = {
    registeredTypes += t
//    println(s"$RegistrarMacroTracker.add(), registeredTypes = $registeredTypes")
  }

  def contains(t: String): Boolean = {
//    println(s"$RegistrarMacroTracker.contains($t), registeredTypes = $registeredTypes")
    registeredTypes contains t
  }
}

case class ParserRegistrarDoesNothing[T]() extends ParserTraversableRegistrar[T] {
  def traversableRegister: List[(String, JsonParser[_], JsonFormatter[_])] = Nil
}


trait ParserTraversableRegistrar[T] {
  def traversableRegister: List[(String, JsonParser[_], JsonFormatter[_])]
}

object ParserTraversableRegistrar {
  
  implicit def registerHelperMacros[T]: ParserTraversableRegistrar[T] = macro materialize[T]
  
  
  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[ParserTraversableRegistrar[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val tpeStr = toMapKey(tpe.toString)
    println(s"\n\n------------------------  starting ParserTraversableRegistrar: $tpe ----------------------------\n\n")

    val result =
      if (ParserRegistrarMacroTracker contains tpeStr) {
        alreadyRegisteredQuote(c)(tpe)
      }
      else {
        ParserRegistrarMacroTracker add tpeStr
        ytrq[T](c)(tpeStr)
      }

//    println(
//      s"""
//         |ParserTraversableRegistrar.materialize(), result =
//         |$result
//       """.stripMargin)

    println(s"\n\n------------------------  stopping ParserTraversableRegistrar: $tpe ---------------------------- t-length: ${result.toString.length}\n\n")
    c.Expr[ParserTraversableRegistrar[T]](result)
  }

  private def alreadyRegisteredQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"""
      au.com.fairfax.adonis.apws.macros.ParserRegistrarDoesNothing[$tpe]()
    """
  }

  private def ytrqDirect(c: Context)(tpe: c.Type)(keyBeAdded: String) = {
    import c.universe._

    val parser = ParserMaterializerImpl.materialiseDirect(c)(tpe)
    val formatter = FormatterMaterializerImpl.materialiseDirect(c)(tpe)
    
    q"($keyBeAdded, $parser, $formatter) :: ${traverseChildrenQuote(c)(tpe)}"
  }
  
  private def ytrq[T: c.universe.WeakTypeTag](c: Context)(keyBeAdded: String) = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val parser = ParserMaterializerImpl.materialize[T](c)
    val formatter = FormatterMaterializerImpl.materialize[T](c)

    q"""
      import au.com.fairfax.adonis.apws.macros.ParserTraversableRegistrar
      import au.com.fairfax.adonis.apws.macros.JsonRegistry
      import au.com.fairfax.adonis.apws.macros.JsonParser
      import au.com.fairfax.adonis.apws.macros.JsonFormatter
      
      implicit object GenTraversableRegistrar extends ParserTraversableRegistrar[${tpe}] {
        val traversableRegister: List[(String, JsonParser[_], JsonFormatter[_])] = {
          ($keyBeAdded, $parser, $formatter) :: ${traverseChildrenQuote(c)(tpe)}
        }
      }

      GenTraversableRegistrar
    """
    
  }

  private def traverseChildrenQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._

    lazy val accessors: List[MethodSymbol] = getAccessors(c)(tpe)
    
    tpe match {
        
      // a collection type
      case collectionTpe: Type if collTypes(c) contains tpeClassNm(c)(collectionTpe) =>
        val itemTpe = collectionTpe.typeArgs.head
        registerToRegistryQuote(c)(itemTpe)

      // a map type
      case mapTpe: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(mapTpe) =>
        val List(keyTpe, valTpe) = mapTpe.dealias.typeArgs
        q"""
          ${registerToRegistryQuote(c)(keyTpe)}
          ${registerToRegistryQuote(c)(valTpe)}
        """

      // an option type
      case optionTpe: Type if tpeClassNm(c)(typeOf[Option[_]]) == tpeClassNm(c)(optionTpe) =>
        val itemTpe = optionTpe.typeArgs.head
        registerToRegistryQuote(c)(itemTpe)

      // an either type
      case eitherTpe: Type if tpeClassNm(c)(typeOf[Either[_, _]]) == tpeClassNm(c)(eitherTpe) =>
        val leftTpe = eitherTpe.dealias.typeArgs.head
        val rightTpe = eitherTpe.dealias.typeArgs.last
        q"""
          ${registerToRegistryQuote(c)(leftTpe)} ::: ${registerToRegistryQuote(c)(rightTpe)}
        """

      // a sealed trait
      case traitTpe: Type if traitTpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        getSealedTraitChildren(c)(traitTpe).withFilter(!hasNoAccessor(c)(_)).map {
          registerToRegistryQuote(c)(_)
        }.foldLeft[Tree](q"Nil") {
          (z, accessorQuote) => q"$z ::: $accessorQuote"
        }

      // a structured type
      case _ if accessors.nonEmpty =>
        accessors.map {
          accessor =>
            val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
            registerToRegistryQuote(c)(accessorTpe)
        }.foldLeft[Tree](q"Nil") {
          (z, accessorQuote) => q"$z ::: $accessorQuote"
        }

      case _ =>
        q"Nil"
    }
  }

  private def registerToRegistryQuote(c: Context)(registerTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val tpeName = toMapKey(registerTpe.toString)
    if (!(ParserRegistrarMacroTracker contains tpeName)) {
      ParserRegistrarMacroTracker add tpeName
      ytrqDirect(c)(registerTpe)(tpeName)
    } else {
      q"Nil"
    }
  }
}







//object FormatterRegistrarMacroTracker {
//  var registeredTypes: Set[String] = Set()
//
//  def add(t: String): Unit = {
//    registeredTypes += t
//    //    println(s"$RegistrarMacroTracker.add(), registeredTypes = $registeredTypes")
//  }
//
//  def contains(t: String): Boolean = {
//    //    println(s"$RegistrarMacroTracker.contains($t), registeredTypes = $registeredTypes")
//    registeredTypes contains t
//  }
//}
//
//case class FormatterRegistrarDoesNothing[T]() extends FormatterTraversableRegistrar[T] {
//  def traversableRegister: Unit = {}
//}
//
//trait FormatterTraversableRegistrar[T] {
//  def traversableRegister: Unit
//}
//
//object FormatterTraversableRegistrar {
//
//  implicit def registerHelperMacros[T]: FormatterTraversableRegistrar[T] = macro materialize[T]
//
//
//  def materialiseDirect(c: Context)(tpe: c.Type): c.Expr[FormatterTraversableRegistrar[_]] = {
//    import c.universe._
//    val tpeStr = toMapKey(tpe.toString)
//
//    println(s"FormatterTraversableRegistrar.materialize(), tpe = $tpe, tpeStr = $tpeStr")
//
//    val result =
//      if (FormatterRegistrarMacroTracker contains tpeStr) {
//        alreadyRegisteredQuote(c)(tpe)
//      }
//      else {
//        FormatterRegistrarMacroTracker add tpeStr
//        ytrqDirect(c)(tpe)(tpeStr)
//      }
//
//    println(
//      s"""
//         |FormatterTraversableRegistrar.materialize(), result =
//         |$result
//       """.stripMargin)
//
//    c.Expr[FormatterTraversableRegistrar[_]](result)
//  }
//
//  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[FormatterTraversableRegistrar[T]] = {
//    import c.universe._
//    val tpe = weakTypeOf[T]
//    val tpeStr = toMapKey(tpe.toString)
//    println(s"\n\n------------------------  starting FormatterTraversableRegistrar: $tpe ----------------------------\n\n")
//    println(s"FormatterTraversableRegistrar.materialize(), tpe = $tpe, tpeStr = $tpeStr")
//
//    val result =
//      if (FormatterRegistrarMacroTracker contains tpeStr) {
//        alreadyRegisteredQuote(c)(tpe)
//      }
//      else {
//        FormatterRegistrarMacroTracker add tpeStr
//        ytrq[T](c)(tpeStr)
//      }
//
//    println(
//      s"""
//         |FormatterTraversableRegistrar.materialize(), result =
//         |$result
//       """.stripMargin)
//
//    println(s"\n\n------------------------  stopping FormatterTraversableRegistrar: $tpe ---------------------------- t-length: ${result.toString.length}\n\n")
//    c.Expr[FormatterTraversableRegistrar[T]](result)
//  }
//
//  private def alreadyRegisteredQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
//    import c.universe._
//    q"""
//      au.com.fairfax.adonis.apws.macros.FormatterRegistrarDoesNothing[$tpe]()
//    """
//  }
//
//  private def ytrqDirect(c: Context)(tpe: c.Type)(keyBeAdded: String) = {
//    import c.universe._
//
//    val formatter = FormatterMaterializerImpl.materialiseDirect(c)(tpe)
//
//    q"""
//      import au.com.fairfax.adonis.apws.macros.FormatterTraversableRegistrar
//      import au.com.fairfax.adonis.apws.macros.JsonRegistry
//
//      implicit object GenTraversableRegistrar extends FormatterTraversableRegistrar[${tpe}] {
//        val traversableRegister: Unit = {
//          JsonRegistry.addFormatter(($keyBeAdded, $formatter))
//          ${traverseChildrenQuote(c)(tpe)}
//        }
//      }
//
//      GenTraversableRegistrar
//    """
//
//  }
//
//  private def ytrq[T: c.universe.WeakTypeTag](c: Context)(keyBeAdded: String) = {
//    import c.universe._
//    val tpe = weakTypeOf[T]
//
//    val formatter = FormatterMaterializerImpl.materialize[T](c)
//
//    q"""
//      import au.com.fairfax.adonis.apws.macros.FormatterTraversableRegistrar
//      import au.com.fairfax.adonis.apws.macros.JsonRegistry
//
//      implicit object GenTraversableRegistrar extends FormatterTraversableRegistrar[${tpe}] {
//        val traversableRegister: Unit = {
//          JsonRegistry.addFormatter(($keyBeAdded, $formatter))
//          ${traverseChildrenQuote(c)(tpe)}
//        }
//      }
//
//      GenTraversableRegistrar
//    """
//
//  }
//
//  private def traverseChildrenQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
//    import c.universe._
//
//    lazy val accessors: List[MethodSymbol] = getAccessors(c)(tpe)
//
//    println("traversing: " + tpe.toString)
//
//    tpe match {
//
//      // a collection type
//      case collectionTpe: Type if collTypes(c) contains tpeClassNm(c)(collectionTpe) =>
//        val itemTpe = collectionTpe.typeArgs.head
//        registerToRegistryQuote(c)(itemTpe)
//
//      // a map type
//      case mapTpe: Type if tpeClassNm(c)(typeOf[Map[_, _]]) == tpeClassNm(c)(mapTpe) =>
//        val List(keyTpe, valTpe) = mapTpe.dealias.typeArgs
//        q"""
//          ${registerToRegistryQuote(c)(keyTpe)}
//          ${registerToRegistryQuote(c)(valTpe)}
//        """
//
//      // an option type
//      case optionTpe: Type if tpeClassNm(c)(typeOf[Option[_]]) == tpeClassNm(c)(optionTpe) =>
//        val itemTpe = optionTpe.typeArgs.head
//        registerToRegistryQuote(c)(itemTpe)
//
//      // an either type
//      case eitherTpe: Type if tpeClassNm(c)(typeOf[Either[_, _]]) == tpeClassNm(c)(eitherTpe) =>
//        val leftTpe = eitherTpe.dealias.typeArgs.head
//        println(s"leftTpe = $leftTpe")
//        val rightTpe = eitherTpe.dealias.typeArgs.last
//        println(s"rightTpe = $rightTpe")
//        q"""
//          ${registerToRegistryQuote(c)(leftTpe)}
//          ${registerToRegistryQuote(c)(rightTpe)}
//        """
//
//      // a sealed trait
//      case traitTpe: Type if traitTpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
//        val childrenQuotes = getSealedTraitChildren(c)(traitTpe).withFilter(!hasNoAccessor(c)(_)) map {
//          registerToRegistryQuote(c)(_)
//        }
//        q"..$childrenQuotes"
//
//      // a structured type
//      case _ if accessors.nonEmpty =>
//        val accessorQuotes = accessors map {
//          accessor =>
//            val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
//            registerToRegistryQuote(c)(accessorTpe)
//        }
//        q"..$accessorQuotes"
//
//      case _ => q"()"
//    }
//  }
//
//  private def registerToRegistryQuote(c: Context)(registerTpe: c.universe.Type): c.universe.Tree = {
//    import c.universe._
//    val tpeName = toMapKey(registerTpe.toString)
//    if (!(FormatterRegistrarMacroTracker contains registerTpe.toString)) {
//      println("registering: " + tpeName)
//      materialiseDirect(c)(registerTpe).tree
//    } else {
//      q"()"
//    }
//  }
//}
