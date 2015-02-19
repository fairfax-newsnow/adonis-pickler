package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object RegistrarMacroTracker {
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

trait TraversableRegistrar[T] {
  def traversableRegister: Unit
}

import au.com.fairfax.adonis.apws.macros.TraversableRegistrar

case class RegistrarDoesNothing[T]() extends TraversableRegistrar[T] {
  def traversableRegister: Unit = {}
}


object TraversableRegistrar {
  
  implicit def registerHelperMacros[T]: TraversableRegistrar[T] = macro materialize[T]
  
  
  def materialiseDirect(c: Context)(tpe: c.Type): c.Expr[TraversableRegistrar[_]] = {
    import c.universe._
    val tpeStr = toMapKey(tpe.toString)

    println(s"TraversableRegistrar.materialize(), tpe = $tpe, tpeStr = $tpeStr")

    val result =
      if (RegistrarMacroTracker contains tpeStr) {
        alreadyRegisteredQuote(c)(tpe)
      }
      else {
        RegistrarMacroTracker add tpeStr
        ytrqDirect(c)(tpe)(tpeStr)
      }

    println(
      s"""
         |TraversableRegistrar.materialize(), result =
         |$result
       """.stripMargin)

    c.Expr[TraversableRegistrar[_]](result)
  }

  def materialize[T: c.universe.WeakTypeTag](c: Context): c.Expr[TraversableRegistrar[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val tpeStr = toMapKey(tpe.toString)
    println(s"\n\n------------------------  starting transversal: $tpe ----------------------------\n\n")
    println(s"TraversableRegistrar.materialize(), tpe = $tpe, tpeStr = $tpeStr")

    val result =
      if (RegistrarMacroTracker contains tpeStr) {
        alreadyRegisteredQuote(c)(tpe)
      }
      else {
        RegistrarMacroTracker add tpeStr
        ytrq[T](c)(tpeStr)
      }

    println(
      s"""
         |TraversableRegistrar.materialize(), result =
         |$result
       """.stripMargin)

    println(s"\n\n------------------------  stopping transversal: $tpe ---------------------------- t-length: ${result.toString.length}\n\n")
    c.Expr[TraversableRegistrar[T]](result)
  }

  private def alreadyRegisteredQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    q"""
      au.com.fairfax.adonis.apws.macros.RegistrarDoesNothing[$tpe]()
    """
  }

  private def ytrqDirect(c: Context)(tpe: c.Type)(keyBeAdded: String) = {
    import c.universe._

    val parser = ParserMaterializerImpl.materialiseDirect(c)(tpe)
    val formatter = FormatterMaterializerImpl.materialiseDirect(c)(tpe)


    q"""
      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar
      import au.com.fairfax.adonis.apws.macros.JsonRegistry

      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe}] {
        val traversableRegister: Unit = {
          $parser
          ${traverseChildrenQuote(c)(tpe)}
        }
      }

      GenTraversableRegistrar
    """

  }
  
  private def ytrq[T: c.universe.WeakTypeTag](c: Context)(keyBeAdded: String) = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val parser = ParserMaterializerImpl.materialize[T](c)
    val formatter = FormatterMaterializerImpl.materialize[T](c)
    
    
    q"""
      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar
      import au.com.fairfax.adonis.apws.macros.JsonRegistry
      
      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe}] {
        val traversableRegister: Unit = {
          JsonRegistry.add(($keyBeAdded, $parser))(($keyBeAdded, $formatter))
          ${traverseChildrenQuote(c)(tpe)}
        }
      }

      GenTraversableRegistrar
    """
    
  }

//  private def yetToRegisterQuote(c: Context)(tpe: c.universe.Type)(keyBeAdded: String): c.universe.Tree = {
//    import c.universe._
//
//
//    q"""
//      import au.com.fairfax.adonis.apws.macros.TraversableRegistrar
//      import au.com.fairfax.adonis.apws.macros.JsonParser
//      import au.com.fairfax.adonis.apws.macros.JsonFormatter
//      import au.com.fairfax.adonis.apws.macros.JsonRegistry
//
//      implicit object GenTraversableRegistrar extends TraversableRegistrar[${tpe}] {
//        def traversableRegister: Unit = registerParserFormatter
//
//
//        private def registerParserFormatter(implicit parser: JsonParser[${tpe}], formatter: JsonFormatter[${tpe}]) {
//          JsonRegistry.add(($keyBeAdded, parser))(($keyBeAdded, formatter))
//          ${traverseChildrenQuote(c)(tpe)}
//        }
//      }
//
//      GenTraversableRegistrar
//    """
//  }

  private def traverseChildrenQuote(c: Context)(tpe: c.universe.Type): c.universe.Tree = {
    import c.universe._

    lazy val accessors: List[MethodSymbol] = getAccessors(c)(tpe)
    
    println("traversing: " + tpe.toString)

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
        println(s"leftTpe = $leftTpe")
        val rightTpe = eitherTpe.dealias.typeArgs.last
        println(s"rightTpe = $rightTpe")
        q"""
          ${registerToRegistryQuote(c)(leftTpe)}
          ${registerToRegistryQuote(c)(rightTpe)}
        """

      // a sealed trait
      case traitTpe: Type if traitTpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isSealed =>
        val childrenQuotes = getSealedTraitChildren(c)(traitTpe).withFilter(!hasNoAccessor(c)(_)) map {
          registerToRegistryQuote(c)(_)
        }
        q"..$childrenQuotes"

      // a structured type
      case _ if accessors.nonEmpty =>
        val accessorQuotes = accessors map {
          accessor =>
            val accessorTpe = accessor.returnType.substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
            registerToRegistryQuote(c)(accessorTpe)
        }
        q"..$accessorQuotes"

      case _ => q"()"
    }
  }

  private def registerToRegistryQuote(c: Context)(registerTpe: c.universe.Type): c.universe.Tree = {
    import c.universe._
    val tpeName = toMapKey(registerTpe.toString)
    if (!(RegistrarMacroTracker contains registerTpe.toString)) {
      println("registering: " + tpeName)
      materialiseDirect(c)(registerTpe).tree
    } else {
      q"()"
    }
  }
}
