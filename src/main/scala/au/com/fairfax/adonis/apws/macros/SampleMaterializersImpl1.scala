package au.com.fairfax.adonis.apws.macros

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.reflect.runtime.universe
import au.com.fairfax.adonis.json.{SampleUnpickler, SamplePickler, SamplePicklerRegistry}
import scala.language.implicitConversions

object SampleMaterializersImpl1 {
  def materializePickler[T: c.WeakTypeTag](c: Context): c.Expr[SamplePickler[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass

    if (!sym.isCaseClass) {
      c.error(c.enclosingPosition,
        "Cannot materialize pickler for non-case class")
      return c.Expr[SamplePickler[T]](q"null")
    }

    val accessors = (tpe.declarations collect {
      case acc: MethodSymbol if acc.isCaseAccessor => acc
    }).toList

    val pickleFields = for {
      accessor <- accessors
    } yield {
      val fieldName = accessor.name.toTermName
      val fieldString = fieldName.toString()
      q"""
        ($fieldString, registry.pickle(value.$fieldName))
      """
    }

    val pickleLogic = q"""
      builder.makeObject(..$pickleFields)
    """

    val result = q"""
      implicit object GenPickler extends au.com.fairfax.adonis.json.SamplePickler[$tpe] {
        import org.scalajs.spickling._
        import au.com.fairfax.adonis.json._
        override def pickle[P](value: $tpe)(
            implicit registry: SamplePicklerRegistry,
            builder: PBuilder[P]): P = $pickleLogic
      }
      GenPickler
    """

    c.Expr[SamplePickler[T]](result)
  }

  def materializeUnpickler[T: c.WeakTypeTag](c: Context): c.Expr[SampleUnpickler[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass

    if (!sym.isCaseClass) {
      c.error(c.enclosingPosition,
        "Cannot materialize pickler for non-case class")
      return c.Expr[SampleUnpickler[T]](q"null")
    }

    val accessors = (tpe.declarations collect {
      case acc: MethodSymbol if acc.isCaseAccessor => acc
    }).toList

    val typeParams = tpe.typeConstructor.typeParams
    val typeArgs = tpe.typeArgs

    val unpickledFields = for {
      accessor <- accessors
    } yield {
      val fieldName = accessor.name
      val fieldString = fieldName.toString()
      val fieldTpe = accessor.returnType.substituteTypes(typeParams, typeArgs)
//      println(s"fieldTpe.toString = ${fieldTpe.toString}")

      q"""
        registry.unpickle(reader.readObjectField(
            pickle, $fieldString)).asInstanceOf[$fieldTpe]
      """
    }

    val unpickleLogic = q"""
      new $tpe(..$unpickledFields)
    """

    val result = q"""
      implicit object GenUnpickler extends au.com.fairfax.adonis.json.SampleUnpickler[$tpe] {
        import org.scalajs.spickling._
        import au.com.fairfax.adonis.json._
        override def unpickle[P](pickle: P)(
            implicit registry: SamplePicklerRegistry,
      reader: PReader[P]): $tpe = $unpickleLogic
      }
      GenUnpickler
    """

    println(
      s"""result =
           $result
       """.stripMargin)
    c.Expr[SampleUnpickler[T]](result)
  }

  def materializeCaseObjectName[T: c.WeakTypeTag](c: Context): c.Expr[SamplePicklerRegistry.SampleSingletonFullName[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol.asClass

    if (!sym.isModuleClass || !sym.isCaseClass)
      c.abort(c.enclosingPosition,
        s"Cannot generate a case object name for non-case object $sym")

    val name = sym.fullName+"$"
    val result = q"""
      new au.com.fairfax.adonis.json.SamplePicklerRegistry.SingletonFullName[$tpe]($name)
    """

    c.Expr[SamplePicklerRegistry.SampleSingletonFullName[T]](result)
  }
}

trait SampleMaterializers1 {
  implicit def materializePickler[T]: SamplePickler[T] =
  macro SampleMaterializersImpl1.materializePickler[T]

  implicit def materializeUnpickler[T]: SampleUnpickler[T] =
  macro SampleMaterializersImpl1.materializeUnpickler[T]

  implicit def materializeCaseObjectName[T]: SamplePicklerRegistry.SampleSingletonFullName[T] =
  macro SampleMaterializersImpl1.materializeCaseObjectName[T]
}

