package au.com.fairfax.adonis.apws.macros.json

import scala.language.experimental.macros
import scala.reflect.macros.blackbox._
import scala.language.higherKinds

//TODO be removed
object TypeDeducerMaterializerImpl {
  def materialize[T: c.WeakTypeTag](c:Context): c.Expr[TypeDeducer[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val result =
    q"""
      object GenTypeDeducer extends au.com.fairfax.adonis.apws.macros.json.TypeDeducer[$tpe] {
        lazy val typeString = ${tpe.toString}
        override def deduce(obj: Any): Option[String] = obj match {
          case x : $tpe => ${TermName("Some")}(toString())
          case _ => None
        }

        override def toString() = typeString
        override def equals(obj: Any) = toString() == obj.toString()
        override def hashCode(): Int = toString().hashCode()
      }
      GenTypeDeducer
    """
    println(result)
    c.Expr[TypeDeducer[T]](result)
  }
}

trait TypeDeducerMaterializer {
  implicit def typeDeducerMacro[T]: TypeDeducer[T] = macro TypeDeducerMaterializerImpl.materialize[T]
}

trait TypeDeducer[T] {
  def deduce(obj: Any): Option[String]
}

object TypeDeducer extends TypeDeducerMaterializer
