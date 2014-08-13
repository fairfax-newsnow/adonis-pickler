package au.com.fairfax.adonis.apws.macros

import scala.collection.mutable.{HashMap => MHashMap}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import au.com.fairfax.adonis.utils.simpleTypeNm
import scala.reflect.macros.blackbox.Context


trait TypeKeyProvider[T] {
  def key: String
}

object JsonRegistry extends BaseJsonRegistry

trait TypeProvider {

  implicit def materializeTypeKeyProvider[T]: TypeKeyProvider[T] =
  macro TypeKeyProvider.materializeTypeKeyProvider[T]
}

trait JsonRegistry extends TypeProvider {

  def format[J, T: ClassTag](obj: T)(implicit builder: JBuilder[J], keyProvider: TypeKeyProvider[T]): J

  def parse[J](json: J)(implicit reader: JReader[J]): Any
}

class BaseJsonRegistry extends JsonRegistry {

  private val parsers = new MHashMap[String, JsonParser[_]]
  private val formatters = new MHashMap[String, JsonFormatter[_]]
  lazy val strReplacement: String Map String =
    (List(className[Short], className[Int], className[Long], className[Double], className[Float], className[Boolean]).map {
      s => s -> s.capitalize
    } ::: List(className[String]).map{
      s => s -> simpleTypeNm(s)
    }).toMap

  private def className[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass.getName


  private def toMapKey(s: String): String = strReplacement.foldLeft(s) {
    (z, x) => if (z == x._1) x._2 else z
  }.replace('$', '.')

  def register[T](implicit parser: JsonParser[T], formatter: JsonFormatter[T], keyProvider: TypeKeyProvider[T]): Unit = {
    val key = keyProvider.key
    println(s"register, keyProvider.key = $key")
    parsers += (key -> parser)
    formatters += (key -> formatter)
  }

  override def format[J, T: ClassTag](obj: T)(implicit builder: JBuilder[J], keyProvider: TypeKeyProvider[T]): J = {
    println(s"format, formatters = $formatters")
    val key = keyProvider.key
    formatters.get(if (key == "T") toMapKey(className[T]) else key) match {
      case Some(formatter) =>
        formatter format obj

      case None =>
        throw new Error(s"No formatter exists for $key")
    }
  }

  override def parse[J](json: J)(implicit reader: JReader[J]): Any = {
    val cmdType = reader.readString(reader.readObjectField(json, "t"))
    val key = toMapKey(cmdType)
    parsers.get(key) match {
      case Some(parser) =>
        parser parse json

      case None =>
        throw new Error(s"No parser exists for $key")
    }
  }

}

object TypeKeyProvider {
  def materializeTypeKeyProvider[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val typeTag = c.universe.weakTypeOf[T]
    q"""
    val provider = new au.com.fairfax.adonis.apws.macros.TypeKeyProvider[$typeTag] {
      def key: String = ${typeTag.toString()}
    }
    provider
    """
  }
}
