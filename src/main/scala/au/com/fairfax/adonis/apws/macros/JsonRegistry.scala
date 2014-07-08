package au.com.fairfax.adonis.apws.macros

import scala.collection.mutable.{HashMap => MHashMap}

import scala.reflect.ClassTag

object JsonRegistry extends BaseJsonRegistry

trait JsonRegistry {
  def format[J, T: ClassTag](obj: T)(implicit builder: JBuilder[J]): J

  def parse[J](json: J)(implicit reader: JReader[J]): Any
}

class BaseJsonRegistry extends JsonRegistry {
  private val parsers = new MHashMap[String, JsonParser[_]]
  private val formatters = new MHashMap[String, JsonFormatter[_]]
  lazy val strReplacement: String Map String =
    List(className[Short], className[Int], className[Long], className[Double], className[Float], className[Boolean]).map {
      s => s -> s.capitalize
    }.toMap

  private def className[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass.getName


  private def toMapKey(s: String): String = strReplacement.foldLeft(s) {
    (z, x) => if (z == x._1) x._2 else z
  }.replace('$', '.')

  def register[T: ClassTag](implicit parser: JsonParser[T], formatter: JsonFormatter[T]): Unit = {
    val key = toMapKey(className[T])
    parsers += (key -> parser)
    formatters += (key -> formatter)

    //    println(s"parsers.key = ${parsers.keys.toList}")
    //    println(s"formatters.key = ${formatters.keys.toList}")
  }

  override def format[J, T: ClassTag](obj: T)(implicit builder: JBuilder[J]): J = {
    val key = toMapKey(className[T])
    formatters(key).format(obj)
  }

  override def parse[J](json: J)(implicit reader: JReader[J]): Any = {
    val cmdType = reader.readString(reader.readObjectField(json, "cmd"))
    parsers(toMapKey(cmdType)).parse(json)
  }

}
