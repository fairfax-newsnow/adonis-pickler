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
  //  lazy val strReplacement = {
  //    val strings = List(typeOf[List[_]], typeOf[Seq[_]], typeOf[Vector[_]]).map(_.toString) :::
  //      List(classOf[Map[_, _]], classOf[Vector[_]], classOf[String]).map(_.getName)
  //    strings.map {
  //      s => removeBracket(s) -> removeBracket(removePkgName(s))
  //    }.toMap
  //  }
  lazy val strReplacement: String Map String =
    List(className[Short], className[Int], className[Long], className[Double], className[Float], className[Boolean]).map {
      s => s -> s.capitalize
    }.toMap

  private def className[T: ClassTag] = implicitly[ClassTag[T]].runtimeClass.getName


  private def removeBracket(s: String): String = {
    val idx = s.indexOf("[")
    if (idx < 0) s else s.substring(0, idx)
  }

  private def toMapKey(s: String): String = strReplacement.foldLeft(s) {
    (z, x) => if (z == x._1) x._2 else z
  }.replace('$', '.')

  def register[T: ClassTag](implicit parser: JsonParser[T], formatter: JsonFormatter[T]): Unit = {
    //    val key = toMapKey(implicitly[ClassTag[T]].runtimeClass.getName)
    val key = toMapKey(className[T])
    println(s"register, key = $key")
    parsers += (key -> parser)
    formatters += (key -> formatter)

    println(s"parsers.key = ${parsers.keys.toList}")
    println(s"formatters.key = ${formatters.keys.toList}")
  }

  override def format[J, T: ClassTag](obj: T)(implicit builder: JBuilder[J]): J = {
    //    val key = toMapKey(implicitly[ClassTag[T]].runtimeClass.getName)
    val key = toMapKey(className[T])
    println(s"format, key = $key")
    formatters(key).format(obj)
  }

  override def parse[J](json: J)(implicit reader: JReader[J]): Any = {
    val cmdType = reader.readString(reader.readObjectField(json, "cmd"))
    parsers(toMapKey(cmdType)).parse(json)
  }

}
