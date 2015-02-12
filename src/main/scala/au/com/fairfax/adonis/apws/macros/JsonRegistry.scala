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


  private def toMapKey(s: String): String = strReplacement.getOrElse(s, s).replace('$', '.')

  def register[T](implicit parser: JsonParser[T], formatter: JsonFormatter[T], keyProvider: TypeKeyProvider[T]): Unit = {
    val key = keyProvider.key
    parsers += (key -> parser)
    formatters += (key -> formatter)
  }
  
  def registerNew[T](implicit transversableReg: RegisterHelper[T], parser: JsonParser[T], formatter: JsonFormatter[T], keyProvider: TypeKeyProvider[T]): Unit = {
    println("JsonRegistry.registerNew!!")
    if (!alreadyRegistered[T]) {
      println("JsonRegistry.registerNew(), not in memory yet, got to call transversableReg.traversableRegister")
      transversableReg.traversableRegister
    } else {
      println("JsonRegistry.registerNew(), in memory already, no need to call transversableReg.traversableRegister")
    }
  }
  
  def add(parser: (String, JsonParser[_]))(formatter: (String, JsonFormatter[_])): Unit = {
    parsers += (parser._1 -> parser._2)
    formatters += (formatter._1 -> formatter._2)
    println(
      s"""
         |add(),
         |parsers = $parsers
         |formatters = $formatters
       """.stripMargin)
  }
  
  def alreadyRegistered[T](implicit keyProvider: TypeKeyProvider[T]): Boolean =
    parsers contains (keyProvider.key)

//  def registerTest[T](b: Boolean)(implicit regHelper: RegisterHelper[T], parser: JsonParser[T], formatter: JsonFormatter[T], keyProvider: TypeKeyProvider[T]) = {
//    val (genParers, genFormatters) = regHelper.traversableRegisterTest(b)
//    parsers ++= genParers
//    formatters ++= genFormatters
//    (parsers, formatters)
//  }
  
  override def format[J, T: ClassTag](obj: T)(implicit builder: JBuilder[J], keyProvider: TypeKeyProvider[T]): J = {
    val key = keyProvider.key
    formatters.get(if (key == "T") toMapKey(className[T]) else key).fold {
      throw new Error(s"No formatter exists for $key")
    } {_ format obj}
  }

  override def parse[J](json: J)(implicit reader: JReader[J]): Any = {
    val cmdType = reader.readString(reader.readObjectField(json, "t"))
    val key = toMapKey(cmdType)
    parsers.get(key).fold {
      throw new Error(s"No parser exists for $key")
    } {_ parse json}
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
