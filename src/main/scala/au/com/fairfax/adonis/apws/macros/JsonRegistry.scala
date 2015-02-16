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

  def format[J, T: ClassTag](obj: T, nameOfFormattedField: String, topObj: Boolean)(implicit builder: JBuilder[J], keyProvider: TypeKeyProvider[T]): J

  def parse[J](json: J, nameOfParsedField: String, objTpe: Option[String])(implicit reader: JReader[J]): Any
}

class BaseJsonRegistry extends JsonRegistry {

  private val parsers = new MHashMap[String, JsonParser[_]]
  private val formatters = new MHashMap[String, JsonFormatter[_]]

  def register[T](implicit traversableReg: TraversableRegistrar[T]): Unit = {
    traversableReg.traversableRegister
//    val key = keyProvider.key
//    val replacedKey = toMapKey(key)
//    parsers += (replacedKey -> parser)
//    formatters += (replacedKey -> formatter)
  }
  
//  def registerNew[T](implicit traversableReg: TraversableRegistrar[T]): Unit =
//    traversableReg.traversableRegister

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
  
  override def format[J, T: ClassTag](obj: T, nameOfFormattedField: String = "args", topObj: Boolean = true)(implicit builder: JBuilder[J], keyProvider: TypeKeyProvider[T]): J = {
    val key = keyProvider.key
    println(s"JsonRegistry.format(), key = $key")
    formatters.get {
      key match {
        case "T" => toMapKey(className[T])
        case k if strReplacement.exists(k contains _._1) => toMapKey(k)
        case _ => key
      }
    }.fold {
      throw new Error(s"No formatter exists for $key")
    } { _.format(obj)(nameOfFormattedField)(topObj) }
  }

  override def parse[J](json: J, nameOfParsedField: String = "args", objTpe: Option[String] = None)(implicit reader: JReader[J]): Any = {
    println(s"JsonRegistry.parse(), json = $json, nameOfParsedField = $nameOfParsedField")
    val cmdType = objTpe.getOrElse(reader.readString(reader.readObjectField(json, "t")))
    val key = toMapKey(cmdType)
    parsers.get(key).fold {
      throw new Error(s"No parser exists for $key")
    } { _.parse(json)(nameOfParsedField) }
  }

}

object TypeKeyProvider {
  def materializeTypeKeyProvider[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val typeTag = c.universe.weakTypeOf[T]
    println(s"TypeKeyProvider typeTag = $typeTag, typeTag.toString = ${typeTag.toString}")
    q"""
    val provider = new au.com.fairfax.adonis.apws.macros.TypeKeyProvider[$typeTag] {
      def key: String = ${typeTag.toString()}
    }
    provider
    """
  }
}
