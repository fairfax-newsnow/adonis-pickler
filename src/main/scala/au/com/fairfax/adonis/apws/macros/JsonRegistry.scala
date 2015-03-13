package au.com.fairfax.adonis.apws.macros

import scala.collection.mutable.{HashMap => MHashMap}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context
import scala.language.existentials

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

  def parse[J](json: J, nameOfParsedField: String, objTpe: Option[String])(implicit reader: JReader[J]): Any
}



class BaseJsonRegistry extends JsonRegistry {

  private val parsers = new MHashMap[String, JsonParser[_]]
  private val formatters = new MHashMap[String, JsonFormatter[_]]

  def register[T](implicit traversableReg: TraversableRegistrar[T]): Unit =
    traversableReg.traversableRegister foreach {
      t =>
        val (key, parser, formatter) = t
        parsers += (key -> parser)
        formatters += (key -> formatter)
    }

  def format[J, T: ClassTag](obj: T)(implicit builder: JBuilder[J], keyProvider: TypeKeyProvider[T]): J = {
    val typeKey = keyProvider.key
    formatters.get {
      typeKey match {
        case "T" | "scala.Any" => toMapKey(className[T])
        case k if strReplacement.exists(k contains _._1) => toMapKey(k)
        case _ => typeKey
      }
    }.fold {
      throw new Error(s"No formatter exists for $typeKey")
    } { _.format(obj)("args")(true) }
  }

  /**
   * A function that should be called by a generated formatter only, the reason is the the formatter can provide the typeKey information without 
   * using a TypeKeyProvider.  Using TypeKeyProvider needs macro which results in high memory usage  
   */
  def internalFormat[J](obj: Any, nameOfFormattedField: String, topObj: Boolean, typeKey: String)(implicit builder: JBuilder[J]): J =
    formatters.get(typeKey).fold {
      throw new Error(s"No formatter exists for $typeKey")
    }(_.format(obj)(nameOfFormattedField)(topObj))

  def parse[J](json: J, nameOfParsedField: String = "args", objTpe: Option[String] = None)(implicit reader: JReader[J]): Any = {
    val cmdType = objTpe getOrElse reader.readString(reader.readObjectField(json, "t"))
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
    q"""
      val provider = new au.com.fairfax.adonis.apws.macros.TypeKeyProvider[$typeTag] {
        def key: String = ${typeTag.toString()}
      }
      provider
    """
  }
}
