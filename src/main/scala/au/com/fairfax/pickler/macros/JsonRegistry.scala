package au.com.fairfax.pickler.macros

import scala.collection.mutable.{HashMap => MHashMap}
import scala.language.existentials
import scala.language.experimental.macros
import scala.reflect.ClassTag
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
        case "T" => toMapKey(className[T])
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
  def internalFormat[J](obj: Any, nameOfFormattedField: String, includeTpeInJson: Boolean, typeKey: String)(implicit builder: JBuilder[J]): J = {
    lazy val objClassName = {
      val className = obj.getClass.getName.replace('$', '.')
      if (className endsWith ".") className + "type" // this a case object, key should be ended with ".type"
      else className
    }
    
    formatters.get(typeKey).fold {
      // typeKey is probably Any or non-sealed trait, therefore objClassName is used
      formatters.get(objClassName).fold {
        throw new Error(s"No formatter exists for $typeKey or $objClassName derived from object of class name ${obj.getClass}.replace('$$', '.')")
      }(_.format(obj)("v")(true)) // includeTpeInJson is true, o.w. the parser won't known which concrete class to parse
    }(_.format(obj)(nameOfFormattedField)(includeTpeInJson))
  }

  def parse[J](json: J, nameOfParsedField: String = "args", objTpe: Option[String] = None)(implicit reader: JReader[J]): Any = {
    val cmdType = objTpe getOrElse reader.readString(reader.readObjectField(json, "t"))
    val typeKey = toMapKey(cmdType)
    lazy val jsonForConcreteType = reader.readObjectField(json, nameOfParsedField)
    lazy val concreteType = reader.readString(reader.readObjectField(jsonForConcreteType, "t"))
    
    parsers.get(typeKey).fold {
      parsers.get(concreteType).fold {
        throw new Error(s"No parser exists for $typeKey or $concreteType")
      } (_.parse(jsonForConcreteType)("v"))
    } { _.parse(json)(nameOfParsedField) }
  }

}

object TypeKeyProvider {
  def materializeTypeKeyProvider[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val typeTag = c.universe.weakTypeOf[T]
    q"""
      val provider = new au.com.fairfax.pickler.macros.TypeKeyProvider[$typeTag] {
        def key: String = ${typeTag.toString()}
      }
      provider
    """
  }
}
