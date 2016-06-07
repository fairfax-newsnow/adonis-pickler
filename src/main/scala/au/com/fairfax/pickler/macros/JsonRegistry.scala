package au.com.fairfax.pickler.macros

import au.com.fairfax.pickler.simpleTypeNm
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
  
  private var aliases: String Map String = Map.empty

  def register[T](implicit traversableReg: TraversableRegistrar[T]): Unit =
    traversableReg.traversableRegister foreach {
      t =>
        val (key, parser, formatter) = t
        parsers += (key -> parser)
        formatters += (key -> formatter)
    }
  /**
   *  Register the old names/locations of type when refactoring to ensure backwards compatibility.
   */
  def registerTypeAlias[T: ClassTag](alias: String)(implicit keyProvider: TypeKeyProvider[T]): Unit = {
    val typeKey = {
      val key = keyProvider.key
      key match {
        case "T" => toMapKey(implicitly[ClassTag[T]].runtimeClass.getName)
        case k if strReplacement.exists(k contains _._1) => toMapKey(k)
        case _ => key
      }
    }
    aliases = aliases + (alias -> typeKey)
  }
  
//  def registerTypeAlias[T: ClassTag, Alias: ClassTag](implicit keyProvider: TypeKeyProvider[T], aliasProvider: TypeKeyProvider[Alias]): Unit = {
//    val aliasKey = {
//      val key = aliasProvider.key
//      key match {
//        case "T" => toMapKey(className[T])
//        case k if strReplacement.exists(k contains _._1) => toMapKey(k)
//        case _ => key
//      }
//    }
//    registerTypeAlias[T](aliasKey)(keyProvider)
//  }

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
   * A function that should be called by a generated formatter only, please check the quasiquote 
   * inside FormatterMaterializerImpl, the reason is the the formatter can provide the typeKey information without
   * using a TypeKeyProvider.  Using TypeKeyProvider needs macro which results in high memory usage  
   */
  def internalFormat[J](obj: Any, nameOfFormattedField: String, includeTpeInJson: Boolean, typeKey: String)(implicit builder: JBuilder[J]): J = {
    // objClassName is needed in case typeKey is Any or non-sealed trait
    lazy val objClassName = {
      val className = obj.getClass.getName.replace('$', '.')
      if (className endsWith ".") className + "type" // this a case object, key should be ended with ".type"
      else className
    }
    // if objClassName doesn't work, then this objClassName might be a primitive data type class,
    // i.e. java.lang.Float, java.lang.Short, ...
    lazy val primitiveType = toMapKey(objClassName)

    formatters.get(typeKey).fold {
      formatters.get(objClassName).fold {
        formatters.get(primitiveType).fold {
          throw new Error(s"No formatter exists for $typeKey or $objClassName derived from object of class name ${obj.getClass}.replace('$$', '.') or $primitiveType")
        }(_.format(obj)("v")(true))
      }(_.format(obj)("v")(true)) // includeTpeInJson should be true, o.w. the parser won't known which concrete class to parse
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
  
  private def formatter(key: String): Option[JsonFormatter[_]] = 
    formatters.get(key).fold(aliases.get(key).flatMap(formatters.get))(Some(_))

  private def parser(key: String): Option[JsonParser[_]] = 
    parsers.get(key).fold(aliases.get(key).flatMap(parsers.get))(Some(_))

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
