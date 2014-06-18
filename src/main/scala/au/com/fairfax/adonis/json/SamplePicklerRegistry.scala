package au.com.fairfax.adonis.json

import scala.reflect.ClassTag
import scala.collection.mutable
import au.com.fairfax.adonis.utils.json.{JsonBuilder, JsonReader}
import au.com.fairfax.adonis.apws.macros.SampleMaterializers1

object SamplePicklerRegistry extends SampleBasePicklerRegistry {
  class SampleSingletonFullName[A](val name: String)

  object SampleSingletonFullName extends SampleMaterializers1
}

trait SamplePicklerRegistry {
  def pickle[P](value: Any)(implicit builder: JsonBuilder[P],
                            registry: SamplePicklerRegistry = this): P

  def unpickle[P](pickle: P)(implicit reader: JsonReader[P],
                             registry: SamplePicklerRegistry = this): Any
}

class SampleBasePicklerRegistry extends SamplePicklerRegistry {

  import SamplePicklerRegistry._

  private val picklers = new mutable.HashMap[String, SamplePickler[_]]
  private val unpicklers = new mutable.HashMap[String, SampleUnpickler[_]]
  private val singletons = new mutable.HashMap[Any, String]
  private val singletonsRev = new mutable.HashMap[String, Any]

  registerBuiltinPicklers()

  private def registerInternal(clazz: Class[_], pickler: SamplePickler[_],
                               unpickler: SampleUnpickler[_]): Unit = {
    picklers(clazz.getName) = pickler
    unpicklers(clazz.getName) = unpickler
    println(
      s"""${getClass.getSimpleName}
         |picklers = $picklers
         |unpicklers = $unpicklers
       """.stripMargin)
  }

  def register[A: ClassTag](pickler: SamplePickler[A],
                            unpickler: SampleUnpickler[A]): Unit = {
    println(s"${getClass.getSimpleName}.register(unpickler, unpickler) triggered")
    registerInternal(implicitly[ClassTag[A]].runtimeClass, pickler, unpickler)
  }

  def register[A: ClassTag](implicit pickler: SamplePickler[A],
                            unpickler: SampleUnpickler[A]): Unit = {
    println(s"${getClass.getSimpleName}.register(implicit unpickler, unpickler) triggered")
    register(pickler, unpickler)
  }

  def register[A <: Singleton](obj: A)(implicit name: SampleSingletonFullName[A]): Unit = {
    println(s"${getClass.getSimpleName}.register(singleton) triggered")
    singletons(obj) = name.name
    singletonsRev(name.name) = obj
  }

  def pickle[P](value: Any)(implicit builder: JsonBuilder[P],
                            registry: SamplePicklerRegistry): P = {
    if (value == null) {
      builder.makeNull()
    } else {
      singletons.get(value) match {
        case Some(name) => builder.makeObject(("s", builder.makeString(name)))
        case _ =>
          val className = value.getClass.getName
          val pickler = picklers(className)
          val pickledValue = pickler.pickle[P](value.asInstanceOf[pickler.Picklee])
          builder.makeObject(
            ("t", builder.makeString(className)),
            ("v", pickledValue))
      }
    }
  }

  def unpickle[P](pickle: P)(implicit reader: JsonReader[P],
                             registry: SamplePicklerRegistry): Any = {
    if (reader.isNull(pickle)) {
      null
    } else {
      val s = reader.readObjectField(pickle, "s")
      if (!reader.isUndefined(s)) {
        singletonsRev(reader.readString(s))
      } else {
        val className = reader.readString(reader.readObjectField(pickle, "t"))
        val unpickler = unpicklers(className)
        unpickler.unpickle[P](reader.readObjectField(pickle, "v"))
      }
    }
  }

  private def registerBuiltinPicklers(): Unit = {
    registerPrimitive[Boolean, java.lang.Boolean]
    registerPrimitive[Char, java.lang.Character]
    registerPrimitive[Byte, java.lang.Byte]
    registerPrimitive[Short, java.lang.Short]
    registerPrimitive[Int, java.lang.Integer]
    registerPrimitive[Long, java.lang.Long]
    registerPrimitive[Float, java.lang.Float]
    registerPrimitive[Double, java.lang.Double]

    register[String]
  }

  private def registerPrimitive[P: ClassTag, W: ClassTag](implicit pickler: SamplePickler[P], unpickler: SampleUnpickler[P]): Unit = {
    register[P]
    registerInternal(implicitly[ClassTag[W]].runtimeClass, pickler, unpickler)
  }
}