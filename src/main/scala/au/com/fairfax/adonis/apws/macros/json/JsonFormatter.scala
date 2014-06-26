package au.com.fairfax.adonis.apws.macros.json

import org.scalajs.spickling._
import au.com.fairfax.adonis.apws.macros.JsonMaterializers
import scala.reflect.runtime.universe._


trait JsonFormatter[T] {
  def format[J](obj: Any)(implicit builder: PBuilder[J]): J
}

object JsonFormatter extends JsonMaterializers
