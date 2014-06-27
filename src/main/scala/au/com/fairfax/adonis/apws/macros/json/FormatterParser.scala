package au.com.fairfax.adonis.apws.macros.json

import org.scalajs.spickling._
import au.com.fairfax.adonis.apws.macros.JsonMaterializers

trait JsonParser[T] extends FormatterParser[T] {
  def parse[J](json: J)(implicit reader: PReader[J]): T
}

object JsonParser extends JsonMaterializers

trait JsonFormatter[T] extends FormatterParser[T] {
  def format[J](obj: Any)(implicit builder: PBuilder[J]): J
}

object JsonFormatter extends JsonMaterializers

trait FormatterParser[T]
