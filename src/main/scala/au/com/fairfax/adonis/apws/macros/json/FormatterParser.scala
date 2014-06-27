package au.com.fairfax.adonis.apws.macros.json

import org.scalajs.spickling._
import au.com.fairfax.adonis.apws.macros._

trait FormatterParser[T]

trait JsonParserRefactored[T] extends FormatterParser[T] {
  def parse[J](json: J)(implicit reader: PReader[J]): T
}

object JsonParserRefactored extends JsonMaterializersRefactored

trait JsonFormatterRefactored[T] extends FormatterParser[T] {
  def format[J](obj: Any)(implicit builder: PBuilder[J]): J
}

object JsonFormatterRefactored extends JsonMaterializersRefactored


trait JsonParser[T] {
  def parse[J](json: J)(implicit reader: PReader[J]): T
}

object JsonParser extends JsonMaterializers

trait JsonFormatter[T] {
  def format[J](obj: Any)(implicit builder: PBuilder[J]): J
}

object JsonFormatter extends JsonMaterializers
