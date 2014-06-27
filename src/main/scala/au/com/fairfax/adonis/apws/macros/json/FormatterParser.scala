package au.com.fairfax.adonis.apws.macros.json

import au.com.fairfax.adonis.utils.json._
import au.com.fairfax.adonis.apws.macros._

trait FormatterParser[T]

trait JsonParser[T] extends FormatterParser[T] {
  def parse[J](json: J)(implicit reader: JReader[J]): T
}

object JsonParser extends ParserMaterializer

trait JsonFormatter[T] extends FormatterParser[T] {
  def format[J](obj: Any)(implicit builder: JBuilder[J]): J
}

object JsonFormatter extends FormatterMaterializer


//TODO BACKUP classes, should be removed
trait JsonParserBACKUP[T] {
  def parse[J](json: J)(implicit reader: JReader[J]): T
}

object JsonParserBACKUP extends JsonMaterializersBACKUP

trait JsonFormatterBACKUP[T] {
  def format[J](obj: Any)(implicit builder: JBuilder[J]): J
}

object JsonFormatterBACKUP extends JsonMaterializersBACKUP
