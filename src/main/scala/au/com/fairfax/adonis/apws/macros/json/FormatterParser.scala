package au.com.fairfax.adonis.apws.macros.json

import org.scalajs.spickling._
import au.com.fairfax.adonis.apws.macros._

trait FormatterParser[T]

trait JsonParser[T] extends FormatterParser[T] {
  def parse[J](json: J)(implicit reader: PReader[J]): T
}

object JsonParser extends ParserMaterializer

trait JsonFormatter[T] extends FormatterParser[T] {
  def format[J](obj: Any)(implicit builder: PBuilder[J]): J
}

object JsonFormatter extends FormatterMaterializer


//BACKUP classes
trait JsonParserBACKUP[T] {
  def parse[J](json: J)(implicit reader: PReader[J]): T
}

object JsonParserBACKUP extends JsonMaterializersBACKUP

trait JsonFormatterBACKUP[T] {
  def format[J](obj: Any)(implicit builder: PBuilder[J]): J
}

object JsonFormatterBACKUP extends JsonMaterializersBACKUP
