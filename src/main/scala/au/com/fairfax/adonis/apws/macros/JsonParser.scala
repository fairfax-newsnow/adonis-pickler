package au.com.fairfax.adonis.apws.macros

trait JsonParser[T] extends FormatterParser[T] {
  def parse[J](json: J)(nameOfParsedField: String)(implicit reader: JReader[J]): T
}

object JsonParser extends ParserMaterializer