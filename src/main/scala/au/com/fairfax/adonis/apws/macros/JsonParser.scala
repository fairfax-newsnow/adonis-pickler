package au.com.fairfax.adonis.apws.macros

trait JsonParser[T] extends FormatterParser[T] {
  def parse[J](json: J)(implicit reader: JReader[J]): T
  
//  def buildChildParsers: String Map JsonParser[_]
}

object JsonParser extends ParserMaterializer