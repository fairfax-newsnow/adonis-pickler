package au.com.fairfax.adonis.apws.macros

trait JsonFormatter[T] extends FormatterParser[T] {
  //TODO
//  def format[J](obj: Any)(nameOfFormattedField: String)(implicit builder: JBuilder[J]): J
  def format[J](obj: Any)(implicit builder: JBuilder[J]): J
}

object JsonFormatter extends FormatterMaterializer