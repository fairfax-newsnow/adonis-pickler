package au.com.fairfax.adonis.apws.macros

trait JsonFormatter[T] extends FormatterParser[T] {
  def format[J](obj: Any)(implicit builder: JBuilder[J]): J
}

object JsonFormatter extends FormatterMaterializer