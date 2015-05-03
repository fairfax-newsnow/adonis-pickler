package au.com.fairfax.pickler.macros

trait JsonFormatter[T] extends FormatterParser[T] {
  def format[J](obj: Any)(nameOfFormattedField: String)(topObj: Boolean)(implicit builder: JBuilder[J]): J
}

object JsonFormatter extends FormatterMaterializer