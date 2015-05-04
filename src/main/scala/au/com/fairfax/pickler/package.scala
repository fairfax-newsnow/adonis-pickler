package au.com.fairfax

package object pickler {
  def simpleTypeNm(typeName: String): String =
    typeName.split("\\.").toList.dropWhile(s => s(0) < 'A' || s(0) > 'Z').mkString(".")

}
