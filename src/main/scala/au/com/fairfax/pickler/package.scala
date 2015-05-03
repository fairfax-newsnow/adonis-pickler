package au.com.fairfax

package object pickler {
  def simpleTypeNm(name: String): String =
    name.substring(name.lastIndexOf(".") + 1)

}
