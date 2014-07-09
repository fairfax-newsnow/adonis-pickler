package au.com.fairfax.adonis.apws.macros

trait JBuilder[J] {
  def makeNull(): J
  def makeBoolean(b: Boolean): J
  def makeNumber(x: Double): J
  def makeString(s: String): J
  def makeArray(elems: J*): J
  def makeObject(fields: (String, J)*): J
}

trait JReader[J] {
  def isUndefined(x: J): Boolean
  def isNull(x: J): Boolean
  def readBoolean(x: J): Boolean
  def readNumber(x: J): Double
  def readString(x: J): String
  def readArrayLength(x: J): Int
  def readArrayElem(x: J, index: Int): J
  def readObjectField(x: J, field: String): J
}
