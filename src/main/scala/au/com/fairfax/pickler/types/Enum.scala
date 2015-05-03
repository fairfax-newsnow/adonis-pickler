package au.com.fairfax.pickler.types

import au.com.fairfax.pickler.simpleTypeNm

object CaseEnum {

  def makeEnum[E <: Enum](caseEnum: CaseEnum[E], name: String): E =
    caseEnum withName name

}

trait CaseEnum[T <: Enum] {

  def values: Seq[T]

  lazy val nameMap: String Map T = values.map { choice =>
    ({
      val choiceStr = choice.toString
      if (choiceStr endsWith "$") choiceStr.substring(0, choiceStr.length - 1) else choiceStr
    } -> choice)
  }.toMap.withDefaultValue(values.head)


  def withName(name: String): T = nameMap(name.toUpperCase)

}

trait Enum {

  override lazy val toString = {
    simpleTypeNm(getClass.getName).toUpperCase.dropWhile(_ != '$').drop(1)
  }
}