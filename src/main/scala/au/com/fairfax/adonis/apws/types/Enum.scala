package au.com.fairfax.adonis.apws.types

import au.com.fairfax.adonis.utils._


object CaseEnum {

  def makeEnum[E <: Enum](caseEnum: CaseEnum[E], name: String): E =
    caseEnum withName name

}

trait CaseEnum[T <: Enum] {

  def values: Seq[T]

  lazy val nameMap: String Map T = values.map( choice =>
    choice.toString -> choice
  ).toMap.withDefaultValue(values.head)

  def withName(name: String): T = nameMap(name.toUpperCase)

}

trait Enum {

  override lazy val toString =
    getSimpleName(this.getClass).toUpperCase.dropWhile(_ != '$').drop(1)
}