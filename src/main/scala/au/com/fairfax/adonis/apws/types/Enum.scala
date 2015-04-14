package au.com.fairfax.adonis.apws.types

import au.com.fairfax.adonis.utils._


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
    val simpleClassName = getSimpleName(this.getClass).toUpperCase
    
    if (simpleClassName contains "$")
      simpleClassName.dropWhile(_ != '$').drop(1)
    else
      simpleClassName
  }
}