package au.com.fairfax.pickler.au.com.fairfax.mock

import au.com.fairfax.pickler.types.{CaseEnum, Enum}
/**
 * Mock objects of CaseEnum to illustrate the pickler's handling on self-defined enum objects   
 */
//the enum are case objects that should be handled by the pickler
object Algorithm {
  object Learning extends CaseEnum[Learning] {
    lazy val values = Seq(Supervised, Unsupervised)
  }
  sealed trait Learning extends Enum
  case object Supervised extends Learning
  case object Unsupervised extends Learning
}

// the enum are empty case classes that should be handled by the pickler
object Regression {
  object Classification extends CaseEnum[Classification] {
    lazy val values = Seq(Linear(), Logistic())
  }
  sealed trait Classification extends Enum
  case class Linear() extends Classification
  case class Logistic() extends Classification
}