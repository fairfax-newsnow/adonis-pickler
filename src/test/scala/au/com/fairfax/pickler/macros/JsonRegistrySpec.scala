package au.com.fairfax.pickler.macros

import org.scalatest.{FlatSpec, Matchers}
import au.com.fairfax.pickler.playjson._
import JsonRegistry._

class JsonRegistrySpec extends FlatSpec with Matchers {
  it should "have integer formatted/parsed successfully" in {
    register[Int]
  }
}
