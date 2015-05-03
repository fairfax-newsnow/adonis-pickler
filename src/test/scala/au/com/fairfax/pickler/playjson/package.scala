package au.com.fairfax.pickler

package object playjson {
  implicit val builder: PlayJsonJBuilder.type = PlayJsonJBuilder
  implicit val reader: PlayJsonJReader.type = PlayJsonJReader
}

