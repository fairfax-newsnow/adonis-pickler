package au.com.fairfax.adonis.apws.macros.json

import org.scalajs.spickling._
import au.com.fairfax.adonis.apws.macros.JsonMaterializers

trait JsonParser[T] {
  def parse[J](json: J)(implicit reader: PReader[J]): T
}

object JsonParser extends JsonMaterializers