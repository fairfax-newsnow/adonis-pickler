package au.com.fairfax.adonis.apws.base

import au.com.fairfax.adonis.apws.macros.{TypeProvider, JBuilder}

/**
 * @author Daniel Le Clere
 */
trait JsSerialisable extends TypeProvider with java.io.Serializable {
  def format[J](implicit builder: JBuilder[J]): J
}

