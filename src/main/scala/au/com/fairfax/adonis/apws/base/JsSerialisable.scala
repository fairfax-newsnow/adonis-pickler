package au.com.fairfax.adonis.apws.base

import au.com.fairfax.adonis.apws.macros.{JReader, JBuilder}

/**
 * @author Daniel Le Clere
 */
trait JsSerialisable {
  def format[J](implicit builder: JBuilder[J]): J
}

