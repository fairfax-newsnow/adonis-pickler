package au.com.fairfax.adonis.json

import au.com.fairfax.adonis.apws.macros.SampleMaterializers1
import au.com.fairfax.adonis.utils.json.JsonBuilder

trait SamplePickler[A] {
  type Picklee = A

  def pickle[P](obj: Picklee)(implicit registry: SamplePicklerRegistry,
                              builder: JsonBuilder[P]): P
}

object SamplePickler extends SampleMaterializers1 {
  implicit object BooleanSamplePickler extends SamplePickler[Boolean] {
    def pickle[P](x: Boolean)(implicit registry: SamplePicklerRegistry,
                              builder: JsonBuilder[P]): P = builder.makeBoolean(x)
  }

  implicit object CharSamplePickler extends SamplePickler[Char] {
    def pickle[P](x: Char)(implicit registry: SamplePicklerRegistry,
                           builder: JsonBuilder[P]): P = builder.makeString(x.toString)
  }

  implicit object ByteSamplePickler extends SamplePickler[Byte] {
    def pickle[P](x: Byte)(implicit registry: SamplePicklerRegistry,
                           builder: JsonBuilder[P]): P = builder.makeNumber(x)
  }

  implicit object ShortSamplePickler extends SamplePickler[Short] {
    def pickle[P](x: Short)(implicit registry: SamplePicklerRegistry,
                            builder: JsonBuilder[P]): P = builder.makeNumber(x)
  }

  implicit object IntSamplePickler extends SamplePickler[Int] {
    def pickle[P](x: Int)(implicit registry: SamplePicklerRegistry,
                          builder: JsonBuilder[P]): P = builder.makeNumber(x)
  }

  implicit object LongSamplePickler extends SamplePickler[Long] {
    def pickle[P](x: Long)(implicit registry: SamplePicklerRegistry,
                           builder: JsonBuilder[P]): P = {
      builder.makeObject(
        ("l", builder.makeNumber(x.toInt & 0x3fffff)),
        ("m", builder.makeNumber((x >> 22).toInt & 0x3fffff)),
        ("h", builder.makeNumber((x >> 44).toInt)))
    }
  }

  implicit object FloatSamplePickler extends SamplePickler[Float] {
    def pickle[P](x: Float)(implicit registry: SamplePicklerRegistry,
                            builder: JsonBuilder[P]): P = builder.makeNumber(x)
  }

  implicit object DoubleSamplePickler extends SamplePickler[Double] {
    def pickle[P](x: Double)(implicit registry: SamplePicklerRegistry,
                             builder: JsonBuilder[P]): P = builder.makeNumber(x)
  }

  implicit object StringSamplePickler extends SamplePickler[String] {
    def pickle[P](x: String)(implicit registry: SamplePicklerRegistry,
                             builder: JsonBuilder[P]): P = builder.makeString(x)
  }
}
