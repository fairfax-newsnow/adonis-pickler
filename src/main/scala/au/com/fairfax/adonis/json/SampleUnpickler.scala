package au.com.fairfax.adonis.json

import org.scalajs.spickling.PReader
import au.com.fairfax.adonis.apws.macros.SampleMaterializers1

trait SampleUnpickler[A] {
  type Unpicklee = A

  def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                             reader: PReader[P]): A
}

object SampleUnpickler extends SampleMaterializers1 {

  implicit object BooleanUnpickler extends SampleUnpickler[Boolean] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Boolean = reader.readBoolean(pickle)
  }

  implicit object CharUnpickler extends SampleUnpickler[Char] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Char = reader.readString(pickle).charAt(0)
  }

  implicit object ByteUnpickler extends SampleUnpickler[Byte] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Byte = reader.readNumber(pickle).toByte
  }

  implicit object ShortUnpickler extends SampleUnpickler[Short] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Short = reader.readNumber(pickle).toShort
  }

  implicit object IntUnpickler extends SampleUnpickler[Int] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Int = reader.readNumber(pickle).toInt
  }

  implicit object LongUnpickler extends SampleUnpickler[Long] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Long = {
      // FIXME This is probably wrong wrt negative numbers
      val l = reader.readNumber(reader.readObjectField(pickle, "l"))
      val m = reader.readNumber(reader.readObjectField(pickle, "m"))
      val h = reader.readNumber(reader.readObjectField(pickle, "h"))
      (h.toLong << 44) | (m.toLong << 22) | l.toLong
    }
  }

  implicit object FloatUnpickler extends SampleUnpickler[Float] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Float = reader.readNumber(pickle).toFloat
  }

  implicit object DoubleUnpickler extends SampleUnpickler[Double] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): Double = reader.readNumber(pickle).toDouble
  }

  implicit object StringUnpickler extends SampleUnpickler[String] {
    def unpickle[P](pickle: P)(implicit registry: SamplePicklerRegistry,
                               reader: PReader[P]): String = reader.readString(pickle)
  }

}

