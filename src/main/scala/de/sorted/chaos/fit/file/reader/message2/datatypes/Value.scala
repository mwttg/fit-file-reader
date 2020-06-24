package de.sorted.chaos.fit.file.reader.message2.datatypes

sealed trait Value

final case class ByteValue(value: Byte) extends Value
final case class UnsignedByteValue(value: Int) extends Value
final case class ShortValue(value: Short) extends Value
final case class UnsignedShortValue(value: Int) extends Value
final case class IntValue(value: Int) extends Value
final case class UnsignedIntValue(value: Long) extends Value
final case class LongValue(value: Long) extends Value
final case class StringValue(value: String) extends Value
final case class FloatValue(value: Float) extends Value
final case class DoubleValue(value: Double) extends Value

object Value {

  implicit class ValueImprovements(val value: Value) {

    def getByte: Byte = {
      value match {
        case ByteValue(v) => v
        case _ => throw new ArithmeticException("Can't transform Value -> ByteValue -> Byte.")
      }
    }

    def getUnsignedByte: Int = {
      value match {
        case UnsignedByteValue(v) => v
        case _ => throw new ArithmeticException("Can't transform Value -> UnsignedByteValue -> Int.")
      }
    }

    def getShort: Short = {
      value match {
        case ShortValue(v) => v
        case _ => throw new ArithmeticException("Can't transform Value -> ShortValue -> Short.")
      }
    }

    def getUnsignedShort: Int = {
      value match {
        case UnsignedShortValue(v) => v
        case _ => throw new ArithmeticException("Can't transform Value -> UnsignedShortValue -> Int.")
      }
    }

    def getInt: Int = {
      value match {
        case IntValue(v) => v
        case _ => throw new ArithmeticException("Can't transform Value -> IntValue -> Int.")
      }
    }

    def getString: String = {
      value match {
        case StringValue(v) => v
        case _ => throw new ArithmeticException("Can't transform Value -> StringValue -> String.")
      }
    }
  }
}
