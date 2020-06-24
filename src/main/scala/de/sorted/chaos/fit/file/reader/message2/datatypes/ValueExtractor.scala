package de.sorted.chaos.fit.file.reader.message2.datatypes

import java.nio.{ByteBuffer, ByteOrder}

import de.sorted.chaos.fit.file.reader.message2.datatypes.BaseTypes.BaseType

object ValueExtractor {

  implicit class ArrayImprovements(val content: Array[Byte]) {
    //noinspection ScalaStyle
    def extract(endian: ByteOrder, baseType: BaseType): Value =
      baseType match {
        case BaseTypes.Enum =>
          UnsignedByteValue(
            value = ByteBuffer.wrap(content).order(endian).get & 0x0F
          )
        case BaseTypes.SInt8 =>
          ByteValue(
            value = ByteBuffer.wrap(content).order(endian).get
          )
        case BaseTypes.UInt8 =>
          UnsignedByteValue(
            value = ByteBuffer.wrap(content).order(endian).get & 0x0F
          )
        case BaseTypes.SInt16 =>
          ShortValue(
            value = ByteBuffer.wrap(content).order(endian).getShort
          )
        case BaseTypes.UInt16 =>
          UnsignedShortValue(
            value = ByteBuffer.wrap(content).order(endian).getShort & 0x00FF
          )
        case BaseTypes.SInt32 =>
          IntValue(
            value = ByteBuffer.wrap(content).order(endian).getInt
          )
        case BaseTypes.UInt32 =>
          UnsignedIntValue(
            value = ByteBuffer.wrap(content).order(endian).getInt & 0x00000000FFFFFFFFL
          )
        case BaseTypes.String =>
          StringValue(
            value = content.map(_.toChar).mkString
          )
        case BaseTypes.Float32 =>
          FloatValue(
            value = ByteBuffer.wrap(content).order(endian).getFloat
          )
        case BaseTypes.Float64 =>
          DoubleValue(
            value = ByteBuffer.wrap(content).order(endian).getDouble
          )
        case BaseTypes.UInt8z =>
          UnsignedByteValue(
            value = ByteBuffer.wrap(content).order(endian).get & 0x0F
          )
        case BaseTypes.UInt16z =>
          UnsignedShortValue(
            value = ByteBuffer.wrap(content).order(endian).getShort & 0x00FF
          )
        case BaseTypes.UInt32z =>
          UnsignedIntValue(
            value = ByteBuffer.wrap(content).order(endian).getInt & 0x00000000FFFFFFFFL
          )
        case BaseTypes.Byte =>
          ByteValue(
            value = ByteBuffer.wrap(content).order(endian).get
          )
      }
  }
}
