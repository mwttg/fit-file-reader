package de.sorted.chaos.fit.file.reader.file

import java.nio.{ByteBuffer, ByteOrder}

object ByteReader {

  def getLong(input: Array[Byte], endian: ByteOrder): Long = ByteBuffer.wrap(input).order(endian).getLong

  def getInt(input: Array[Byte], endian: ByteOrder): Int = ByteBuffer.wrap(input).order(endian).getInt

  def getUnsignedInt(input: Array[Byte], endian: ByteOrder): Long = {
    val result = ByteBuffer.wrap(input).order(endian).getInt
    result & 0x00000000FFFFFFFFL
  }

  def getShort(input: Array[Byte], endian: ByteOrder): Short = ByteBuffer.wrap(input).order(endian).getShort

  def getUnsignedShort(input: Array[Byte], endian: ByteOrder): Int = {
    val result = ByteBuffer.wrap(input).order(endian).getShort
    result & 0x0000FFFF
  }

  def getByte(input: Array[Byte], endian: ByteOrder): Byte = ByteBuffer.wrap(input).order(endian).get

  def getUnsignedByte(input: Array[Byte], endian: ByteOrder): Int = {
    val result = ByteBuffer.wrap(input).order(endian).get
    result & 0x0F
  }

  def getString(input: Array[Byte], endian: ByteOrder): String =
    endian match {
      case ByteOrder.LITTLE_ENDIAN => input.map(_.toChar).mkString
      case ByteOrder.BIG_ENDIAN => input.reverse.map(_.toChar).mkString
    }
}
