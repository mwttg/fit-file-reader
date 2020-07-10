package de.sorted.chaos.fit.file.reader.message.header

import java.nio.ByteOrder

import de.sorted.chaos.fit.file.reader.file.ByteReader
import de.sorted.chaos.fit.file.reader.message.{ DataExtractorResult, PrettyPrintable }

/**
  * @param size in Byte (minimum is 12 Bytes - 12 Bytes is legacy, 14 Bytes is preferred)
  * @param protocolVersion
  * @param profileVersion
  * @param dataSize length of the Data Records section in bytes
  * @param fileType
  * @param checksum
  */
final case class Header(size: Int, protocolVersion: Short, profileVersion: Int, dataSize: Int, fileType: String, checksum: Int)
    extends PrettyPrintable {

  override def prettyPrint: String =
    s"""
       |+ Header
       |   - size ......................................... $size Byte(s)
       |   - protocol version ............................. $protocolVersion
       |   - profile version .............................. $profileVersion
       |   - data size (Data Records) ..................... $dataSize Byte(s)
       |   - file type .................................... $fileType
       |   - checksum ..................................... $checksum""".stripMargin
}

object Header {

  def from(content: Array[Byte]): DataExtractorResult[Header] = {
    val size       = ByteReader.getInt(content.slice(4, 8), ByteOrder.LITTLE_ENDIAN)
    val headerSize = content.head
    val message = Header(
      size            = headerSize,
      protocolVersion = content(1),
      profileVersion  = ByteReader.getShort(content.slice(2, 4), ByteOrder.LITTLE_ENDIAN),
      dataSize        = size,
      fileType        = ByteReader.getString(content.slice(8, 12), ByteOrder.LITTLE_ENDIAN),
      checksum        = if (content.head == 14) ByteReader.getShort(content.slice(12, 15), ByteOrder.LITTLE_ENDIAN) else 0
    )
    val restContent = content.drop(headerSize)

    DataExtractorResult(restContent, message)
  }
}
