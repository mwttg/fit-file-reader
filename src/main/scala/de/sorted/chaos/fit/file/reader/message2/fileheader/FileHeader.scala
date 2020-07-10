package de.sorted.chaos.fit.file.reader.message2.fileheader

import java.nio.ByteOrder

import de.sorted.chaos.fit.file.reader.message2.datatypes.BaseTypes
import de.sorted.chaos.fit.file.reader.message2.{ Formatted, Message, MessageResult }

final case class FileHeader(
    size: Int,
    protocolVersion: Short,
    profileVersion: Int,
    dataSize: Int,
    fileType: String,
    checksum: Int
) extends Message
    with Formatted {

  override def formatted: String =
    s"""+ Header
       |   - size ............................................... $size Byte(s)
       |   - protocol version ................................... $protocolVersion
       |   - profile version .................................... $profileVersion
       |   - data size (Data Records) ........................... $dataSize Byte(s)
       |   - file type .......................................... $fileType
       |   - checksum ........................................... $checksum""".stripMargin
}

object FileHeader {
  import de.sorted.chaos.fit.file.reader.message2.datatypes.Value.ValueImprovements
  import de.sorted.chaos.fit.file.reader.message2.datatypes.ValueExtractor.ArrayImprovements

  implicit class FileHeaderExtractor(content: Array[Byte]) {

    def extractFileHeader: MessageResult[FileHeader] = {
      val size = content
        .slice(4, 8)
        .extract(ByteOrder.LITTLE_ENDIAN, BaseTypes.SInt32)
        .getInt
      val headerSize = content
        .slice(0, 1)
        .extract(ByteOrder.LITTLE_ENDIAN, BaseTypes.Byte)
        .getByte

      val message =
        FileHeader(
          size = headerSize,
          protocolVersion = content
            .slice(1, 2)
            .extract(ByteOrder.LITTLE_ENDIAN, BaseTypes.Byte)
            .getByte,
          profileVersion = content
            .slice(2, 4)
            .extract(ByteOrder.LITTLE_ENDIAN, BaseTypes.SInt16)
            .getShort,
          dataSize = size,
          fileType = content
            .slice(8, 12)
            .extract(ByteOrder.LITTLE_ENDIAN, BaseTypes.String)
            .getString,
          checksum = if (headerSize == 14) {
            content
              .slice(12, 14)
              .extract(ByteOrder.LITTLE_ENDIAN, BaseTypes.SInt16)
              .getShort
          } else {
            0
          }
        )
      val restContent = content.drop(headerSize)

      MessageResult(restContent, message)
    }
  }
}
