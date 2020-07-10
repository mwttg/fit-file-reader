package de.sorted.chaos.fit.file.reader.message.data

import java.time.{LocalDateTime, ZoneId, ZonedDateTime}

import de.sorted.chaos.fit.file.reader.file.{ByteReader, TextFileReader}
import de.sorted.chaos.fit.file.reader.message.definition.DefinitionMessage
import de.sorted.chaos.fit.file.reader.message.header.RecordHeader
import de.sorted.chaos.fit.file.reader.message.{DataExtractorResult, Message}

final case class FileIdMessage(
    fileType: String,
    manufacturer: String,
    product: String,
    serialNumber: Long,
    timeCreated: LocalDateTime
) extends DataMessage {

  override def prettyPrint: String =
    s"""
       |   + File ID Message
       |      - file type ................................. $fileType
       |      - manufacturer .............................. $manufacturer
       |      - product ................................... $product
       |      - serial number ............................. $serialNumber
       |      - time created .............................. $timeCreated""".stripMargin
}

object FileIdMessage {

  private val ManufacturerById: Map[Int, String] = TextFileReader.readAsMap("files/manufacturers.tsv")
  private val ProductById: Map[Int, String]      = TextFileReader.readAsMap("files/products.tsv")
  private val FileTypeById: Map[Int, String]     = TextFileReader.readAsMap("files/file-types.tsv")
  val StartDate: ZonedDateTime = LocalDateTime.of(1989, 12, 31, 0, 0, 0).atZone(ZoneId.of("UTC"))

  def from(inputMessage: DataExtractorResult[Message[DefinitionMessage]]): DataExtractorResult[Message[FileIdMessage]] = {
    val recordHeader               = RecordHeader.from(inputMessage.restContent.head)
    val contentWithoutRecordHeader = inputMessage.restContent.drop(1)
    val definitionMessage          = inputMessage.messageWithHeader.message

    val positions = DataExtractor.getStartPositions(definitionMessage)

    val fileIdMessage = FileIdMessage(
      fileType = getFileType(
        contentWithoutRecordHeader,
        positions,
        definitionMessage
      ),
      manufacturer = getManufacturer(
        contentWithoutRecordHeader,
        positions,
        definitionMessage
      ),
      product = getProduct(
        contentWithoutRecordHeader,
        positions,
        definitionMessage
      ),
      serialNumber = getSerialNumber(
        contentWithoutRecordHeader,
        positions,
        definitionMessage
      ),
      timeCreated = getTimeCreated(
        contentWithoutRecordHeader,
        positions,
        definitionMessage
      )
    )

    val restContent = contentWithoutRecordHeader.drop(positions.last)
    val message     = Message(recordHeader, fileIdMessage)
    DataExtractorResult(restContent, message)
  }

  private def getFileType(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 0)
    val id    = ByteReader.getByte(input, definitionMessage.endianness)
    FileTypeById(id)
  }

  private def getManufacturer(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 1)
    val id    = ByteReader.getUnsignedShort(input, definitionMessage.endianness)
    ManufacturerById(id)
  }

  private def getProduct(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 2)
    val id    = ByteReader.getUnsignedShort(input, definitionMessage.endianness)
    ProductById(id)
  }

  private def getSerialNumber(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 3)
    ByteReader.getUnsignedInt(input, definitionMessage.endianness)
  }

  private def getTimeCreated(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input   = DataExtractor.getByteData(content, positions, definitionMessage, 4)
    val seconds = ByteReader.getUnsignedInt(input, definitionMessage.endianness)
    StartDate
      .plusSeconds(seconds)
      .withZoneSameInstant(ZoneId.systemDefault())
      .toLocalDateTime // TODO Profile.xlsx system time vs time
  }
}
