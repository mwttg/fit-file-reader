package de.sorted.chaos.fit.file.reader.message.data

import de.sorted.chaos.fit.file.reader.file.ByteReader
import de.sorted.chaos.fit.file.reader.message.definition.DefinitionMessage
import de.sorted.chaos.fit.file.reader.message.header.RecordHeader
import de.sorted.chaos.fit.file.reader.message.{ DataExtractorResult, Message }

final case class FileCreatorMessage(softwareVersion: Int, hardwareVersion: Int) extends DataMessage {

  override def prettyPrint: String =
    s"""
       |   + File Creator Message
       |      - software version .......................... $softwareVersion
       |      - hardware version .......................... $hardwareVersion""".stripMargin
}

object FileCreatorMessage {

  def from(inputMessage: DataExtractorResult[Message[DefinitionMessage]]): DataExtractorResult[Message[FileCreatorMessage]] = {
    val recordHeader               = RecordHeader.from(inputMessage.restContent.head)
    val contentWithoutRecordHeader = inputMessage.restContent.drop(1)
    val definitionMessage          = inputMessage.messageWithHeader.message

    val positions = DataExtractor.getStartPositions(definitionMessage)

    val fileCreatorMessage = FileCreatorMessage(
      softwareVersion = getSoftwareVersion(
        contentWithoutRecordHeader,
        positions,
        definitionMessage
      ),
      hardwareVersion = getHardwareVersion(
        contentWithoutRecordHeader,
        positions,
        definitionMessage
      )
    )
    val restContent = contentWithoutRecordHeader.drop(positions.last)
    val message     = Message(recordHeader, fileCreatorMessage)
    DataExtractorResult(restContent, message)
  }

  private def getSoftwareVersion(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 0)
    ByteReader.getUnsignedShort(input, definitionMessage.endianness)
  }

  private def getHardwareVersion(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 1)
    ByteReader.getUnsignedByte(input, definitionMessage.endianness)
  }
}
