package de.sorted.chaos.fit.file.reader.message.definition

import java.nio.ByteOrder

import de.sorted.chaos.fit.file.reader.file.ByteReader
import de.sorted.chaos.fit.file.reader.message.header.RecordHeader
import de.sorted.chaos.fit.file.reader.message.{DataExtractorResult, Message, MessageNumber, PrettyPrintable}

import scala.annotation.tailrec

final case class DefinitionMessage(
    endianness: ByteOrder,
    globalMessageType: MessageNumber.Value,
    numberOfFields: Byte,
    fieldDefinitions: List[FieldDefinition],
    numberOfDeveloperFields: Byte,
    developerFieldDefinitions: List[DeveloperFieldDefinition]
) extends PrettyPrintable {

  override def prettyPrint: String =
    s"""
       |   + Definition Message
       |      - endianness ................................ $endianness
       |      - global message type ....................... $globalMessageType
       |      - number of fields .......................... $numberOfFields ${fieldDefinitions.map(_.prettyPrint).mkString}
       |      - number of developer fields ................ not implemented yet
       |      - developer field definitions ............... not implemented yet""".stripMargin
}

object DefinitionMessage {

  /**
    * Byte 0 is reserved
    * @param content
    * @return
    */
  def from(content: Array[Byte]): DataExtractorResult[Message[DefinitionMessage]] = {
    val recordHeader = RecordHeader.from(content.head)
    val contentWithoutRecordHeader = content.drop(1)

    val architecture   = getArchitecture(contentWithoutRecordHeader(1))
    val numberOfFields = contentWithoutRecordHeader(4)

    val definitionMessage = DefinitionMessage(
      endianness                = architecture,
      globalMessageType         = getGlobalMessageType(contentWithoutRecordHeader.slice(2, 4), architecture),
      numberOfFields            = numberOfFields,
      fieldDefinitions          = getFieldDefinitions(contentWithoutRecordHeader.drop(5), numberOfFields),
      numberOfDeveloperFields   = 0, // TODO implement developer fields
      developerFieldDefinitions = List.empty
    )
    val size = 5 + (numberOfFields * 3)
    val restContent = contentWithoutRecordHeader.drop(size)

    val message = Message(recordHeader, definitionMessage)
    DataExtractorResult(restContent, message)
  }

  private def getFieldDefinitions(content: Array[Byte], numberOfFields: Byte) = {
    @tailrec
    def helper(currentContent: Array[Byte], currentField: Int, accumulator: List[FieldDefinition]): List[FieldDefinition] =
      if (currentField == numberOfFields) {
        accumulator
      } else {
        val splitContent    = currentContent.splitAt(3)
        val fieldDefinition = FieldDefinition.from(splitContent._1)

        helper(
          splitContent._2,
          currentField + 1,
          accumulator :+ fieldDefinition
        )
      }

    helper(content, 0, List.empty)
  }

  private def getGlobalMessageType(content: Array[Byte], byteOrder: ByteOrder) = {
    val number = ByteReader.getUnsignedShort(content, byteOrder)
    MessageNumber.MessageByNumber(number)
  }

  private def getArchitecture(architecture: Byte) =
    if (architecture == 1) {
      ByteOrder.BIG_ENDIAN
    } else {
      ByteOrder.LITTLE_ENDIAN
    }
}
