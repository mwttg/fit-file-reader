package de.sorted.chaos.fit.file.reader.message2.definition

import java.nio.ByteOrder

import de.sorted.chaos.fit.file.reader.message2.datatypes.BaseTypes
import de.sorted.chaos.fit.file.reader.message2.{ Formatted, Message, MessageResult, RecordHeader }

import scala.annotation.tailrec

final case class DefinitionMessage(header: RecordHeader, content: RecordContent) extends Message with Formatted {

  override def formatted: String =
    s"""+ Definition Message
       |${header.formatted}
       |${content.formatted}""".stripMargin
}

final case class RecordContent(
    endianness: ByteOrder,
    globalMessageType: String,
    numberOfFields: Byte,
    fieldDefinitions: List[FieldDefinition],
    numberOfDeveloperFields: Byte,
    developerFieldDefinitions: List[DeveloperFieldDefinition]
) extends Message
    with Formatted {

  override def formatted: String =
    s"""   + Record Content
       |      - endianness ...................................... $endianness
       |      - global message type ............................. $globalMessageType
       |      - number of fields ................................ $numberOfFields ${fieldDefinitions.map(_.formatted).mkString}
       |      - number of developer fields ...................... not implemented yet
       |      - developer field definitions ..................... not implemented yet""".stripMargin

}

object DefinitionMessage {
  import de.sorted.chaos.fit.file.reader.message2.datatypes.Value.ValueImprovements
  import de.sorted.chaos.fit.file.reader.message2.datatypes.ValueExtractor.ArrayImprovements
  import de.sorted.chaos.fit.file.reader.message2.definition.FieldDefinition.FieldDefinitionExtractor
  import de.sorted.chaos.fit.file.reader.utility.GlobalMessageNumber.GlobalMessageNumberHelper

  implicit class DefinitionMessageExtractor(val messageResult: MessageResult[RecordHeader]) {

    def extractDefinitionMessage: MessageResult[DefinitionMessage] = {
      val recordHeader   = messageResult.message
      val content        = messageResult.restContent
      val architecture   = getArchitecture(content(1))
      val numberOfFields = content(4)

      val recordContent = RecordContent(
        endianness                = architecture,
        globalMessageType         = getGlobalMessageType(content, architecture),
        numberOfFields            = numberOfFields,
        fieldDefinitions          = getFieldDefinitions(content.drop(5), numberOfFields),
        numberOfDeveloperFields   = 0, // TODO
        developerFieldDefinitions = List.empty
      )

      val message     = DefinitionMessage(recordHeader, recordContent)
      val size        = 5 + (numberOfFields * 3)
      val restContent = content.drop(size)
      MessageResult(restContent, message)
    }

    private def getFieldDefinitions(content: Array[Byte], numberOfFields: Byte) = {
      @tailrec
      def helper(currentContent: Array[Byte], currentField: Int, accumulator: List[FieldDefinition]): List[FieldDefinition] =
        if (currentField == numberOfFields) {
          accumulator
        } else {
          val splitContent    = currentContent.splitAt(3)
          val fieldDefinition = splitContent._1.extractFieldDefinition

          helper(
            splitContent._2,
            currentField + 1,
            accumulator :+ fieldDefinition
          )
        }

      helper(content, 0, List.empty)
    }

    private def getGlobalMessageType(content: Array[Byte], architecture: ByteOrder) =
      content
        .slice(2, 4)
        .extract(architecture, BaseTypes.UInt16)
        .getUnsignedShort
        .getGlobalMessageType

    private def getArchitecture(architecture: Byte) =
      if (architecture == 1) {
        ByteOrder.BIG_ENDIAN
      } else {
        ByteOrder.LITTLE_ENDIAN
      }
  }
}
