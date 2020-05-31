package de.sorted.chaos.fit.file.reader.message.data

import java.time.{LocalDateTime, ZoneId}

import de.sorted.chaos.fit.file.reader.file.{ByteReader, TextFileReader}
import de.sorted.chaos.fit.file.reader.message.data.FileIdMessage.{ProductById, StartDate}
import de.sorted.chaos.fit.file.reader.message.{DataExtractorResult, Message, PrettyPrintable}
import de.sorted.chaos.fit.file.reader.message.definition.DefinitionMessage
import de.sorted.chaos.fit.file.reader.message.header.RecordHeader

final case class EventMessage(
    timestamp: LocalDateTime,
    event: String,
    eventType: String,
    eventGroup: Int,
    data: Long
) extends PrettyPrintable {

  override def prettyPrint: String =
    s"""
       |   + Event Message
       |      - timestamp ................................. $timestamp
       |      - event ..................................... $event
       |      - event type ................................ $eventType
       |      - event group ............................... $eventGroup
       |      - data ...................................... $data""".stripMargin
}

object EventMessage {

  private val EventById: Map[Int, String]      = TextFileReader.readAsMap("files/events.tsv")
  private val EventTypeById: Map[Int, String]      = TextFileReader.readAsMap("files/event-types.tsv")

  def from(inputMessage: DataExtractorResult[Message[DefinitionMessage]]): DataExtractorResult[Message[EventMessage]] = {
    val recordHeader               = RecordHeader.from(inputMessage.restContent.head)
    val contentWithoutRecordHeader = inputMessage.restContent.drop(1)
    val definitionMessage          = inputMessage.messageWithHeader.message

    val positions = DataExtractor.getStartPositions(definitionMessage)

    val eventMessage = EventMessage(
      timestamp = getTimestamp(contentWithoutRecordHeader, positions, definitionMessage),
      event = getEvent(contentWithoutRecordHeader, positions, definitionMessage),
      eventType = getEventType(contentWithoutRecordHeader, positions, definitionMessage),
      eventGroup = getEventGroup(contentWithoutRecordHeader, positions, definitionMessage),
      data = getData(contentWithoutRecordHeader, positions, definitionMessage)
    )

    val restContent = contentWithoutRecordHeader.drop(positions.last)
    val message     = Message(recordHeader, eventMessage)
    DataExtractorResult(restContent, message)
  }

  private def getTimestamp(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 253)
    val seconds = ByteReader.getUnsignedInt(input, definitionMessage.endianness)
    StartDate
      .plusSeconds(seconds)
      .withZoneSameInstant(ZoneId.systemDefault())
      .toLocalDateTime // TODO Profile.xlsx system time vs time
  }

  private def getEvent(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 0)
    val id    = ByteReader.getUnsignedByte(input, definitionMessage.endianness)
    EventById(id)
  }

  private def getEventType(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 1)
    val id    = ByteReader.getUnsignedByte(input, definitionMessage.endianness)
    EventTypeById(id)
  }

  private def getData(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 3)
    ByteReader.getUnsignedInt(input, definitionMessage.endianness)
  }

  private def getEventGroup(content: Array[Byte], positions: List[Int], definitionMessage: DefinitionMessage) = {
    val input = DataExtractor.getByteData(content, positions, definitionMessage, 4)
    ByteReader.getUnsignedByte(input, definitionMessage.endianness)
  }
}
