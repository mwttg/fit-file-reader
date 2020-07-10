package de.sorted.chaos.fit.file.reader.message2

sealed trait MessageType

case object DefinitionMessage extends MessageType
case object DataMessage extends MessageType

sealed trait RecordHeader extends Message with Formatted

final case class NormalHeader(messageType: MessageType, containsDeveloperFields: Boolean, localMessageType: String)
    extends RecordHeader {

  override def formatted: String =
    s"""   + Record Header (Normal Header)
       |      - message type .................................... $messageType
       |      - contains developer fields ....................... $containsDeveloperFields
       |      - local message type .............................. $localMessageType""".stripMargin

}

final case class CompressedTimestampHeader(localMessageType: String, timeOffset: Int) extends RecordHeader {

  override def formatted: String =
    s"""      + Record Header (Compressed Timestamp Header)
       |         - local message type .............................. $localMessageType
       |         - time offset ..................................... $timeOffset seconds""".stripMargin
}

object RecordHeader {
  import de.sorted.chaos.fit.file.reader.utility.ByteUtility.{ BitHelper, RecordHeaderHelper }

  implicit class RecordHeaderExtractor[T <: Message](val messageResult: MessageResult[T]) {

    def extractRecordHeader: MessageResult[RecordHeader] = {
      val content            = messageResult.restContent
      val headerByte         = content.head
      val restContent        = content.drop(1)
      val bitAtPositionSeven = headerByte.bitAtPosition(7)

      val message = if (bitAtPositionSeven) {
        CompressedTimestampHeader(
          localMessageType = headerByte.getLocalMessageType,
          timeOffset       = 456 // TODO
        )
      } else {
        NormalHeader(
          messageType             = headerByte.getMessageType,
          containsDeveloperFields = headerByte.bitAtPosition(5),
          localMessageType        = headerByte.getLocalMessageType
        )
      }

      MessageResult(restContent, message)
    }
  }
}
