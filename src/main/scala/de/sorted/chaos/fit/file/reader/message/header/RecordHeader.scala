package de.sorted.chaos.fit.file.reader.message.header

import de.sorted.chaos.fit.file.reader.message.header.MessageType.{ DATA_MESSAGE, DEFINITION_MESSAGE }
import de.sorted.chaos.fit.file.reader.message.{ MessageNumber, PrettyPrintable }

object MessageType extends Enumeration {
  type MessageType = Value
  val DEFINITION_MESSAGE, DATA_MESSAGE = Value
}

/**
  * The normal record header is a one byte bit field. There are actually two types of record header
  * [[de.sorted.chaos.fit.file.reader.message.header.NormalRecordHeader]] and
  * [[de.sorted.chaos.fit.file.reader.message.header.CompressedTimestampHeader]].
  * The header type is indicated in the most significant bit (msb). The normal header identifies
  * whether the record is a [[de.sorted.chaos.fit.file.reader.message.definition.DefinitionMessage]]
  * or a [[de.sorted.chaos.fit.file.reader.message.data.DataMessage]] and identifies the local message
  * type. A compressed timestamp header is a special compressed header taht may also be used in some
  * local data messages to allow a compressed time format.
  *
  * The one bit field of a normal header is described in the following table
  * | Bit   | Value       | Description
  * | :---: | :---:       | :---
  * | 7     | 0           | Normal Header
  * | 6     | 0 or 1      | Message Type
  * |       |             | 1: Definition Message
  * |       |             | 0: Data Message
  * | 5     | 0 (default) | Message Type specific
  * | 4     | 0           | Reserved
  * | 0-3   | 0-15        | Local Message Type
  *
  * The one bit field of a compressed timestamp header is described in the following table
  * | Bit   | Value | Description
  * | :---: | :---: | :---
  * | 7     | 1     | Compressed Timestamp Header
  * | 5-6   | 0-3   | Local Message Type
  * | 0-4   | 0-31  | Time Offset (seconds)
  */
sealed trait RecordHeader extends PrettyPrintable

/**
  * This class is the internal representation of a normal header bit field
  *
  * @param messageType [[de.sorted.chaos.fit.file.reader.message.definition.DefinitionMessage]] or
  *                    [[de.sorted.chaos.fit.file.reader.message.data.DataMessage]]
  * @param containsExtendedDefinitions if this is true, the message contains extended definitions for developer data.
  * @param localMessageType The local message type is used to create an association between the definition message,
  *                         data message and the message in the Global FIT Profile. In a Definition Message, the local
  *                         the local message type is assigned to a Global FIT Message Number (mesg_num) relating the
  *                         local message to their respective FIT message. In a Data Message the local message type
  *                         associates a Data Message to its respective Definition Message, and hence, its' global
  *                         FIT message. A Data Message will follow the format specified in its Definition Message
  *                         of matching local message type. The local message type can be redefined within a single
  *                         FIT file.
  */
final case class NormalRecordHeader(
    messageType: MessageType.Value,
    containsExtendedDefinitions: Boolean,
    localMessageType: MessageNumber.Value
) extends RecordHeader {

  override def prettyPrint: String =
    s"""
       |   + Record Header (Normal Header)
       |      - message type .............................. $messageType
       |      - contains developer fields ................. $containsExtendedDefinitions
       |      - local message type ........................ $localMessageType""".stripMargin
}

/**
  * This class is the internal representation of a compressed timestamp header bit field
  */
final case class CompressedTimestampHeader(localMessageType: MessageNumber.Value, timeOffset: Int) extends RecordHeader {
  override def prettyPrint: String =
    s"""
       |   + Record Header (Compressed Timestamp Header)
       |      - local message type ........................ $localMessageType
       |      - time offset ............................... $timeOffset seconds""".stripMargin
}

object RecordHeader {
  import de.sorted.chaos.fit.file.reader.message.BitHelper.Improvements

  def from(recordHeader: Byte): RecordHeader =
    if (!recordHeader.bitAtPosition(7)) {
      NormalRecordHeader(
        messageType                 = getMessageType(recordHeader),
        containsExtendedDefinitions = recordHeader.bitAtPosition(5),
        localMessageType            = getLocalMessageTypeForNormalHeader(recordHeader)
      )
    } else {
      CompressedTimestampHeader(MessageNumber.ZONES_TARGET, 4)
    }

  private def getMessageType(recordHeader: Byte) =
    if (recordHeader.bitAtPosition(6)) {
      DEFINITION_MESSAGE
    } else {
      DATA_MESSAGE
    }

  private def getLocalMessageTypeForNormalHeader(recordHeader: Byte) = {
    val number = recordHeader & 0xF // number of the lowest 4 Bits
    MessageNumber.MessageByNumber(number)
  }
}
