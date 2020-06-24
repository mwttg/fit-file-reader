package de.sorted.chaos.fit.file.reader.utility

import de.sorted.chaos.fit.file.reader.message2.{DataMessage, DefinitionMessage, MessageType}

object ByteUtility {

  private val LocalMessageTypeById = TsvReader.readAsMap("message-files/local-message-types.tsv")

  implicit class BitHelper(val number: Byte) {

    def bitAtPosition(position: Byte): Boolean = {
      val result = (number >> position) & 1
      if (result == 1) {
        true
      } else {
        false
      }
    }
  }

  implicit class RecordHeaderHelper(val headerByte: Byte) {

    def getMessageType: MessageType =
      if (headerByte.bitAtPosition(6)) {
        DefinitionMessage
      } else {
        DataMessage
      }

    def getLocalMessageType: String = {
      val id = headerByte & 0xF // number of the lowest 4 Bits
      LocalMessageTypeById(id)
    }

  }
}
