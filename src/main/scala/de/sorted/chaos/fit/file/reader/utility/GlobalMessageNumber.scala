package de.sorted.chaos.fit.file.reader.utility

object GlobalMessageNumber {

  // TODO find out what global message types are
  private val GlobalMessageTypeById = TsvReader.readAsMap("message-files/local-message-types.tsv")

  implicit class GlobalMessageNumberHelper(val id: Int) {

    def getGlobalMessageType: String =
      GlobalMessageTypeById(id)
  }
}
