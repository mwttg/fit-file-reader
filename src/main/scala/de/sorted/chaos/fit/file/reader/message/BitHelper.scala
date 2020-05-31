package de.sorted.chaos.fit.file.reader.message

object BitHelper {

  implicit class Improvements(val number: Byte) {

    def bitAtPosition(position: Byte): Boolean = {
      val result = (number >> position) & 1
      if (result == 1) {
        true
      } else {
        false
      }
    }
  }
}
