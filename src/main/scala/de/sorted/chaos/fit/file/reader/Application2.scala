package de.sorted.chaos.fit.file.reader

import de.sorted.chaos.fit.file.reader.file.ActivityFileReader
import de.sorted.chaos.fit.file.reader.message2.fileheader.FileHeader
import de.sorted.chaos.fit.file.reader.message2.{ MessageResult, RecordHeader }

object Application2 {
  import de.sorted.chaos.fit.file.reader.message2.definition.DefinitionMessage.DefinitionMessageExtractor
  import de.sorted.chaos.fit.file.reader.message2.RecordHeader.RecordHeaderExtractor
  import de.sorted.chaos.fit.file.reader.message2.fileheader.FileHeader.FileHeaderExtractor

  def main(args: Array[String]): Unit = {
    val content: Array[Byte]                      = ActivityFileReader.read("test.fit")
    val header: MessageResult[FileHeader]         = content.extractFileHeader
    val recordHeader: MessageResult[RecordHeader] = header.extractRecordHeader
    val definitionMessage = recordHeader.extractDefinitionMessage

    println(header.message.formatted)
    println(definitionMessage.message.formatted)
    //println(recordHeader.message.formatted)
  }
}
