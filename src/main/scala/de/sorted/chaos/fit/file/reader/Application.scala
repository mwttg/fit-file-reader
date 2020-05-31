package de.sorted.chaos.fit.file.reader

import de.sorted.chaos.fit.file.reader.file.ActivityFileReader
import de.sorted.chaos.fit.file.reader.message.data.{EventMessage, FileCreatorMessage, FileIdMessage}
import de.sorted.chaos.fit.file.reader.message.definition.DefinitionMessage
import de.sorted.chaos.fit.file.reader.message.header.{Header, RecordHeader}

object Application {

  def main(args: Array[String]): Unit = {
    val content            = ActivityFileReader.read("test.fit")
    val header             = Header.from(content)

    val definitionMessage1  = DefinitionMessage.from(header.restContent)
    val fileIdMessage      = FileIdMessage.from(definitionMessage1)

    val definitionMessage2 = DefinitionMessage.from(fileIdMessage.restContent)
    val fileCreatorMessage = FileCreatorMessage.from(definitionMessage2)

    val definitionMessage3 = DefinitionMessage.from(fileCreatorMessage.restContent)
    val eventMessage = EventMessage.from(definitionMessage3)

    val definitionMessage4 = DefinitionMessage.from(eventMessage.restContent)

    print(header.messageWithHeader.prettyPrint)
    print(definitionMessage1.messageWithHeader.prettyPrint)
    print(fileIdMessage.messageWithHeader.prettyPrint)
    print(definitionMessage2.messageWithHeader.prettyPrint)
    print(fileCreatorMessage.messageWithHeader.prettyPrint)
    print(definitionMessage3.messageWithHeader.prettyPrint)
    print(eventMessage.messageWithHeader.prettyPrint)


    print(definitionMessage4.messageWithHeader.prettyPrint)
  }
}
