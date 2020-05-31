package de.sorted.chaos.fit.file.reader.message.data

import de.sorted.chaos.fit.file.reader.message.definition.DefinitionMessage

object DataExtractor {

  def getStartPositions(definitionMessage: DefinitionMessage): List[Int] =
    definitionMessage.fieldDefinitions.foldLeft(List(0)) { (currentPosition, item) =>
      {
        val sizeInByte = item.sizeInByte
        val start      = currentPosition.last + sizeInByte
        currentPosition :+ start
      }
    }

  def getByteData(
      content: Array[Byte],
      positions: List[Int],
      definitionMessage: DefinitionMessage,
      fieldDefinitionNumber: Int
  ): Array[Byte] = {
    val fieldDefinition = //todo optional error handling (e.g. does position exists)
      definitionMessage.fieldDefinitions.find(definition => definition.fieldDefinitionNumber == fieldDefinitionNumber).get
    val index      = definitionMessage.fieldDefinitions.indexOf(fieldDefinition)
    val position   = positions(index)
    val sizeInByte = fieldDefinition.sizeInByte
    content.slice(position, position + sizeInByte)
  }
}
