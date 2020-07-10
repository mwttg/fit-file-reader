package de.sorted.chaos.fit.file.reader.message2.definition

import de.sorted.chaos.fit.file.reader.message2.Formatted
import de.sorted.chaos.fit.file.reader.message2.datatypes.BaseTypes
import de.sorted.chaos.fit.file.reader.message2.datatypes.BaseTypes.BaseType

final case class FieldDefinition(fieldDefinitionNumber: Int, sizeInByte: Byte, baseType: BaseType) extends Formatted {
  override def formatted: String =
    s"""
       |         + Field Definition
       |            - field definition number ................... $fieldDefinitionNumber
       |            - size ...................................... $sizeInByte Byte(s)
       |${baseType.formatted}""".stripMargin
}

object FieldDefinition {

  implicit class FieldDefinitionExtractor(val content: Array[Byte]) {
    def extractFieldDefinition: FieldDefinition =
      FieldDefinition(
        fieldDefinitionNumber = content.head & 0xFF,
        sizeInByte = content(1),
        baseType = BaseTypes.fromBaseTypeField(content(2) & 0xFF)
      )
  }
}
