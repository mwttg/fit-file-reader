package de.sorted.chaos.fit.file.reader.message.definition

/**
  * The Field Definition bytes are used to specify which FIT fields of the global FIT message are to be included
  * in the upcoming data message in this instance. Any subsequent data message of a particular local message type
  * are considered to be using the format described by the definition message of matching local message type. All
  * fit messages and their respective FIT fields are listed in the global FIT profile. Each Field Definition consists
  * of 3 bytes. This case class is a representation of these 3 bytes.
  *
  * @param fieldDefinitionNumber Defined in the Global FIT Profile for the specific FIT Message
  * @param sizeInByte Size (in bytes) of the specific FIT message's field
  * @param baseType [[de.sorted.chaos.fit.file.reader.message.definition.BaseType]] of the specified FIT message's field
  */
final case class FieldDefinition(fieldDefinitionNumber: Int, sizeInByte: Byte, baseType: BaseType) {

  def prettyPrint: String =
    s"""
       |   + Field Definition
       |      - field definition number ................... $fieldDefinitionNumber
       |      - size ...................................... $sizeInByte Byte(s)
       |${baseType.prettyPrint}""".stripMargin
}

object FieldDefinition {

  /**
    * Create the Field Definition from 3 Bytes of the Record Content
    *
    * @param content The 3 Bytes of the Field Definition
    */
  def from(content: Array[Byte]): FieldDefinition = {
    val unsigned = content(2) & 0xFF

    FieldDefinition(
      fieldDefinitionNumber = content.head & 0xFF,
      sizeInByte            = content(1),
      baseType              = BaseType.BaseTypeByField(unsigned)
    )
  }
}
