package de.sorted.chaos.fit.file.reader.message.definition

/**
  * Base Type describes the FIT field as a specific type of FIT variable (unsigned char, signed short, etc). This allows
  * the FIT decoder to appropriately handle invalid or unknown data of this type. The format of the base type bit field
  * is shown below in the table.
  *
  * | Bit   | Name             | Description                                                         |
  * | :---: | :---             | :---                                                                |
  * |  7    | Endian Ability   | 0 - for single byte data                                            |
  * |       |                  | 1 - if base type has endianness (i.e. base type is 2 or more bytes) |
  * | 5-6   | Reserved         | Reserved                                                            |
  * | 0-4   | Base Type Number | Number assigned to Base Type (provided by SDK)                      |
  *
  * The base type bit field is used to define the BaseType. The BaseType itself is like a constant, which describes
  * the data type of the a field.
  *
  * @param baseTypeNumber The number encoded in Bit 0-4 in the base type bit field
  * @param endianAbility The ability of endian (when size is greater than 2 Byte)
  * @param baseTypeField the base type number in hex
  * @param typeName the name of the type
  * @param invalidValue when the decoder encounters unknown or invalid data, it will assign an invalid value according
  *                     to the designated base type.
  * @param sizeInByte the size in byte
  */
final case class BaseType(
    baseTypeNumber: Byte,
    endianAbility: Boolean,
    baseTypeField: Short,
    typeName: String,
    invalidValue: Long,
    sizeInByte: Byte
) {

  def prettyPrint: String =
    s"""      + Base Type
       |         - base type number ....................... $baseTypeNumber
       |         - endian ability ......................... $endianAbility
       |         - base type field ........................ $baseTypeField
       |         - type name .............................. $typeName
       |         - invalid value .......................... $invalidValue
       |         - size ................................... $sizeInByte Byte(s)""".stripMargin
}

object BaseType {

  val BaseTypeByField: Map[Int, BaseType] = initialize()

  //noinspection ScalaStyle
  private def initialize(): Map[Int, BaseType] =
    Map(
      0x00 -> BaseType(
        baseTypeNumber = 0,
        endianAbility  = false,
        baseTypeField  = 0x00,
        typeName       = "enum",
        invalidValue   = 0xFF,
        sizeInByte     = 1
      ),
      0x01 -> BaseType( // 2's complement format
        baseTypeNumber = 1,
        endianAbility  = false,
        baseTypeField  = 0x01,
        typeName       = "sint8",
        invalidValue   = 0x7F,
        sizeInByte     = 1
      ),
      0x02 -> BaseType(
        baseTypeNumber = 2,
        endianAbility  = false,
        baseTypeField  = 0x02,
        typeName       = "uint8",
        invalidValue   = 0xFF,
        sizeInByte     = 1
      ),
      0x83 -> BaseType( // 2's complement format
        baseTypeNumber = 3,
        endianAbility  = true,
        baseTypeField  = 0x83,
        typeName       = "sint16",
        invalidValue   = 0x7FFF,
        sizeInByte     = 2
      ),
      0x84 -> BaseType(
        baseTypeNumber = 4,
        endianAbility  = true,
        baseTypeField  = 0x84,
        typeName       = "uint16",
        invalidValue   = 0xFFFF,
        sizeInByte     = 2
      ),
      0x85 -> BaseType( // 2's complement format
        baseTypeNumber = 5,
        endianAbility  = true,
        baseTypeField  = 0x85,
        typeName       = "sint32",
        invalidValue   = 0x7FFFFFFF,
        sizeInByte     = 4
      ),
      0x86 -> BaseType(
        baseTypeNumber = 6,
        endianAbility  = true,
        baseTypeField  = 0x86,
        typeName       = "uint32",
        invalidValue   = 0xFFFFFFFF,
        sizeInByte     = 4
      ),
      0x07 -> BaseType( // Null terminated string encoded in UTF-8 format
        baseTypeNumber = 7,
        endianAbility  = false,
        baseTypeField  = 0x07,
        typeName       = "string",
        invalidValue   = 0x00,
        sizeInByte     = 1
      ),
      0x88 -> BaseType( // Null terminated string encoded in UTF-8 format
        baseTypeNumber = 8,
        endianAbility  = true,
        baseTypeField  = 0x88,
        typeName       = "float32",
        invalidValue   = 0xFFFFFFFF,
        sizeInByte     = 4
      ),
      0x89 -> BaseType(
        baseTypeNumber = 9,
        endianAbility  = true,
        baseTypeField  = 0x89,
        typeName       = "float64",
        invalidValue   = 0xFFFFFFFFFFFFFFFFL,
        sizeInByte     = 8
      ),
      0x0A -> BaseType(
        baseTypeNumber = 10,
        endianAbility  = false,
        baseTypeField  = 0x0A,
        typeName       = "uint8z",
        invalidValue   = 0x00,
        sizeInByte     = 1
      ),
      0x8B -> BaseType(
        baseTypeNumber = 11,
        endianAbility  = true,
        baseTypeField  = 0x8B,
        typeName       = "uint16z",
        invalidValue   = 0x0000,
        sizeInByte     = 2
      ),
      0x8C -> BaseType(
        baseTypeNumber = 12,
        endianAbility  = true,
        baseTypeField  = 0x8C,
        typeName       = "uint32z",
        invalidValue   = 0x00000000,
        sizeInByte     = 4
      ),
      0x8D -> BaseType( // Array of bytes. Field is invalid if all bytes are invalid
        baseTypeNumber = 13,
        endianAbility  = false,
        baseTypeField  = 0x8D,
        typeName       = "byte",
        invalidValue   = 0xFF,
        sizeInByte     = 1
      )
    )
}
