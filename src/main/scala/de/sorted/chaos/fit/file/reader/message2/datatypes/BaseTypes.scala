package de.sorted.chaos.fit.file.reader.message2.datatypes

import de.sorted.chaos.fit.file.reader.message2.Formatted

object BaseTypes {

  sealed abstract class BaseType(
      val baseTypeNumber: Byte,
      val endianAbility: Boolean,
      val baseTypeField: Short,
      val typeName: String,
      val invalidValue: Long,
      val sizeInByte: Byte
  ) extends Formatted {

    override def formatted: String =
      s"""            + Base Type
         |               - base type number ....................... $baseTypeNumber
         |               - endian ability ......................... $endianAbility
         |               - base type field ........................ 0x${f"$baseTypeField%02X"}
         |               - type name .............................. $typeName
         |               - invalid value .......................... $invalidValue
         |               - size ................................... $sizeInByte Byte(s)""".stripMargin
  }

  final case object Enum extends BaseType(0, false, 0x00, "enum", 0xFF, 1)
  final case object SInt8 extends BaseType(1, false, 0x01, "sint8", 0x7F, 1)
  final case object UInt8 extends BaseType(2, false, 0x02, "uint8", 0xFF, 1)
  final case object SInt16 extends BaseType(3, true, 0x83, "sint16", 0x7FFF, 2)
  final case object UInt16 extends BaseType(4, true, 0x84, "uint16", 0xFFFF, 2)
  final case object SInt32 extends BaseType(5, true, 0x85, "sint32", 0x7FFFFFFF, 4)
  final case object UInt32 extends BaseType(6, true, 0x86, "uint32", 0xFFFFFFFF, 4)
  final case object String extends BaseType(7, false, 0x07, "string", 0x00, 1)
  final case object Float32 extends BaseType(8, true, 0x88, "float32", 0xFFFFFFFF, 4)
  final case object Float64 extends BaseType(9, true, 0x89, "float64", 0xFFFFFFFFFFFFFFFFL, 8)
  final case object UInt8z extends BaseType(10, false, 0x0A, "uint8z", 0x00, 1)
  final case object UInt16z extends BaseType(11, true, 0x88, "uint16z", 0x0000, 2)
  final case object UInt32z extends BaseType(12, true, 0x8C, "uint32z", 0x00000000, 4)
  final case object Byte extends BaseType(13, false, 0x8D, "byte", 0xFF, 1)

  def fromBaseTypeField(baseTypeField: Int): BaseType = {
    AllBaseTypes.find(item => item.baseTypeField == baseTypeField) match {
      case Some(item) => item
      case None => throw new NoSuchElementException(s"There is no BaseType with baseTypeField='$baseTypeField'.")
    }
  }

  def fromBaseTypeNumber(baseTypeNumber: Byte): BaseType = {
    AllBaseTypes.find(item => item.baseTypeNumber == baseTypeNumber) match {
      case Some(item) => item
      case None => throw new NoSuchElementException(s"There is no BaseType with baseTypeNumber='$baseTypeNumber'.")
    }
  }

  private val AllBaseTypes = Set(
    Enum,
    SInt8,
    UInt8,
    SInt16,
    UInt16,
    SInt32,
    UInt32,
    String,
    Float32,
    Float64,
    UInt8z,
    UInt16z,
    UInt32z,
    Byte
  )
}
