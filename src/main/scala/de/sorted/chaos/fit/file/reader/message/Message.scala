package de.sorted.chaos.fit.file.reader.message

import de.sorted.chaos.fit.file.reader.message.header.RecordHeader

final case class Message[T <: PrettyPrintable](recordHeader: RecordHeader, message: T) extends PrettyPrintable {

  override def prettyPrint: String =
    s"""
       |+ Message (${message.getClass.getSimpleName})${recordHeader.prettyPrint}${message.prettyPrint}""".stripMargin
}
