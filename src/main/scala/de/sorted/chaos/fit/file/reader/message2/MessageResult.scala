package de.sorted.chaos.fit.file.reader.message2

trait Message

final case class MessageResult[T <: Message](restContent: Array[Byte], message: T)
