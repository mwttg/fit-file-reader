package de.sorted.chaos.fit.file.reader.message

final case class DataExtractorResult[T](restContent: Array[Byte], messageWithHeader: T)
