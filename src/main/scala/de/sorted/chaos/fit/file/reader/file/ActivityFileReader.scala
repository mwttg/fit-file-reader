package de.sorted.chaos.fit.file.reader.file

object ActivityFileReader {

  // todo later input = file with path (not in resources)
  def read(filename: String): Array[Byte] = this.getClass.getClassLoader.getResourceAsStream(filename).readAllBytes()
}
