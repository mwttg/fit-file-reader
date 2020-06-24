package de.sorted.chaos.fit.file.reader.utility

import scala.io.Source
import scala.util.{Failure, Success, Try}

object TsvReader {

  def readAsMap(filename: String): Map[Int, String] = //todo error handling + logging
    Try(Source.fromResource(filename).getLines().toList) match {
      case Success(lines) =>
        lines
          .map(line => line.split("\\s+"))
          .map(item => {
            val key   = item(1).toInt // TODO error handling
            val value = item.head
            (key, value)
          })
          .toMap
      case Failure(_) => Map.empty
    }
}
