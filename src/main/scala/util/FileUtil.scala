package util

import scala.io.Source

object FileUtil {

  def readInput(path: String): Iterator[String] = {
     Source.fromResource("inputs/"+path)
      .getLines
  }



}
