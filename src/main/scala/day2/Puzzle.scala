package day2

import util.FileUtil


object Puzzle {
  def main(args: Array[String]): Unit = {
    val values = FileUtil.readInput("day2.txt")
      .map(parseLine)
      .toList

    val firstPart = values
      .count {
        case (min: Int, max: Int, char: Char, password: String) =>
          password.count(_ == char) >= min && password.count(_ == char) <= max
      }

    println(firstPart)


    val secondPart = values
      .count {
        case (min: Int, max: Int, char: Char, password: String) =>
          (password.charAt(min - 1) == char) ^ (password.charAt(max - 1) == char)
      }

    println(secondPart)
  }

  def parseLine(line: String): (Int, Int, Char, String) = {
    val pattern = "(\\d+)-(\\d+) ([a-z]): ([a-z]+)".r
    val pattern(min, max, char, password) = line

    (min.toInt, max.toInt, char(0), password)
  }


}

