package day4

import util.FileUtil

object Puzzle {
  def main(args: Array[String]): Unit = {
    val input = FileUtil.readInput("day4.txt").toList
    val fields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")
    val optional = Set("cid")
    val required = fields.diff(optional)

    val passports = splitBySeparator(input, "")
      .map(l =>
        l.flatMap(s =>
          s.split(" ")
            .toList
        )
          .map(s => (s.split(":")(0), s.split(":")(1))
          ).toMap
      )

    val valids = passports.filter(passport =>
      passport.keySet.union(required) == passport.keySet
    )
    println(valids.length)

    val validated = valids.filter(validate)
    println(validated.size)
  }

  def validate(passport: Map[String, String]): Boolean = {
    val validKeys = passport.keySet.filter((key: String) => {
      key match {
        case "byr" =>
          val year: Int = passport(key).toInt
          year >= 1920 && year <= 2002
        case "iyr" =>
          val year: Int = passport(key).toInt
          year >= 2010 && year <= 2020
        case "eyr" =>
          val year: Int = passport(key).toInt
          year >= 2020 && year <= 2030
        case "hgt" =>
          val pattern = "([0-9]+)(cm|in)".r
          passport(key) match {
            case pattern(n, u) =>
              val height = n.toInt
              u match {
                case "cm" =>
                  height >= 150 && height <= 193
                case "in" =>
                  height >= 59 && height <= 76
                case _ => false
              }
            case _ => false
          }
        case "hcl" =>
          val pattern = "#[a-fA-F0-9]{6}".r
          passport(key) match {
            case pattern() => true
            case _ => false

          }
        case "ecl" =>
          Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
            .contains(passport(key))
        case "pid" =>
          val pattern = "\\d{9}".r
          passport(key) match {
            case pattern() => true
            case _ => false
          }
        // "cid" is optional
        case _ => true
      }
    })


    validKeys.size match {
      case x if x >= passport.keySet.size => true
      case _ => false
    }
  }

  def splitBySeparator(l: List[String], sep: String): List[List[String]] = {
    l.span(_ != sep) match {
      case (hd, _ :: tl) => hd :: splitBySeparator(tl, sep)
      case (hd, _) => List(hd)
    }
  }

}
