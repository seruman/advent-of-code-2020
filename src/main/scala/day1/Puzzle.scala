package day1

import util.FileUtil

import scala.io.Source

object Puzzle {
  def main(args: Array[String]): Unit = {
    val inputs = FileUtil.readInput("day1.txt")
      .map((line: String) => line.toInt)
      .toList

    println(s"part-one=${calculate(inputs, 2)}")
    println(s"part-two=${calculate(inputs, 3)}")
  }

  def calculate(expenses: List[Int], n: Int): Int = {
    val combinations = expenses.combinations(n)
    val entries = combinations.
      filter((combination: List[Int]) => combination.sum == 2020)

    entries.toList.flatten.product
  }
}
