package day3

import util.FileUtil

object Puzzle {
  def main(args: Array[String]): Unit = {
    val input = FileUtil.readInput("day3.txt").toList

    println(numberOfTrees(input, 3, 1))

    val result = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map { case (right: Int, down: Int) => numberOfTrees(input, right, down) }
    
    println(result.product)
  }

  def numberOfTrees(grid: List[String], rightStep: Int, downStep: Int): BigInt = {
    val (numberOfColumns, numberOfRows) = (grid.head.length, grid.length)

    val coordinates = (1 to numberOfRows)
      .map(n => f(n, rightStep, numberOfColumns))
      .zip(1 to numberOfRows by downStep)
      .toList

    val indices = coordinates
      .map { case (x, y) => (x - 1, y - 1) }

    val trees = indices
      .filter { case (col, row) => grid(row).charAt(col) == '#' }

    trees.length
  }


  def f(x: Int, rightStep: Int, numberOfColumns: Int): Int = x match {
    case 0 => 0
    case 1 => 1
    case _ =>
      (f(x - 1, rightStep, numberOfColumns) + rightStep) % numberOfColumns match {
        case 0 => numberOfColumns
        case v => v
      }
  }
}
