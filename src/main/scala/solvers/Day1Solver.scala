package solvers
import aocutil.InputReader

class Day1Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
  val day = 1
  val reader = new InputReader(inputRoot, 1)
  val depths = reader.readInt(test, testCase)
  def countDeepers(x : List[Int]) : Int = {
    x.sliding(2).count(x => x(0) < x(1))
  }

  override def part1() : String = {
      val solution = countDeepers(depths)
      s"$solution depths are deeper than their predecessors"
  }

  override def part2() : String = {
    val solution = countDeepers(depths.sliding(3).map(_.sum).toList)
    s"$solution windows are deeper than their predecessors"
  }
}