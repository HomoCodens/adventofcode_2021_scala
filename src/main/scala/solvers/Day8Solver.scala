package solvers
import aocutil.InputReader

class Day8Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
  val day = 8

  override def part1(): String = {
      val reader = new InputReader(inputRoot, day)
      val lines = reader.readText(test, testCase)

      val p1 = lines.map(_.split(" \\| ")(1).split(" ").map(s => if(Array(2, 3, 4, 7).contains(s.length)) 1 else 0).sum)
      p1.sum.toString
  }
}