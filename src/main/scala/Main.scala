import aocutil.InputReader
import solvers.Day1Solver
import solvers.Day2Solver
import solvers.Day3Solver
import solvers.Day4Solver
import solvers.Day5Solver
import solvers.Day6Solver
object Main extends App {
  val d6 = new Day6Solver("./inputs", test = true, testCase = 1, verbose = true)
  d6.run()
}