import aocutil.InputReader
import solvers.Day1Solver
import solvers.Day2Solver
import solvers.Day3Solver
import solvers.Day4Solver
import solvers.Day5Solver
import solvers.Day6Solver
import solvers.Day7Solver
import solvers.Day8Solver
import solvers.Day9Solver
import solvers.Day10Solver
object Main extends App {
  val d10 = new Day10Solver("./inputs", test = false, testCase = 1, verbose = true)
  d10.run()
}