import aocutil.InputReader
import solvers.Day1Solver
import solvers.Day2Solver
import solvers.Day3Solver
import solvers.Day4Solver
import solvers.Day5Solver
import solvers.Day6Solver
import solvers.Day7Solver
import solvers.Day8Solver
object Main extends App {
  val d8 = new Day8Solver("./inputs", test = false, testCase = 1, verbose = true)
  d8.run()
}