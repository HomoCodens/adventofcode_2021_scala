import aocutil.InputReader
import solvers.Day1Solver
import solvers.Day2Solver
import solvers.Day3Solver
import solvers.Day4Solver
import solvers.Day5Solver
import solvers.Day6Solver
import solvers.Day7Solver
object Main extends App {
  val d7 = new Day7Solver("./inputs", test = false, testCase = 1, verbose = true)
  d7.run()
}