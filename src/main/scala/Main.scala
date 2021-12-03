import aocutil.InputReader
import solvers.Day1Solver
import solvers.Day2Solver
import solvers.Day3Solver
object Main extends App {

  val d1 = new Day1Solver("./inputs", false)
  d1.run()

  val d2 = new Day2Solver("./inputs", false)
  d2.run()

  val d3 = new Day3Solver("./inputs", false)
  d3.run()
}