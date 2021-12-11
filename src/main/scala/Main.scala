import solvers._
object Main extends App {
  val day = 6

  getSolver(day).run()

  def getSolver(day: Int): Solver = {
    val c = Class.forName(s"solvers.Day${day}Solver")
    val cc = c.getConstructors()(0)
    val i = cc.newInstance("./inputs", false, false, 1).asInstanceOf[Solver]
    i
  }
}