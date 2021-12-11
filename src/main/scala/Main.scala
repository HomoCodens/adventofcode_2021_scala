import solvers._
object Main extends App {
  val defaultArgs: Map[String, Any] = Map(
    "test" -> false,
    "testCase" -> 1,
    "verbose" -> false,
    "time" -> false
  )

  val a = getArgs(defaultArgs, args.toList)
  
  getSolver(a).run()
  
  def getSolver(args: Map[String, Any]): Solver = {
    val c = Class.forName(s"solvers.Day${a("day")}Solver")
    val cc = c.getConstructors()(0)
    val i = cc.newInstance(a("input"), a("verbose"), a("test"), a("testCase")).asInstanceOf[Solver]
    i
  }

  def getArgs(acc: Map[String, Any], tail: List[String]): Map[String, Any] = {
    tail match {
      case "-d" :: day :: rest => getArgs(acc + ("day" -> day.toInt), rest)
      case "-t" :: testCase :: rest => getArgs(acc + ("test" -> true, "testCase" -> testCase.toInt), rest)
      case "-v" :: rest => getArgs(acc + ("verbose" -> true), rest)
      case "-i" :: inputDir :: rest => getArgs(acc + ("input" -> inputDir), rest)
      case "--time" :: rest => getArgs(acc + ("time" -> true), rest)
      case Nil => acc
    }
  }
}