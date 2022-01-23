import scopt.OParser
import solvers._
import java.lang.reflect.InvocationTargetException
object Main extends App {
  case class Config(
    testCase: Int = 0,
    verbose: Boolean = false,
    time: Boolean = false,
    day: Int = 1,
    input: String = ""
  )

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("aoc21"),
      opt[Int]('t', "test")
        .action((x, c) => c.copy(testCase = x))
        .valueName("<test case>")
        .validate(x => 
          if(x > 0) success
          else failure("<test case> must be > 0"))
        .text("Run day against test input."),
      opt[Unit]('v', "verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("Print addidional output."),
      opt[Unit]('T', "time")
        .action((_, c) => c.copy(time = true))
        .text("Time the solution."),
      opt[Int]('d', "day")
        .required()
        .action((x, c) => c.copy(day = x))
        .valueName("<day>")
        .validate(x => 
          if(x > 0 && x < 27) success
          else failure("<day> must be 1-26."))
        .text("The day to run."),
      opt[String]('i', "input")
        .required()
        .action((x, c) => c.copy(input = x))
        .valueName("<input>")
        .text("Path to input folder."),
      help("help").text("Print this help message.")
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) => {
      getSolver(config).run()
    }
    case _ => {}
  }
  
  def getSolver(config: Config): Solver = {
    val c = Class.forName(s"solvers.Day${config.day}Solver")
    val cc = c.getConstructors()(0)
    try {
      val i = cc.newInstance(config.input,
                              config.verbose,
                              config.testCase > 0,
                              config.testCase,
                              config.time)
      i.asInstanceOf[Solver]
    } catch {
      // Preetty sure this is not how we do it but it gets the proper exception to the top :shrug:
      case e: InvocationTargetException => throw e.getCause()
    }
  }
}