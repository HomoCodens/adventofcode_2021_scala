import aocutil.InputReader
import solvers.Day1Solver
import solvers.Day2Solver
object Main extends App {

  val d1 = new Day1Solver("./inputs", false)
  d1.run()

  val d2 = new Day2Solver("./inputs", false)
  d2.run()

  /*val day3Reader = new InputReader[List[Int]]("./inputs", 3)

  def parse(x: String) : List[Int] = {
    x.split("").toList.map(_.toInt)
  }

  val diagnostics = day3Reader.readParsed(parse)
  for(x <- diagnostics) {
    println(x)
  }

  val threshold = diagnostics.length / 2
  val counts = diagnostics.reduce((a, b) => a.zip(b).map(x => x._1 + x._2))
  println(counts)
  val gamma = Integer.parseInt(counts.map(x => if (x > threshold) 1 else 0).mkString, 2)
  println(gamma)
  val epsilon = Integer.parseInt(counts.map(x => if (x < threshold) 1 else 0).mkString, 2)
  println(epsilon)
  println(epsilon * gamma)

  def lifeSupportFilter(inputs: List[List[Int]], digit: Int, ogr: Boolean) = {
    val threshold: Double = inputs.length / 2.0
    val counts = inputs.reduce((a, b) => a.zip(b).map(x => x._1 + x._2))
    println(counts)
    println(threshold)
    if(ogr) {
      if(counts(digit) >= threshold) {
        inputs.filter(x => x(digit) == 1)
      } else {
        inputs.filter(x => x(digit) == 0)
      }
    } else {
      if(counts(digit) < threshold) {
        inputs.filter(x => x(digit) == 1)
      } else {
        inputs.filter(x => x(digit) == 0)
      }
    }
  }

  var bla = diagnostics
  var di = 0
  while(bla.length > 1) {
    bla = lifeSupportFilter(bla, di, true)
    di += 1
    println(bla)
    println("======================")
  }
  println(bla)

  var blu = diagnostics
  di = 0
  while(blu.length > 1) {
    blu = lifeSupportFilter(blu, di, false)
    di += 1
    println(blu)
    println("==========================")
  }
  println(blu)

  val ogr = Integer.parseInt(bla(0).mkString, 2)
  val co2 = Integer.parseInt(blu(0).mkString, 2)
  println(ogr)
  println(co2)
  println(ogr * co2)*/
}