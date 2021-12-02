import aocutil.InputReader

object Main extends App {
  println("no main, well that is stupid...")
  for(a <- args) {
    println(a)
  }
  println("done")
  println("done")

  val day1Reader = new InputReader("./inputs", 1)
  val depths = day1Reader.readInt()
  println(doit(depths))
  println(doit(depths.sliding(3).map(_.sum).toList))

  def doit(x : List[Int]) : Int = {
    x.sliding(2).map(a => a(0) < a(1)).count(a => a)
  }

  def parseInstruction(x : String) : Tuple2[String, Int] = {
    val parts = x.split(" ")
    (parts(0), parts(1).toInt)
  }

  val day2Reader = new InputReader[Tuple2[String, Int]]("./inputs", 2)
  val instructions = day2Reader.readParsed(parseInstruction)
  var h = 0
  var d = 0
  for(x <- instructions) {
    if(x._1 == "forward") {
      h += x._2
    } else if(x._1 == "down") {
      d += x._2
    } else {
      d -= x._2
    }
  }
  println(h)
  println(d)
  println(h*d)

  h = 0
  d = 0
  var a = 0
  for(x <- instructions) {
    if(x._1 == "forward") {
      h += x._2
      d += a * x._2
    } else if(x._1 == "down") {
      a += x._2
    } else {
      a -= x._2
    }
  }
  println(h)
  println(d)
  println(h * d)
}