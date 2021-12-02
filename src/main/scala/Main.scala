object Main extends App {
  println("no main, well that is stupid...")
  for(a <- args) {
    println(a)
  }
  println("done")
  println("done")

  val depths = scala.io.Source.fromFile("inputs/day1/input.txt").getLines.toArray.map(_.toInt)
  println(doit(depths))
  println(doit(depths.sliding(3).map(_.sum).toArray))

  def doit(x : Array[Int]) : Int = {
    x.sliding(2).map(a => a(0) < a(1)).count(a => a)
  }

  def parseInstruction(x : String) : Tuple2[String, Int] = {
    val parts = x.split(" ")
    (parts(0), parts(1).toInt)
  }

  val instructions = scala.io.Source.fromFile("inputs/day2/input.txt").getLines.toArray.map(parseInstruction)
  var h = 0
  var d = 0
  for(x <- instructions) {
    if(x._1 == "forward") {
      println("going forward")
      h += x._2
    } else if(x._1 == "down") {
      println("going down")
      d += x._2
    } else {
      println("going up")
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
      println("going forward")
      h += x._2
      d += a * x._2
    } else if(x._1 == "down") {
      println("going down")
      a += x._2
    } else {
      println("going up")
      a -= x._2
    }
  }
  println(h)
  println(d)
  println(h * d)
}