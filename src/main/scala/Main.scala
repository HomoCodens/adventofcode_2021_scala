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
}