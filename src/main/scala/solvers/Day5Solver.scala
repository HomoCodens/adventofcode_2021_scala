package solvers

import scala.collection.mutable.HashMap
import aocutil.InputReader

case class Point(x: Int, y: Int) {
    def add(other: Point): Point = {
        Point(x + other.x, y + other.y)
    }
}
class Line(start: Point, end: Point) {
    def getPoints(): List[Point] = {
        val dx = (end.x - start.x).sign
        val dy = (end.y - start.y).sign
        val dp = Point(dx, dy)
        var points: List[Point] = List(start)
        var current = start
        while(current != end) {
            current = current.add(dp)
            points = points ++ List(current)
        }
        points
    }

    def isGridAligned(): Boolean = {
        return start.x == end.x || start.y == end.y
    }

    override def toString(): String = {
        s"Line: $start -> $end"       
    }
}

class Day5Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 5
    val lines: List[Line] = {
        val reader = new InputReader[Line](inputRoot, day)
        reader.readParsedByLine(parseLine, test, testCase)
    }

    def parseLine(input: String): Line = {
        val parts = input.split(" -> ")

        def parsePoint(input: String): Point = {
            val numbers = input.replaceAll("[()]", "").split(",").map(_.toInt)
            Point(numbers(0), numbers(1))
        }

        new Line(parsePoint(parts(0)), parsePoint(parts(1)))
    }

    def countOverlaps(lines: List[Line]): Int = {
        var allThePoints: HashMap[Point, Int] = new HashMap()
        lines.foreach(l => {
            l.getPoints().foreach(p => {
                allThePoints += allThePoints.get(p).map(x => p -> (x + 1)).getOrElse(p -> 1)
            })
        })
        allThePoints.filter({ case (p: Point, n: Int) => n > 1}).size
    }
    
    override def part1(): String = {
        countOverlaps(lines.filter(_.isGridAligned)).toString
    }

    override def part2(): String = {
        countOverlaps(lines).toString
    }
}