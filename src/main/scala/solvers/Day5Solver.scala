package solvers

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

    def isStraight(): Boolean = {
        return start.x == end.x || start.y == end.y
    }
}

class Day5Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 5
    val Lines: List[Line] = {
        val reader = new InputReader[List[Line]](inputRoot, day)
        reader.readParsedByLine(parseLine)
    }

    def parseLine(input: String): Line = {
        val parts = input.split(" -> ")

        def parsePoint(input: String): Point = {
            val numbers = input.replaceAll("(").replaceAll(")").split(",")
            println(numbers)
            Point(0, 0)
        }

        new Line(parsePoint(parts(0)), parsePoint(parts(1)))
    }
}