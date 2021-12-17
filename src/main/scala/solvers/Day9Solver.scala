package solvers
import aocutil.InputReader
import scala.collection.immutable.Queue

case class Cell(pos: Tuple2[Int, Int], value: Int)

class CaveFloor(lines: List[String]) {

    val grid = lines.map(l => l.split("").map(_.toInt).toList).toList
    val dimI = grid.length
    val dimJ = grid(0).length

    def getAt(i: Int, j: Int): Int = {
        if(i < 0 | j < 0 | i >= dimI | j >= dimJ) {
            // "Wall"
            9
        } else {
            grid(i)(j)
        }
    }

    def isLowPoint(i: Int, j: Int): Boolean = {
        // Loops, shmoops...
        List(
            getAt(i-1, j),
            getAt(i+1, j),
            getAt(i, j-1),
            getAt(i, j+1)
        ).forall(_ > getAt(i, j))
    }

    def getNeighbours(i: Int, j: Int): List[Cell] = {
        List(
            Cell((i - 1, j), getAt(i - 1, j)),
            Cell((i + 1, j), getAt(i + 1, j)),
            Cell((i, j - 1), getAt(i, j - 1)),
            Cell((i, j + 1), getAt(i, j + 1))
        )
    }

    def getLowPoints(): List[Cell] = {
        var lows = List[Cell]()
        for(i <- 0 to (dimI - 1); j <- 0 to (dimJ - 1)) {
            if(isLowPoint(i, j)) {
                lows = Cell((i, j), getAt(i, j)) :: lows
            }
        }
        lows
    }

    def getBasin(start: Cell): List[Cell] = {
        def rec(visited: List[Cell], queue: Queue[Cell]): List[Cell] = {
            queue match {
                // Eery
                case head +: tail => rec(visited :+ head,
                                            tail ++ (getNeighbours(head.pos._1, head.pos._2).filterNot(x => visited.contains(x) | x.value >= 9 | queue.contains(x))))
                case _: Queue[Cell] => visited
            }
        }
        rec(List[Cell](), Queue[Cell](start)).toList
    }
}

class Day9Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 9
    val floor = {
        val reader = new InputReader[CaveFloor](inputRoot, day)
        reader.readParsedWhole(x => new CaveFloor(x), test, testCase)
    }

    override def part1(): String = {
        val lows = floor.getLowPoints().map(_.value)
        s"The sum of lowpoints + 1 is ${lows.sum + lows.length}"
    }

    override def part2(): String = {
        val lows = floor.getLowPoints()
        val basins = lows.map(floor.getBasin(_))
        val p2 = basins.sortBy(-_.length).take(3).map(_.length).product
        s"Multiplying the sizes of the three largest basins gives $p2"
    }
}