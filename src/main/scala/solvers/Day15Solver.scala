package solvers
import aocutil.InputReader
import scala.collection.mutable.PriorityQueue

class Day15Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 15
    
    // This is the point (pun xD) where it would pay to pull this grid stuff into utils
    case class Point(x: Int, y: Int)
    type Grid = List[List[Int]]
    
    val grid = {
        val reader = new InputReader[List[Int]](inputRoot, day)

        reader.readParsedByLine(_.split("").map(_.toInt).toList, test, testCase)
    }
    val dim = grid.length

    def getNeighbours(constrain: Boolean, diagonal: Boolean = true, gridSize: Int = 0)(p: Point): IndexedSeq[Point] = {
        val Point(x, y) = p
        for(dx <- -1 to 1; dy <- -1 to 1 if !(dx == 0 & dy == 0) & 
                                                (!constrain | 
                                                    (x + dx >= 0 & 
                                                     x + dx < gridSize &
                                                     y + dy >= 0 &
                                                     y + dy < gridSize)) &
                                                (diagonal |
                                                    (dx == 0 |
                                                     dy == 0)))
            yield Point(x + dx, y + dy)
    }

    // Manahattan distance from bottom right
    def manhattanToGoal(gridSize: Int)(p: Point): Int = {
        2*gridSize - p.x - p.y
    }

    def getExpandedGridValue(at: Point, grid: Grid): Int = {
        val gridSize: Int = grid.length
        val tileX: Int = at.x/gridSize
        val tileY: Int = at.y/gridSize
        val value = (grid(at.y % gridSize)(at.x % gridSize) + tileX + tileY) % 9
        if(value == 0) 9 else value
    }

    def walkTheGraph(start: Point, end: Point, grid: Grid, h: Point => Int): Int = {
        // Only works if end is at bottom right. Which is always the case :Shrug:
        val neighbours: (Point) => IndexedSeq[Point] = getNeighbours(true, false, end.x + 1)

        def rec(at: Point,
                queue: PriorityQueue[(Point, Int)],
                gScore: Map[Point, Int], // Wiki says "Hi"
                path: Map[Point, Point]): Int = {
                    if(at == end) {
                        gScore(end)
                    } else {
                        var g = gScore
                        var qq = queue.clone
                        neighbours(at).foreach(n => {
                            val s = g.getOrElse(at, 0) + getExpandedGridValue(n, grid)
                            if(s < g.getOrElse(n, Integer.MAX_VALUE)) {
                                g = g + (n -> s)
                                qq.enqueue((n, s + h(n)))
                            }
                        })

                        val next = qq.dequeue()._1
                        rec(next, qq, g, path)
                    }
        }

        val ord = Ordering.by[(Point, Int), Int](-_._2)
        val queue = PriorityQueue()(ord)
        queue.enqueue((start, h(start)))

        rec(start, queue, Map[Point, Int](), Map[Point, Point]())
    }

    override def part1(): String = {
        val qq = walkTheGraph(Point(0, 0), Point(dim-1, dim-1), grid, manhattanToGoal(dim-1))
        s"Scanning the immediate surroundings, risk is $qq"
    }
    
    override def part2(): String = {
        val qq = walkTheGraph(Point(0, 0), Point(5*dim-1, 5*dim-1), grid, manhattanToGoal(2*dim-1))
        s"Walking the full cave, risk is $qq"
    }
}
