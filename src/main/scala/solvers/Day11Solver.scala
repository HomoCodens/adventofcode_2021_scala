package solvers
import aocutil.InputReader

class Day11Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 11

    case class Position(x: Int, y: Int)
    type DumboQueue = Map[Position, Int]
    type DumboGrid = List[List[Int]]
    
    val grid: DumboGrid = {
        val reader = new InputReader[List[Int]](inputRoot, day)
        reader.readParsedByLine(x => x.split("").map(_.toInt).toList, test, testCase)
    }


    def enQueue(queue: DumboQueue, position: Position, energy: Int): DumboQueue = {
        queue.get(position) match {
            case Some(x) => queue + (position -> (x+1))
            case None => queue + (position -> energy)
        }
    }

    def unQueue(queue: DumboQueue): Option[Tuple2[Position, DumboQueue]] = {
        val flashyBoisAndGurls = queue.filter({ case (_, e) => e > 9 })
        if(flashyBoisAndGurls.size > 0) {
            val k = flashyBoisAndGurls.keys.head
            Some((k, queue - k))
        } else {
            None
        }
    }

    def advance(grid: DumboGrid): DumboGrid = {
        grid.map(r => r.map(_ + 1))
    }

    def getNeighbours(p: Position): IndexedSeq[Position] = {
        val Position(x, y) = p
        for(i <- -1 to 1; j <- -1 to 1 if !(i == 0 & j == 0) & (x + j >= 0) & (x + j < 10) & (y + i >= 0) & (y + i < 10)) 
            yield Position(x + j, y + i)
    }

    def getFlashers(grid: DumboGrid): (DumboGrid, Int) = {
        def rec(queue: DumboQueue, flashed: List[Position]): (DumboQueue, List[Position]) = {
            unQueue(queue) match {
                case Some(x) => {
                    val flash = x._1
                    var nq = x._2
                    //p(s"Flashing $flash")
                    for(n <- getNeighbours(flash) if !flashed.contains(n)) {
                        //p(s"Enqueuing $n")
                        nq = enQueue(nq, n, grid(n.y)(n.x) + 1)
                    }
                    //printGridInProgress(grid, nq)
                    flashed
                    rec(nq, flashed :+ flash)
                }
                case None => (queue, flashed)
            }
        }

        var init = (for(i <- 0 to 9; j <- 0 to 9 if grid(j)(i) > 9) yield (Position(i, j), grid(j)(i))).toMap
        val (flashedAt, havingFlashed) = rec(init, List[Position]())

        (grid.zipWithIndex.map({
            case (r, j) => r.zipWithIndex.map({
                case (e, i) => if(flashedAt.contains(Position(i, j))) {
                    flashedAt(Position(i, j))
                } else {
                    if(havingFlashed.contains(Position(i, j))) {
                        0
                    } else {
                        if(grid(j)(i) > 9) {
                            0
                        } else {
                            grid(j)(i)
                        }
                    }
                }
            })
        }), havingFlashed.length)
    }

    def printGridInProgress(grid: DumboGrid, queue: DumboQueue): Unit = {
        val g = grid.zipWithIndex.map({
            case (r, i) => r.zipWithIndex.map({ case (e, j) => if(queue.contains(Position(j, i))) queue(Position(j, i)) else grid(j)(i)})
        })
        p(g.map(x => x.map(e => e).mkString("|")).mkString("\n"))
    }

    def resetFlashers(grid: DumboGrid, flashers: List[Position]): DumboGrid = {
        grid.zipWithIndex.map({
            case (r, i) => r.zipWithIndex.map({ case (e, j) => if(flashers.contains(Position(j, i))) 0 else e})
        })
    }

    override def part1(): String = {
        var gridP1 = grid

        var totalFlashes = 0
        for(i <- 1 to 100) {
            gridP1 = advance(gridP1)
            val r = getFlashers(gridP1)
            gridP1 = r._1
            p(s"${r._2} squiddies went *flash*")
            //p(gridP1.map(_.mkString).mkString("\n"))
            totalFlashes += r._2
        }
        totalFlashes.toString
    }

    override def part2(): String = {
        var havingFlashed = 0
        var step = 0
        var gridP1 = grid
        while(havingFlashed < 100) {
            gridP1 = advance(gridP1)
            val r = getFlashers(gridP1)
            gridP1 = r._1
            havingFlashed = r._2
            p(s"${r._2} squiddies went *flash*")
            step += 1
        }
        step.toString
    }
}