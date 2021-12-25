package solvers
import aocutil.InputReader
import scala.collection.immutable.Queue

class Day25Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 25

    case class Position(row: Int, col: Int)
    case class Board(nRow: Int, nCol: Int, cucumbers: Map[Position, Char]) {
        def nextPosition(pos: Position): Position = {
            Position(pos.row % nRow, pos.col % nCol)
        }

        def isFree(pos: Position, cucumbers: Map[Position, Char]): Boolean = {
            cucumbers.getOrElse(pos, '.') == '.'
        }

        def step(): Tuple2[Board, Boolean] = {
            val posStart = cucumbers.keySet

            // eastbound herd
            val afterEast = cucumbers.map({
                case (Position(row, col), '>') => {
                    val nextPos = nextPosition(Position(row, col + 1))
                    if(isFree(nextPos, cucumbers)) (nextPos, '>') else (Position(row, col), '>')
                }
                case southy => southy
            })

            // southbound herd
            var afterSouth = afterEast.map({
                case (Position(row, col), 'v') => {
                    val nextPos = nextPosition(Position(row + 1, col))
                    if(isFree(nextPos, afterEast)) (nextPos, 'v') else (Position(row, col), 'v')
                }
                case easty => easty
            })

            (Board(nRow, nCol, afterSouth), posStart != afterSouth.keySet)
        }

        override def toString(): String = {
            (0 until nRow).map(r => {
                (0 until nCol).map(c => cucumbers.getOrElse(Position(r, c), '.')).mkString
            }).mkString("\n")
        }
    }

    val init = {
        val reader = new InputReader[Board](inputRoot, day)

        def parse(lines: List[String]): Board = {
            val nr = lines.length
            val nc = lines(0).length
            val x = lines.zipWithIndex.map({
                        case (l, row) => l.zipWithIndex.filterNot(_._1 == '.').map({
                            case (cucumbr, col) => (Position(row, col), cucumbr)
                        })
                    }).flatten
            Board(nr, nc, Map(x: _*))
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    def runUntilStuck(board: Board): Int = {
        var nSteps = 0
        var notDone = true
        var b = board
        while(notDone) {
            nSteps += 1
            val state = b.step()
            b = state._1
            notDone = state._2

            p(s"After step $nSteps:")
            p(b)
            p("")
        }
        nSteps
    }

    override def part1(): String = {
        runUntilStuck(init).toString
    }

    override def part2(): String = {
        "No part 2, it's the 25th after all!"
    }
}