package solvers
import aocutil.InputReader

class Day17Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 17

    case class TargetArea(minX: Int, minY: Int, maxX: Int, maxY: Int)
    val target: TargetArea = {
        val reader = new InputReader[TargetArea](inputRoot, day)

        def parse(lines: List[String]): TargetArea = {
            val line = lines(0)

            // target area: x=60..94, y=-171..-136
            val TargetAreaPattern = raw"target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)".r
            line match {
                case TargetAreaPattern(minX, maxX, minY, maxY) => TargetArea(minX.toInt, minY.toInt, maxX.toInt, maxY.toInt)
            }
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    def sumilate(dx: Int, dy: Int, goal: TargetArea, x: Int = 0, y: Int = 0, maxY: Int = 0, step: Int = 0): Option[Int] = {
        val newDx = dx + -dx.sign*step
        val newX = if(newDx.sign == dx.sign) x + newDx else x
        val newY = y + dy - step

        if(newX >= goal.minX & newX <= goal.maxX & newY >= goal.minY & newY <= goal.maxY) {
            Some(maxY)
        } else if (newX > goal.maxX | newY < goal.minY) {
            None
        } else {
            sumilate(dx, dy, goal, newX, newY, if(newY > maxY) newY else maxY, step + 1)
        }
    }

    def search(): Int = {
        val vs = for(dx <- 0 to 400; dy <- 0 to 400) yield (dx, dy)
        vs.foldLeft(0)((acc, v) => { 
            v match {
                case (dx, dy) => {
                    sumilate(dx, dy, target) match {
                        case Some(m) => if(m > acc) m else acc
                        case None => acc
                    }
                }
            }
        })
    }

    def search2(): Int = {
        val vs = for(dx <- 0 to 400; dy <- -400 to 400; if !sumilate(dx, dy, target).isEmpty) yield (dx, dy)
        vs.length
    }
    override def part1(): String = {
        s"Maximum height trick shot is ${search().toString}"
    }
    
    override def part2(): String = {
        s"Alright, alright... number of possible shots: ${search2().toString}"
    }
}
