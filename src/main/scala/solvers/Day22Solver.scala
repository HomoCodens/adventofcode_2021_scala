package solvers
import aocutil.InputReader

class Day22Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 22

    case class AxisBound(min: Int, max: Int) {
        def range(): Seq[Int] = (min to max)
    }
    object AxisBound {
        def apply(min: String, max: String): AxisBound = AxisBound(min.toInt, max.toInt)
    }
    
    case class InitInstruction(instruction: String, x: AxisBound, y: AxisBound, z: AxisBound)

    type CubePoint = Tuple3[Int, Int, Int]
    type Cube = Set[CubePoint]
    
    val init = {
        val reader = new InputReader[InitInstruction](inputRoot, day)

        def parse(l: String): InitInstruction = {
            val InstructionPattern = raw"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)".r

            l match {
                case InstructionPattern(instruction, xMin, xMax, yMin, yMax, zMin, zMax) => 
                        InitInstruction(instruction,
                                            AxisBound(xMin, xMax),
                                            AxisBound(yMin, yMax),
                                            AxisBound(zMin, zMax))
            }
        }

        reader.readParsedByLine(parse, test, testCase)
    }

    def getCoordinates(xBounds: AxisBound, yBounds: AxisBound, zBounds: AxisBound): IndexedSeq[CubePoint] = {
        for(x <- xBounds.min to xBounds.max;
                y <- yBounds.min to yBounds.max;
                    z <- zBounds.min to zBounds.max)
                        yield (x, y, z)
    }

    def initCube(instructions: List[InitInstruction], cube: Cube = Set()): Cube = {
        instructions match {
            case instruction :: tail => {
                println(instruction)
                instruction match {
                    case InitInstruction("on", xb, yb, zb) => initCube(tail, cube ++ getCoordinates(xb, yb, zb))
                    case InitInstruction("off", xb, yb, zb) => initCube(tail, cube -- getCoordinates(xb, yb, zb))
                    case _ => throw new RuntimeException("Invalid day 22 instruction.")
                }
            }
            case List() => cube
        }
    }

    override def part1(): String = {
        val c = initCube(init.filter({
            case InitInstruction(i, xb, yb, zb) => xb.min >= -50 & xb.max <= 50 & yb.min >= -50 & yb.max <= 50 & zb.min >= -50 & zb.max <= 50
        }))
        println(c.size)
        ""
    }
    
    override def part2(): String = {
        ""
    }
}
