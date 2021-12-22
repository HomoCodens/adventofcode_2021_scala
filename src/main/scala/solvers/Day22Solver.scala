package solvers
import aocutil.InputReader
import scala.math.BigInt

class Day22Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 22
    val X = 0
    val Y = 1
    val Z = 2

    case class Instruction(turnOn: Boolean, region: CubeThing)
    val instructions = {
        val reader = new InputReader[Instruction](inputRoot, day)

        def parse(l: String): Instruction = {
            val InstructionPattern = raw"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)".r

            l match {
                case InstructionPattern(instruction, xMin, xMax, yMin, yMax, zMin, zMax) => 
                    Instruction(instruction == "on", CubeThing(xMin, xMax, yMin, yMax, zMin, zMax))                        
            }
        }

        reader.readParsedByLine(parse, test, testCase)
    }

    case class Point(x: Int, y: Int, z: Int)

    // A cuboid defined by two points, inclusive i.e. each integer point within the cube
    // defines a +x +y +z aligned 1x1x1 volume of space
    //
    // Well, technically bottom-left-forward or however you want to look at it
    case class CubeThing(bl: List[Int], tr: List[Int]) {

        def volume(): BigInt = {
            //(tr(X) - bl(X) + 1)*(tr(Y) - bl(Y) + 1)*(tr(Z) - bl(Z) + 1)
            val dx: BigInt = tr(X) - bl(X) + 1
            val dy: BigInt = tr(Y) - bl(Y) + 1
            val dz: BigInt = tr(Z) - bl(Z) + 1
            dx*dy*dz
        }

        def intersects(other: CubeThing): Boolean = {
            !((tr(X) < other.bl(X) | bl(X) > other.tr(X)) |  // separation along x axis 
              (tr(Y) < other.bl(Y) | bl(Y) > other.tr(Y)) |  // y
              (tr(Z) < other.bl(Z) | bl(Z) > other.tr(Z)))   // or z
        }

        // Calculate a set of CubeThings defining the volume of
        // this lying outside of other
        def splinterSect(other: CubeThing): List[CubeThing] = {
            if(!this.intersects(other)) {
                List(this)
            } else {
                // + 1 because other extends 1 unit past the its bound and we don't
                // want that to be part of the right side of the split
                this.splitMany(X, List(other.bl(X), other.tr(X) + 1)).flatMap(
                    _.splitMany(Y, List(other.bl(Y), other.tr(Y) + 1))).flatMap(
                        _.splitMany(Z, List(other.bl(Z), other.tr(Z) + 1))).filterNot(_.intersects(other))
            }
        }

        // Split the CubeThing along the plane axis == plane
        def split(axis: Int, plane: Int): List[CubeThing] = {
            if(plane < this.bl(axis) | plane > this.tr(axis)) {
                List(this)
            } else {
                List(
                    CubeThing(bl, tr.zipWithIndex.map(x => if(x._2 == axis) plane - 1 else x._1)),
                    CubeThing(bl.zipWithIndex.map(x => if(x._2 == axis) plane else x._1), tr)
                )
            }
        }

        // Split along multiple parallel planes
        def splitMany(axis: Int, planes: List[Int]): List[CubeThing] = {
            planes.foldLeft(List(this))((cubes: List[CubeThing], plane: Int) => cubes.init ++ cubes.last.split(axis, plane))
        }
    }

    object CubeThing {
        def apply(xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int): CubeThing = {
            new CubeThing(List(xMin, yMin, zMin), List(xMax, yMax, zMax))
        }
        def apply(xMin: String, xMax: String, yMin: String, yMax: String, zMin: String, zMax: String): CubeThing = {
            CubeThing(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, zMin.toInt, zMax.toInt)
        }
    }

    def applyInstruction(cubes: List[CubeThing], instruction: Instruction): List[CubeThing] = {
        val Instruction(turnOn, region) = instruction
        (cubes.flatMap(_.splinterSect(region)) :+ region).filterNot(c => !turnOn & c.intersects(region))
    }

    def restartCoeur(instructions: List[Instruction]): List[CubeThing] = {
        instructions.foldLeft(List[CubeThing]())((c, i) => applyInstruction(c, i))
    }

    def getNRunning(cores: List[CubeThing]): BigInt = {
        cores.map(_.volume()).sum
    }
    override def part1(): String = {
        val smallInstructions = instructions.filter(i => i.region.bl.forall(_ >= -50) & i.region.tr.forall(_ <= 50))
        s"Number of cores running after initialization: ${getNRunning(restartCoeur(smallInstructions))}"
    }
    
    override def part2(): String = {
        s"Number of cores running after full reboot: ${getNRunning(restartCoeur(instructions))}"
    }
}
