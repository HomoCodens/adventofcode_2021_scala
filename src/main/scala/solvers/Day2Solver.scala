package solvers
import aocutil.InputReader

class Day2Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 2
    type Instruction = Tuple2[String, Int]
    // Does this sound... scala-y? ô.0
    val instructions = {
        val day2Reader = new InputReader[Instruction](inputRoot, day)

        def parseInstruction(x : String) : Instruction = {
            x.split(" ") match {
                case Array(dir, mag) => (dir.substring(0, 1), mag.toInt)
            }
        }
        
        day2Reader.readParsedByLine(parseInstruction)
    }
    
    override def part1() : String = {
        val (h, d) = instructions.foldLeft((0, 0))((acc, instr) => {
            // Shame I haven't found a way to destruct those both at the same time
            val (i, m) = instr
            val (h, d) = acc
            i match {
                case "f" => (h + m, d)
                case "d" => (h, d + m)
                case "u" => (h, d - m)
            }
        })
        (h*d).toString
    }


    override def part2() : String = {
        val (h, d, a) = instructions.foldLeft((0, 0, 0))((acc, instr) => {
            val (i, m) = instr
            val (h, d, a) = acc
            i match {
                case "f" => (h + m, d + a*m, a)
                case "d" => (h, d, a + m)
                case "u" => (h, d, a - m)
            }
        })
        (h*d).toString
    }
}