package solvers
import aocutil.InputReader

class Day7Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 7
    val positions = {
        def parse(line: String): List[Int] = {
            line.split(",").map(_.toInt).toList
        }

        val reader = new InputReader[List[Int]](inputRoot, day)
        reader.readParsedByLine(parse, test, testCase)(0)
    }

    def solve(fuelCost: (Int, Int) => Int): Tuple2[Int, Int] = {
        // ya know, we _could_ do gradient descent or something but meh
        val pFrom = positions.min
        val pTo = positions.max
        val distances = (pFrom to pTo).map(x => positions.map(y => fuelCost(y, x)).sum)
        distances.zipWithIndex.minBy(_._1)
    }
    override def part1(): String = {
        solve((a, b) => (a - b).abs)._1.toString
    }

    override def part2(): String = {
        solve((a, b) => {
            val d = (a - b).abs
            d * (d + 1) / 2
        })._1.toString
    }
}