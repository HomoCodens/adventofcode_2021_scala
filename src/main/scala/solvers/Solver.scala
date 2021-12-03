package solvers

abstract class Solver(inputRoot: String, verbose: Boolean = false, test: Boolean = false, testCase: Int = 1) {
    val day: Int

    def p(x: Any) = {
        if(verbose) {
            println(x)
        }
    }

    def run() : Unit = {
        println("==========================")
        println(s"Day $day")
        println("==========================")
        val p1 = part1()
        val p2 = part2()
        println(s"Part 1: $p1")
        println(s"Part 2: $p2")
        println()
        println()
    }

    def part1() : String = {
        "Not implemented yet"
    }

    def part2() : String = {
        "Not implemented yet"
    }
}