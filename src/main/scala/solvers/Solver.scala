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
        val p1t0 = System.nanoTime()
        val p1 = part1()
        val p1t = System.nanoTime() - p1t0
        val p2t0 = System.nanoTime()
        val p2 = part2()
        val p2t = System.nanoTime() - p2t0
        println(s"Part 1: $p1")
        println(s"Part 1 ran in ${p1t.toFloat/1000000}ms")
        println(s"Part 2: $p2")
        println(s"Part 2 ran in ${p2t.toFloat/1000000}ms")
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