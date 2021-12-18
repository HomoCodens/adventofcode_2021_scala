package solvers

abstract class Solver(inputRoot: String,
                        var verbose: Boolean = false,
                        test: Boolean = false,
                        testCase: Int = 1,
                        timeSolutions: Boolean) {
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

        if(timeSolutions) {
            println()
            print("Timing part 1... ")
            
            val verbose0 = verbose
            verbose = false

            val t1 = timeSolution(part1, 100)
            println(s"Part 1 ran in ${t1}ms")
            
            print("Timing part 2... ")
            val t2 = timeSolution(part2, 100)
            println(s"Part 2 ran in ${t2}ms")

            verbose = verbose0
        }

        println()
        println()
    }

    def timeSolution(f: () => String, n: Int): Float = {
        (1 to n).foldLeft(List[Float]())((acc, i) => {
            val t0 = System.nanoTime()
            val x = f()
            acc :+ ((System.nanoTime() - t0).toFloat)/1000000
        }).sum / n
    }

    def part1() : String = {
        "Not implemented yet"
    }

    def part2() : String = {
        "Not implemented yet"
    }
}