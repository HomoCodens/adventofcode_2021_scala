package solvers

import aocutil.InputReader

// Let's recurse because for the lulz
// and mayhaps it'll halp in part 2 (prolly not)
// 
// Aaaand... not. Keeping it here because for the lulz ;)
class Phishie(initialClock: Int = 8) {
    var childes: List[Phishie] = List()
    var babyClock: Int = initialClock

    def tick(): Unit = {
        childes.foreach(_.tick())
        if(babyClock == 0) {
            babyClock = 6
            childes = childes ++ List(new Phishie())
        } else {
            babyClock -= 1
        }
    }

    def getSize(): Int = {
        1 + childes.map(_.getSize).sum
    }

    override def toString(): String = {
        s"Phishie($babyClock, ${childes.length})"
    }
}

class Day6Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 6
    val pop: List[Phishie] = {
        def parse(line: String): List[Phishie] = {
            line.split(",").map(_.toInt).toList.map(new Phishie(_))
        }

        val reader = new InputReader[List[Phishie]](inputRoot, day)
        reader.readParsedByLine(parse, test, testCase)(0)
    }

    override def part1(): String = {
        val part1Pop = pop.map(p => new Phishie(p.babyClock))
        for(i <- 1 to 80) {
            p(s"After $i days: ${part1Pop.map(_.getSize).sum} phishies exist")
            part1Pop.foreach(_.tick)
        }
        part1Pop.map(_.getSize).sum.toString
    }

    override def part2(): String = {
        val part1Pop = pop.map(p => new Phishie(p.babyClock))
        for(i <- 1 to 256) {
            part1Pop.foreach(_.tick)
            p(s"day $i done")
        }
        part1Pop.map(_.getSize).sum.toString
    }
}