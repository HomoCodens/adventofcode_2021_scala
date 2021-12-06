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

case class Cohort(var population: BigInt = 0, var juveniles: BigInt = 0)

class Day6Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 6
    val initial: List[Int] = {
        def parse(line: String): List[Int] = {
            line.split(",").map(_.toInt).toList
        }

        val reader = new InputReader[List[Int]](inputRoot, day)
        reader.readParsedByLine(parse, test, testCase)(0)
    }

    def getPopulation(): List[Cohort] = {
        (0 to 6).map(x => Cohort(initial.filter(_ == x).length)).toList
    }

    def run(nDays: Int): BigInt = {
        var pop = getPopulation()
        for(i <- 1 to (nDays - 1)) {
            p(s"day $i")
            val cohortToSpawn = i % 7
            val cohortToSpawnInto = (cohortToSpawn + 2) % 7
            if(pop(cohortToSpawn).population > 0) {
                p(s"Cohort $cohortToSpawn spawning ${pop(cohortToSpawn).population} bebbehs")
            }
            pop(cohortToSpawnInto).juveniles = pop(cohortToSpawn).population
            pop(cohortToSpawn).population += pop(cohortToSpawn).juveniles
            pop(cohortToSpawn).juveniles = 0
            p(pop)
        }
        pop.map(x => x.population + x.juveniles).sum
    }

    override def part1(): String = {
        run(80).toString
    }

    override def part2(): String = {
        run(256).toString
    }
}