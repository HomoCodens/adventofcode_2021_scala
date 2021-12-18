package solvers
import aocutil.InputReader

class Day14Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 14
    type Pair = Tuple2[Char, Char]
    type Polymer = Map[Pair, BigInt]
    type Reactions = Map[Pair, Char]
    val (startymer: String, reactions: Reactions) = {
        val reader = new InputReader[Tuple2[String, Reactions]](inputRoot, day)

        def parse(lines: List[String]) = {
            var startymer: String = ""
            // https://stackoverflow.com/questions/51957242/how-to-initialize-empty-map-that-has-been-type-aliased
            var reactions: Reactions = Map()
            val ReactionPattern = "([A-Z]{2}) -> ([A-Z])".r

            for(l <- lines) {
                l match {
                    case "" => {}
                    case ReactionPattern(reactants, product) => reactions = reactions + ((reactants.head, reactants.last) -> product.head)
                    case _ => startymer = l
                }
            }

            (startymer, reactions)
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    def getStartymer(): Polymer = {
        val one: BigInt = 1
        val xx: Polymer = Map()
        startymer.sliding(2).foldLeft(xx)((acc, x) => acc + ((x.head, x.last) -> ((acc.getOrElse((x.head, x.last), 0): BigInt) + one)))
    }

    def react(polymer: Polymer, reactions: Reactions): Polymer = {
        var pp: Polymer = Map() // yeyeh, not functional... I'll get to it
        for(r <- reactions) {
            pp = growPear(pp, r._1, r._2, polymer.getOrElse(r._1, 0))
        }
        pp
    }

    def growPear(polymer: Polymer, pair: Pair, product: Char, n: BigInt): Polymer = {
        val a = (pair._1, product)
        val b = (product, pair._2)
        polymer + (a -> ((polymer.getOrElse(a, 0): BigInt) + n), b -> ((polymer.getOrElse(b, 0): BigInt) + n))
    }

    def solve(n: Int = 10): BigInt = {
        var pp = getStartymer()
        for(i <- 1 to n) {
            pp = react(pp, reactions)
        }
        var c = Map[Char, BigInt]()
        for(x <- pp) {
            val a = x._1._1
            val b = x._1._2
            val n = x._2
            if(a == b) {
                c = c + (a -> ((c.getOrElse(a, 0): BigInt) + 2*n))
            } else {
                c = c + (a -> ((c.getOrElse(a, 0): BigInt) + n), b -> ((c.getOrElse(b, 0): BigInt) + n))
            }
        }
        c = c.map({ case (cha, n) => (cha, (n + (if(cha == startymer.head | cha == startymer.last) { 1 } else { 0 })) / 2) })
        c.values.max - c.values.min
    }
    override def part1(): String = {
        solve().toString
    }

    override def part2(): String = {
        solve(40).toString
    }
}
