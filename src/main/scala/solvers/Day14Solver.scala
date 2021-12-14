package solvers
import aocutil.InputReader

class Day14Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 14
    val (startymer: List[String], reactions: Map[String, String]) = {
        val reader = new InputReader[Tuple2[List[String], Map[String, String]]](inputRoot, day)

        def parse(lines: List[String]) = {
            var startymer = List[String]()
            var reactions = Map[String, String]()
            val ReactionPattern = "([A-Z]{2}) -> ([A-Z])".r

            for(l <- lines) {
                l match {
                    case "" => {}
                    case ReactionPattern(reactants, product) => reactions = reactions + (reactants -> product)
                    case _ => startymer = l.split("").toList
                }
            }

            (startymer, reactions)
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    List(1) match {
        case x :: rest => {println(x); println(rest)}
        case x => println(x)
        case _ => println("blegh")
    }

    def reactizise(left: List[String], right: List[String], reactions: Map[String, String], product: List[String]): List[String] = {
        right match {
            case x :: tail => {
                reactizise(left :+ x,
                            tail,
                            reactions,
                            product ++ (reactions.get(s"${left.last}$x") match {case Some(p) => List(p, x); case None => List(x) }))
            }
            case List() => product
        }
    }

    override def part1(): String = {
        var afterymer = startymer
        for(i <- 1 to 10) {
            afterymer = reactizise(List(afterymer.head), afterymer.tail, reactions, List(afterymer.head))
        }
        val counts = afterymer.foldLeft(Map[String, Int]())((acc, x) => acc + (x -> (acc.getOrElse(x, 0) + 1)))
        println(counts.values.max - counts.values.min)
        ""
    }

    override def part2(): String = {
        var afterymer = startymer
        for(i <- 1 to 40) {
            println(i)
            afterymer = reactizise(List(afterymer.head), afterymer.tail, reactions, List(afterymer.head))
        }
        val counts = afterymer.foldLeft(Map[String, BigInt]())((acc, x) => acc + (x -> (acc.getOrElse(x, 0:BigInt) + 1:BigInt)))
        println(counts.values.max - counts.values.min)
        ""
    }
}