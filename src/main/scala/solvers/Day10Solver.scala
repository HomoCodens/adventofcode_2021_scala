package solvers
import aocutil.InputReader

class Day10Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 10
    val lines = {
        val reader = new InputReader[List[String]](inputRoot, day)
        reader.readParsedByLine(_.split("").toList, test, testCase)
    }

    def isCloser(x: String): Boolean = {
        x == ")" | x == "]" | x == "}" | x == ">"
    }

    def closes(a: String, b: String): Boolean = {
        a == getCloser(b)
    }

    def getCloser(x: String): String = {
        val pairs = Map(
            "(" -> ")",
            "[" -> "]",
            "{" -> "}",
            "<" -> ">"
        )
        pairs(x)
    }

    type ParsingResult = Tuple2[List[String], Option[String]]

    def parseLine(line: List[String]): ParsingResult = {
        def rec(stack: List[String], tail: List[String]): ParsingResult = {
            tail match {
                case h +: t if !isCloser(h) => rec(h +: stack, t)
                case h +: t if isCloser(h) & closes(h, stack.head) => rec(stack.tail, t)
                case h +: t if isCloser(h) => (stack, Some(h))
                case List(x) if closes(x, stack.head) => (stack.tail, None)
                case List(x) => (stack, Some(x))
                case _: List[String] => (stack, None)
            }
        }

        rec(List[String](), line)
    }

    override def part1(): String = {
        val inv = lines.map(parseLine(_)).map(_._2).flatten
        val scores = Map(
            ")" -> 3,
            "]" -> 57,
            "}" -> 1197,
            ">" -> 25137
        )

        val p1 = inv.map(scores(_)).sum
        s"The score of the invalid characters is $p1"
    }

    override def part2(): String = {
        val out = lines.map(parseLine(_)).filter(_._2 == None).map(_._1)
        val points = Map[String, BigInt](
            "(" -> 1,
            "[" -> 2,
            "{" -> 3,
            "<" -> 4
        )
        val scores: List[BigInt] = out.map(x => {
            x.foldLeft(0: BigInt)((acc, x) => 5*acc + points(x))
        }).sorted
        val p2 = scores((scores.length / 2))
        s"Autocompleters are weird... $p2"
    }
}