package solvers
import aocutil.InputReader

class Day12Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    type CaveMap = Map[String, List[String]]
    val day = 12
    val cave = {
        val reader = new InputReader[Tuple2[String, String]](inputRoot, day)

        def parse(line: String): Tuple2[String, String] = {
            val parts = line.split("-")
            (parts(0), parts(1))
        }

        var m = Map[String, List[String]]()
        for(c <- reader.readParsedByLine(parse, test, testCase)) {
            c match {
                case (a, b) => {
                    // Do not pass start, do not collect free paths
                    if(b != "start") {
                        m = m + (a -> (m.getOrElse(a, List()) :+ b))
                    }
                    if(a != "start") {
                        m = m + (b -> (m.getOrElse(b, List()) :+ a))
                    }
                }
            }
        }
        m
    }
    
    def purgeNode(cave: CaveMap, node: String): CaveMap = {
        cave.map({
            case (k, v) => (k, v.filterNot(_ == node))
        })
    }

    def purgeNodes(cave: CaveMap, nodes: Set[String]): CaveMap = {
        cave.map({
            case (k, v) => (k, v.filterNot(nodes.contains(_)))
        })
    }

    def spelunk(cave: CaveMap, at: String = "start", from: String = "start", path: List[String] = List[String]()): Int = {
        at match {
            case "end" => {
                p((path :+ "end").mkString(","))
                1
            }
            case _ => {
                val nextCave = if(from.charAt(0).isUpper) cave else purgeNode(cave, from)
                nextCave(at).map(to => spelunk(nextCave, to, at, path :+ at)).sum
            }
        }
    }

    def spelunk2(cave: CaveMap,
                    at: String = "start",
                    from: String = "start",
                    path: List[String] = List[String](),
                    visited: Set[String] = Set[String](),
                    mayRevisitSmall: Boolean = true): Int = {
        at match {
            case "end" => {
                p((path :+ "end").mkString(","))
                1
            }
            case _ => {
                val nextCave = if(mayRevisitSmall & visited.contains(at)) {
                                    purgeNodes(cave, visited)
                                } else if(mayRevisitSmall | from.charAt(0).isUpper) {
                                    cave
                                } else {
                                    purgeNode(cave, from)
                                }

                nextCave(at).map(to => spelunk2(nextCave,
                                                    to,
                                                    at,
                                                    path :+ at,
                                                    if(at.charAt(0).isUpper) visited else visited + at,
                                                    mayRevisitSmall & !visited.contains(at))).sum
            }
        }
    }

    override def part1(): String = {
        val nPaths1 = spelunk(cave)
        s"Not visiting any small caves twice, we get $nPaths1 possible paths."
    }

    override def part2(): String = {
        val nPaths2 = spelunk2(cave)
        s"When visiting A SINGLE small cave twice, we get $nPaths2."
    }
}