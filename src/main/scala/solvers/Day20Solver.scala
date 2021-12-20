package solvers

import aocutil.InputReader

// Test 2 kudos to https://www.reddit.com/r/adventofcode/comments/rkg19w/2021_day_20_part_1_can_someone_give_a_sample/

class Day20Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 20
    type Picture = Array[Array[Int]]
    type Algorithm = Array[Int]
    val (algorithm, picture) = {
        val reader = new InputReader[Tuple2[Algorithm, Picture]](inputRoot, day)
        
        def parse(lines: List[String]): Tuple2[Algorithm, Picture] = {
            val alg :: _ :: pic = lines

            (alg.map(x => if(x == '.') 0 else 1).toArray, pic.map(l => l.map(x => if(x == '.') 0 else 1).toArray).toArray)
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    def getOrElse(pic: Picture, r: Int, c: Int, default: Int = 0): Int = {
        val height = pic.length
        val width = pic(0).length
        if(r < 0 | c < 0 | r >= height | c >= width) {
            default
        } else {
            pic(r)(c)
        }
    }

    def squareAround(r: Int, c: Int): IndexedSeq[Tuple2[Int, Int]] = {
        for(rr <- (r - 1) to (r + 1); cc <- (c - 1) to (c + 1)) yield (rr, cc)
    }

    def getIndexAt(pic: Picture, r: Int, c: Int, infinity: Int): Int = {
        // Could be cached
        Integer.parseInt(squareAround(r, c).map({
            // Alg(0) may be 1 meaning the infinigrid alternates
            case (rr, cc) => getOrElse(pic, rr, cc, infinity)
        }).mkString, 2)
    }

    def convovolve(pic: Picture, alg: Algorithm, infiniPlane: Int = 0): Picture = {
        // No pic.length + 1 because it already is one over the max index
        (-1 to pic.length).map(r => {
            (-1 to pic(0).length).map(c => {
                alg(getIndexAt(pic, r, c, infiniPlane))
            }).toArray
        }).toArray
    }

    def prettyPrint(pic: Picture) = {
        p(pic.map(l => l.map(x => if(x == 0) "." else "#").mkString).mkString("\n"))
    }

    def countLit(pic: Picture): Int = {
        pic.flatten.sum
    }

    def enhance(pic: Picture, alg: Algorithm, nSteps: Int): Picture = {
        // If alg(0) is 1, the infinite plain will oscillate between 1 and algorithm(511) (which may also be 1 :shrug:)
        (1 to nSteps).foldLeft(pic)((p, i) => convovolve(p, alg, if(alg(0) == 1 & (i % 2) == 1) algorithm(511) else algorithm(0)))
    }

    override def part1(): String = {
        // The search for the game of life in the deep sea
        /*val rools = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
        val roolsb = rools.split("").map({ case "." => 0; case "#" => 1}).toList
        
        var counts = Array(Array.fill(9)(Array(0, 0)), Array.fill(9)(Array[Int](0, 0)))
        for(i <- 0 to 511) {
            val bits = i.toBinaryString.reverse.padTo(9, '0').reverse.split("").map(_.toInt)
            val alive = bits(4)
            val neighboursAlive = bits.zipWithIndex.filter({ case (_, i) => i != 4}).foldLeft(0)((acc, x) => acc + x._1)
            counts(alive)(neighboursAlive)(roolsb(i)) = counts(alive)(neighboursAlive)(roolsb(i)) + 1
        }
        counts.foreach(x => {x.foreach(x => {println(x.toList)}); println()})*/
        
        val enhanced = enhance(picture, algorithm, 2)
        countLit(enhanced).toString()
    }
    
    override def part2(): String = {
        val enhanced = enhance(picture, algorithm, 50)
        countLit(enhanced).toString()
    }
}
