package solvers
import aocutil.InputReader

class Segment() {

}

class SevenSeg(sequences: List[String]) {
    // But this feels so... manual ;(
    val mapping = {
        val one = sequences.filter(_.length == 2)(0).sorted
        val four = sequences.filter(_.length == 4)(0).sorted
        val seven = sequences.filter(_.length == 3)(0).sorted
        val eight = sequences.filter(_.length == 7)(0).sorted
        val twoThreeFive = sequences.filter(_.length == 5)
        val zeroSixNine = sequences.filter(_.length == 6)
    
        val six = zeroSixNine.filter(_.intersect(one).length == 1)(0).sorted
        val zero = zeroSixNine.filter(_.intersect(four).length == 3).filter(_.sorted != six)(0).sorted
        val nine = zeroSixNine.filter(_.intersect(four).length == 4)(0).sorted
    
        val three = twoThreeFive.filter(_.intersect(one).length == 2)(0).sorted
        val five = twoThreeFive.filter(_.intersect(nine).length == 5).filter(_.sorted != three)(0).sorted
        val two = twoThreeFive.filter(x => x.sorted != three & x.sorted != five)(0).sorted

        Map(
            zero -> 0,
            one -> 1,
            two -> 2,
            three -> 3,
            four -> 4,
            five -> 5,
            six -> 6,
            seven -> 7,
            eight -> 8,
            nine -> 9
        )
    }

    def decode(sequence: List[String]) = {
        sequence.map(x => mapping(x.sorted))
    }

    override def toString(): String = {
        mapping.toString
    }
}
class Day8Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 8
    case class Input(display: SevenSeg, numbers: List[String])
    val displays = {
        val reader = new InputReader[Input](inputRoot, day)
        reader.readParsedByLine(l => {
            val parts = l.split(" . ")
            Input(new SevenSeg(parts(0).split(" ").toList), parts(1).split(" ").toList)
        }, test, testCase)
    }

    override def part1(): String = {
        // Darkly Dreaming Dexter level of Ds here (show >>> books)
        displays.map(d => d.display.decode(d.numbers).filter(x => List(1, 4, 7, 8).contains(x)).length).sum.toString
    }

    override def part2(): String = {
        displays.map(d => Integer.parseInt(d.display.decode(d.numbers).mkString)).sum.toString
    }
}