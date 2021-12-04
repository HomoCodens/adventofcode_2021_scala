package solvers
import aocutil.InputReader

class Day3Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 3
    type Diagnostic = List[Int]
    val diagnostics = {        
        val reader = new InputReader[Diagnostic](inputRoot, day)
        reader.readParsedByLine(_.split("").toList.map(_.toInt), test, testCase)
    }

    def getBitCounts(diagnostics: List[Diagnostic]) : List[Int] = {
        diagnostics.reduce((a, b) => a.zip(b).map({ case (acc, x) => acc + x}))
    }

    def bitsToInt(bits: List[Boolean]) : Int = {
        Integer.parseInt(bits.map(if (_) 1 else 0).mkString, 2)
    }

    def filterLifeSupport(diagnostics: List[Diagnostic], digit: Int, ogr: Boolean) : List[Diagnostic] = {
        val threshold = diagnostics.length / 2.0f
        val bitCount = getBitCounts(diagnostics)(digit)
        val keep = if(ogr) {
            // Woulda worked more elegantly with booleans
            if(bitCount >= threshold) 1 else 0
        } else {
            if(bitCount< threshold) 1 else 0
        }
        diagnostics.filter(_(digit) == keep)
    }

    // Typoed o twice, kept it for the german lulz
    def runFolter(diagnostics: List[Diagnostic], ogr: Boolean) : Int = {
        var diag = diagnostics
        var digit = 0
        while(diag.length > 1 && digit < diag(0).length) {
            diag = filterLifeSupport(diag, digit, ogr)
            digit += 1
        }
        if(diag.length > 1) {
            throw new RuntimeException("Summat went wrong")
        }
        bitsToInt(diag(0).map(_ == 1))
    }
    override def part1() : String = {
        val threshold: Float = diagnostics.length / 2.0f
        val counts = getBitCounts(diagnostics)
        val gamma = bitsToInt(counts.map(_ >= threshold))
        val epsilon = bitsToInt(counts.map(_ < threshold))
        p(s"Gamma is $gamma")
        p(s"Epsilon is $epsilon")
        (gamma * epsilon).toString
    }

    override def part2() : String = {
        val ogr = runFolter(diagnostics, true)
        val co2 = runFolter(diagnostics, false)
        p(s"Oxygen generator rating: $ogr")
        p(s"CO2 scrubber rating: $co2")
        (ogr * co2).toString
    }
}