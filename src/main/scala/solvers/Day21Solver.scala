package solvers
import aocutil.InputReader
import scala.math.BigInt

// Just so you know, Jeff, you are not the only one to have watched a Series
// that should have stopped 3 seasons earlier.
class Day21Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 21
    val List(startP1, startP2) = {
        val reader = new InputReader[Int](inputRoot, day)

        def parse(l: String): Int = {
            // Let's just say players must always be specified in order
            val PlayerPattern = raw"Player \d starting position: (\d)".r
            l match {
                case PlayerPattern(start) => start.toInt
            }
        }

        reader.readParsedByLine(parse, test, testCase)
    }

    // A map of (p1Total, p1PositionOnRing, p2Total, p2PositionOnRing) -> number of universes in this state at step X
    type Multiverse = Map[Tuple4[Int, Int, Int, Int], BigInt]

    // TODO: Is there inlining in scala?
    def getScore(position: Int, roll: Int): Int = {
        (((position - 1) + roll) % 10) + 1
    }

    def playDirac(p1: Int, p2: Int): List[BigInt] = {
        // Turns are to be 0 based
        // Turn 0 -> player1's first turn
        // Turn 1 -> player2's first turn
        // Turn 2 -> player1's second turn
        // ...

        def stepVerses(universes: Multiverse, turn: Int): Multiverse = {
            // Steps rolled -> number of universes with that result
            val crossroads = Map[Int, BigInt](
                3 -> 1,
                4 -> 3,
                5 -> 6,
                6 -> 7,
                7 -> 6,
                8 -> 3,
                9 -> 1
            )
            
            universes.map({
                case ((t1, s1, t2, s2), n) => crossroads.map({
                    case (roll, m) if turn % 2 == 0 => {
                        val score = getScore(s1, roll)
                        // Number of 'verses in the new state is number in the previous states * number of verses with this outcome
                        ((t1 + score, score, t2, s2), m*n)
                    }
                    case (roll, m) if turn % 2 == 1 => {
                        val score = getScore(s2, roll)
                        ((t1, s1, t2 + score, score), m*n)
                    }
                })
            }).flatten.groupMapReduce(_._1)(_._2)(_ + _) // <- black sorcery
        }

        def rec(universes: Multiverse, wins: List[BigInt], turn: Int): Tuple2[Multiverse, List[BigInt]] = {
            if(universes.size == 0) {
                (Map(), wins)
            } else {
                val newVerses = stepVerses(universes, turn).groupBy({ case (((t1, _, t2, _)), n) => t1 >= 21 | t2 >= 21})
                val winsThisTurn: BigInt = newVerses.get(true) match {
                    case Some(x) => x.values.sum
                    case None => 0
                }
                rec(newVerses.getOrElse(false, Map()),
                    if(turn % 2 == 0) List(wins(0) + winsThisTurn, wins(1))
                     else List(wins(0), wins(1) + winsThisTurn),
                    turn + 1)
            }
        }
        
        rec(Map((0, p1, 0, p2) -> 1), List(0, 0), 0)._2
    }

    override def part1(): String = {
        // With the determidie the scores will cycle thusly
        val rolls = List(6, 5, 4, 3, 2, 1, 0, 9, 8, 7)

        // Each player always gets the same rolls
        val rollsP1 = rolls.filter(_ % 2 == 0)
        val rollsP2 = rolls.filter(_ % 2 == 1)

        // Worked out the scores handedly. p1 lands in the same state every 5 rolls, p2 every 10
        val scoresP1 = Stream.continually(List(10, 4, 6, 6, 4).toStream).flatten.scanLeft(0)(_ + _).takeWhile(_ <= 1000).toList
        val scoresP2 = Stream.continually(List(7, 10, 1, 10, 7, 2, 5, 6, 5, 2).toStream).flatten.scanLeft(0)(_+_).takeWhile(_ <= 1000).toList

        val p1TurnsToWin = scoresP1.length - 1 // -1 because 0 is included
        val p2TurnsToWin = scoresP2.length - 1

        if(p1TurnsToWin < p2TurnsToWin) {
            val rolls = (2*p1TurnsToWin - 1)*3
            val p2Score = scoresP2(p1TurnsToWin - 1)
            s"Player 1 wins! The answer is ${rolls} die rolls * ${p2Score} points = ${rolls*p2Score} dierollpoints."
        } else {
            val rolls = 2*p2TurnsToWin*3
            val p1Score = scoresP1(p2TurnsToWin)
            s"Player 2 wins! The answer is ${rolls} die rolls * ${p1Score} points = ${rolls*p1Score} dierollpoints."
        }
    }

    override def part2(): String = {
        val verses = playDirac(startP1, startP2)
        val p1Wins = verses(0) > verses(1)
        s"Player ${if(p1Wins) 1 else 2} wins in ${verses.max} universes (vs ${verses.min})."
    }
}