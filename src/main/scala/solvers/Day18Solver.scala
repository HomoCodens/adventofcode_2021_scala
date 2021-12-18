package solvers

import aocutil.InputReader

class Day18Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 18 // In which we learn on trees in FP
    val numbers = {
        val reader = new InputReader[SnailFishNumber](inputRoot, day)
        reader.readParsedByLine(l => SnailFishNumber.fromString(l), test, testCase)
    }
    
    abstract class SnailFishNumber {
        def split(): Tuple2[SnailFishNumber, Boolean]

        def splode(): Tuple2[SnailFishNumber, Boolean] = {
            val (out, _, _, sploded) = _splode()
            (out, sploded)
        }
        def _splode(depth: Int = 0): Tuple4[SnailFishNumber, Option[Int], Option[Int], Boolean]

        // Come to think of it... reduce is not the best name
        def reduce(): SnailFishNumber = {
            this.splode() match {
                case (x, true) => x.reduce()
                case (x, false) => {
                    this.split() match {
                        case (x, true) => x.reduce()
                        case (x, false) => x
                    }
                }
            }
        }

        def +(other: SnailFishNumber): SnailFishNumber = SnailFishPair(this, other).reduce()
        
        def getValue(): Int
        
        def insertLeft(x: Int): SnailFishNumber
        def insertRight(x: Int): SnailFishNumber
        
        def magnitude(): Int
    }

    object SnailFishNumber {
        def fromString(stringRep: String): SnailFishPair = {
            val (x: SnailFishPair, _) = SnailFishNumber(stringRep.split("").toList)
            x
        }
        
        def apply(tokens: List[String]): Tuple2[SnailFishNumber, List[String]] = {
            tokens match {
                case "[" :: tail => {
                    val (left, tailLeft) = SnailFishNumber(tail)
                    val (right, tailRight) = SnailFishNumber(tailLeft)

                    // Recursive calls clean up the "," and "]" so we might end up with an empty tail
                    (SnailFishNumber(left, right), if(tailRight.length > 0) tailRight.tail else List())
                }
                case value :: tail => (SnailFishNumber(value.toInt), tail.tail)
                case Nil => throw new RuntimeException("Failed to parse SnailFishNumber. Please check input.")
            }
        }
        
        // TODO: Are these stylistically necessary/proper here instead of the children?
        def apply(value: Int): SnailFishNumber = SnailFishRegular(value)
        def apply(left: Int, right: Int): SnailFishNumber = SnailFishPair(SnailFishRegular(left), SnailFishRegular(right))
        def apply(left: SnailFishNumber, right: SnailFishNumber): SnailFishNumber = SnailFishPair(left, right)
    }



    case class SnailFishRegular(value: Int) extends SnailFishNumber {
        override def split(): Tuple2[SnailFishNumber, Boolean] = {
            if(value < 10) {
                (SnailFishRegular(value), false)
            } else {
                (SnailFishNumber(math.floor(value / 2.0f).toInt , math.ceil(value / 2.0f).toInt), true)
            }
        }

        override def _splode(depth: Int = 0): Tuple4[SnailFishNumber, Option[Int], Option[Int], Boolean] = {
            // Regulars can't explode
            (SnailFishRegular(value), None, None, false)
        }

        override def insertLeft(x: Int) = SnailFishRegular(value + x)

        override def insertRight(x: Int) = SnailFishRegular(value + x)

        override def getValue(): Int = value

        override def magnitude(): Int = value

        override def toString(): String = value.toString
    }



    case class SnailFishPair(left: SnailFishNumber, right: SnailFishNumber) extends SnailFishNumber {
        override def split(): Tuple2[SnailFishNumber, Boolean] = {
            left.split() match {
                case (nn, true) => (SnailFishPair(nn, right), true)
                case (_, false) => {
                    val (rNew, splitHappened) = right.split()
                    (SnailFishPair(left, rNew), splitHappened)
                }
            }
        }

        override def _splode(depth: Int = 0): Tuple4[SnailFishNumber, Option[Int], Option[Int], Boolean] = {
            if(depth == 4) {
                // It works because we know the sum of two properly reduced SnailFishNumbers
                // can not get any deeper so left and right must be Regular
                // Could be done more safely
                (SnailFishRegular(0), Some(left.getValue()), Some(right.getValue()), true)
            } else {
                val (lNew, lLFragment, lRFragment, sploded) = left._splode(depth + 1)
                if(sploded) {
                    lRFragment match {
                        // We came up from the left and have not dropped the right fragment
                        // back down yet.
                        case Some(rf) => (SnailFishPair(lNew, right.insertLeft(rf)), lLFragment, None, true)
                        case None => (SnailFishPair(lNew, right), lLFragment, lRFragment, true)
                    }
                } else {
                    // No splosion on the left branch, try the right
                    val (rNew, rLFragment, rRFragment, sploded) = right._splode(depth + 1)
                    if(sploded) {
                        rLFragment match {
                            // Vice versa above
                            case Some(lf) => (SnailFishPair(left.insertRight(lf), rNew), None, rRFragment, true)
                            case None => (SnailFishPair(left, rNew), lLFragment, rRFragment, true)
                        }
                    } else {
                        (SnailFishPair(left, right), None, None, false)
                    }
                }
            }
        }

        def insertLeft(x: Int) = SnailFishPair(left.insertLeft(x), right)

        def insertRight(x: Int) = SnailFishPair(left, right.insertRight(x))

        override def getValue() = throw new RuntimeException("Tried getting value from SnailFishPair")

        override def magnitude(): Int = 3*left.magnitude() + 2*right.magnitude()

        override def toString(): String = s"[${left.toString},${right.toString}]"
    }

    override def part1(): String = {
        val sum = numbers.reduce((a, b) => a + b)
        p(s"The sum of all the numbers is $sum")
        sum.magnitude().toString()
    }
    
    override def part2(): String = {
        val n = numbers.length

        // Don't constrain j to > i because we WANT both orders
        val indices = for(i <- 0 until n; j <- 0 until n if i != j) yield (numbers(i), numbers(j))
        indices.foldLeft(0)({ case (acc, (a, b)) => List(acc, (a+b).magnitude()).max}).toString
    }
}
