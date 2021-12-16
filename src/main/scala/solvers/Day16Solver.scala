package solvers
import aocutil.InputReader

class Day16Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 15
    type BITSSequence = List[Int] // Hoping to wrangle some sort of lazily evaluated expandor our of this
    
    def decimalize(bits: BITSSequence): Int = {
        Integer.parseInt(bits.mkString, 2)
    }

    // Literal: vvv|100|1mmmm|1nnnn|...|0zzzz -> mmmmnnnn....zzzz binary
    // Operator:
    //  Bitlength: vvv|ooo|0|lllllllllllllll|ddddd... -> 15 l bits indicate number of d bits
    //  Packetlength: vvv|ooo|1|lllllllllll|dddddd... -> 11 l bits indicate number of subpackets in d


    abstract class BITS(version: Int) {
        def checkSum(): Int
        def apply(): Int
    }

    object BITS {
        def bootstrap(bits: BITSSequence): Tuple2[List[BITS], BITSSequence] = {
            val (version, t1) = bits.splitAt(3)
            val (typeID, t2) = t1.splitAt(3)

            decimalize(typeID) match {
                case 4 => List(BITSLiteral.fromBits(decimalize(version)), t2)
                case op => BITSOperator.fromBits(decimalize(version), op, t2)
            }
        }
    }

    class BITSLiteral(version: Int, value: Int) extends BITS(version) {
        override def checkSum(): Int = version
        override def apply(): Int = value

        override def toString(): String = {
            s"Literal, v$version, value: $value"
        }
    }
    
    object BITSLiteral {
        def fromBits(version: Int, bits: BITSSequence): Tuple2[BITSLiteral, BITSSequence] = {
            def parseLiteral(bits: BITSSequence, value: BITSSequence = List()): Tuple2[Int, BITSSequence] = {
                val (chunk, tail) = bits.splitAt(5)
                chunk match {
                    case 1 :: v => parseLiteral(tail, value ++ v)
                    case 0 :: v => (decimalize(value ++ v), tail)
                }
            }
            val (value, tail) = parseLiteral(bits)
            (new BITSLiteral(version, value), tail)
        }
    }

    class BITSOperator(version: Int, op: Int, children: List[BITS]) extends BITS(version) {
        override def checkSum(): Int = {
            version + children.map(_.checkSum()).sum
        }

        override def apply(): Int = 3
    }
    
    object BITSOperator {
        def fromBits(version: Int, op: Int, bits: BITSSequence): Tuple2[List[BITS], BITSSequence] = {
            bits match {
                case 0 :: tail => {
                    val (nBits, t1) = tail.splitAt(15)
                    // TODO: make up an unapply-able decimalize
                    val nBitsDec = decimalize(nBits)
                    val (myBits, notMyBits) = t1.splitAt(nBitsDec)
                    val (children, rest) = BITS.bootstrap(myBits)
                    (new BITSOperator(version, op, children), notMyBits)
                }
                case 1 :: tail => {
                    val (nPackets, t1) = tail.splitAt(11)
                    val nPacketsDec = decimalize(nPackets)

                    def parseNPackets(bits: BITSSequence, n: Int, done: Int = 0, packets: List[BITS] = List()): Tuple2[List[BITS], BITSSequence] = {
                        
                    }
                }
            }
            (new BITSOperator(1, 2, List()), List())
        }
    }

    override def part1(): String = {
        val (l, tail) = BITS.bootstrap(List(1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0))
        println(l)
        println(tail)
        ""
    }
    
    override def part2(): String = {
        ""
    }
}
