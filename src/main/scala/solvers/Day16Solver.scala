package solvers
import aocutil.InputReader
import scala.math.BigInt

class Day16Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 16
    val hex = {
        val reader = new InputReader(inputRoot, day)
        reader.readText(test, testCase)(0)
    }

    type BITSSequence = List[Int] // Hoping to wrangle some sort of lazily evaluated expandor our of this

    def hexToBits(hex: String): BITSSequence = {
        Integer.parseInt(hex, 16).toBinaryString.reverse.padTo(4, '0').reverse.sliding(1).map(_.toInt).toList
    }

    def getBits(hex: String): BITSSequence = {
        hex.sliding(1).map(hexToBits(_)).toList.flatten
    }

    // Literal: vvv|100|1mmmm|1nnnn|...|0zzzz -> mmmmnnnn....zzzz binary
    // Operator:
    //  Bitlength: vvv|ooo|0|lllllllllllllll|ddddd... -> 15 l bits indicate number of d bits
    //  Packetlength: vvv|ooo|1|lllllllllll|dddddd... -> 11 l bits indicate number of subpackets in d


    object BITSDecimal {
        // def apply(value: nt): BITSSequence = Integer.toBinaryString(value).split("").map(_.toInt).toList
        def unapply(bits: BITSSequence): Option[BigInt] = {
            val padLength = ((bits.length / 8) + 1)*8
            Some(BigInt(bits.reverse.padTo(padLength, 0).reverse.grouped(8).map(x => Integer.parseInt(x.mkString, 2).toByte).toArray))
        }
    }

    abstract class BITS(version: Int) {
        def checkSum(): Int

        // Method Apply, no relation
        def apply(): BigInt
    }

    object BITS {
        def bootstrap(bits: BITSSequence, packets: List[BITS] = List[BITS](), nPackets: Int = Integer.MAX_VALUE): Tuple2[List[BITS], BITSSequence] = {
            p(s"Parsed ${packets.length}/${if(nPackets < Integer.MAX_VALUE) nPackets else "Inf"}")
            if(packets.length == nPackets | bits.length == 0 | bits.forall(_ == 0)) {
                (packets, bits)
            } else {
                // TODO: Is BITSDecimal(x, 3) :: BITSDecimal(y, 3) :: tail possible somehow?
                val (BITSDecimal(version), t1) = bits.splitAt(3)
                val (BITSDecimal(typeID), t2) = t1.splitAt(3)
                p(s"Packet of version $version, type $typeID")
                typeID.toInt match {
                    case 4 => {
                        p("Literal...")
                        val (literal, tail) = BITSLiteral.fromBits(version.toInt, t2)
                        bootstrap(tail, packets :+ literal, nPackets)
                    }
                    case op => {
                        p("Operator...")
                        val (operator, tail) = BITSOperator.fromBits(version.toInt, op, t2)
                        bootstrap(tail, packets :+ operator, nPackets)
                    }
                }
            }
        }
    }

    class BITSLiteral(version: Int, value: BigInt) extends BITS(version) {
        override def checkSum(): Int = version
        override def apply(): BigInt = value

        override def toString(): String = {
            s"Literal(v$version, value: $value)"
        }
    }
    
    object BITSLiteral {
        def fromBits(version: Int, bits: BITSSequence): Tuple2[BITSLiteral, BITSSequence] = {

            def parseLiteral(bits: BITSSequence, value: BITSSequence = List()): Tuple2[BigInt, BITSSequence] = {
                val (chunk, tail) = bits.splitAt(5)
                p(s"Matching aginst $chunk")
                chunk match {
                    case 1 :: v => parseLiteral(tail, value ++ v)
                    case 0 :: v => {
                        p(s"Got bits of value: ${(value ++ v).mkString}")
                        val BITSDecimal(result) = (value ++ v)
                        p(s"Parsing literal done, got value $result")
                        (result, tail)
                    }
                }
            }
            p(s"Parsing literal ${bits.mkString}")
            val (value, tail) = parseLiteral(bits)
            (new BITSLiteral(version, value), tail)
        }
    }

    class BITSOperator(version: Int, op: Int, children: List[BITS]) extends BITS(version) {
        override def checkSum(): Int = {
            version + children.map(_.checkSum()).sum
        }

        override def apply(): BigInt = {
            val childValues = children.map(_.apply())
            op match {
                case 0 => childValues.sum
                case 1 => childValues.product
                case 2 => childValues.min
                case 3 => childValues.max
                case 5 => if(childValues(0) > childValues(1)) 1 else 0
                case 6 => if(childValues(0) < childValues(1)) 1 else 0
                case 7 => if(childValues(0) == childValues(1)) 1 else 0
            }
        }

        override def toString(): String = {
            s"Operator(v$version, op$op, [${children.map(_.toString).mkString(", ")}])"
        }
    }
    
    object BITSOperator {
        def fromBits(version: Int, op: Int, bits: BITSSequence): Tuple2[BITSOperator, BITSSequence] = {
            bits match {
                case 0 :: tail => {
                    p("Type 0, bit length...")
                    val (BITSDecimal(nBits), t1) = tail.splitAt(15)
                    val (myBits, notMyBits) = t1.splitAt(nBits.toInt)
                    p(s"Descending for the next $nBits bits...")
                    val (children, rest) = BITS.bootstrap(myBits)
                    p(s"Came back up with ${children.length} children...")
                    (new BITSOperator(version, op, children), notMyBits)
                }
                case 1 :: tail => {
                    p("Type 1, packet number...")
                    val (BITSDecimal(nPackets), t1) = tail.splitAt(11)
                    p(s"Descending for the next $nPackets packets...")
                    val (children, rest) = BITS.bootstrap(t1, nPackets = nPackets.toInt)
                    p(s"Came back up with ${children.length} children...")
                    (new BITSOperator(version, op, children), rest)
                }
            }
        }
    }

    override def part1(): String = {
        val bits = BITS.bootstrap(getBits(hex))._1(0)
        s"The checksum for the message is ${bits.checkSum().toInt}"
    }
    
    override def part2(): String = {
        val bits = BITS.bootstrap(getBits(hex))._1(0)
        s"The message is ${bits.apply()}"
    }
}
