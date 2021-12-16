package solvers
import aocutil.InputReader

class Day16Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 15
    val hex = "620D79802F60098803B10E20C3C1007A2EC4C84136F0600BCB8AD0066E200CC7D89D0C4401F87104E094FEA82B0726613C6B692400E14A305802D112239802125FB69FF0015095B9D4ADCEE5B6782005301762200628012E006B80162007B01060A0051801E200528014002A118016802003801E2006100460400C1A001AB3DED1A00063D0E25771189394253A6B2671908020394359B6799529E69600A6A6EB5C2D4C4D764F7F8263805531AA5FE8D3AE33BEC6AB148968D7BFEF2FBD204CA3980250A3C01591EF94E5FF6A2698027A0094599AA471F299EA4FBC9E47277149C35C88E4E3B30043B315B675B6B9FBCCEC0017991D690A5A412E011CA8BC08979FD665298B6445402F97089792D48CF589E00A56FFFDA3EF12CBD24FA200C9002190AE3AC293007A0A41784A600C42485F0E6089805D0CE517E3C493DC900180213D1C5F1988D6802D346F33C840A0804CB9FE1CE006E6000844528570A40010E86B09A32200107321A20164F66BAB5244929AD0FCBC65AF3B4893C9D7C46401A64BA4E00437232D6774D6DEA51CE4DA88041DF0042467DCD28B133BE73C733D8CD703EE005CADF7D15200F32C0129EC4E7EB4605D28A52F2C762BEA010C8B94239AAF3C5523CB271802F3CB12EAC0002FC6B8F2600ACBD15780337939531EAD32B5272A63D5A657880353B005A73744F97D3F4AE277A7DA8803C4989DDBA802459D82BCF7E5CC5ED6242013427A167FC00D500010F8F119A1A8803F0C62DC7D200CAA7E1BC40C7401794C766BB3C58A00845691ADEF875894400C0CFA7CD86CF8F98027600ACA12495BF6FFEF20691ADE96692013E27A3DE197802E00085C6E8F30600010882B18A25880352D6D5712AE97E194E4F71D279803000084C688A71F440188FB0FA2A8803D0AE31C1D200DE25F3AAC7F1BA35802B3BE6D9DF369802F1CB401393F2249F918800829A1B40088A54F25330B134950E0"
    /*{
        val reader = new InputReader(inputRoot, day)
        reader.readText(test, testCase)
    }*/

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
        def apply(value: Int): BITSSequence = Integer.toBinaryString(value).split("").map(_.toInt).toList
        def unapply(bits: BITSSequence): Option[Int] = Some(Integer.parseInt(bits.mkString, 2))
    }

    abstract class BITS(version: Int) {
        def checkSum(): Int
        def apply(): Int
    }

    object BITS {
        def bootstrap(bits: BITSSequence, packets: List[BITS] = List[BITS](), nPackets: Int = Integer.MAX_VALUE): Tuple2[List[BITS], BITSSequence] = {
            if(packets.length == nPackets | bits.length == 0 | bits.forall(_ == 0)) {
                (packets, bits)
            } else {
                // TODO: Is BITSDecimal(x, 3) :: BITSDecimal(y, 3) :: tail possible somehow?
                val (BITSDecimal(version), t1) = bits.splitAt(3)
                val (BITSDecimal(typeID), t2) = t1.splitAt(3)
                p(s"Packet of version $version, type $typeID")
                typeID match {
                    case 4 => {
                        p("Literal...")
                        val (literal, tail) = BITSLiteral.fromBits(version, t2)
                        bootstrap(tail, packets :+ literal)
                    }
                    case op => {
                        p("Operator...")
                        val (operator, tail) = BITSOperator.fromBits(version, op, t2)
                        bootstrap(tail, packets :+ operator)
                    }
                }
            }
        }
    }

    class BITSLiteral(version: Int, value: Int) extends BITS(version) {
        override def checkSum(): Int = version
        override def apply(): Int = value

        override def toString(): String = {
            s"Literal(v$version, value: $value)"
        }
    }
    
    object BITSLiteral {
        def fromBits(version: Int, bits: BITSSequence): Tuple2[BITSLiteral, BITSSequence] = {
            def parseLiteral(bits: BITSSequence, value: BITSSequence = List()): Tuple2[Int, BITSSequence] = {
                val (chunk, tail) = bits.splitAt(5)
                println(s"Matching aginst $chunk")
                chunk match {
                    case 1 :: v => parseLiteral(tail, value ++ v)
                    case 0 :: v => {
                        println(s"Got bits of value: ${(value ++ v).mkString}")
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

        override def apply(): Int = 3

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
                    val (myBits, notMyBits) = t1.splitAt(nBits)
                    p(s"Descending for the next $nBits bits...")
                    val (children, rest) = BITS.bootstrap(myBits)
                    p(s"Came back up with ${children.length} children...")
                    (new BITSOperator(version, op, children), notMyBits)
                }
                case 1 :: tail => {
                    p("Type 1, packet number...")
                    val (BITSDecimal(nPackets), t1) = tail.splitAt(11)
                    println(s"Descending for the next $nPackets packets...")
                    val (children, rest) = BITS.bootstrap(t1, nPackets = nPackets)
                    p(s"Came back up with ${children.length} children...")
                    (new BITSOperator(version, op, children), rest)
                }
            }
        }
    }

    override def part1(): String = {
        /*val x = BITSDecimal(4)
        println(x)
        val q = List(1, 0, 1, 1, 1, 0, 1)
        val (BITSDecimal(y), t) = q.splitAt(3)
        println(y)
        println(t)

        val (l, tt) = BITS.bootstrap(List(1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
        println(l)*/

        /*println()
        println(BITS.bootstrap(getBits("D2FE28")))
        println()
        println(BITS.bootstrap(getBits("38006F45291200")))
        println()
        println(BITS.bootstrap(getBits("EE00D40C823060")))
        println()
        println(BITS.bootstrap(getBits("8A004A801A8002F478")))
        println(BITS.bootstrap(getBits("8A004A801A8002F478"))._1(0).checkSum)
        println()
        println(BITS.bootstrap(getBits("620080001611562C8802118E34")))
        println(BITS.bootstrap(getBits("620080001611562C8802118E34"))._1(0).checkSum)
        println()
        println(BITS.bootstrap(getBits("C0015000016115A2E0802F182340")))
        println(BITS.bootstrap(getBits("C0015000016115A2E0802F182340"))._1(0).checkSum)
        println()
        println(BITS.bootstrap(getBits("A0016C880162017C3686B18A3D4780")))
        println(BITS.bootstrap(getBits("A0016C880162017C3686B18A3D4780"))._1(0).checkSum)
        println("and for the grand finale:")
        println(BITS.bootstrap(getBits(hex)))*/
        //println(getBits(hex).take(1000).mkString)
        
        println(BITS.bootstrap(getBits(hex))._1(0).checkSum)

        ""
    }
    
    override def part2(): String = {
        ""
    }
}
