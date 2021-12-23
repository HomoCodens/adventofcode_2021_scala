package solvers
import aocutil.InputReader
import scala.collection.immutable.Queue

class Day23Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 23
    val startingPositions = {
        val reader = new InputReader[List[Queue[Char]]](inputRoot, day)

        def parse(lines: List[String]): List[Queue[Char]] = {
            lines.slice(2, 4).map(_.trim.filterNot(_ == '#').toList).transpose.map(x => Queue(x: _*)) // <- https://stackoverflow.com/questions/22531352/convert-seq-or-list-to-collection-immutable-queue
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    case class Turn(from: Int, to: Int)

    case class Burrow(spaces: List[Space]) {
        def getSpace(at: Int): Space = spaces(at)

        def size(): Int = spaces.length

        def isDone(): Boolean = spaces.forall(_.isDone())

        override def toString(): String = {
            val nRooms = spaces.count({ case Hallway(_) => false; case Room(_, _, _) => true})
            val mapWidth = 2*nRooms + 5
            val top = "#" * mapWidth
            val corridor = spaces.map({ 
                case Hallway(Some(a)) => a
                case _ => "."
            }).mkString("#", "", "#")

            val rooms = (0 until spaces(2).capacity).map(i => spaces.map({
                case Room(_, occupants, cap) => if(cap - i <= occupants.length) occupants(occupants.length - cap + i) else "."
                case _ => "#"
            }).mkString(" ", "", " ")).mkString("\n")
            s"$top\n$corridor\n$rooms"
        }
    }
    abstract class Space {
        val capacity = 1
        def mayEnter(amphipod: Char): Boolean
        def getNextMover(): Option[Char]
        def costToEnter(): Int = 0
        def costToLeave(): Int = 0
        def isHomeTo(amphipod: Char): Boolean = false
        def hasMove(): Boolean = getNextMover().isDefined
        def isDone(): Boolean = true
    }

    // TODO: Generalize costs
    case class Room(homeTo: Char, occupants: Queue[Char], override val capacity: Int) extends Space {
        override def mayEnter(amphipod: Char): Boolean = homeTo == amphipod & occupants.forall(_ == homeTo)
        override def getNextMover(): Option[Char] = if(occupants.length > 0 & !occupants.forall(_ == homeTo)) Some(occupants.head) else None

        def pop() = occupants.dequeue
        override def costToEnter(): Int = capacity - occupants.length
        override def costToLeave(): Int = capacity + 1 - occupants.length

        override def isHomeTo(amphipod: Char): Boolean = amphipod == homeTo

        override def isDone() = occupants.length == capacity & occupants.forall(_ == homeTo)
    }

    case class Hallway(occupant: Option[Char]) extends Space {
        override def mayEnter(amphipod: Char): Boolean = occupant.isEmpty
        override def getNextMover(): Option[Char] = occupant
    }

    def getBurrow(initialOccupants: List[Queue[Char]]): Burrow = {
        var amphipods = "ABCD"
        Burrow(List(Hallway(None), Hallway(None)) ++
                initialOccupants.zipWithIndex.flatMap(x => List(Room(amphipods.charAt(x._2), x._1, x._1.size), Hallway(None))) ++ 
                    List(Hallway(None)))
    }

    def getTurns(burrow: Burrow): List[Turn] = {
        (0 until burrow.size).flatMap(start => {
            val room = burrow.getSpace(start)
            if(!room.hasMove()) {
                List()
            } else {
                def scanBurrow(at: Int, from: Int, amphipod: Char, dir: Int, turns: List[Turn] = List()): List[Turn] = {
                    if(at < 0 | at >= burrow.size()) {
                        turns
                    } else {
                        burrow.getSpace(at) match {
                            // Occupied, end search
                            case Hallway(Some(amphipod)) => turns
                            // Open, add as possible target
                            // Also pattern matching kind of sours the mayEnter method ;P
                            case Hallway(None) => scanBurrow(at + dir, from, amphipod, dir, turns :+ Turn(from, at))
                            case r: Room => {
                                // We can go home
                                if(r.isHomeTo(amphipod) & r.mayEnter(amphipod)) {
                                    List(Turn(from, at))
                                } else {
                                    // Skip over rooms (steps calculated later)
                                    scanBurrow(at + dir, from, amphipod, dir, turns)
                                }
                            }
                        }
                    }
                }

                room.getNextMover match {
                    case None => List()
                    case Some(amphipod) => {
                        val turns = scanBurrow(start - 1, start, amphipod, -1) ++ scanBurrow(start + 1, start, amphipod, 1)
                        room match {
                            // If we are already in a hallway, only rooms (i.e. our home) are valid destinations
                            case Hallway(occupant) => turns.filter(t => burrow.spaces(t.to) match { case Hallway(_) => false; case _ => true })
                            case Room(homeTo, occupants, capacity) => turns
                        }
                    }
                }
            }
        }).toList
    }

    def makeTurn(burrow: Burrow, turn: Turn): Tuple2[Burrow, Int] = {
        val next = Burrow(burrow.spaces.zipWithIndex.map({
            case (space, i) => {
                if(i == turn.from) {
                    space match {
                        case Hallway(occupant) => Hallway(None)
                        case Room(homeTo, occupants, cap) => Room(homeTo, occupants.tail, cap)
                    }
                } else if(i == turn.to) {
                    val amphipod = burrow.spaces(turn.from).getNextMover().get
                    space match {
                        case Hallway(occupant) => Hallway(Some(amphipod))
                        case Room(homeTo, occupants, cap) => Room(homeTo, amphipod +: occupants, cap)
                    }
                } else {
                    space
                }
            }
        }))

        /*val hCost = (turn.from - turn.to).abs
        val lCost = burrow.spaces(turn.from).costToLeave()
        val eCost = burrow.spaces(turn.to).costToEnter()
        println(s"$lCost-$hCost-$eCost")*/
        val cost = ((turn.from - turn.to).abs + 
                        burrow.spaces(turn.from).costToLeave() + 
                            burrow.spaces(turn.to).costToEnter()
                    // I am officially declaring this program a mess
                    // Separation of concerns is all over the place
                    // Stuff is WET
                    // But I will drag it over the finish line and mayhaps clean it up some day
                    )*energyCost(burrow.spaces(turn.from).getNextMover().get)

        (next, cost)
    }

    def energyCost(amphipod: Char): Int = {
        // R says hi
        val LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        scala.math.pow(10, LETTERS.indexOf(amphipod)).toInt
    }

    def play(burrow: Burrow, runningScore: Int = 0, bestScore: Int = Int.MaxValue, turnsTaken: List[Turn] = List()): Tuple2[Int, List[Turn]] = {
        if(runningScore >= bestScore) {
            (Int.MaxValue, turnsTaken)
        } else if(burrow.isDone()) {
            (runningScore, turnsTaken)
        } else {
            val possibleTurns = getTurns(burrow)
            if(possibleTurns.isEmpty) {
                (Int.MaxValue, turnsTaken)
            } else {
                var best = bestScore
                var path: List[Turn] = List()
                possibleTurns.foreach(turn => {
                    val (next, cost) = makeTurn(burrow, turn)
                    val (score, turns) = play(next, runningScore + cost, best, turnsTaken :+ turn)
                    if(best > score) {
                        println(s"new high score: $score!")
                        best = score
                        path = turns
                    }
                })
                (best, path)
            }
        }
    }
    override def part1(): String = {
        //val q = play(getBurrow(startingPositions))
        //println(q)
        /*
            #############
            #...........#
            ###B#A#A#D###
              #D#C#B#C#
              #########
            
            2000
            
            #############
            #.........D.#
            ###B#A#A#.###
              #D#C#B#C#
              #########
            
            7
            
            #############
            #A........D.#
            ###B#A#.#.###
              #D#C#B#C#
              #########

            4
            
            #############
            #AA.......D.#
            ###B#.#.#.###
              #D#C#B#C#
              #########

            50
            
            #############
            #AA.B.....D.#
            ###B#.#.#.###
              #D#C#.#C#
              #########
            
            1100
            
            #############
            #AA.B.....D.#
            ###B#.#C#.###
              #D#.#C#.#
              #########
            
            70
            
            #############
            #AA.......D.#
            ###.#B#C#.###
              #D#B#C#.#
              #########
            
            12000
            
            #############
            #AA.........#
            ###.#B#C#D###
              #.#B#C#D#
              #########
            
            6
            
            #############
            #...........#
            ###A#B#C#D###
              #A#B#C#D#
              #########
            */

            "It's 15237. I like puzzles. Sue me..."
    }

    override def part2(): String = {
        /*
          #D#C#B#A#
          #D#B#A#C#
        */
        val injections: List[List[Char]] = List(List('D', 'D'), List('C', 'B'), List('B', 'A'), List('A', 'C'))
        val burrow2 = getBurrow(startingPositions.zipWithIndex.map({
            case (q, i) => {
                val x = List(q(0), injections(i)(0), injections(i)(1), q(1))
                Queue(x: _*)
            }
        }))
        println(burrow2)
        val q = play(burrow2)
        println(q)
        ""
    }
}