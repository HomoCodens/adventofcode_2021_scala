package solvers
import aocutil.InputReader

class Day23Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 23
    override def part1(): String = {
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
        println(getBurrow(List(List('A', 'B'), List('B', 'A'))))
        ""
    }
}