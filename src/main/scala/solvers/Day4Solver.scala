package solvers

import aocutil.InputReader

case class BingoNumber(number: Int, marked: Boolean = false)

class BingoBoard(input: List[Int], dim: Int = 5) {
    var numbers = input.map(BingoNumber(_))

    def isWin(): Boolean = {
        // Horizontal
        val h = numbers.grouped(dim).map(_.forall(_.marked)).exists(a => a)
        // Vertical
        // Also TODO: unspaghetti
        val v = numbers.zipWithIndex.map({ case (n, i) => (n, i % dim)}).groupBy(_._2).map( x => x._2.forall(_._1.marked)).exists(a => a)
        h || v
    }

    def getUnmarked(): List[Int] = {
        numbers.filter(!_.marked).map(_.number)
    }

    def markNumber(numberToMark: Int) : Unit = {
        numbers = numbers.map({ 
            case BingoNumber(number, marked) if number == numberToMark => BingoNumber(number, true)
            case n:BingoNumber => n
        })
    }

    override def toString(): String = {
        numbers.grouped(dim).map(_.map({ 
            case BingoNumber(n, true) => s"[$n]"
            case BingoNumber(n, false) => n.toString
        }).mkString(" ")).mkString("\n")
   }
}

class Bingo(input: List[String]) {
    // A proper bingo game would have a drawNumber(Int?) method
    // but we're playing by Giant Squid Rules
    val numbers: List[Int] = input(0).split(",").map(_.toInt).toList
    var currentNumber: Int = 0
    var boards: List[BingoBoard] = {
        val boardInputs = input.slice(2, input.length)
        var nextBoard: List[Int] = List()
        var boards: List[BingoBoard] = List()
        for(l <- boardInputs) {
            if(l == "") {
                boards = (new BingoBoard(nextBoard) :: boards)
                nextBoard = List()
            } else {
                nextBoard = nextBoard ++ "[ ]+".r.split(l.trim).map(_.trim.toInt).toList
            }
        }
        boards = (new BingoBoard(nextBoard) :: boards)
        boards
    }

    def getWinningBoard(): Int = {
        boards.indexWhere(_.isWin)
    }

    def playTurn(): Unit = {
        boards.foreach(_.markNumber(numbers(currentNumber)))
        currentNumber += 1
        /*boards.foreach({ x =>
            println(x)
            println()
        })
        println()*/
    }

    def play(): Int = {
        var winner: Int = -1
        while(winner < 0) {
            playTurn()
            winner = getWinningBoard()
        }
        val winningBoard = boards(winner)
        val sumOfUnmarked = winningBoard.getUnmarked.sum
        println(sumOfUnmarked)
        sumOfUnmarked * numbers(currentNumber - 1)
    }

    def playToLose(): Int = {
        while(boards.length > 1) {
            playTurn()
            boards = boards.filterNot(_.isWin)
        }
        play()
    }
}

class Day4Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    val day = 4
    val input = {
        val reader = new InputReader(inputRoot, day)
        reader.readText(test, testCase)
    }

    override def part1(): String = {
        // You lose
        val theGame = new Bingo(input)
        theGame.play().toString
    }

    override def part2(): String = {
        val theGame = new Bingo(input)
        theGame.playToLose().toString
    }
}