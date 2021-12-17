package solvers
import aocutil.InputReader

class Day13Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1) extends Solver(inputRoot, verbose) {
    case class Point(x: Int, y: Int)
    case class Fold(axis: String, offset: Int)
    val day = 13
    val (points: Set[Point], folds: List[Fold]) = {
        val reader = new InputReader[Tuple2[Set[Point], List[Fold]]](inputRoot, day)

        def parse(lines: List[String]): Tuple2[Set[Point], List[Fold]] = {
            var points = Set[Point]()
            var folds = List[Fold]()
            val PointPattern = "([0-9]+),([0-9]+)".r
            val FoldPattern = "fold along ([a-z])=([0-9]+)".r
            for(l <- lines) {
                l match {
                    case PointPattern(x, y) => points = points + Point(x.toInt, y.toInt)
                    case FoldPattern(axis, offset) => folds = folds :+ Fold(axis, offset.toInt)
                    case _ => {}
                }
            }
            (points, folds)
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    def foldX(points: Set[Point], fold: Fold): Set[Point] = {
        points.filter(_.x < fold.offset) ++ points.filter(_.x > fold.offset).map(p => Point(fold.offset - (p.x - fold.offset), p.y))
    }
    
    def foldY(points: Set[Point], fold: Fold): Set[Point] = {
        points.filter(_.y < fold.offset) ++ points.filter(_.y > fold.offset).map(p => Point(p.x, fold.offset - (p.y - fold.offset)))
    }

    override def part1(): String = {
        var pp = points
        val fold = folds(0)
        if(fold.axis == "x") {
            pp = foldX(pp, fold)
        } else {
            pp = foldY(pp, fold)
        }
        pp.size.toString
    }

    override def part2(): String = {
        val finalPoints = folds.foldLeft(points)((p, f) => {
            f match {
                case Fold("x", offset) => foldX(p, f)
                case Fold("y", offset) => foldY(p, f)
                case _ => throw new RuntimeException(s"Illegal fold in input: $f")
            }
        })
        val xMax = finalPoints.maxBy(_.x).x
        val yMax = finalPoints.maxBy(_.y).y
        val captcha = (0 to yMax).map(y => (0 to xMax).map(x => if(finalPoints.contains(Point(x, y))) "#" else " "))
        "\n" + captcha.map(_.mkString).mkString("\n")
    }
}