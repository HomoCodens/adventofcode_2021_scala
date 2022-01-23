package solvers
import aocutil.InputReader

class Day19Solver(inputRoot: String,
                 verbose: Boolean = false,
                 test: Boolean = false,
                 testCase: Int = 1,
                 timeSolutions: Boolean = false) extends Solver(inputRoot, verbose, timeSolutions = timeSolutions) {
    val day = 19
    val clouds = {
        val reader = new InputReader[List[PointCloud]](inputRoot, day)

        def parse(lines: List[String]): List[PointCloud] = {
            val ScannerPattern = raw"--- scanner (\d+) ---".r
            val ProbePattern = raw"(-?\d+),(-?\d+),(-?\d+)".r
            def rec(ll: List[String], currentPoints: List[Point3D], currentClouds: List[PointCloud], nextId: Int): List[PointCloud] = {
                ll match {
                    case ScannerPattern(id) :: tail => rec(tail, currentPoints, currentClouds, nextId = id.toInt)
                    case ProbePattern(x, y, z) :: tail => rec(tail, currentPoints :+ Point3D(x.toInt, y.toInt, z.toInt), currentClouds, nextId)
                    case List() => currentClouds :+ PointCloud(currentPoints, id = nextId)
                    case _ :: tail => rec(tail, List(), currentClouds :+ PointCloud(currentPoints, id = nextId), nextId)
                }
            }

            rec(lines, List(), List(), 0)
        }

        reader.readParsedWhole(parse, test, testCase)
    }

    // The fancy way:
    // https://stackoverflow.com/questions/21787505/list-sum-on-custom-class
    // Also there's https://www.scalabel.ai/doc/point-cloud.html but lib-mania was last year
    case class Point3D(x: Float, y: Float, z: Float) {
        // Operators (Add as needed)
        def +(other: Point3D): Point3D = Point3D(x + other.x, y + other.y, z + other.z)
        def unary_-(): Point3D = Point3D(-x, -y, -z)
        def -(other: Point3D): Point3D = this + -other
        def dot(other: Point3D): Point3D = Point3D(x * other.x, y * other.y, z * other.z)
        def /(other: Point3D): Point3D = Point3D(x / other.x, y / other.y, z / other.z)
        def /(other: Float): Point3D = Point3D(x / other, y / other, z / other)
        def mhd(other: Point3D): Float = (this - other).abs.sum
        def sum(): Float = x + y + z
        // Sqrt is for slowpokes
        def megatude(): Float = x*x + y*y + z*z
        def abs(): Point3D = Point3D(x.abs, y.abs, z.abs) 
    }

    case class PointCloud(points: List[Point3D],
                            p2p: List[Tuple3[Float, Int, Int]] = List(),
                            scanners: List[Point3D] = List(Point3D(0f, 0f, 0f)),
                            id: Int = 0) {
        val pointToPoint: List[Tuple3[Float, Int, Int]] = if(p2p.length > 0) {
            p2p
        } else {
            // This is where we lernt to stop writing for(i <- 0 until x; j <- i + 1 until x - i) yield (i, j) =D
            // Lost the SO link to a Windows update reboot unfortunately...
            points.toList.zipWithIndex.tails.flatMap({
                case (p1, i) :: tail => tail.map({ case (p2, j) => ((p1 - p2).megatude(), i, j)})
                case List() => Nil
            }).toList
        }

        def overlaps(other: PointCloud): Boolean = {
            // At least 12 points overlap -> at least sum(1..11) point to point distances are the same
            // 11, not 12, because we omit distances from points to themselves
            pointToPoint.map(_._1).intersect(other.pointToPoint.map(_._1)).size >= 66
        }

        def mergeMany(others: List[PointCloud]): PointCloud = {
            others.foldLeft(this)((a, b) => a.merge(b))
        }

        def merge(other: PointCloud): PointCloud = {
            p(s"Merging ${other.id} into $id...")

            // Find the indices in both clouds of all point pairs that share a distance
            val commonDistances = other.pointToPoint.flatMap(x => {
                pointToPoint.find({ case (d2, i2, j2) => x._1 == d2 }) match {
                    case Some((d1, i1, j1)) => Some(((i1, j1), (x._2, x._3)))
                    case None => None
                }
            })

            
            // The first and last matched pairs ought to be distinct and yield a solution
            // We need to pass all 4 since we still don't know which is which within a pair
            val firstPair = commonDistances(0)
            val secondPair = commonDistances(commonDistances.length - 1)
            
            val transform = sussOutTransform(
                List(
                        points(firstPair._1._1),
                        points(firstPair._1._2),
                        points(secondPair._1._1),
                        points(secondPair._1._2)
                    ),
                    List(
                        other.points(firstPair._2._1),
                        other.points(firstPair._2._2),
                        other.points(secondPair._2._1),
                        other.points(secondPair._2._2)
                )
            )

            PointCloud(
                points ++ other.points.map(transform(_)),
                // we need not calculate all point2point distances of the new cloud but still update the indices of the newly
                // joined ones in the new list of points
                pointToPoint ++ other.pointToPoint.map({ case (d, i, j) => (d, i + points.length, j + points.length)}),
                scanners ++ other.scanners.map(transform(_))
            )
        }

        def sussOutTransform(pointsA: List[Point3D], pointsB: List[Point3D]): (Point3D) => Point3D = {
            val centroidA = pointsA.reduce((a, b) => a + b) / pointsA.length.toFloat
            val centroidB = pointsB.reduce((a, b) => a + b) / pointsB.length.toFloat

            // We can now finally find out which pointA corresponds to which pointB by sorting them by
            // distance from their respective centroids
            // Also transform them into a scanner-invariant space
            val pointsASorted = pointsA.sortWith((a, b) => (a - centroidA).megatude() < (b - centroidA).megatude())
            val pointsACentered = pointsASorted.map(_ - centroidA)
            val pointsBSorted = pointsB.sortWith((a, b) => (a - centroidB).megatude() < (b - centroidB).megatude())
            val pointsBCentered = pointsBSorted.map(_ - centroidB)

            // We could get all matrixy and linalgy here
            // and there are even libraries for that but since we got all the way here in
            // base Scala and we are only dealing with simple 90 degree rotations...
            val pA = pointsACentered(0)
            val pB = pointsBCentered(0)

            // Columns of rotation matrix R such that Rxb = a
            val xRot = Point3D(
                if(pB.x.abs == pA.x.abs) pB.x.sign * pA.x.sign else 0,
                if(pB.x.abs == pA.y.abs) pB.x.sign * pA.y.sign else 0,
                if(pB.x.abs == pA.z.abs) pB.x.sign * pA.z.sign else 0
            )
            val yRot = Point3D(
                if(pB.y.abs == pA.x.abs) pB.y.sign * pA.x.sign else 0,
                if(pB.y.abs == pA.y.abs) pB.y.sign * pA.y.sign else 0,
                if(pB.y.abs == pA.z.abs) pB.y.sign * pA.z.sign else 0
            )
            val zRot = Point3D(
                if(pB.z.abs == pA.x.abs) pB.z.sign * pA.x.sign else 0,
                if(pB.z.abs == pA.y.abs) pB.z.sign * pA.y.sign else 0,
                if(pB.z.abs == pA.z.abs) pB.z.sign * pA.z.sign else 0
            )

            val rot = List(xRot, yRot, zRot)
            val trans = centroidA - simpleTransform(rot, Point3D(0, 0, 0))(centroidB)

            simpleTransform(rot, trans)
        }

        // Point3D, Vector, Column... all the same to me ;)
        def simpleTransform(rot: List[Point3D], trans: Point3D)(p: Point3D): Point3D = {
            Point3D(
                p.x * rot(0).x + p.y * rot(1).x + p.z * rot(2).x,
                p.x * rot(0).y + p.y * rot(1).y + p.z * rot(2).y,
                p.x * rot(0).z + p.y * rot(1).z + p.z * rot(2).z
            ) + trans
        }

        def size(): Int = points.size
    }

    object PointCloud {
        def apply(points: List[Point3D]): PointCloud = {
            new PointCloud(points)
        }
    }

    def mergeAll(clouds: List[PointCloud]): PointCloud = {
        def rec(ref: PointCloud, others: Option[List[PointCloud]]): PointCloud = {
            p()
            p(s"Has ${ref.points.size} points atm...")
            p(s"Has ${ref.points.toSet.size} unique points atm...")
            others match {
                case None => ref
                case Some(clds) => {
                    val matches = clds.groupBy(c => c.overlaps(ref))
                    p(s"${matches(true).length} clouds overlap at stage now...")
                    rec(ref.mergeMany(matches(true)), matches.get(false))
                }
            }
        }

        rec(clouds(0), Some(clouds.slice(1, clouds.length)))
    }

    override def part1(): String = {
        val merged = mergeAll(clouds)
        s"After aligning all scanners, there are ${merged.points.toSet.size} points."
    }
    
    override def part2(): String = {
        val merged = mergeAll(clouds)

        val maxMHD = merged.scanners.tails.flatMap({
            case s :: tail => tail.map(s mhd _)
            case List() => Nil
        }).max

        s"The maximum distance between two scanners is ${maxMHD.toInt}"
    }
}
