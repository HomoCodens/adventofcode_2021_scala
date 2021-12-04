package aocutil

import scala.io.Source
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

class InputReader[A](inputRoot: String, day: Int) {
    val root = new File(inputRoot)
    if(!(root.exists && root.isDirectory)) {
        throw new RuntimeException(s"$inputRoot is not a directory or does not exist (super clear, ye?)!")
    }
    val dayFolder = root.toPath.resolve(s"day$day").toFile
    if(!dayFolder.exists) {
        throw new RuntimeException(s"$dayFolder does not exist!")
    }

    def getFile(test: Boolean = false, testCase: Int = 1) : String = {
        //Contrast the readability of the Scala ternary syntax with the Java ternary operator syntax:
        // i == 1 ? x : y
        // 
        // If you say so, chief...
        val f = dayFolder.toPath.resolve(s"${if(test) s"test$testCase.txt" else "input.txt"}")
        if(!(new File(f.toString)).exists) {
            throw new RuntimeException(s"File $f does not exist!")
        }
        f.toString
    }

    def readText(test: Boolean = false, testCase: Int = 1) : List[String] = {
        Source.fromFile(getFile(test, testCase)).getLines.toList
    }

    def readInt(test: Boolean = false, testCase: Int = 1) : List[Int] = {
        readText(test, testCase).map(_.toInt)
    }

    def readParsedByLine(parser: (String) => A, test: Boolean = false, testCase: Int = 1) : List[A] = {
        readText(test, testCase).map(parser)
    }

    def readParsedWhole(parser: (List[String]) => A, test: Boolean = false, testCase: Int = 1) : A = {
        parser(readText(test, testCase))
    }
}