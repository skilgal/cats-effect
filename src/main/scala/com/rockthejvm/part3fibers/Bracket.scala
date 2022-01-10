package com.rockthejvm.part3fibers

import cats.effect._
import com.rockthejvm.IOOps

import java.io.*
import java.util.Scanner
import scala.concurrent.duration.DurationInt

object Bracket extends IOApp.Simple {

  def openFileScanner(path: String): IO[Scanner] = {
    IO(new Scanner(new FileReader(new File(path))))
  }

  def bracketReadFile(path: String): IO[Unit] = {
    openFileScanner(path).bracket { scanner =>
      def readLineAndPrint(): IO[Unit] =
        if (scanner.hasNext)
          IO(println(scanner.nextLine())) >> IO.sleep(
            100.millis
          ) >> readLineAndPrint()
        else IO.unit

    readLineAndPrint()
    }(scanner => IO(s"closing file ${path}").debug >> IO(scanner.close()))
  }

  def resourceReadFile(path: String): IO[Unit] = {
    Resource
      .make(openFileScanner(path))(scanner =>
        IO(s"closing file ${path}").debug >> IO(scanner.close())
      )
      .use { scanner =>
        def readLineAndPrint(): IO[Unit] =
          if (scanner.hasNext)
            IO(println(scanner.nextLine())) >> IO.sleep(
              100.millis
            ) >> readLineAndPrint()
          else IO.unit

        readLineAndPrint()
      }
  }

  def run = resourceReadFile(
    "src/main/scala/com/rockthejvm/part3fibers/Bracket.scala"
  )

}
