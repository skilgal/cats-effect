package com
import cats.effect._

package object rockthejvm {

  implicit class IOOps(io: IO[String]) {
    def debug: IO[Unit] = io.map(println)
  }

}
