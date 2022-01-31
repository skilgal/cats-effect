package com
import cats.effect._

package object rockthejvm {

  implicit class IOOps[A](io: IO[A]) {
    def debug: IO[A] = io.map{a =>
      println( s"[${Thread.currentThread.getName}] ${a}")
      a
    }
  }

}
