package com.rockthejvm.part2effects

import scala.util.Try
import cats.effect.IO

object IOErrorHandling {

  /** Exercises
    */
  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option.fold(IO.raiseError(ifEmpty))(IO.pure)

  def try2IO[A](aTry: Try[A]): IO[A] =
    aTry.fold(IO.raiseError, IO.pure)

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] =
    anEither.fold(IO.raiseError, IO.pure)

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)
  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, IO.pure)

}
