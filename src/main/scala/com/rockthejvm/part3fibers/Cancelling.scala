package com.rockthejvm.part3fibers

import cats.effect.IOApp
import cats.instances.list
import cats.effect.IO
import com.rockthejvm._
import concurrent.duration._

object Cancelling extends IOApp.Simple {

  /*
   * The uncancelable API is more complex and more general
   * It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance
   * The Poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   *
   */

  /*
   example: authentication service. Has two parts:
   - input password, can be canceled, because otherwise we might block indefinitely on user input
   - verify password, CANNOT be canceled once it's started*/

  val inputPassword =
    IO("Input password:").debug >> IO("(typing password)").debug >> IO.sleep(2.seconds) >> IO(
      "We've done!!"
    )
  val verifyPassword = (pw: String) =>
    IO("verifying...").debug >> IO.sleep(2.seconds) >> IO(pw == "We've done!!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(
        IO("Authentication timed out. Try again later").debug.void
      ) // cancelable
      verified <- verifyPassword(pw) // not cancelable
      _ <-
        if (verified) IO("Authentication successful").debug // this is NOT cancelable
        else IO("Authentication failed").debug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debug >> authFib.cancel
  } yield ()

  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debug
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug)

  // 2

  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(3.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debug >> authFib.cancel
  } yield ()

  // 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debug >> IO.sleep(1.second)) >>
        IO("uncancelable").debug >> IO.sleep(1.second) >>
        poll(IO("second cancelable").debug >> IO.sleep(1.second))
    }

    for {
      fib <- sequence.start
      _   <- IO.sleep(1100.millis) >> IO("CANCELING").debug >> fib.cancel
      _   <- fib.join
    } yield ()
  }

  override def run = threeStepProgram()

}
