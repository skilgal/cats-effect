package com.rockthejvm.part4PolymorphicEffects

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Resource
import java.io.FileWriter
import java.io.File
import cats.syntax.writer
import scala.io.Source
import cats.effect.std.CountDownLatch

object CountDownLatchIO extends IOApp.Simple {

  /** Exercise: simulate a file downloader on multiple threads
    */

  object FileServer {
    val fileChunksList = Array(
      "I love Scala.",
      "Cats Effect seems quite fun.",
      "Never would I have thought I would do low-level concurrency WITH pure FP."
    )

    def getNumChunks: IO[Int]            = IO(fileChunksList.length)
    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))
  }

  def writeToFile(path: String, contents: String): IO[Unit] = {
    val fileResource =
      Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fileResource.use { writer =>
      IO(writer.write(contents))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val composedResource = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer =>
        IO(writer.close())
      )
    } yield (reader, writer)

    composedResource.use { case (reader, writer) =>
      IO(reader.getLines().foreach(writer.write))
    }
  }

  /*
   * - call file server API and get the number of chunks(n)
   * - start a CDLatch
   * - start n fibers which download a chunk of the file (use the file server's download chunk API)
   * - block on the latch until each task has finished
   * - after all chunks are done, stitch the files together under the same file on disk
   */

  def downloadFile(filename: String, destFolder: String): IO[Unit] = {
    for {
      chunkNumber <- FileServer.getNumChunks
      latch       <- CountDownLatch[IO](chunkNumber)
      // _           <- (1 to chunkNumber).toListp(n => IO(FileServer.dow))

    } yield ()
  }

  override def run = ???
}
