package advent

import fs2._
import cats._
import cats.syntax.all._
import cats.effect._
import java.io.IOException

extension (name: String)
  def bytes[F[_]: Sync]: Stream[F, Byte] =
    def resource = Option(getClass().getResourceAsStream(name))
      .getOrElse(throw new IOException(s"resource $name not found"))
    io.readInputStream(Sync[F].delay(resource), 256, true)
  def resourceLines[F[_]: Sync]: Stream[F, String] =
    bytes.through(text.utf8.decode).through(text.lines)

type StreamIO[A] = Stream[IO, A]

extension [F[_], G[_], A](singletonstream: Stream[F, A])
  def compileSingleton(using Compiler[F, G], Functor[G]) = singletonstream.compile.last.map(_.get)
