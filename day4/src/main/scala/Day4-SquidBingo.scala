package advent

import fs2._
import cats._
import cats.effect._
import cats.syntax.all._
import scala.collection.immutable.ArraySeq

object Bingo:
  val size = 5 // 5 * 5 boards
  private val length = size * size
  opaque type BingoSheet = Vector[Option[Int]]
  type Score = Int

  def rowIndices(index: Int): Seq[Int] =
    val start = (index / size) * size
    Range.inclusive(start, start + size - 1)

  def colIndices(index: Int): Seq[Int] =
    val col = index % size
    Range.inclusive(col, size * (size - 1) + col, size)

  extension (sheet: BingoSheet)
    def score = sheet.foldMap(_.getOrElse(0))
    def clear(num: Int): Either[Score, BingoSheet] = {
      val (cleared: BingoSheet, clearedIndices1) = sheet.zipWithIndex.map {
        case (Some(`num`), i) => None -> Some(i)
        case (x, _)           => x -> None
      }.separate

      val clearedIndices = clearedIndices1.collect { case Some(x) => x }
      val clearedOnRows = clearedIndices.map(rowIndices)
      val clearedOnCols = clearedIndices.map(colIndices)
      val clearedBunches = clearedOnRows ++ clearedOnCols
      if (clearedBunches.exists(bunch => bunch.forall(index => cleared(index).isEmpty))) then
        val totalScore = cleared.score * num
        println(s"score $totalScore on\n${cleared.formatted}")
        Left(totalScore)
      else Right(cleared)
    }

    def formatted = sheet
      .grouped(size)
      .map(row =>
        row
          .map {
            case Some(num) => f"$num%2d"
            case None      => "  "
          }
          .mkString(" ")
      )
      .mkString("\n")

  object BingoSheet:
    def apply(nums: Seq[Int]): BingoSheet =
      nums.map(Option.apply).toVector.ensuring(v => v.length == length)

object Day4SquidBingo extends IOApp.Simple:
  import Bingo._
  override def run: IO[Unit] =
    for {
      input <- "/input.txt".resourceLines[IO].compile.toList
      bingoNumbers = input.head.split(',').toList.map(_.toInt)
      bingoMapInput = input
        .drop(1)
        .grouped(size + 1)
        .map(readCard)
      bingoCards = bingoMapInput.toList.map(numbers => BingoSheet(numbers.toVector))
      resultWin = playToWin(bingoNumbers, bingoCards).fold("bad luck, everybody loses")(i =>
        s"first winner scores $i"
      )
      resultLose = playToLose(bingoNumbers, bingoCards).fold("bad luck, everybody loses")(i =>
        s"last winner scores $i"
      )
      _ <- IO(println(resultWin))
      _ <- IO(println(resultLose))
    } yield ()

  def readCard(lines: Seq[String]) = lines.foldMap(_.split("\\s+").toVector.collect {
    ((s: String) => s.toIntOption).unlift
  })

  def playToWin(numbers: List[Int], sheets: List[BingoSheet]): Option[Score] =
    play(numbers, sheets, win)
  def playToLose(numbers: List[Int], sheets: List[BingoSheet]): Option[Score] =
    play(numbers, sheets, lose)

  // this looks suspiciously much like tailrecM
  def play[A, B, C](moves: List[A], sheets: B, step: (A, B) => Either[C, B]): Option[C] =
    moves match {
      case head :: tail =>
        step(head, sheets) match {
          case Right(next) => play(tail, next, step)
          case Left(score) => Some(score)
        }
      case Nil => None
    }

  def win(number: Int, sheets: List[BingoSheet]): Either[Score, List[BingoSheet]] =
    sheets.traverse(sheet => sheet.clear(number))

  def lose(number: Int, sheets: List[BingoSheet]) =
    val (winners, losers) = sheets.map(sheet => sheet.clear(number)).separate
    if losers.isEmpty then Left(winners.head)
    else Right(losers)
