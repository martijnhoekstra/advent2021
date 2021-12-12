package advent

import cats.*
import cats.effect.*
import fs2.*
import cats.syntax.all.*
import cats.data.NonEmptyList

object Day12Pathfinding extends AdventDay:

  def routes1[A](connections: List[(A, A)], from: A, end: A, mayRevisit: A => Boolean) =
    def rec(remaining: List[(A, A)], from: A): List[List[A]] =
      if from == end then List(List(from))
      else {
        val remove = !mayRevisit(from)
        val (steps, stripped) = remaining.partitionMap {
          case (`from`, to) => Left(to)
          case (to, `from`) => Left(to)
          case x            => Right(x)
        }
        val next = if remove then stripped else remaining
        steps.flatMap(to => rec(next, to).map(tail => from :: tail))
      }
    rec(connections, from)

  def routes2[A: Eq](connections: List[(A, A)], start: A, end: A, mayRevisit: A => Boolean) =
    def rec(route: NonEmptyList[A], usedDispensation: Boolean): List[NonEmptyList[A]] =
      if route.head == end then List(route)
      else
        val destinations = connections.collect {
          case (from, to) if from == route.head => to
          case (to, from) if from == route.head => to
        }
        val nonStartDestinations = destinations.filterNot(dest => dest == "start")
        if !usedDispensation then
          nonStartDestinations.flatMap(to => {
            val needsDispensations = !mayRevisit(to) && route.contains_(to)
            rec(to :: route, needsDispensations)
          })
        else
          val (alwaysLegal, potentiallyIllegalRevisits) = nonStartDestinations.partition(mayRevisit)
          val nonRevisits = potentiallyIllegalRevisits.filterNot(to => route.contains_(to))
          (alwaysLegal ++ nonRevisits).flatMap(to => rec(to :: route, usedDispensation))

    rec(NonEmptyList.one(start), false)

  override def puzzle1(in: Stream[IO, String]): IO[String] =
    for
      connections <- in
        .map { case s"$from-$to" => (from -> to) }
        .compile
        .toList
    yield
      val found = routes1(connections, "start", "end", label => label.forall(ch => ch.isUpper))
      s"${found.length} routes from start to end"

  override def puzzle2(in: Stream[IO, String]): IO[String] =
    for
      connections <- in
        .map { case s"$from-$to" => (from -> to) }
        .compile
        .toList
    yield
      val found = routes2(connections, "start", "end", label => label.forall(ch => ch.isUpper))
      s"${found.length} routes from start to end"
