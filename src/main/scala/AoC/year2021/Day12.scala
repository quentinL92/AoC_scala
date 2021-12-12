package AoC.year2021

import AoC.AoCDay

object Day12 extends AoCDay(2021, 12) {
  override def resolveDay(): Unit = {
    val testMode = false

    lazy val lines = getLines(test = testMode).map(_.trim)
    val connections: Map[String, Vector[String]] = lines
      .map(_.split('-'))
      .flatMap {
        case a @ Array(head, last) => Array(a, Array(last, head))
      }
      .groupBy(_.head)
      .filterNot(_._1 == "end")
      .view
      .mapValues(_.map(_.last).filter(_ != "start"))
      .toMap

    if (testMode) println(s"Connections = $connections")

    def part1(): Unit = {

      def inner(
          currentPath: Vector[(String, String)],
          visitedSmallCaves: Set[String]
      ): Vector[Vector[(String, String)]] = {
        lazy val lastConnection = currentPath.last
        if (lastConnection._2 == "end") Vector(currentPath)
        else {
          if (currentPath.isEmpty) Vector(Vector.empty[(String, String)])
          else {
            val possibleNextConnections = connections
              .getOrElse(lastConnection._2, Vector.empty[String])
              .filterNot(visitedSmallCaves.contains)

//            val filteredPossibleNextConnection =
//              if (currentPath.length < 3) possibleNextConnections
//              else {
//                val connectionBeforeLast = currentPath(currentPath.length - 3)
//                possibleNextConnections
//                  .filter(_ != connectionBeforeLast._2)
//              }

            possibleNextConnections
              .flatMap { nextPossibleDestination =>
                inner(
                  currentPath.appended(lastConnection._2 -> nextPossibleDestination),
                  if (lastConnection._2.head.isLower) visitedSmallCaves.incl(lastConnection._2)
                  else visitedSmallCaves
                )
              }
          }
        }

        // (start, A) (A, c) (c, A) (A, c)

      }

      val allPossibleConnections = connections
        .getOrElse("start", Vector.empty[String])
        .flatMap { to =>
          inner(Vector("start" -> to), Set("start").concat(if (to.head.isLower) Set(to) else Set.empty[String]))
        }

      allPossibleConnections.foreach(println)

      println(s"Part1 = ${allPossibleConnections.length}")
    }

    implicit class Ops(vector: Vector[(String, String)]) {
      def toPath: String = s"start,${vector.map(_._2).mkString(",")}"
    }

    def part2(): Unit = {

      def inner(
          currentPath: Vector[(String, String)],
          visitedSmallCaves: Map[String, Int],
          smallCaveException: String
      ): Vector[Vector[(String, String)]] = {
        lazy val lastConnection: (String, String) = currentPath.last

        if (lastConnection._2 == "end") Vector(currentPath)
        else {
          connections
            .getOrElse(lastConnection._2, Vector.empty[String])
            .filter { to =>
              visitedSmallCaves.getOrElse(to, 0) < (if (to == smallCaveException) 2 else 1)
            }
            .flatMap { nextPossibleDestination =>
              inner(
                currentPath = currentPath.appended(lastConnection._2 -> nextPossibleDestination),
                visitedSmallCaves = if (nextPossibleDestination.head.isLower) {
                  visitedSmallCaves.updated(
                    nextPossibleDestination,
                    visitedSmallCaves.getOrElse(nextPossibleDestination, 0) + 1
                  )
                } else visitedSmallCaves,
                smallCaveException = smallCaveException
              )
            }
        }

      }

      val smallCaves: Vector[String] = connections.keySet
        .filter(cave => !List("start", "enc").contains(cave) && cave.head.isLower)
        .toVector

      if (testMode) println(s"SmallCaves = $smallCaves\n")

      val allPossibleConnections: Set[Vector[(String, String)]] = for {
        caveException <- smallCaves.toSet[String]
        to <- connections.getOrElse("start", Vector.empty[String]).toSet[String]
        possiblePaths <- inner(
          Vector("start" -> to),
          Map("start" -> 1).concat(if (to.head.isLower) Map(to -> 1) else Map.empty[String, Int]),
          caveException
        )
      } yield possiblePaths

      if (testMode) allPossibleConnections.foreach(path => println(path.toPath))

      println(s"Part2 = ${allPossibleConnections.size}")

    }

//    part1()
    part2()

  }
}
