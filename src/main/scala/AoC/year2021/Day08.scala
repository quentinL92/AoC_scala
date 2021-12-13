package AoC.year2021

import AoC.AoCDay

object Day08 extends AoCDay(2021, 8) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    lazy val lines = getLines

    def part1(): Unit = {
      val output: Vector[String] = lines.map(_.split('|').last.trim)
      val res = output.flatMap(_.split(' ').map(_.trim)).count(str => List(2, 3, 4, 7).contains(str.length))

      println(s"Part1 = $res")
    }

    def part2(): Unit = {

      val values: Vector[Array[String]] = lines.map(_.split('|').map(_.trim))
      val res: Long = values.map {
        case Array(uniqueValues, outputValues) =>
          val uniqueValuesPossibilities: Vector[List[Char]] = uniqueValues
            .split(' ')
            .map(_.trim.toList)
            .toVector

          val uniqueKnownValues: Map[Int, List[Char]] = Map(
            1 -> uniqueValuesPossibilities.find(_.length == 2).get,
            7 -> uniqueValuesPossibilities.find(_.length == 3).get,
            4 -> uniqueValuesPossibilities.find(_.length == 4).get,
            8 -> uniqueValuesPossibilities.find(_.length == 7).get
          )

          val uniqueUnkownValues: Map[Int, List[List[Char]]] = Map(
            0 -> uniqueValuesPossibilities.filter(_.length == 6).toList,
            2 -> uniqueValuesPossibilities.filter(_.length == 5).toList,
            3 -> uniqueValuesPossibilities.filter(_.length == 5).toList,
            5 -> uniqueValuesPossibilities.filter(_.length == 5).toList,
            6 -> uniqueValuesPossibilities.filter(_.length == 6).toList,
            9 -> uniqueValuesPossibilities.filter(_.length == 6).toList
          )

          val res9: List[Char] = uniqueUnkownValues
            .getOrElse(9, Nil)
            .filter { possibilities =>
              possibilities.intersect(uniqueKnownValues.getOrElse(4, Nil)).length == 4
            }
            .head

          val res0 = uniqueUnkownValues
            .getOrElse(0, Nil)
            .filter { possibilities =>
              possibilities.intersect(uniqueKnownValues.getOrElse(7, Nil)).length == 3
            }
            .diff(res9 :: Nil)
            .head

          val res6: List[Char] = uniqueUnkownValues.getOrElse(6, Nil).diff(res0 :: res9 :: Nil).head

          val res3: List[Char] = uniqueUnkownValues
            .getOrElse(3, Nil)
            .filter { possibilities =>
              possibilities.intersect(uniqueKnownValues.getOrElse(1, Nil)).length == 2
            }
            .head

          val commonPart6And1And2: Char = res6.intersect(uniqueKnownValues.getOrElse(1, Nil)).head

          val res5 = uniqueUnkownValues
            .getOrElse(2, Nil)
            .filter { possibilities =>
              possibilities.contains(commonPart6And1And2)
            }
            .diff(res3 :: Nil)
            .head

          val res2: List[Char] = uniqueUnkownValues.getOrElse(5, Nil).diff(res5 :: res3 :: Nil).head

          val tmpMap: Map[Int, List[Char]] = (uniqueKnownValues + (3 -> res3)) + (9 -> res9) + (0 -> res0) +
            (5 -> res5) + (6 -> res6) + (2 -> res2)

          val finalMap: Map[String, Int] = tmpMap.map {
            case (i, value) => value.sorted.mkString -> i
          }

          outputValues
            .split(' ')
            .map(_.trim)
            .map { output =>
              finalMap.get(output.sorted) match {
                case Some(value) => value
                case None        => throw new Exception(s"Error for $output in map ${finalMap.toList.sortBy(_._2)}")
              }
            }
            .mkString
            .toInt

      }.sum

      println(s"Part2 = $res")

    }

//    part1()

    part2()

  }
}
