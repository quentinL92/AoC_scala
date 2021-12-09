package AoC

object Models {

  def get(year: Int, day: Int): String = s"""package AoC.year$year
                                              |
                                              |import AoC.AoCDay
                                              |
                                              |object Day${day.formatted("%02d")} extends AoCDay($year, $day) {
                                              |  override def resolveDay(): Unit = {
                                              |    val testMode = true
                                              |
                                              |    lazy val lines = getLines(test = testMode)
                                              |    lazy val line = getLine(test = testMode)
                                              |
                                              |    def part1(): Unit = {
                                              |
                                              |    }
                                              |
                                              |    def part2(): Unit = {
                                              |
                                              |    }
                                              |
                                              |    part1()
                                              |//    part2()
                                              |
                                              |  }
                                              |}""".stripMargin

}
