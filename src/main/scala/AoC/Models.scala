package AoC

object Models {

  def get(year: Int, day: Int): String = s"""package AoC.year$year
                                              |
                                              |import AoC.AoCDay
                                              |
                                              |object Day${day.formatted("%02d")} extends AoCDay($year, $day) {
                                              |
                                              |  override lazy val testMode: Boolean = true
                                              |
                                              |  override def resolveDay(): Unit = {
                                              |
                                              |    lazy val lines = getLines
                                              |    lazy val line = getLine
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
