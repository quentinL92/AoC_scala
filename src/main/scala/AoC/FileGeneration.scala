package AoC

import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Using

object FileGeneration extends App {
  def testFolder(year: Int): String = s"src/main/resources/AoC/year$year"
  def codeFolder(year: Int): String = s"src/main/scala/AoC/year$year"

  def generateTestFileFor(year: Int): Unit = {
    (1 to 25)
      .map { day =>
        new File(testFolder(year) + s"/test$day.txt")
      }
      .filterNot(f => f.exists())
      .foreach { f =>
        Using(new BufferedWriter(new FileWriter(f)))(_ => ())
      }

  }

  def generateEmptyDays(year: Int): Unit = {
    (1 to 25)
      .map(day => day -> new File(codeFolder(year) + s"/Day${day.formatted("%02d")}.scala"))
      .filterNot(f => f._2.exists())
      .foreach {
        case (day, f) =>
          Using(new BufferedWriter(new FileWriter(f)))(_.write(Models.get(year, day)))
      }
  }

  generateEmptyDays(2021)
}
