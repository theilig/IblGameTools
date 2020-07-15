import java.io.FileWriter

import scala.io.StdIn

object CreateGeoFile extends App {
  print("Name:")
  while (true) {
    val name = StdIn.readLine()
    print("Teams:")
    val teams = StdIn.readLine().split(",")
    val tempRanges: List[(String, List[String])] = for {
      timeOfDay <- List("Day","Night")
      temp <- List("Hot", "Warm", "Cool", "Cold")
    } yield {
      getRanges(s"$timeOfDay:$temp")
    }

    val skyRanges = for {
      cover <- List("Clear", "PartlyCloudy", "Cloudy")
    } yield {
      getRanges(cover)
    }

    val precipRanges = for {
      precip <- List("Thunderstorms", "Showers", "Snow", "Fog", "None")
    } yield {
      getRanges(precip)
    }

    val writer = new FileWriter("geo2020.txt", true)
    def writeRanges(m: List[(String, List[String])]): Unit = {
      m.foreach(p =>
        writer.write(p._1 + ":" + p._2.mkString(" ") + "" + "\n")
      )
    }
    writer.write(name + "\n")
    writer.write(teams.mkString(",") + "\n")
    writeRanges(tempRanges)
    writeRanges(skyRanges)
    writeRanges(precipRanges)
    writer.flush()
  }

  private def getRanges(key: String) = {
    print(s"$key:")
    key -> StdIn.readLine().split(" ").map {
      case "x" => "     "
      case s => s
    }.toList
  }
}
