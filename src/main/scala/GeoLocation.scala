import java.sql.{Connection, Statement}

import scala.io.StdIn
case class GeoLocation(
                        name: Option[String],
                        teams: List[String],
                        temp: List[Temperature],
                        cover: List[CloudCover],
                        precip: List[Precipitation]
                      ) extends CardEntity {
  override def finished: Boolean = precip.exists(p => p.precip == "None")

  override def save(connection: Connection): Unit = {
    val validated = MonthRange.validate(precip) &&
      MonthRange.validate(cover) &&
      MonthRange.validate(temp.collect({case t if t.timeOfDay == "Day" => t})) &&
      MonthRange.validate(temp.collect({case t if t.timeOfDay == "Night" => t}))
    if (!validated) {
      throw new IndexOutOfBoundsException("Range isn't validated")
    }
    val statement = connection.prepareStatement("""INSERT INTO GeographicLocation (name) VALUES (?)""",
      Statement.RETURN_GENERATED_KEYS
    )
    statement.setString(1, name.get)
    statement.execute()
    val keys = statement.getGeneratedKeys
    keys.next()
    val geoId = keys.getInt(1)
    statement.close()
    ((1 to 100) zip temp).foreach({
      case (rank, t) => t.save(connection, geoId, rank)
    })
    ((1 to 100) zip cover).foreach({
      case (rank, c) => c.save(connection, geoId, rank)
    })
    ((1 to 100) zip precip).foreach({
      case (rank, p) => p.save(connection, geoId, rank)
    })
    teams.foreach(t => {
      print(t + ":")
      val city = StdIn.readLine()
      print("Team:")
      val nickname = StdIn.readLine()
      print("ParkId:")
      val park = StdIn.readLine().toInt
      Team(city, nickname, park, geoId).save(connection)
    })
  }

  override def addTokens(tokens: List[String]): (CardEntity, Boolean, List[String]) = {
    tokens match {
      case Nil => (this, false, Nil)
      case x :: tail if x.contains(",") =>
        (copy(teams = x.split(",").toList), false, tail)
      case x :: "EMPTY" :: tail if Set("Cloudy:", "Clear:").contains(x) =>
        parseCloudCover(x.substring(0, x.length - 1), tail)
      case x :: tail if x == "PartlyCloudy:" =>
        parseCloudCover("Partly Cloudy", tail)
      case x :: "EMPTY" :: tail if Set("Snow:", "Fog:", "Showers:", "None:").contains(x) =>
        parsePrecipitation(x.substring(0, x.length - 1), tail)
      case x :: tail if x == "Thunderstorms:" =>
        parsePrecipitation(x.substring(0, x.length - 1), tail)

      case x :: tail if x.startsWith("Day") || x.startsWith("Night") =>
        val timeOfDay = x.split(":")(0)
        val tempValue = x.split(":")(1)
        val newTemps: List[Temperature] = monthRanges(tail).map{
          case (month, range) =>
            Temperature(month,timeOfDay, tempValue, range)
        }
        (copy(temp = temp ::: newTemps), false, Nil)
      case x :: Nil if x.toUpperCase == x =>
        (copy(teams = List(x)), false, Nil)
      case x => (copy(name = Some(x.mkString(" "))), false, Nil)
    }
  }

  private def monthRanges(ranges: List[String]) = {
    List("April", "May", "June", "July", "August", "September", "October") zip
      ranges.map(t => CardReader.rangeToValue(t))
  }

  private def parseCloudCover(coverType: String, ranges: List[String]) = {
    val newCover: List[CloudCover] = monthRanges(ranges).map {
      case (month, range) => CloudCover(month, range, coverType)
    }
    (copy(cover = cover ::: newCover), false, Nil)
  }
  private def parsePrecipitation(precipType: String, ranges: List[String]) = {
    val newPrecip: List[Precipitation] = monthRanges(ranges).map {
      case (month, range) => Precipitation(month, range, precipType)
    }
    (copy(precip = precip ::: newPrecip), precipType == "None", Nil)
  }
}
