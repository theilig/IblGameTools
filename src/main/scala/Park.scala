import java.sql.{Connection, Statement}

import scala.util.Try

case class Park(
                 name: Option[String],
                 surface: Option[String],
                 quality: Option[String],
                 foulTerritory: Option[String],
                 ifrAdjustment: Option[Int],
                 ofrAdjustment: Option[Int],
                 wind: List[Wind],
                 distance: List[Distance],
                 fenceHeight: List[FenceHeight],
                 templateId: Option[Int],
                 results: Option[String],
                 parkEffects: List[String]
               ) extends CardEntity {
  override def finished: Boolean =
    // nonEmpty on IterableOnce (parent of List and Option) is deprecated, by splitting we get
    // Option.nonEmpty and IterableOnceExtensionMethods neither of which is deprecated
    List(name, surface, quality, foulTerritory, ifrAdjustment, ofrAdjustment, templateId, results).
      forall(x => x.nonEmpty) &&
    List(wind, distance, fenceHeight).forall(x => x.nonEmpty)

  def splitRangeTokens(tokens: List[String]): (List[String], List[String], List[String]) = {
    @scala.annotation.tailrec
    def splitInternal(results: List[String], ranges: List[String], remaining: List[String]): (List[String], List[String], List[String]) = {
      remaining match {
        case Nil => (results, ranges, remaining)
        case x :: xs if x.isEmpty => splitInternal(results, ranges, xs)
        case x :: xs if """.*\w.*00.*$""".r.matches(x) =>
          val chunks = x.split("00")
          if (chunks(0).nonEmpty) {
            if (ranges.nonEmpty) {
              (results ::: (chunks(0) :: Nil), ranges, ("00" + chunks(1)) :: xs)
            } else {
              splitInternal(results ::: (chunks(0) :: Nil), Nil, ("00" + chunks(1)) :: xs)
            }
          } else if (ranges.nonEmpty) {
            (results, ranges, remaining)
          } else {
            splitInternal(results, x :: Nil, xs)
          }
        case x :: _ if x.contains("00") && results.nonEmpty && ranges.nonEmpty =>
          (results, ranges, remaining)
        case x :: xs if """.*99.*\w.*$""".r.matches(x) =>
          val chunks = x.split("99")
          if (chunks.length > 1 && chunks(1).nonEmpty) {
            if (results.nonEmpty) {
              (results, ranges ::: ((chunks(0) + "99") :: Nil), chunks(1) :: xs)
            } else {
              splitInternal(results, ranges ::: ((chunks(0) + "99") :: Nil), chunks(1) :: xs)
            }
          } else if (results.nonEmpty) {
            (results, ranges ::: (x :: Nil), xs)
          } else {
            splitInternal(results, ranges ::: (x :: Nil), xs)
          }
        case x :: xs if x.contains("99") && results.nonEmpty =>
          (results, ranges ::: (x :: Nil), xs)
        case x :: xs if """^[\d-\s]+$""".r.matches(x) =>
          splitInternal(results, ranges ::: (x :: Nil), xs)
        case x :: xs =>
          splitInternal(results ::: (x :: Nil), ranges, xs)
      }
    }
    splitInternal(List(), List(), tokens)
  }

  def updateParkFromRange(tokens: List[String]): (Park, List[String]) = {
    val (resultList, rangeList, remaining) = splitRangeTokens(tokens)
    val ranges = resultList.mkString(" ").split("""\s+""").toList.zipAll(rangeList.mkString(" ").split("""\s+""").toList, "99-98", "99-98")
    val templateId = CreateParks.findTemplate(ranges.map(_._1))
    val results = CreateParks.rangeToResults(ranges.map(_._2))
    (this.copy(templateId = Some(templateId), results = Some(results)), remaining)
  }

  def getLocationNumbers(tokens: List[String]): (List[Int], List[String]) = {
    val LocationCount = 7
    def strip(soFar: List[Int], rest: List[String]): (List[Int], List[String]) = {
      if (soFar.length == LocationCount) {
        (soFar, rest)
      } else if (rest.isEmpty) {
        (soFar, rest)
      } else {
        try {
          strip(soFar ::: rest.head.replaceAll("""[A-Za-z:]""", "").split("""\s+""").map(f => f.toInt).toList, rest.tail)
        } catch {
          case _ : NumberFormatException => strip(soFar, rest.tail)
        }
      }
    }
    strip(List(), tokens)
  }

  def updateFenceHeight(tokens: List[String]): (Park, List[String]) = {
    val (numbers, remaining) = getLocationNumbers(tokens)
    val heights = (List("LFL", "LF", "LCF", "CF", "RCF", "RF", "RFL") zip numbers).map(p =>
      FenceHeight(p._1, p._2)
    )
    (this.copy(fenceHeight = heights), remaining)
  }

  def windSpecification(token: String): Boolean = {
    token.startsWith("straight") || token.startsWith("cross") || token.startsWith("no wind")
  }

  def updateWindProbabilities(windType: String, tokens: List[String]): (Park, List[String]) = {
    val Combined = """(.*?)\s+(\d.*)""".r
    if (Combined.matches(windType)) {
      val m = """(.*?)\s(\d.*)""".r.findAllIn(windType)
      updateWindProbabilities(m.group(1), m.group(2) :: tokens)
    } else {
      if (tokens.head.matches("""^\s*\d.*""")) {
        val ranges = CreateParks.rangeToResults(tokens.head.replace("W", "").split("""\s+""").toList)
        if (ranges.nonEmpty) {
          val addedWind = (List("April", "May", "June", "July", "August", "September") zip
            ranges.split("/").toList).map(p =>
            Wind(p._1, windType, p._2.toInt)
          )
          (this.copy(wind = addedWind ::: this.wind), tokens.tail)
        } else {
          (this, tokens)
        }
      } else {
        (this, tokens)
      }
    }
  }

  def updateDistance(windType: String, tokens: List[String]): (Park, List[String]) = {
    if (tokens.nonEmpty && windSpecification(tokens.head)) {
      updateDistance(tokens.head, tokens.tail)
    } else {
      val (numbers, remaining) = getLocationNumbers(tokens)
      if (numbers.nonEmpty) {
        val distances = (List("LFL", "LF", "LCF", "CF", "RCF", "RF", "RFL") zip numbers).map(p =>
          Distance(p._1, windType, p._2)
        )
        (this.copy(distance = distances ::: this.distance), remaining)
      } else {
        (this, remaining)
      }
    }
  }

  override def addTokens(tokens: List[String]): (Park, Boolean, List[String]) = {
    def updateName(x: String) = {
      val newPark = this.copy(name = Some(this.name.map(n => x + " " + n).getOrElse(x)))
      newPark
    }

    val Range = """.*\d-\d.*""".r
    tokens match {
      case Nil => (this, false, Nil)
      case "PARK?" :: _ =>
        (this, false, Nil)
      case x :: tail if x.contains("cardset") => (this, this.finished, tail)
      case x :: tail if x == "Special Park Effects" =>
        if (tail.length > 1 && tail.head != "Special Park Effects") {
          if (tail.tail.head.startsWith("Roof")) {
            (this.copy(parkEffects = "Roof" :: this.parkEffects), false, tail.drop(2))
          } else {
            (this, false, tail.drop(1))
          }
        } else {
          (this, false, tail.drop(1))
        }
      case x :: remaining if x.startsWith("Special Park Effects Dome") =>
        val newPark = this.copy(parkEffects = "Roof" :: this.parkEffects)
        (newPark, false, remaining)
      case x :: remaining if x.startsWith("Special Park Effects Roof") =>
        val newPark = this.copy(parkEffects = "Roof" :: this.parkEffects)
        (newPark, false, remaining)
      case "3b for Rp/Lo" :: "ss for Rsp" :: _ =>
        (this, false, Nil)
      case "*fielder is 1b for Lp/Ro" :: "2b for Lsp" :: _ =>
        (this, false, Nil)
      case x :: tail if windSpecification(x) && (Range.matches(x) || tail.exists(t => Range.matches(t))) =>
        val (newPark, remaining) = updateWindProbabilities(x, tail)
        (newPark, false, remaining)
      case x :: y :: tail if windSpecification(y) && Range.matches(x) =>
        val (newPark, remaining) = updateWindProbabilities(y, x :: tail)
        (newPark, false, remaining)
      case x if x.exists(p => p.contains("00")) =>
        val (newPark, remaining) = updateParkFromRange(x)
        (newPark, false, remaining)
      case x :: y :: remaining if y.nonEmpty && """^[A-Z]{2,3}$""".r.matches(y) =>
        val newPark: Park = updateName(x)
        (newPark, true, remaining)
      case "Distance" :: _ =>
        (this, false, Nil)
      case "Wind" :: _ =>
        (this, false, Nil)
      case x if x.exists(t => t.contains("Fence Height")) =>
        val (newPark, remaining) = updateFenceHeight(x)
        (newPark, false, remaining)
      case x :: tail if windSpecification(x) =>
        val (newPark, remaining) = updateDistance(x, tail)
        (newPark, false, remaining)
      case "Foul Territory:" :: ft :: "OFR Adj:" :: adjustment :: remaining =>
        val newPark = this.copy(foulTerritory = Some(ft), ofrAdjustment = Try(adjustment.toInt).recover(_ => 0).toOption)
        (newPark, false, remaining)
      case "Surface:" :: surface :: "IFR Adj:" :: adjustment :: remaining =>
        val surfaceValues = surface.split("/")
        val newPark = this.copy(surface = Some(surfaceValues(0)), quality = Some(surfaceValues(1)),
          ifrAdjustment = Try(adjustment.toInt).recover(_ => 0).toOption)
        (newPark, false, remaining)
      case x :: _ if x.startsWith("Weather") =>
        (this, false, Nil)
      case x :: _ if x.startsWith("weather") =>
        (this, false, Nil)
      case x :: remaining if x.isEmpty =>
        addTokens(remaining.dropWhile(_.isEmpty))
        (this, false, remaining)
      case x :: remaining if x.startsWith("hot, cold") =>
        (this, false, remaining)
      case x :: remaining if x.startsWith("L results on batter card") =>
        val newPark = this.copy(parkEffects = "Elevation" :: this.parkEffects)
        (newPark, false, remaining)
      case x :: remaining =>
        val newPark: Park = updateName(x)
        (newPark, false, remaining)
    }
  }

  def save(connection: Connection): Unit = {
    val statement = connection.prepareStatement(
      """INSERT into Park (name, surface, quality, foul_territory, ifr_adjustment,
      ofr_adjustment, template_id, results) VALUES (?,?,?,?,?,?,?,?)""",
      Statement.RETURN_GENERATED_KEYS
    )
    statement.setString(1, name.get)
    statement.setString(2, surface.get)
    statement.setString(3, quality.get)
    statement.setString(4, foulTerritory.get)
    statement.setInt(5, ifrAdjustment.get)
    statement.setInt(6, ofrAdjustment.get)
    statement.setInt(7, templateId.get)
    statement.setString(8, results.get)
    statement.execute()
    val keys = statement.getGeneratedKeys
    keys.next()
    val parkId = keys.getInt(1)
    statement.close()
    parkEffects.foreach(effect => {
      val effectStatement = connection.prepareStatement("INSERT INTO ParkEffect (park_id, effect) VALUES (?,?)")
      effectStatement.setInt(1, parkId)
      effectStatement.setString(2, effect)
      effectStatement.execute()
      effectStatement.close()
    })
    (wind zip (1 to 100)).foreach(w => w._1.save(connection, parkId, w._2))
    distance.foreach(d => d.save(connection, parkId))
    fenceHeight.foreach(f => f.save(connection, parkId))
  }
}
