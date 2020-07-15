import java.sql.{Connection, Statement}
case class Pitcher(
                    name: Option[String],
                    throws: Option[String],
                    leftSlash: Option[String],
                    rightSlash: Option[String],
                    leftResults: List[String],
                    rightResults: List[String],
                    leftValues: List[Int],
                    rightValues: List[Int],
                    leftFatigueValues: List[Int],
                    rightFatigueValues: List[Int],
                    clutch: Option[String],
                    pickoff: Option[Int],
                    balk: Option[String],
                    wildPitch: Option[String],
                    hold: Option[String],
                    againstSteal: Option[Int],
                    defense: Option[Defense],
                    slashStamina: Option[String],
                    reliefStamina: Option[Int],
                    startingStamina: Option[Int],
                    oldStamina: Option[String],
                    usage: Option[String],
                    wildPlay: List[String],
                    atBats: Option[Int],
                    runs: Option[Int],
                    steal: Option[Int],
                    bunts: Option[Bunting],
                    bats: Option[String],
                    hittingAbility: Option[Int],
                    power: Option[String],
                    injuryDays: Option[Int],
                    durability: Option[Int]
                  ) extends CardEntity {
  override def finished: Boolean = name.nonEmpty

  def savePitcher(connection: Connection): Int = {
    val statement = connection.prepareStatement(
      """INSERT INTO Pitcher (throws, slash_left, slash_right, template_id_left, template_id_right,
         results_left, results_right, fatigue_results_left, fatigue_results_right, balk, hold,
         stamina_slash, stamina_starting, stamina_relieving, stamina_old, `usage`, durability, injury_days)
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)""",
      Statement.RETURN_GENERATED_KEYS
    )
    statement.setString(1, throws.get)
    statement.setString(2, leftSlash.get)
    statement.setString(3, rightSlash.get)
    statement.setInt(4, CreatePitchers.findTemplate(leftResults))
    statement.setInt(5, CreatePitchers.findTemplate(rightResults))
    statement.setString(6, leftValues.mkString("/"))
    statement.setString(7, rightValues.mkString("/"))
    statement.setString(8, leftFatigueValues.mkString("/"))
    statement.setString(9, rightFatigueValues.mkString("/"))
    statement.setString(10, balk.get)
    statement.setString(11, hold.get)
    statement.setString(12, slashStamina.get)
    statement.setInt(13, startingStamina.get)
    statement.setInt(14, reliefStamina.get)
    statement.setString(15, oldStamina.get)
    statement.setString(16, usage.getOrElse(""))
    statement.setInt(17, durability.getOrElse(0))
    statement.setInt(18, injuryDays.getOrElse(0))
    statement.execute()
    val keys = statement.getGeneratedKeys
    keys.next()
    val pitcherId = keys.getInt(1)
    statement.close()
    pitcherId
  }

  override def save(connection: Connection): Unit = {
    val pitcherId = savePitcher(connection)
    val statement = connection.prepareStatement(
      """INSERT into Player (name, bats, durability, injury_days, running, stealing, jump,
         slash_left, slash_right, template_id_left, template_id_right, results_left, results_right,
         ifr_location, ofr_location, df_location, power, clutch, `usage`, pitcher_id)
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)""",
      Statement.RETURN_GENERATED_KEYS
    )
    statement.setString(1, name.get)
    statement.setString(2, bats.getOrElse(throws.get))
    statement.setInt(3, durability.get)
    statement.setInt(4, injuryDays.get)
    statement.setInt(5, runs.get)
    statement.setInt(6, steal.get)
    statement.setInt(7, 0)
    // noinspection FieldFromDelayedInit
    val hitter = CreatePitchers.extraCards.find({
      case p: PitcherHitting if p.batter.name.get == s"#${hittingAbility.getOrElse(0)}" =>
        true
      case _ => false
    }).get.asInstanceOf[PitcherHitting].batter
    statement.setString(8, hitter.leftSlash.get)
    statement.setString(9, hitter.rightSlash.get)
    if (bats.getOrElse(throws.get) == "Left") {
      statement.setInt(10, 4)
    } else {
      statement.setInt(10, 2)
    }
    if (bats.getOrElse(throws.get) == "Right") {
      statement.setInt(11, 2)
    } else {
      statement.setInt(11, 4)
    }
    statement.setString(12, hitter.leftValues.mkString("/"))
    statement.setString(13, hitter.rightValues.mkString("/"))
    statement.setString(14, "sp/sp")
    statement.setString(15, "sp/sp")
    statement.setString(16, "sp/sp")
    statement.setString(17, s"${power.getOrElse("Poor")}/${power.getOrElse("Poor")}")
    statement.setString(18, clutch.get)
    statement.setString(19, atBats.get.toString)
    statement.setInt(20, pitcherId)
    statement.execute()
    val keys = statement.getGeneratedKeys
    keys.next()
    val playerId = keys.getInt(1)
    statement.close()
    bunts.get.save(connection, playerId)
    defense.get.save(connection, playerId)
    val wildStatement = connection.prepareStatement(
      """INSERT INTO WildRating (player_id, rating, games)
          VALUES (?, ?, ?)""")
    wildPlay.foreach(s => {
      val m = """([A-Z]+)\s*(\((\d+)\))?""".r.findAllIn(s)
      wildStatement.setInt(1, playerId)
      wildStatement.setString(2, m.group(1))
      if (m.group(2) == null) {
        wildStatement.setNull(3, java.sql.Types.INTEGER)
      } else {
        wildStatement.setInt(3, m.group(3).toInt)
      }
      wildStatement.execute()
    })
  }

  private def addResultFromList(tokens: List[String]): Pitcher = {
    parseResultFromList(tokens) match {
      case Some((result, leftValue, rightValue)) =>
        val changedPitcher = copy(
            leftResults = result.left :: this.leftResults,
            rightResults = result.right :: this.rightResults,
            leftValues = leftValue :: this.leftValues,
            rightValues = rightValue :: this.rightValues
        )
        if (changedPitcher.leftValues.length > changedPitcher.leftFatigueValues.length) {
          changedPitcher.copy(
            leftFatigueValues = leftValue :: leftFatigueValues,
            rightFatigueValues = rightValue :: rightFatigueValues
          )
        } else {
          changedPitcher
        }
      case _ => this
    }
  }

  override def addTokens(tokens: List[String]): (CardEntity, Boolean, List[String]) = {
    val ConcatenatedName = """(.*?)[A-Z]{2,3}$""".r
    tokens match {
      case Nil => (this, false, Nil)
      case x :: y :: _ if y.contains("cardset") => (this, false, Nil)
      case x :: _ if x.contains("cardset") => (this, false, Nil)
      case a :: b :: c :: tail if isListRange(List(a, b, c)) =>
        val (newPitcher, remaining) = addResultFromList(List(a, b, c)).addRightToken(tail)
        (newPitcher, false, remaining)
      case a :: b :: tail if isRange(a) && isRange(b) =>
        val (newPitcher, remaining) = copy(
          leftFatigueValues = CardReader.rangeToValue(a) :: leftFatigueValues,
          rightFatigueValues = CardReader.rangeToValue(b) :: rightFatigueValues
        ).addRightToken(tail)
        (newPitcher, false, remaining)
      case a :: b :: tail if Result.isResult(b) && isRange(a) =>
        val (newPitcher, remaining) = addResultFromList(List(a, b, "EMPTY")).addRightToken(tail)
        (newPitcher, false, remaining)
      case a :: b :: tail if isRange(a) =>
        val (newPitcher, remaining) = copy(
          leftFatigueValues = 0 :: leftFatigueValues,
          rightFatigueValues = CardReader.rangeToValue(a) :: rightFatigueValues
        ).addRightToken(b :: tail)
        (newPitcher, false, remaining)
      case a :: b :: tail if isCombinedRange(a) && isRange(b) =>
        val (newPitcher, remaining) = addResultFromList(combinedToList(a) ::: (b :: Nil)).addRightToken(tail)
        (newPitcher, false, remaining)
      case a :: tail if isCombinedRange(a) =>
        val (newPitcher, remaining) = addResultFromList(combinedToList(a)).addRightToken(tail)
        (newPitcher, false, remaining)
      case a :: b :: tail if Result.isResult(a) && isRange(b) =>
        val (newPitcher, remaining) = addResultFromList(List("", a, b)).addRightToken(tail)
        (newPitcher, false, remaining)
      case a :: tail if Result.isResult(a) =>
        val (newPitcher, remaining) = addResultFromList(List("", a, "")).addRightToken(tail)
        (newPitcher, false, remaining)
      case "vs. LH" :: _ :: _ :: remaining =>
        (this, false, remaining)
      case "vs LH" :: _ :: _ :: remaining =>
        (this, false, remaining)
      case leftSlash :: rightSlash :: Nil if leftSlash.contains("/") && rightSlash.contains("/") =>
        (copy(leftSlash = Some(leftSlash), rightSlash = Some(rightSlash)), false, Nil)
      case "Emergency Pitcher" :: _ =>
        (copy(throws = Some("Both"), name = Some("Emergency Pitcher")), true, Nil)
      case "i" :: _ :: tail =>
        (this, false, tail)
      case throws :: name :: _ :: remaining if throws.length == 1 =>
        (copy(throws = Some(CardReader.abbreviationMap(throws.toLowerCase)), name = Some(name)), true, remaining)
      case throws :: name :: Nil if throws.length == 1 =>
        val splitName = name.split("""\s+""")
        if ("""[A-Z]{2,3}""".r.matches(splitName.reverse.head)) {
          (copy(throws = Some(CardReader.abbreviationMap(throws.toLowerCase)), name = Some(splitName.dropRight(1).mkString(" "))), true, Nil)
        } else if (ConcatenatedName.matches(name)) {
          val matches = ConcatenatedName.findAllIn(name)
          (copy(throws = Some(CardReader.abbreviationMap(throws.toLowerCase)), name = Some(matches.group(1))), true, Nil)
        } else {
          (this, false, Nil)
        }
      case tokens if tokens.forall(t => t.length == 3) =>
        (this, false, Nil)

      case _ => (this, false, Nil)
    }
  }

  def addRightToken(tokens: List[String]): (Pitcher, List[String]) = {
    val Clutch = "Clutch:"
    val Pickoff = "Pickoff:"
    val Balk = "Balk:"
    val WildPitch = "WP:"
    val Hold = "Hold:"
    val Wild = "!"
    val Bunts = "Bunts:"
    val PitchersHitting = """Bats: ([RLB])sp#(\d)/([A-Z][a-z]+)""".r
    val Injury = "Inj Days:"
    val Durability = "Durability:"
    val DefenseRating = "Def(E/R):"
    val Usage = """\d+ SP,(\s+\d+ RP)?""".r
    val WildPlay = """[A-Z]+(\(\d+\))?""".r
    val AtBats = "AB:"

    tokens match {
      case Nil => (this, Nil)
      case Clutch :: rating :: tail =>
        (copy(clutch = Some(rating)), tail)
      case rating :: tail if rating.startsWith(Clutch) =>
        (copy(clutch = Some(rating.substring(8))), tail)
      case Pickoff :: rating :: tail =>
        (copy(pickoff = Some(rating.toInt)), tail)
      case Balk :: rating :: tail =>
        (copy(balk = Some(rating)), tail)
      case balkRating :: tail if balkRating.startsWith(Balk) =>
        (copy(balk = Some(balkRating.substring(6))), tail)
      case WildPitch :: rating :: tail =>
        (copy(wildPitch = Some(rating)), tail)
      case Hold :: rating :: tail =>
        val splitTokens = rating.split("/")
        (copy(
          hold = Some(CardReader.abbreviationMap(splitTokens(0).toLowerCase)),
          againstSteal = Some(splitTokens(1).toInt)
        ), tail)
      case DefenseRating :: rating :: tail =>
        val splitRating = rating.split("/")
        (copy(
          defense = Some(Defense("P", primary = true, splitRating(0).toInt, splitRating(1), None, againstSteal, wildPitch, pickoff))
        ), tail)
      case defense :: tail if defense.startsWith(DefenseRating) =>
        val rating = defense.split("""\s+""")(1)
        val splitRating = rating.split("/")
        (copy(
          defense = Some(Defense("P", primary = true, splitRating(0).toInt, splitRating(1), None, againstSteal, wildPitch, pickoff))
        ), tail)
      case "BF RP:" :: rating :: tail =>
        (copy(reliefStamina = Some(rating.replace("-", "0").toInt)), tail)
      case "BF SP:" :: rating :: tail =>
        (copy(startingStamina = Some(rating.replace("-", "0").toInt)), tail)
      case slashStamina :: tail if slashStamina.startsWith("BF ") =>
        val rating = slashStamina.split("""\s+""")(1)
        (copy(slashStamina = Some(rating)), tail)
      case "0  1  2  3" :: tail =>
        (this, tail)
      case "Rest: (days)" :: tail =>
        (this, tail)
      case "Old:" :: rating :: tail =>
        (copy(oldStamina = Some(rating)), tail)
      case usage :: releifUsage :: tail if Usage.matches(usage) =>
        (copy(usage = Some(usage + " " + releifUsage)), tail)
      case usage :: Nil if Usage.matches(usage) =>
        (copy(usage = Some(usage)), Nil)
      case Wild :: wildAttributes :: xs if !isRange(wildAttributes) =>
        (copy(wildPlay = wildAttributes.split(",").map(_.trim).toList.filter(_.nonEmpty) ::: wildPlay), xs)
      case Wild :: tail => (this, tail)
      case AtBats :: value :: tail =>
        (copy(atBats = Some(value.toInt)), tail)
      case "BR/SB:" :: rating :: tail =>
        val splitRating = rating.split("/")
        (copy(runs = Some(splitRating(0).toInt), steal = Some(splitRating(1).toInt)), tail)
      case Bunts :: value :: tail => (copy(bunts = Some(Bunting(value))), tail)
      case hittingInfo :: tail if PitchersHitting.matches(hittingInfo) =>
        val m = PitchersHitting.findAllIn(hittingInfo)
        (copy(bats = Some(CardReader.abbreviationMap(m.group(1).toLowerCase)), hittingAbility = Some(m.group(2).toInt),
          power = Some(CardReader.abbreviationMap(m.group(3).toLowerCase()))), tail)
      case Injury :: value :: tail => (copy(injuryDays = Some(value.toInt)), tail)
      case Durability :: value :: tail =>
        (copy(durability = Some(value.toInt)), tail)
      case extraWildPlay :: tail if WildPlay.matches(extraWildPlay) =>
          (copy(wildPlay = extraWildPlay.split(",").map(_.trim).toList), tail)
      case AtBats :: Nil =>
        (copy(atBats = Some(0)), Nil)
      case "Bats:" :: Nil =>
        (this, tokens.tail)
      case _ => (this, tokens.tail)
    }

  }
}
