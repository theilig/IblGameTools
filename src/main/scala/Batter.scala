import java.sql.{Connection, Statement}

case class Batter(
                 name: Option[String],
                 bats: Option[String],
                 leftSlash: Option[String],
                 rightSlash: Option[String],
                 leftResults: List[String],
                 rightResults: List[String],
                 leftValues: List[Int],
                 rightValues: List[Int],
                 defense: List[Defense],
                 ifrTendency: Option[String],
                 ofrTendency: Option[String],
                 dfTendency: Option[String],
                 power: Option[String],
                 runs: Option[Int],
                 steals: Option[Int],
                 jumps: Option[Int],
                 bunts: Option[Bunting],
                 durability: Option[Int],
                 injuryDays: Option[Int],
                 clutch: Option[String],
                 wildPlay: List[String],
                 plateAppearances: Option[String]
                 ) extends CardEntity {
  override def finished: Boolean = List(name, leftSlash, rightSlash, ifrTendency, ofrTendency,
    dfTendency, power, runs, steals, jumps, bunts, durability, injuryDays, clutch, plateAppearances).forall(_.nonEmpty) &&
  List(leftValues, rightValues, leftResults, rightResults).forall(_.nonEmpty)

  def addRightToken(tokens: List[String]): (Batter, List[String]) = {
    val PaSplit = """\d+ vL,\s+\d+ vR""".r
    val Wild = "!"
    val Clutch = "Clutch:"
    val Injury = "Inj Days:"
    val Durability = "Durability:"
    val Bunts = "Bunts:"
    val Jumps = "Jump:"
    val Steals = "Stealing:"
    val Runs = "Runs:"
    val Power = "Power:"
    val DeepFly = "DF:"
    val Ifr = "IFR:"
    val Ofr = "OFR:"
    tokens match {
      case Nil => (this, Nil)
      case x :: _ if isRange(x) => (this, tokens)
      case "LAW" :: tail =>
        (copy(wildPlay = List("LAW")), tail)
      case Wild :: wildAttributes :: xs if !isRange(wildAttributes) => (copy(wildPlay = wildAttributes.split(",").map(_.trim).filter(_.nonEmpty).toList ::: wildPlay), xs)
      case Wild :: tail => (this, tail)
      case x :: tail if PaSplit.matches(x) => (copy(plateAppearances = Some(tokens.head)), tail)
      case Clutch :: rating :: tail => (copy(clutch = Some(rating)), tail)
      case Injury :: value :: tail => (copy(injuryDays = Some(value.toInt)), tail)
      case Durability :: value :: tail => (copy(durability = Some(value.toInt)), tail)
      case Bunts :: value :: tail => (copy(bunts = Some(Bunting(value))), tail)
      case Jumps :: value :: tail => (copy(jumps = Some(value.toInt)), tail)
      case Steals :: value :: tail => (copy(steals = Some(value.toInt)), tail)
      case Runs :: value :: tail => (copy(runs = Some(value.toInt)), tail)
      case Power :: value :: tail => (copy(power = Some(value)), tail)
      case DeepFly :: value :: tail => (copy(dfTendency = Some(value)), tail)
      case Ifr :: value :: tail => (copy(ifrTendency = Some(value)), tail)
      case Ofr :: value :: tail => (copy(ofrTendency = Some(value)), tail)
      case "vL/vR" :: tail => (this, tail)
      case position :: tail if Defense.positions.contains(position.split("""\s+""").toList.head.toUpperCase) =>
        (copy(defense = Defense(position) :: defense), tail)
      case _ => (this, tokens)
    }
  }

  override def addTokens(tokens: List[String]): (CardEntity, Boolean, List[String]) = {
    val ConcatenatedName = """(.*?)[A-Z]{2,3}$""".r
    tokens match {
      case Nil => (this, false, tokens)
      case x :: tail if x.contains("cardset") => (this, this.finished, tail)
      case a :: b :: c :: tail if isListRange(List(a, b, c)) =>
        val (newBatter, remaining) = addResultFromList(List(a, b, c)).addRightToken(tail)
        (newBatter, false, remaining)
      case a :: b :: tail if isListRange(List(a, b)) =>
        val (newBatter, remaining) = addResultFromList(List(a, b)).addRightToken(tail)
        (newBatter, false, remaining)
      case a :: b :: tail if isCombinedRange(a) && isCombinedRange(b) =>
        (addResultFromList(combinedToList(a)), false, b :: tail)
      case a :: b :: tail if isCombinedRange(a) && isRange(b) =>
        val (newBatter, remaining) = addResultFromList(combinedToList(a) ::: (b :: Nil)).addRightToken(tail)
        (newBatter, false, remaining)
      case a :: tail if isCombinedRange(a) =>
        val (newBatter, remaining) = addResultFromList(combinedToList(a)).addRightToken(tail)
        (newBatter, false, remaining)
      case a :: b :: _ :: tail if Result.isResult(a) && isRange(b) =>
        val (newBatter, remaining) = addResultFromList(List("", a, b)).addRightToken(tail)
        (newBatter, false, remaining)
      case a :: b :: tail if Result.isResult(a) && isRange(b) =>
        val (newBatter, remaining) = addResultFromList(List("", a, b)).addRightToken(tail)
        (newBatter, false, remaining)
      case a :: b :: tail if Result.isResult(a) && (Result.isResult(b) || isCombinedRange(b)) =>
        (addResultFromList(List("", a, "")), false, b :: tail)
      case a :: tail if Result.isResult(a) =>
        val (newBatter, remaining) = addResultFromList(List("", a, "")).addRightToken(tail)
        (newBatter, false, remaining)
      case "vs. LH" :: _ :: _ :: remaining =>
        (this, false, remaining)
      case leftSlash :: rightSlash :: "Defense:" :: remaining =>
        (copy(leftSlash = Some(leftSlash), rightSlash = Some(rightSlash)), false, remaining)
      case tokens if tokens.forall(t => t.length == 3) =>
        (this, false, Nil)
      case batSide :: name :: _ :: remaining if batSide.length == 1 =>
        (copy(bats = Some(CardReader.abbreviationMap(batSide.toLowerCase)), name = Some(name)), true, remaining)
      case batSide :: name :: Nil if batSide.length == 1 =>
        val splitName = name.split("""\s+""")
        if ("""[A-Z]{2,3}""".r.matches(splitName.reverse.head)) {
          (copy(bats = Some(CardReader.abbreviationMap(batSide.toLowerCase)), name = Some(splitName.dropRight(1).mkString(" "))), true, Nil)
        } else if (ConcatenatedName.matches(name)) {
          val matches = ConcatenatedName.findAllIn(name)
          (copy(bats = Some(CardReader.abbreviationMap(batSide.toLowerCase)), name = Some(matches.group(1))), true, Nil)
        } else {
          (this, false, Nil)
        }
      case "DH ONLY" :: tail =>
        (this, false, tail)

      case _ => (this, false, tokens)
    }
  }

  private def addResultFromList(tokens: List[String]): Batter = {
    parseResultFromList(tokens) match {
      case Some((result, leftValue, rightValue)) =>
        copy(
          leftResults = result.left :: this.leftResults,
          rightResults = result.right :: this.rightResults,
          leftValues = leftValue :: this.leftValues,
          rightValues = rightValue :: this.rightValues
        )
      case _ => this
    }
  }

  override def save(connection: Connection): Unit = {
    val statement = connection.prepareStatement(
      """INSERT into Player (name, bats, durability, injury_days, running, stealing, jump,
         slash_left, slash_right, template_id_left, template_id_right, results_left, results_right,
         ifr_location, ofr_location, df_location, power, clutch, `usage`)
         VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)""",
      Statement.RETURN_GENERATED_KEYS
    )
    val leftTemplateId = CreateBatters.findTemplate(leftResults)
    val rightTemplateId = CreateBatters.findTemplate(rightResults)
    statement.setString(1, name.get)
    statement.setString(2, bats.get)
    statement.setInt(3, durability.get)
    statement.setInt(4, injuryDays.get)
    statement.setInt(5, runs.get)
    statement.setInt(6, steals.get)
    statement.setInt(7, jumps.get)
    statement.setString(8, leftSlash.get)
    statement.setString(9, rightSlash.get)
    statement.setInt(10, leftTemplateId)
    statement.setInt(11, rightTemplateId)
    statement.setString(12, leftValues.mkString("/"))
    statement.setString(13, rightValues.mkString("/"))
    statement.setString(14, ifrTendency.get)
    statement.setString(15, ofrTendency.get)
    statement.setString(16, dfTendency.get)
    statement.setString(17, power.get)
    statement.setString(18, clutch.get)
    statement.setString(19, plateAppearances.get)
    statement.execute()
    val keys = statement.getGeneratedKeys
    keys.next()
    val playerId = keys.getInt(1)
    statement.close()
    defense.foreach(d => d.save(connection, playerId))
    bunts.get.save(connection, playerId)
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
}

