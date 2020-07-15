import java.sql.Connection

case class Defense(
                  position: String,
                  primary: Boolean,
                  error: Int,
                  rating: String,
                  pivot: Option[Char],
                  throws: Option[Int],
                  lostPitch: Option[String],
                  stopJump: Option[Int]
                  ) {
  def save(connection: Connection, playerId: Int): Unit = {
    val defenseStatement = connection.prepareStatement("""INSERT INTO Defense
      (player_id, position, `primary`, error, rating, pivot, throws, lost_pitch, stop_jump)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""")
    defenseStatement.setInt(1, playerId)
    defenseStatement.setString(2, position)
    defenseStatement.setBoolean(3, primary)
    defenseStatement.setInt(4, error)
    defenseStatement.setString(5, rating)
    if (pivot.nonEmpty) {
      defenseStatement.setString(6, pivot.get.toString)
    } else {
      defenseStatement.setNull(6, java.sql.Types.CHAR)
    }
    if (throws.nonEmpty) {
      defenseStatement.setInt(7, throws.get)
    } else {
      defenseStatement.setNull(7, java.sql.Types.INTEGER)
    }
    if (lostPitch.nonEmpty) {
      defenseStatement.setString(8, lostPitch.get)
    } else {
      defenseStatement.setNull(8, java.sql.Types.VARCHAR)
    }
    if (stopJump.nonEmpty) {
      defenseStatement.setInt(9, stopJump.get)
    } else {
      defenseStatement.setNull(9, java.sql.Types.INTEGER)
    }
    defenseStatement.execute()
  }
}

object Defense {
  val positions: Set[String] = Set("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF")
  def apply(token: String): Defense = {
    val Ratings = """(\w+)\s+(\d+)/([A-Z][AK]?)([+-])?/?(([+-])?\d+)?/?(\w+)?/?(([+-])?\d+)?""".r
    token match {
      case Ratings(position, error, rating, null, null, null, null, null, null) =>
        new Defense(position, positions.contains(position), error.toInt, rating, None, None, None, None)
      case Ratings(position, error, rating, pivot, null, null, null, null, null) =>
        new Defense(position, positions.contains(position), error.toInt, rating, Some(pivot(0)), None, None, None)
      case Ratings(position, error, rating, null, arm, sign, null, null, null) =>
        val armAsInt = if (sign == '+') { arm.substring(1).toInt} else arm.toInt
        new Defense(position, positions.contains(position), error.toInt, rating, None, Some(armAsInt), None, None)
      case Ratings(position, error, rating, null, arm, armSign, lostPitch, stopJump, stopJumpSign) =>
        val armAsInt = if (armSign == '+') { arm.substring(1).toInt} else arm.toInt
        val stopJumpAsInt = if (stopJumpSign == '+') { stopJump.substring(1).toInt} else stopJump.toInt
        new Defense(position, positions.contains(position), error.toInt, rating, None, Some(armAsInt),
          Some(CardReader.abbreviationMap(lostPitch.toLowerCase)), Some(stopJumpAsInt))
    }
  }
}
