import java.sql.Connection

case class Bunting(
                  inPlay: Int,
                  placement: String,
                  runningSpeed: Int
                  ) {
  def save(connection: Connection, playerId: Int): Unit = {
    val buntingStatement = connection.prepareStatement(
      """INSERT INTO Bunting
      (player_id, in_play, placement, speed)
      VALUES (?, ?, ?, ?)""")
    buntingStatement.setInt(1, playerId)
    buntingStatement.setInt(2, inPlay)
    buntingStatement.setString(3, placement)
    buntingStatement.setInt(4, runningSpeed)
    buntingStatement.execute()
  }
}

object Bunting {
  def apply(token: String): Bunting = {
    val split = token.split("/")
    new Bunting(split(0).toInt, CardReader.abbreviationMap(split(1).toLowerCase), split(2).toInt)
  }
}
