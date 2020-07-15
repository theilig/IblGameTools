import java.sql.Connection

case class Wind(
                 month: String,
                 windType: String,
                 frequency: Int
               ) {
  def save(connection: Connection, parkId: Int, rank: Int): Unit = {
    val statement = connection.prepareStatement(
      """INSERT into Wind (park_id, game_month, wind_type, `rank`, frequency) VALUES (?,?,?,?,?)""")
    statement.setInt(1, parkId)
    statement.setString(2, month)
    statement.setString(3, windType)
    statement.setInt(4, rank)
    statement.setInt(5, frequency)
    statement.execute()
  }
}
