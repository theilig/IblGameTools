import java.sql.Connection

case class Precipitation (
                           month: String,
                           range: Int,
                           precip: String
                         ) extends MonthRange(month, range) {
  def save(connection: Connection, geoId: Int, rank: Int): Unit = {
    val statement = connection.prepareStatement(
      """INSERT INTO Precipitation
        (geographic_location_id, game_month, precip, `rank`, frequency)
       VALUES (?, ?, ?, ?, ?)""")
    statement.setInt(1, geoId)
    statement.setString(2, month)
    statement.setString(3, precip)
    statement.setInt(4, rank)
    statement.setInt(5, range)
    statement.execute()
  }
}
