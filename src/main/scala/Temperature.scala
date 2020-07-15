import java.sql.Connection

case class Temperature(
                        month: String,
                        timeOfDay: String,
                        temp: String,
                        frequency: Int
                      ) extends MonthRange(month, frequency) {
  def save(connection: Connection, geoId: Int, rank: Int): Unit = {
    val statement = connection.prepareStatement(
      """INSERT INTO Temperature
        (geographic_location_id, time_of_day, game_month, temp, `rank`, frequency)
       VALUES (?, ?, ?, ?, ?, ?)""")
    statement.setInt(1, geoId)
    statement.setString(2, timeOfDay)
    statement.setString(3, month)
    statement.setString(4, temp)
    statement.setInt(5, rank)
    statement.setInt(6, frequency)
    statement.execute()
  }

}
