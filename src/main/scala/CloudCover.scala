import java.sql.Connection

case class CloudCover(
                     month: String,
                     range: Int,
                     cover: String
                     ) extends MonthRange(month, range) {
  def save(connection: Connection, geoId: Int, rank: Int): Unit = {
    val statement = connection.prepareStatement(
      """INSERT INTO CloudCover
        (geographic_location_id, game_month, cover, `rank`, frequency)
       VALUES (?, ?, ?, ?, ?)""")
    statement.setInt(1, geoId)
    statement.setString(2, month)
    statement.setString(3, cover)
    statement.setInt(4, rank)
    statement.setInt(5, range)
    statement.execute()
  }

}
