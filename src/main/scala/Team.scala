import java.sql.Connection

case class Team(
               city: String,
               nickname: String,
               geoId: Int,
               parkId: Int
               ) {
  def save(connection: Connection): Unit = {
    val statement = connection.prepareStatement("INSERT INTO Team (city, nickname, geographic_location_id, home_park_id) VALUES (?, ?, ?, ?)")
    statement.setString(1, city)
    statement.setString(2, nickname)
    statement.setInt(3, geoId)
    statement.setInt(4, parkId)
    statement.execute()
  }
}
