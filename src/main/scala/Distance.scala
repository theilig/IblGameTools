import java.sql.Connection

case class Distance(
                     location: String,
                     windType: String,
                     fenceDistance: Int
                   ) {
  def save(connection: Connection, parkId: Int): Unit = {
    val statement = connection.prepareStatement(
      """INSERT into Distance (park_id, location, wind_type, fence_distance) VALUES (?,?,?,?)""")
    statement.setInt(1, parkId)
    statement.setString(2, location)
    statement.setString(3, windType)
    statement.setInt(4, fenceDistance)
    statement.execute()
  }
}

