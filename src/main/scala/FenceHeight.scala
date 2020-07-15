import java.sql.Connection

case class FenceHeight(
                        location: String,
                        fenceHeight: Int
                      ) {
  def save(connection: Connection, parkId: Int): Unit = {
    val statement = connection.prepareStatement(
      """INSERT into FenceHeight (park_id, location, fence_height) VALUES (?,?,?)""")
    statement.setInt(1, parkId)
    statement.setString(2, location)
    statement.setInt(3, fenceHeight)
    statement.execute()
  }
}
