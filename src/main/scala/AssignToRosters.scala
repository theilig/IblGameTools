import java.io.FileWriter

import scala.io.StdIn

object AssignToRosters extends App with CardReader {
  override def getArgs: Array[String] = args

  override val sourceFile: String = "player_team_mapping2020.txt"

  override def newEntity: CardEntity = Roster(None, None)

  def save(playerMap: Map[String, Int]): Unit = {
    val statement = connection.prepareStatement("""INSERT INTO Roster (player_id, team_id) (SELECT player_id, ? from Player where name = ?)""")
    playerMap.foreach{
      case (name, teamId) =>
        statement.setString(2, name)
        statement.setInt(1, teamId)
        statement.execute()
    }
  }

  override def execute(firstSplit: Int, secondSplit: Int): Unit = {
    val readFromFile: Map[String, Int] = lines.flatMap(l =>
    l.split(",").toList match {
      case name :: team :: _ => Some(name -> team.toInt)
      case _ => None
    }).toMap
    save(readFromFile)
    input.close()
    val statement = connection.prepareStatement("""SELECT name from Player order by player_id""")
    val resultSet = statement.executeQuery
    while (resultSet.next()) {
      val name = resultSet.getString(1)
      print(s"$name:\n")
      if (!readFromFile.contains(name)) {
        val team = StdIn.readLine().toInt
        val writer = new FileWriter("src/main/resources/player_team_mapping2020.txt", true)
        writer.write(s"$name,$team\n")
        writer.flush()
        writer.close()
        save(Map(name -> team))
      }
    }
  }
  execute(0, 1)
}
