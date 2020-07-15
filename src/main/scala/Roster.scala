import java.sql.Connection
case class Roster(team: Option[Int], player: Option[Int]) extends CardEntity {
  override def finished: Boolean = ???

  override def save(connection: Connection): Unit = ???

  override def addTokens(tokens: List[String]): (CardEntity, Boolean, List[String]) = ???
}
