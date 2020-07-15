import java.sql.Connection
class PitcherHitting extends CardEntity {
  var batter = Batter(None, None, None, None, Nil, Nil, Nil, Nil, Nil, None, None, None, None, None, None, None,
    None, None, None, None, Nil, None)

  override def addTokens(tokens: List[String]): (CardEntity, Boolean, List[String]) = tokens match {
    case "Pitcher’s Batting" :: number :: _ =>
      batter = batter.copy(name = Some(number))
      (this, true, Nil)
    case "Card" :: _ => (this, false, Nil)
    case x :: _ if x.startsWith("Rsp") => (this, false, Nil)
    case leftSlash :: rightSlash :: Nil if leftSlash.contains("/") && rightSlash.contains("/") =>
      batter = batter.copy(leftSlash = Some(leftSlash), rightSlash = Some(rightSlash))
      (this, finished, Nil)
    case _ =>
      val (newBatter, finished, remaining) = batter.addTokens(tokens)
      batter = newBatter.asInstanceOf[Batter]
      (this, finished, remaining)
  }

  override def finished: Boolean = batter.name.nonEmpty

  override def save(connection: Connection): Unit = ???
}
