import scala.annotation.tailrec

class ExtraCards(args: Array[String]) extends CardReader {
  override def getArgs: Array[String] = args

  override val sourceFile: String = "xcards.txt"

  override def newEntity: CardEntity = new PitcherHitting

  var nextEntities = List(
    newEntity, newEntity,
    newEntity, newEntity, newEntity,
    CreatePitchers.newEntity, newEntity, newEntity, newEntity
  )

  def addToCard(tokens: List[String], currentEntity: CardEntity): (CardEntity, Option[CardEntity]) = {
    val (changedEntity, finished, _) = currentEntity.addTokens(tokens)
    if (finished && changedEntity.finished) {
      val next = nextEntities.head
      nextEntities = nextEntities.tail
      (next, Some(changedEntity))
    } else {
      (changedEntity, None)
    }
  }
  @tailrec
  final def createCards(finished: List[CardEntity], currentCard: CardEntity, remainingLines: List[List[String]]): List[CardEntity] = {
    remainingLines match {
      case Nil => List(currentCard).filter(p => p.finished) ::: finished
      case x :: xs =>
        val (nextCurrentCard, moreFinished) = addToCard(x, currentCard)
        createCards(moreFinished.map(m => m :: finished).getOrElse(finished), nextCurrentCard, xs)
    }
  }
  def load(): List[CardEntity] = {
    createCards(List(), new PitcherHitting(), allTokens(69, 140))
  }
}
