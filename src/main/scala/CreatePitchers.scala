object CreatePitchers extends App with CardReader {
  override def getArgs: Array[String] = args

  override val sourceFile = "pitchers2020.txt"

  override def newEntity: CardEntity = Pitcher(None, None, None, None, Nil, Nil, Nil, Nil, Nil, Nil, None, None, None,
    None, None, None, None, None, None, None, None, None, Nil, None, None, None, None, None, None, None, None, None)


  override def tokenize(line: String) =
    if (line.startsWith("                    ")) {
      super.tokenize(line.replaceFirst("                    ", "        EMPTY    "))
   } else {
      super.tokenize(line)
  }

  val extraCards: List[CardEntity] = new ExtraCards(args).load()
  extraCards.foreach {
    case p: Pitcher =>
      p.savePitcher(connection)
    case _ => ;
  }
  execute(175, 345)
}
