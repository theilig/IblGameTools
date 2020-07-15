

object CreateBatters extends App with CardReader {
  override def getArgs: Array[String] = args

  override val sourceFile = "batters2020.txt"

  override def newEntity: Batter = Batter(None, None, None, None, Nil, Nil, Nil, Nil, Nil, None, None, None, None, None,
    None, None, None, None, None, None, Nil, None)

  execute(69, 140)
}
