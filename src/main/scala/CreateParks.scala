object CreateParks extends App with CardReader {

  override def getArgs: Array[String] = args

  def rangeToResults(values: List[String]): String = {
      values.map(CardReader.rangeToValue).mkString("/")
  }

  override val sourceFile = "parks2020.txt"

  override def tokenize(line: String): List[String] = {
    val Cross = """(\d\d)(cross|no)""".r
    val fixedLine = Cross.replaceAllIn(line, m => m.group(1) + "   " + m.group(2)).
      replaceAll("9900", "99   00").
      replaceAll("crossno", "cross   no")
    val tokens = fixedLine.trim().split("""\s\s\s+""").map(_.trim).toList
    tokens
  }


  val fixedLines = lines.flatMap(l => {
    val hasCross = l.contains("cross")
    val hasNoWind = l.contains("no wind") && !l.contains("Special Park")
    val hasStraight = l.contains("straight")
    var fixedLines: List[String] = List()
    if (hasStraight) {
      fixedLines = """(straight[^a-z]*)""".r.findAllIn(l).mkString("    ") :: fixedLines
    }
    if (hasNoWind) {
      fixedLines = """(no wind[^a-z]*)""".r.findAllIn(l).mkString("    ") :: fixedLines
    }
    if (hasCross) {
      fixedLines = """(cross[^a-z]*)""".r.findAllIn(l).mkString("    ") :: fixedLines
    }
    if (fixedLines.nonEmpty) {
      fixedLines
    } else {
      List(l)
    }
  })
  override def allTokens(firstSplit: Int, secondSplit: Int): List[List[String]] = fixedLines.map(tokenize).toList.reverse
  input.close()

  def newEntity: CardEntity = Park(None, None, None, None, None, None, List(), List(), List(), None, None, List())

  execute(69, 140)
}
