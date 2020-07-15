object CreateTeamsAndGeoraphicAreas extends App with CardReader {
  override def getArgs = args

  override val sourceFile = "geo2020.txt"

  override def newEntity = GeoLocation(None, Nil, Nil, Nil, Nil)

  execute(200, 400)

  override def allTokens(firstSplit: Int, secondSplit: Int): List[List[String]] = {
    lines.map(l =>
      l.replaceAll("""\s{6}""", " EMPTY ").split("""\s+""").toList).toList
  }

}

