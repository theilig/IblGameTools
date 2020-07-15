import java.sql.{Connection, DriverManager, ResultSet}

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

trait CardReader {
  def getArgs: Array[String]
  lazy val host: String = getArgs(0)
  lazy val user: String = getArgs(1)
  lazy val password: String = getArgs(2)
  val driver = "com.mysql.jdbc.Driver"
  lazy val url = s"jdbc:mysql://$getHost/IblGame"
  lazy val connection: Connection = {
    Class.forName(driver)
    DriverManager.getConnection(url, user, password)
  }

  def allTokens(firstSplit: Int, secondSplit: Int): List[List[String]] = {
    val broken = lines.map(l => {
      l.length match {
        case x if x < firstSplit =>
          l.substring(5)
        case x if x < secondSplit =>
          (l.substring(5, firstSplit - 3), l.substring(firstSplit))
        case _ =>
          (l.substring(5, firstSplit - 3), l.substring(firstSplit, secondSplit - 3), l.substring(secondSplit))
      }
    }).toList.reverse
    broken.map({
      case (x: String, _, _) => tokenize(x)
      case (x: String, _) => tokenize(x)
      case x: String => tokenize(x)
    }) :::
      broken.flatMap({
        case (_, y: String, _) => Some(tokenize(y))
        case (_, y: String) => Some(tokenize(y))
        case _ => None
      }) :::
      broken.flatMap({
        case (_, _, z: String) => Some(tokenize(z))
        case _ => None
      })
  }

  val sourceFile: String
  lazy val input: BufferedSource = Source.fromResource(sourceFile)
  lazy val lines: Iterator[String] = input.getLines()

  def getHost: String = host

  def splitResult(result: String): (String, Option[String]) = {
    val Locations: List[String] = List("8", "*", "lcf", "lc", "rc", "*", "ss", "inf", "gcf", "lcf", "ss", "2b", "3b", "1b", "rcf", "llf", "lrf",
      "cfw", "lfl", "rfl", "lfw", "glf", "grf", "cf", "rfw", "lf", "rf")
    val resultSplit = Locations.find(l => result.endsWith(l)).map(l => (result.substring(0, result.length - l.length).trim, Some(l))).
      getOrElse((result.trim, None))
    if (resultSplit._2.contains("8")) {
      (resultSplit._1, Some("cf"))
    } else {
      resultSplit
    }
  }

  def createTemplate(results: List[String]): Int = {
    val statement = connection.createStatement()
    val resultSet = statement.executeQuery("SELECT MAX(template_id) FROM ResultTemplate")
    resultSet.next()
    val templateId = resultSet.getInt(1) + 1
    statement.close()
    val insertStatement = connection.prepareStatement("INSERT INTO ResultTemplate (template_id, `rank`, result, location) VALUES (?, ?, ?, ?)")
    var rank = 0
    results.foreach(r => {
      val (result, location) = splitResult(r)
      insertStatement.setInt(1, templateId)
      insertStatement.setInt(2, rank)
      insertStatement.setString(3, result)
      if (location.nonEmpty && location.get != "*" ) {
        insertStatement.setString(4, location.get)
      } else {
        insertStatement.setNull(4, java.sql.Types.VARCHAR)
      }
      insertStatement.execute()
      rank += 1
    })
    insertStatement.close()
    templateId
  }

  def findTemplate(results: List[String]): Int = {
    @scala.annotation.tailrec
    def matchTemplate(results: List[String], queryResultSet: ResultSet): Option[Int] = {
      def clearTemplate(id: Int): Unit = {
        while (queryResultSet.next() && queryResultSet.getInt("template_id") == id) {}
      }

      @scala.annotation.tailrec
      def innerMatch(resultsToMatch: List[String]): Boolean = {
        resultsToMatch match {
          case Nil => true
          case x :: xs =>
            val (result, location) = splitResult(x)
            val resultMatches = result.toUpperCase() == queryResultSet.getString("result").toUpperCase()
            val locationMatches = if (queryResultSet.getString("location") == null) {
              location.isEmpty || location.get == "*"
            } else {
              location.nonEmpty &&
                location.get.toUpperCase() == queryResultSet.getString("location").toUpperCase()
            }
            if (resultMatches && locationMatches) {
              if (queryResultSet.next()) {
                innerMatch(xs)
              } else {
                xs.isEmpty
              }
            } else {
              clearTemplate(queryResultSet.getInt("template_id"))
              false
            }
        }
      }

      val templateId = queryResultSet.getInt("template_id")
      if (innerMatch(results)) {
        Some(templateId)
      } else if (!queryResultSet.isAfterLast) {
        matchTemplate(results, queryResultSet)
      } else {
        None
      }
    }

    val statement = connection.createStatement()
    val queryResultSet = statement.executeQuery("SELECT template_id, result, location FROM ResultTemplate ORDER BY template_id, rank")
    val matchingId: Option[Int] = if (queryResultSet.next()) {
      matchTemplate(results, queryResultSet)
    } else {
      None
    }
    statement.close()
    matchingId.getOrElse(createTemplate(results))
  }


  def tokenize(line: String): List[String] = {
    line.trim().split("""\s\s\s+""").map(_.trim).toList
  }

  def addTokens(tokens: List[String], currentEntities: List[CardEntity]): (List[CardEntity], List[CardEntity]) = {
    currentEntities match {
      case park :: otherParks =>
        val (changedPark, finished, remainingTokens) = park.addTokens(tokens)
        val (otherResults, otherFinished) = addTokens(remainingTokens, otherParks)
        if (finished && changedPark.finished) {
          (newEntity :: otherResults, changedPark :: otherFinished)
        } else {
          (changedPark :: otherResults, otherFinished)
        }
      case Nil =>
        (Nil, Nil)
    }
  }

  def newEntity: CardEntity

  def createEntities(allTokens: List[List[String]]): List[CardEntity] = {

    @tailrec
    def createInternal(finished: List[CardEntity], currentParks: List[CardEntity], remainingLines: List[List[String]]): List[CardEntity] = {
      remainingLines match {
        case Nil => currentParks.filter(p => p.finished) ::: finished
        case x :: xs =>
          val (nextCurrentParks, moreFinished) = addTokens(x, currentParks)
          createInternal(moreFinished ::: finished, nextCurrentParks, xs)
      }
    }
    createInternal(List(), List(newEntity, newEntity, newEntity), allTokens)
  }
  def execute(firstSplit: Int, secondSplit: Int): Unit = {
    val entities: Seq[CardEntity] = createEntities(allTokens(firstSplit, secondSplit))
    entities.foreach(p => p.save(connection))
    connection.close()
  }
}

object CardReader {
  val abbreviationMap: Map[String, String] =
    Map("av" -> "Average", "pr" -> "Poor", "r" -> "Right", "l" -> "Left", "vg" -> "Very Good", "ex" -> "Excellent",
      "b" -> "Both", "fr" -> "Fair"
    )
  def rangeToValue(r: String): Int = {
    if (r.trim.isEmpty || r.trim == "EMPTY") {
      0
    } else if (r.contains("-")) {
      val range = r.trim.split("-")
      range(1).toInt - range(0).toInt + 1
    } else {
      1
    }
  }
}
