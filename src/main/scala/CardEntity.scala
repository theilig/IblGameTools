import java.sql.Connection

import scala.util.Try

trait CardEntity {
  def finished: Boolean

  def save(connection: Connection): Unit

  def addTokens(tokens: List[String]): (CardEntity, Boolean, List[String])

  def isRange(s: String): Boolean = {
    s == "EMPTY" || Try(s.split("-").map(_.trim.toInt)).toOption.nonEmpty
  }

  def isListRange(l: List[String]): Boolean = l match {
    case a :: b :: c :: d :: e :: _ => isRange(a) && isRange(e) && Result.isResult(s"$b $c $d")
    case a :: b :: c :: d :: _ => isRange(a) && isRange(c) && Result.isResult(b) && !isRange(d)
    case a :: b :: c :: _ => isRange(a) && isRange(c) && Result.isResult(b)
    case a :: b :: _ => (isRange(a) && Result.isResult(b)) || (isRange(b) && Result.isResult(a))
    case _ => false
  }

  def combinedToList(a: String): List[String] = {
    val unpacked: Array[String] = a.split("""\s+""")
    unpacked.length match {
      case x if x >= 5 =>
        List(unpacked(0), s"${unpacked(1)} ${unpacked(2)} ${unpacked(3)}", unpacked(4))
      case 3 => List(s"${unpacked(0)} ${unpacked(1)} ${unpacked(2)}")
      case 4 if Result.isResult(unpacked.drop(1).mkString(" ")) =>
        List(unpacked(0), s"${unpacked(1)} ${unpacked(2)} ${unpacked(3)}")
      case 4 =>
        List(s"${unpacked(0)} ${unpacked(1)} ${unpacked(2)}", unpacked(3))
      case _ => List()
    }
  }

  def isCombinedRange(token: String): Boolean =
    isListRange(combinedToList(token))

  def parseResultFromList(tokens: List[String]): Option[(Result, Int, Int)] = {
    tokens match {
      case a :: b :: c :: _ =>
        val result = new Result(b)
        Some((result, CardReader.rangeToValue(a), CardReader.rangeToValue(c)))
      case a :: b :: Nil if Result.isResult(b) =>
        val result = new Result(b)
        Some((result, CardReader.rangeToValue(a), 0))
      case a :: b :: Nil =>
        val result = new Result(a)
        Some((result, 0, CardReader.rangeToValue(b)))
      case a :: Nil =>
        val result = new Result(a)
        Some((result, 0, 0))
      case _ => None
    }
  }
}
