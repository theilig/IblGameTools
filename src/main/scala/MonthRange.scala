class MonthRange(month: String, range: Int) {
  def getMonth: String = month
  def getRange: Int = range
}

object MonthRange {
  def validate(entries: List[MonthRange]): Boolean = {
    List("April", "May", "June", "July", "August", "September", "October").forall(month => {
      val sum = entries.collect({
        case x if x.getMonth == month => x.getRange
      }).sum
      sum == 100
    })
  }
}
