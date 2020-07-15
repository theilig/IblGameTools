class Result(r: String) {
  private val tokens = r.split("""\s+""")
  val (left, right) = if (Result.nonLocatedResults.contains(r)) {
    (r, r)
  } else {
    (s"${tokens(1)} ${tokens(0)}", s"${tokens(1)} ${tokens(2)}")
  }
}

object Result {
  val nonLocatedResults = Set("SO", "BB", "HB", "E", "WP & PB", "!", "HR", "CFR", "IFR", "OFR", "DF", "PARK?")

  def isLocation(s: String): Boolean = {
    val locations = Set("1b", "ss", "2b", "3b", "glf", "lfl", "lfw", "llf", "lf", "lcf", "lc", "cfw", "cf", "gcf",
      "grf", "rfl", "rfw", "lrf", "rf", "rcf", "rc", "rf", "p", "c", "inf")
    locations.contains(s.trim.toLowerCase)
  }

  def isResult(s: String): Boolean = {
    s.split("""\s+""").toList match {
      case x :: _ :: z :: _ if isLocation(x) && isLocation(z) => true
      case x :: Nil if nonLocatedResults.contains(x) => true
      case _ => nonLocatedResults.contains(s)
    }
  }
}
