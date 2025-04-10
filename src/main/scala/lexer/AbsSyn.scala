package lexer

class AbsSyn

sealed trait RegEx

case object Emp extends RegEx
case class Ch(char: Char) extends RegEx
case class Alt(left: RegEx, right: RegEx) extends RegEx
case class Star(regEx: RegEx) extends RegEx
case class Sequence(regExs: Vector[RegEx]) extends RegEx

object Sequence {
  def apply(regexs: RegEx*): Sequence = new Sequence(regexs.toVector)
}

object RegEx {
  def prettyPrint(regExp: RegEx): String = {
    val sb = StringBuilder()
    prettyPrint(regExp, sb)
    sb.toString()
  }

  private def prettyPrint(exp: RegEx, sb: StringBuilder): Unit = {
    sb.append('(')
    exp match {
      case Ch(c) => sb.append(c)
      case Alt(left, right) =>
        prettyPrint(left, sb)
        sb.append('|')
        prettyPrint(right, sb)

      case Star(regExp) =>
        prettyPrint(regExp, sb)
        sb.append('*')

      case Sequence(regExprs) =>
        regExprs.foreach { regExpr => prettyPrint(regExpr, sb) }
      case Emp =>
    }
    sb.append(')')
  }

}

extension (regex: RegEx)
  def ~>(that: RegEx): Sequence = {
    regex match {
      case Sequence(regexs) =>
        Sequence(regexs.appended(that))

      case _ =>
        that match {
          case Sequence(regexes) => Sequence(regexes.prepended(regex))
          case _                 => Sequence(Vector.apply(regex, that))
        }
    }
  }
