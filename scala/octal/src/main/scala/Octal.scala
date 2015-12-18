import scala.annotation.tailrec

object Octal {
  type Octal = String

  def octalToInt(octal: Octal): Int = {
    def isOctalDigit(digit: Char): Boolean = "01234567" contains digit

    require(octal.nonEmpty && (octal forall isOctalDigit))

    octal.foldLeft(0){
      case (result, octalDigit) => result * 8 + octalDigit.asDigit
    }
  }

  def intToOctal(int: Int): Octal =
    if (int == 0) "0"
    else
      unfoldLeft(int) { n =>
        if (n == 0) None
        else Some(n / 8, n % 8)
      } mkString

  private def unfoldLeft[A, B](seed: B)(f: B => Option[(B, A)]): Seq[A] = {
    @tailrec
    def loop(seed: B)(as: Seq[A]): Seq[A] = f(seed) match {
      case Some((b, a)) => loop(b)(a +: as)
      case None => as
    }

    loop(seed)(Seq.empty[A])
  }
}
