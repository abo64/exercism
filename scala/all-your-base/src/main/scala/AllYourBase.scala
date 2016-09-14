import scala.annotation.tailrec

object AllYourBase {
  type Base = Int
  type Digit = Int
  type Digits = List[Digit]
  type Dec = Int

  def rebase(inputBase: Base, inputDigits: Digits, outputBase: Base): Option[Digits] =
    for {
      inBase <- validBase(inputBase)
      outBase <- validBase(outputBase)
      dec <- toDecimal(inBase, inputDigits) if inputDigits.nonEmpty
    } yield fromDecimal(outBase, dec)

  private def validBase(base: Base): Option[Base] = Some(base) filter (_ > 1)

  private def validDigit(base: Base, digit: Digit): Option[Digit] =
    Some(digit) filter (x => x > -1 && x < base)

  private def toDecimal(base: Base, digits: Digits): Option[Dec] = {
    digits.foldLeft(Option(0)) {
      case (acc, digit) =>
        for {
          d <- validDigit(base, digit)
          a <- acc
        } yield a * base + d
    }

//    digits.reverse.zipWithIndex.foldLeft(Option(0)) {
//      case (acc, (digit, index)) =>
//        for {
//          d <- validDigit(base, digit)
//          a <- acc
//        } yield a + d * math.pow(base, index).toInt
//    }
  }

  private def fromDecimal(base: Base, dec: Dec): Digits = {
    def nextDigit(dec: Dec): Option[(Dec,Digit)] =
      if (dec <= 0) None
      else Some(dec / base, dec % base)

    unfoldLeft(dec)(nextDigit)

//    @tailrec
//    def loop(dec: Dec, acc: Digits): Digits =
//      if (dec <= 0) acc
//      else loop(dec / base, dec % base :: acc)
//
//    loop(dec, List())
  }

  private def unfoldLeft[A, B](seed: B)(f: B => Option[(B, A)]) = {
    @tailrec
    def loop(seed: B)(ls: List[A]): List[A] = f(seed) match {
      case Some((b, a)) => loop(b)(a :: ls)
      case None => ls
    }

    loop(seed)(Nil)
  }
}
