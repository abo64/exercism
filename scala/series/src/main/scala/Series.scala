import scala.annotation.tailrec

object Series {
  type DigitStr = String
  type Digit = Int
  type Digits = Seq[Digit]
  type DigitSeries = Seq[Digits]

  def slices(size: Int, digitStr: DigitStr): DigitSeries =
    digitStr map(_.asDigit) mySliding(size)

  private implicit class SeqOps[T](seq: Seq[T]) {
    type Slides = Seq[Seq[T]]

    def mySliding(size: Int): Slides = {
      require(size > 0, s"size must be > 0: $size")

      @tailrec def loop(seq: Seq[T], result: Slides): Slides =
        seq match {
          case s if s.size >= size =>
            val slice = s take size
            loop(s tail, result :+ slice)
          case _ => result
        }

      loop(seq, Seq())
    }
  }
}
