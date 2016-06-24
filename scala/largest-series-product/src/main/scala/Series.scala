object Series {

  type DigitString = String
  type Digits = Seq[Int]
  type Slizes = Seq[Digits]
  type Product = Int

  def digits(digitStr: DigitString): Digits =
    digitStr map (_.asDigit)

  private val NoSlices: Slizes = Seq()

  def slices(size: Int, digitStr: DigitString): Option[Slizes] = {
    val digitsSliding: DigitString => Slizes =
      digits(_) sliding size toSeq

    Option(digitStr)
      .filter (_.length >= size)
      .map (digitsSliding)
  }

  def largestProduct(size: Int, digitStr: DigitString): Option[Product] = {
    def getLargestProduct(slices: Slizes): Product =
      slices map (_.product) max

    if (size == 0)
      Some(1)
    else
      slices(size, digitStr) map getLargestProduct
  }
}
