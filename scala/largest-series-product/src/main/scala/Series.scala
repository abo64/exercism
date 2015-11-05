object Series {

  type DigitString = String
  type Digits = Seq[Int]
  type Slizes = Seq[Digits]
  type Product = Int

  def digits(digitStr: DigitString): Digits = {
    digitStr map (_.asDigit)
  }

  private val NoSlices: Slizes = Seq()
  private val ProductZero: Product = 1

  def slices(size: Int, digitStr: DigitString): Slizes = {
    if (digitStr.length < size) NoSlices
    else {
      val digits = this.digits(digitStr)
      digits sliding size toSeq
    }
  }

  def largestProduct(size: Int, digitStr: DigitString): Product = {
    val intSlices = slices(size, digitStr) map (_.product)

    if (intSlices != NoSlices) intSlices max
    else ProductZero
  }
}
