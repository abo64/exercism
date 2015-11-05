object Hexadecimal {

  type HexDigit = Char
  type Hex = String

  private val InvalidHex = 0

  private val HexDigits: Seq[HexDigit] =
    ('0' to '9') ++ ('a' to 'f')

  private def isValidHex(hex: Hex) =
    hex.nonEmpty && (hex.toLowerCase forall HexDigits.contains)

  private def toInt(hex: Hex): Int = {
    def hexDigitWithIndexToInt(hexDigit: HexDigit, index: Int): Int = {
      val hexFactor = math.pow(16, index).toInt
      val hexDigitAsInt = HexDigits.indexOf(hexDigit.toLower)
      hexDigitAsInt * hexFactor
    }

    val hexDigitsWithIndex: Seq[(HexDigit, Int)] =
      hex.reverse zipWithIndex

    hexDigitsWithIndex map hexDigitWithIndexToInt sum
  }

  def hexToInt(hex: Hex): Int =
    Option(hex) filter isValidHex map toInt getOrElse InvalidHex

  private implicit def function2AsTupled[A,B,C](f: (A,B) => C): ((A,B)) => C =
    f.tupled
}
