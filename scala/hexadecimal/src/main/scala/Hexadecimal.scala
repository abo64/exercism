object Hexadecimal {

  type HexChar = Char
  type Hex = String

  private val InvalidHex = 0

  private val HexChars: Seq[HexChar] =
    ('0' to '9') ++ ('a' to 'f')

  private def isValidHex(hex: Hex) =
    hex.nonEmpty && (hex.toLowerCase forall HexChars.contains)

  private def toInt(hex: Hex): Int = {
    def asInt(hexChar: HexChar): Int =
      HexChars indexOf hexChar.toLower

    def next(int: Int, hexChar: HexChar): Int =
      int * 16 + asInt(hexChar)

    hex.foldLeft(0)(next)
  }

  def hexToInt(hex: Hex): Int =
    Option(hex) filter isValidHex map toInt getOrElse InvalidHex

  private implicit def function2AsTupled[A,B,C](f: (A,B) => C): ((A,B)) => C =
    f.tupled
}
