import Ocr._
import Pattern._

class Ocr(grid: Grid) {
  def convert: RecognizedChars = {
    val patternLines = toPatternLines(grid)
    val charsPerLine = patternLines map recognizedCharsInLine
    charsPerLine mkString(",")
  }

  private def recognizedCharsInLine(patternLine: PatternLine): String =
    patternLine map Lexicon.apply mkString

  private def lineToPatterns(line: Line): Seq[Pattern] = {
    def groupedPerPattern(row: Int): Seq[String] =
      line(row) grouped CharCols toSeq

    val top = groupedPerPattern(0)
    val middleUp = groupedPerPattern(1)
    val middleDown = groupedPerPattern(2)
    val bottom = groupedPerPattern(3)

    def toPattern(patternPos: Int): Pattern =
      Seq(top(patternPos), middleUp(patternPos), middleDown(patternPos), bottom(patternPos))

    val patternsInLine = top.size
    (0 until patternsInLine) map toPattern
  }

  private def toPatternLines(grid: Grid): PatternLines = {
    val lines: Seq[Line] = ((grid split "\n").toSeq grouped(CharRows) toSeq)
    lines map lineToPatterns
  }
}

object Ocr {
  type Grid = String
  type Line = Seq[String]
  type PatternLine = Seq[Seq[String]]
  type PatternLines = Seq[PatternLine]
  type RecognizedChars = String

  def apply(grid: Grid) = new Ocr(grid)

  val Garble = '?'
  val CharRows = 4
  val CharCols = 3
}

object Pattern {
  type Pattern = Seq[String]

  lazy val Lexicon: Map[Pattern,Char] =
    Map(Zero -> '0', One -> '1', Two -> '2', Three -> '3',
        Four -> '4', Five -> '5', Six -> '6', Seven -> '7',
        Eight -> '8', Nine -> '9') withDefaultValue Garble

  val Zero: Pattern =
        List(" _ "
           , "| |"
           , "|_|"
           , "   ")
  val One: Pattern =
        List("   "
           , "  |"
           , "  |"
           , "   ")
  val Two: Pattern =
        List(" _ "
           , " _|"
           , "|_ "
           , "   ")
  val Three: Pattern =
        List(" _ "
           , " _|"
           , " _|"
           , "   ")
  val Four: Pattern =
        List("   "
           , "|_|"
           , "  |"
           , "   ")
  val Five: Pattern =
        List(" _ "
           , "|_ "
           , " _|"
           , "   ")
  val Six: Pattern =
        List(" _ "
           , "|_ "
           , "|_|"
           , "   ")
  val Seven: Pattern =
        List(" _ "
           , "  |"
           , "  |"
           , "   ")
  val Eight: Pattern =
        List(" _ "
           , "|_|"
           , "|_|"
           , "   ")
  val Nine: Pattern =
        List(" _ "
           , "|_|"
           , " _|"
           , "   ")
}