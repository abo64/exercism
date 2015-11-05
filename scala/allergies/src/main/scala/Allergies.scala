object Allergies {

  import Allergen._

  // needed for the tests to compile and run
  def apply() = this

  type Score = Int

  def isAllergicTo(allergen: Allergen, score: Score): Boolean =
    isInScore(score)(allergen)

  def allergies(score: Score): Allergens =
    AllAllergens filter isInScore(score)

  private def isInScore(score: Score)(allergen: Allergen): Boolean =
    scoreToValues(score)(allergen.value)

  private def scoreToValues(score: Score): Set[Value] = {
    def binaryWithIndexToValue: PartialFunction[(Char,Int),Value] = {
      case (char, index) if char == '1' => math.pow(2, index) toInt
    }

    val binariesWithIndex: Seq[(Char, Int)] =
      score.toBinaryString.reverse.zipWithIndex
    binariesWithIndex collect binaryWithIndexToValue toSet
  }
}

object Allergen {
  type Value = Int
  type Allergens = List[Allergen]

  sealed trait Allergen { val value: Value }
  object Eggs extends Allergen { override val value = 1 }
  object Peanuts extends Allergen { override val value = 2 }
  object Shellfish extends Allergen { override val value = 4 }
  object Strawberries extends Allergen { override val value = 8 }
  object Tomatoes extends Allergen { override val value = 16 }
  object Chocolate extends Allergen { override val value = 32 }
  object Pollen extends Allergen { override val value = 64 }
  object Cats extends Allergen { override val value = 128 }

  val AllAllergens: Allergens =
    List(Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats)
}