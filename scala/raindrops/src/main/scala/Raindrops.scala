object Raindrops {

  // needed for the tests to compile and run
  def apply() = this

  private val factorMessages =
    Seq((3, "Pling"),
        (5, "Plang"),
        (7, "Plong"))

  def convert(number: Int): String = {
    def hasFactor(factor: Int): Boolean =
      number % factor == 0

    import Monad._

    val getMessage: ((Int, String)) => Option[String] = {
      case (factor, message) =>
        (Option(factor) guard hasFactor(factor)) >> unit(message)
//        Option(factor) filter hasFactor map const(message)
//        for {
//          _ <- Option(factor) if hasFactor(factor)
//        } yield message
    }
    // Haskell: getMessage (f, xs) = guard (x `mod` f == 0) >> return xs

    import Foldable._

    factorMessages foldMap getMessage getOrElse number.toString
    // Haskell: fromMaybe (show x) (foldMap getMessage factorMessages)
  }

//  private def const[A,B](a: A)(ignore: B): A = a
}
