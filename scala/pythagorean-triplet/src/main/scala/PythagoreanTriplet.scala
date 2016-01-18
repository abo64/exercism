object PythagoreanTriplet {
  type Triplet = (Int, Int, Int)
  type PythagoreanTriplets = Seq[Triplet]

  private val isPythagoreanSeq: Seq[Int] => Boolean = {
    def sq(x: Int) = x * x

    { case Seq(a, b, c) => sq(a) + sq(b) == sq(c) }
  }

  val isPythagorean: Triplet => Boolean = {
    val toSeq: Triplet => Seq[Int] = { case (a, b, c) => Seq(a, b, c) }

    toSeq andThen (_.permutations) andThen (_.exists(isPythagoreanSeq))
  }

  def pythagoreanTriplets(start: Int, end: Int): PythagoreanTriplets = {
    val triplets =
      for {
        c <- start to end
        b <- start to c
        a <- start to b if isPythagoreanSeq(Seq(a, b, c))
      } yield (a,b,c)

    triplets.sorted
  }
}
