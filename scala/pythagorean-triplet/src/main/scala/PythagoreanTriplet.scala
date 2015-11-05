object PythagoreanTriplet {
  type Triplet = (Int,Int,Int)
  type PythagoreanTriplets = Seq[Triplet]

  def isPythagorean(triplet: Triplet): Boolean = {
    def sq(x: Int) = x*x

    val (a,b, c) = triplet
    sq(a) + sq(b) == sq(c) || sq(c) + sq(b) == sq(a) || sq(a) + sq(c) == sq(b)
  }

  def pythagoreanTriplets(start: Int, end: Int): PythagoreanTriplets = {
    def sortedTriplet(a: Int, b: Int, c: Int): Triplet = {
      val Seq(x, y, z) = Seq(a,b,c).sorted
      (x,y,z)
    }

    val range = start to end
    val triplets =
      for {
        a <- range
        b <- range
        c <- range if isPythagorean((a, b, c))
        triplet = sortedTriplet(a, b, c)
      } yield triplet
    triplets.distinct
  }
}
