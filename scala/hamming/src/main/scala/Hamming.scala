object Hamming {
  type Nucleotide = Char
  type Strand = Seq[Nucleotide]
  type NucleotidePair = (Nucleotide, Nucleotide)

  private val differentNucleotides: NucleotidePair => Boolean =
    different[Nucleotide]

  def compute(strand1: Strand, strand2: Strand): Int = {
    require(strand1.length == strand2.length, "strands of unequal length")

    val nucleotidePairs: Seq[NucleotidePair] = strand1 zip strand2
    nucleotidePairs count differentNucleotides
  }

  private  def different[T](t1: T, t2: T) =
    t1 != t2

  private implicit def function2AsTupled[A,B,C](f: (A,B) => C): ((A,B)) => C =
    f.tupled
}
