import DNA._

class DNA(dna: String) {

  require(isValidDNA(dna),
      s"DNA must contain only nucleotides ${DnaNucleotides.mkString}: $dna")

  lazy val nucleotideCounts: NucleotideCounts = {
    val counts = dna groupBy(identity) mapValues(_.length)
    EmptyDnaNucleotideCounts ++ counts
  }
}

object DNA {
  type Nucleotide = Char
  type NucleotideCounts = Map[Nucleotide,Int]

  val DnaNucleotides: Set[Nucleotide] = Set('A', 'T', 'C', 'G')

  val EmptyDnaNucleotideCounts: NucleotideCounts = {
    def zeroCount(nucleotide: Nucleotide) = nucleotide -> 0

    DnaNucleotides map zeroCount toMap
  }

  def isValidDNA(dna: String) =
    dna forall DnaNucleotides.contains
}