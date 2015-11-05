import Dna._

class Dna {
  def toRna(dna: Strand): Strand =
    dna map toRnaNucleotide
}

object Dna {
  def apply() = new Dna

  type Strand = String
  type Nucleotide = Char

  val toRnaNucleotide: Map[Nucleotide,Nucleotide] =
    Map('G' -> 'C', 'C' -> 'G', 'T' -> 'A', 'A' -> 'U')
}
