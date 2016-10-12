//import scalaz.std.list._
//import scalaz.std.option._
//import scalaz.syntax.traverse._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.traverse._


object Dna {
  private type Strand = String
  private type Nucleotide = Char

  def toRna(dna: String): Option[Strand] =
    dna.toList.traverse(toRnaNucleotide) map (_.mkString)

  private val DnaToRna =
    Map('G' -> 'C', 'C' -> 'G', 'T' -> 'A', 'A' -> 'U')

  private def toRnaNucleotide(char: Char): Option[Nucleotide] =
     DnaToRna get char
}
