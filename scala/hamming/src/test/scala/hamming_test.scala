import org.scalatest._

class HammingSpecs extends FlatSpec with Matchers {
  it should "detect no difference between empty strands" in {
    Hamming.compute("", "") should be (Some(0))
  }

  it should "detect no difference between identical strands" in {
    Hamming.compute("GGACTGA", "GGACTGA") should be (Some(0))
  }

  it should "detect complete hamming distance in small strand" in {
    Hamming.compute("ACT", "GGA") should be (Some(3))
  }

  it should "give hamming distance in off by one strand" in {
    Hamming.compute("GGACGGATTCTG", "AGGACGGATTCT") should be (Some(9))
  }

  it should "give small hamming distance in middle somewhere" in {
    Hamming.compute("GGACG", "GGTCG") should be (Some(1))
  }

  it should "give a larger distance" in {
    Hamming.compute("ACCAGGG", "ACTATGG") should be (Some(2))
  }

  it should "be undefined for first String longer" in {
    Hamming.compute("AGGCTAGCGGTAGGAC", "AAACTAGGGG") should be (None)
  }

  it should "be undefined for second String longer" in {
    Hamming.compute("AAACTAGGGG", "AGGCTAGCGGTAGGAC") should be (None)
  }
}
