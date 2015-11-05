object House {
  type Sentence = String
  type Noun = String
  type Verb = String

  def rhyme: String =
    sentences(Lexicon.size)

  private val SentenceSeparator = "\n\n"
  private val RelativeClauseSeparator = "\n"

  private def sentences(howMany: Int): String = {
    val sentences: Seq[Sentence] =
      (0 until howMany) map sentence
    sentences mkString("", SentenceSeparator, SentenceSeparator)
  }

  private def sentence(n: Int): String =
    "This is" + nounPhrase(n)

  private def nounPhrase(embeddings: Int, text: String = ""): String = {
    val np = " the " + noun(embeddings)
    relativeClause(embeddings, text + np)
  }

  private def relativeClause(embeddings: Int, text: String = "") = {
    val terminate = embeddings == 0

    val separator = if (terminate) " " else RelativeClauseSeparator
    val clause = separator + "that " + verb(embeddings)
    val newText = text + clause

    if (terminate)
      newText
    else
      nounPhrase(embeddings - 1, newText)
  }

  private def noun(whichOne: Int): Noun = Lexicon(whichOne)._1
  private def verb(whichOne: Int): Verb = Lexicon(whichOne)._2

  private val Lexicon: Seq[(Noun, Verb)] = Seq(
    ("horse and the hound and the horn", "belonged to"),
    ("farmer sowing his corn", "kept"),
    ("rooster that crowed in the morn", "woke"),
    ("priest all shaven and shorn", "married"),
    ("man all tattered and torn", "kissed"),
    ("maiden all forlorn", "milked"),
    ("cow with the crumpled horn", "tossed"),
    ("dog", "worried"),
    ("cat", "killed"),
    ("rat", "ate"),
    ("malt", "lay in"),
    ("house", "Jack built.")
  ).reverse
}
