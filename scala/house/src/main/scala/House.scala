object House {
  def rhyme: String =
    versions(lines) map combine mkString("", "\n", "\n")

  val combine: Seq[String] => String =
    ("This is " + _) compose (_.mkString)

  val versions: Seq[String] => Seq[Seq[String]] =
    ((_:Seq[String]).tails.toSeq) andThen (_.reverse) andThen (_.tail)

  val lines = Seq(
                "the horse and the hound and the horn\nthat belonged to ",
                "the farmer sowing his corn\nthat kept ",
                "the rooster that crowed in the morn\nthat woke ",
                "the priest all shaven and shorn\nthat married ",
                "the man all tattered and torn\nthat kissed ",
                "the maiden all forlorn\nthat milked ",
                "the cow with the crumpled horn\nthat tossed ",
                "the dog\nthat worried ",
                "the cat\nthat killed " ,
                "the rat\nthat ate ",
                "the malt\nthat lay in " ,
                "the house that Jack built.\n"
              )
}
