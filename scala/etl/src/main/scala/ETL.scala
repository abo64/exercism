object ETL {
  type Letter = String
  type Point = Int
  type Letters = Seq[Letter]
  type OldScoreFormat = Map[Point,Letters]
  type NewScoreFormat = Map[Letter,Point]

  def transform(oldScoreFormat: OldScoreFormat): NewScoreFormat =
    oldScoreFormat.foldLeft(Map.empty[Letter,Point]) {
      case (result, (point, letters)) =>
        result ++ (letters map (letter => (letter.toLowerCase, point)))
      }
}
