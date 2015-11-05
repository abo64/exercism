object ETL {
  type Letter = String
  type Point = Int
  type Letters = Seq[Letter]
  type OldScore = (Point,Letters)
  type NewScore = (Letter,Point)
  type OldScoreFormat = Map[Point,Letters]
  type NewScoreFormat = Map[Letter,Point]

  def transform(oldScoreFormat: OldScoreFormat): NewScoreFormat =
    for {
      (point, letters) <- oldScoreFormat
      letter <- letters
      newScore = (letter.toLowerCase, point)
    } yield newScore
}
