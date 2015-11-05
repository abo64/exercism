import Garden._
import Plant._
import Children._

class Garden(children: Children, gardenLayout: GardenLayout) {

  private lazy val sortedChildren = children.sorted

  private lazy val plantRows: Rows[Plants] =
    getPlantRows(gardenLayout)

  private lazy val childrensPlants: Map[Child,Plants] = {
    def mergePlantsOfChildInRows(plants1: Plants, plants2: Plants): Plants =
      List(plants1(0), plants1(1), plants2(0), plants2(1))

    val Seq(firstRow, secondRow) =
      plantRows map (_ grouped PlantsPerChildInRow toSeq)
    val plants: Plants =
      (firstRow zip secondRow) flatMap mergePlantsOfChildInRows toList
    val plantsPerChild: Seq[Plants] = plants grouped CupsPerChild toSeq
    val plantsOfChildren = sortedChildren zip plantsPerChild toMap

    plantsOfChildren withDefaultValue NoPlants
  }

  def getPlants(child: Child): Plants =
    childrensPlants(child)
}

object Garden {
  type GardenLayout = String
  type Rows[T] = Seq[T]
  type Cups = Seq[PlantShortcut]
  type CupPosition = Int

  def apply(children: Children, gardenLayout: GardenLayout) =
    new Garden(children, gardenLayout)

  val GardenRows = 2
  val CupsPerChild = 4
  val PlantsPerChildInRow = CupsPerChild / GardenRows

  def defaultGarden(gardenLayout: GardenLayout) =
    new Garden(DefaultChildren, gardenLayout)

  private val RowSeparator = """\n"""
  def getPlantRows(gardenLayout: GardenLayout): Rows[Plants] = {
    def cupsToPlants(cups: Cups): Plants =
      cups flatMap shortcutToPlant toList

    val cupRows: Rows[Cups] = (gardenLayout.split(RowSeparator) map (_.toSeq) toSeq)
    cupRows map cupsToPlants
  }

  implicit def function2AsTupled[A,B,C](f: (A,B) => C): ((A,B)) => C =
    f.tupled
}

object Plant {
  type Plants = List[Plant]
  type PlantShortcut = Char

  sealed trait Plant { def shortcut: PlantShortcut }
  case object Violets extends Plant { override val shortcut = 'V' }
  case object Clover extends Plant { override val shortcut = 'C' }
  case object Radishes extends Plant { override val shortcut = 'R' }
  case object Grass extends Plant { override val shortcut = 'G' }

  val AllPlants: Plants =
    List(Violets, Clover, Radishes, Grass)
 
  val NoPlants: Plants = List()

  def shortcutToPlant(shortcut: PlantShortcut): Option[Plant] =
    AllPlants find (_.shortcut == shortcut)
}

object Children {
  type Child = String
  type Children = List[Child]

  val DefaultChildren: Children =
    List("Alice", "Bob", "Charlie", "David", "Eve", "Fred",
         "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry")
}