import scala.collection.immutable.SortedMap
import School._

class School {

  private var myDB: DB = Map()

  def db: DB = myDB

  def add(student: Student, grade: Grade): Unit = {
    val oldStudents = this.grade(grade)
    val newStudents = oldStudents :+ student
    myDB += (grade -> newStudents)
  }

  def grade(grade: Grade): Seq[Student] = {
    myDB getOrElse(grade, NoStudents)
  }

  def sorted: DB =
    SortedMap(myDB.toSeq:_*) mapValues (_.sorted)
}

object School {
  type Student = String
  type Grade = Int
  type Students = Seq[Student]
  type DB = Map[Grade,Students]

  val NoStudents: Students = Seq()
}
