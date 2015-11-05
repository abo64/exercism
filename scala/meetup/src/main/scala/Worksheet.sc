object Worksheet {
import Meetup._
import WeekDay.distance

distance(Mon, Tue)                                //> res0: Int = 1
distance(Tue, Mon)                                //> res1: Int = 6
distance(Mon, Tue, false)                         //> res2: Int = -6
distance(Tue, Mon, false)                         //> res3: Int = -1
  
distance(Mon, Fri)                                //> res4: Int = 4
distance(Fri, Mon)                                //> res5: Int = 3
distance(Mon, Fri, false)                         //> res6: Int = -3
distance(Fri, Mon, false)                         //> res7: Int = -4
}