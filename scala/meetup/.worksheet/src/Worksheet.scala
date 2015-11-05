object Worksheet {
import Meetup._
import WeekDay.distance;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(78); val res$0 = 

distance(Mon, Tue);System.out.println("""res0: Int = """ + $show(res$0));$skip(19); val res$1 = 
distance(Tue, Mon);System.out.println("""res1: Int = """ + $show(res$1));$skip(26); val res$2 = 
distance(Mon, Tue, false);System.out.println("""res2: Int = """ + $show(res$2));$skip(26); val res$3 = 
distance(Tue, Mon, false);System.out.println("""res3: Int = """ + $show(res$3));$skip(22); val res$4 = 
  
distance(Mon, Fri);System.out.println("""res4: Int = """ + $show(res$4));$skip(19); val res$5 = 
distance(Fri, Mon);System.out.println("""res5: Int = """ + $show(res$5));$skip(26); val res$6 = 
distance(Mon, Fri, false);System.out.println("""res6: Int = """ + $show(res$6));$skip(26); val res$7 = 
distance(Fri, Mon, false);System.out.println("""res7: Int = """ + $show(res$7))}
}
