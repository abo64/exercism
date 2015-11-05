object Worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(94); val res$0 = 
  "42" match {
    case x @ Number(n) => s"$x - $n"
    case _ => "???"
  };System.out.println("""res0: String = """ + $show(res$0))}
}
