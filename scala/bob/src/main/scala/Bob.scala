import Bob._

class Bob {
  val hey: Reply =
    silence orElse
    shouting orElse
    question orElse
    defaultReply
}

object Bob {
  type Reply = PartialFunction[String,String]

  val question: Reply = {
    val Question = """.+\?""".r

    { case Question() => "Sure." }
  }

  val silence: Reply = {
    val Silence = """\s*""".r

    { case Silence() => "Fine. Be that way!" }
  }

  val shouting: Reply = {
    def isShouting(str: String): Boolean =
      (str exists (_.isLetter)) && (str forall (!_.isLower))

    { case s if isShouting(s) => "Whoa, chill out!" }
  }

  val defaultReply: Reply =
    PartialFunction(_ => "Whatever.")
}
