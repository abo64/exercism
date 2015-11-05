import PhoneNumber._

class PhoneNumber(phoneNumber: String) {

  lazy val (areaCode, prefix, lineNumber) =
    parse(phoneNumber) getOrElse InvalidPhoneNumberParts

  lazy val number: String = s"$areaCode$prefix$lineNumber"

  override def toString = s"($areaCode) $prefix-$lineNumber"
}

object PhoneNumber {
  type AreaCode = String
  type Prefix = String
  type LineNumber = String
  type PhoneNumberParts = (AreaCode, Prefix, LineNumber)

  private[this] val AreaCodeLength = 3
  private[this] val PrefixLength = 3
  private[this] val LineNumberLength = 4

  val InvalidPhoneNumberParts: PhoneNumberParts =
    ("0" * AreaCodeLength, "0" * PrefixLength, "0" * LineNumberLength)

  private[this] val PhoneNumberPattern = {
    def part(length: Int) = s"(\\d{$length})"
    s"""1?${part(AreaCodeLength)}${part(PrefixLength)}${part(LineNumberLength)}""".r
  }

  def parse(phoneNumber: String): Option[PhoneNumberParts] = {
    val digits = phoneNumber filter (_.isDigit)
    digits match {
      case PhoneNumberPattern(areaCode, prefix, lineNumber) => Some((areaCode, prefix, lineNumber))
      case _ => None
    }
  }
}