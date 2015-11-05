object Atbash {

  // needed for the tests to compile and run
  def apply() = this

  type Plain = String
  type Cipher = String

  private val CipherGroupSize = 5

  def encode(text: Plain): Cipher = {
    val ungroupedCipherText =
      text.toLowerCase filter (_.isLetterOrDigit) map atbashCipher
    ungroupedCipherText grouped(CipherGroupSize) mkString(" ")
  }

  private val atbashCipher: Map[Char,Char] = {
    val alphabet = ('a' to 'z')
    val cipher = alphabet zip alphabet.reverse toMap

    cipher withDefault identity
  }
}
