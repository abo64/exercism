import org.scalatest.{Matchers, FunSuite}

class CipherTest extends FunSuite with Matchers {

  test("Random key cipher - can encode/decode") {
    // Here we take advantage of the fact that plaintext of "aaa..."
    // outputs the key. This is a critical problem with shift ciphers, some
    // characters will always output the key verbatim.

    val cipher = Cipher(None)
    cipher.encode("aaaaaaaaaa") should be (cipher.key.substring(0, 10))
    cipher.decode(cipher.key.substring(0, 10)) should be ("aaaaaaaaaa")
  }

  test("Invalid key - contains caps") {
    intercept[IllegalArgumentException] {
      Cipher(Some("ABCD"))
    }
  }

  test("Invalid key - contains numerics") {
    intercept[IllegalArgumentException] {
      Cipher(Some("123"))
    }
  }

  test("Invalid key - is empty") {
    intercept[IllegalArgumentException] {
      Cipher(Some(""))
    }
  }

  test("Substitution cipher - can encode") {
    Cipher(Some("abcdefghij")).encode("aaaaaaaaaa") should be ("abcdefghij")
  }

  test("Substitution cipher - can decode") {
    Cipher(Some("abcdefghij")).decode("abcdefghij") should be ("aaaaaaaaaa")
  }

  test("Substitution cipher - is reversible") {
    val cipher = Cipher(Some("abcdefghij"))
    cipher.decode(cipher.encode("abcdefghij")) should be ("abcdefghij")
  }

  test("Substitution cipher - is reversible 2") {
    val cipher = Cipher(Some("alkjsdhflkjahsuid"))
    cipher.decode(cipher.encode("asdf")) should be ("asdf")
  }

  test("Substitution cipher - is reversible 3") {
    val key = ('a' to 'z').reverse mkString
    val cipher = Cipher(Some(key))
    cipher.decode(cipher.encode("asdf")) should be ("asdf")
  }

  test("Substitution cipher - known cipher") {
    val key = ('a' to 'j') mkString
    val text = ('a' to 'z') mkString
    val cipher = Cipher(Some(key))
    cipher.decode(cipher.encode(text)) should be (text)
  }

  test("Substitution cipher - known cipher 2") {
    val key = ('a' to 'j') mkString
    val text = "z" + (('a' to 'i') mkString)
    val cipher = Cipher(Some(key))
    cipher.decode(cipher.encode(text)) should be (text)
  }

  test("Substitution cipher - can double shift") {
    val cipher = Cipher(Some("iamapandabear"))
    cipher.encode("iamapandabear") should be ("qayaeaagaciai")
    cipher.decode("qayaeaagaciai") should be ("iamapandabear")
  }

  test("Substitution cipher - can wrap") {
    val cipher = Cipher(Some("abcdefghij"))
    cipher.encode("zzzzzzzzzz") should be ("zabcdefghi")
    cipher.decode("zabcdefghi") should be ("zzzzzzzzzz")
  }

  test("Substitution cipher - random key") {
    val text = Stream.continually(('a' to 'z')).flatten.take(1000) mkString
    val cipher1 = Cipher(None)
    val encoded1 = cipher1.encode(text)
    cipher1.decode(encoded1) should be (text)
    val cipher2 = Cipher(None)
    val encoded2 = cipher2.encode(text)
    cipher2.decode(encoded2) should be (text)
    encoded1 should not be encoded2
  }
}
