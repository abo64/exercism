import org.scalatest._

class GrainsTest extends FunSuite with Matchers {
  test ("square 1") {
    Grains.square(1) should be (Some(1))
  }

  test ("square 2") {
    Grains.square(2) should be (Some(2))
  }

  test ("square 3") {
    Grains.square(3) should be (Some(4))
  }

  test ("square 4") {
    Grains.square(4) should be (Some(8))
  }

  test ("square 16") {
    Grains.square(16) should be (Some(32768))
  }

  test ("square 32") {
    Grains.square(32) should be (Some(2147483648L))
  }

  test ("square 64") {
    Grains.square(64) should be (Some(BigInt("9223372036854775808")))
  }

  test ("square negative") {
    Grains.square(-1) should be (None)
  }

  test ("square bigger than 64") {
    Grains.square(65) should be (None)
  }

  test ("total grains") {
    Grains.total should be (BigInt("18446744073709551615"))
  }
}
