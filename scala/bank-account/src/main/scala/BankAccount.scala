import BankAccount._

class BankAccount {

  private var balance: Balance = Some(0)

  def getBalance: Balance = balance

  def incrementBalance(amount: Money): Balance = {
    synchronized {
      balance = balance map (_ + amount)
    }
    balance
  }

  def closeAccount(): Unit =
    balance = None
}

object BankAccount {
  def apply() = new BankAccount

  type Money = Int
  type Balance = Option[Money]
}