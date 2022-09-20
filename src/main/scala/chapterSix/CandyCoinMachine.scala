package chapterSix

case class CandyCoinMachine(locked: Boolean, candies: Int, coins: Int) extends CoinMachine:
  override def takeInput: MachineAction =
    input =>
      input match
        case _ if candies <= 0 =>
          this
        case Input.Coin =>
          coinActions(this)
        case Input.Turn =>
          turnActions(this)

  private def coinActions: InputAction =
    candyCoinMachine =>
      candyCoinMachine match
        case CandyCoinMachine(false,_,_) =>
          this
        case CandyCoinMachine(true, candies, coins) if candies >= 1 =>
          CandyCoinMachine(false, candies, coins + 1)

  private def turnActions: InputAction =
    candyCoinMachine =>
      candyCoinMachine match
        case CandyCoinMachine(true, _, _) =>
          this
        case CandyCoinMachine(false, candies, coins) =>
          CandyCoinMachine(true, candies - 1, coins)
