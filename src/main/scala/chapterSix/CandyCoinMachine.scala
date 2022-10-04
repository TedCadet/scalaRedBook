package chapterSix

// Not good, I shoud be using State Actions to keep to not mutate the state of the machine
case class CandyCoinMachine(locked: Boolean, candies: Int, coins: Int) extends CoinMachine:
  override def takeInput: MachineAction =
    input =>
      input match
        case _ if candies <= 0 =>
          (this, (this.coins, this.candies))
        case Input.Coin =>
          coinActions(this)
        case Input.Turn =>
          turnActions(this)

  private def coinActions: InputAction =
    candyCoinMachine =>
      candyCoinMachine match
        case ccm @ CandyCoinMachine(false,candies,coins) =>
          (ccm, (candies, coins))

        case ccm @ CandyCoinMachine(true, candies, coins)if candies >= 1 =>
          (ccm, (coins + 1, candies))


  private def turnActions: InputAction =
    candyCoinMachine =>
      candyCoinMachine match
        case ccm @ CandyCoinMachine(true, candies, coins)=>
          (ccm, (coins, candies))
        case ccm @ CandyCoinMachine(false, candies, coins)=>
          (ccm, (coins, candies - 1))
          
  private def createState: CandyCoinMachine => ((Int, Int), CandyCoinMachine) = ccm => ((ccm.coins,ccm.candies), ccm)
    

//  def getCoinsAndCandies: Input => (Int, Int) = (this.coins, this.candies)

// for comprehension on inputs, State.modify with anonymous function where we pass the old state and we modify it we takeInput
//  def simulateMachine(inputs: List[Input]): State[CandyCoinMachine, (Int, Int)] =
//    val firstStateAction: State[CandyCoinMachine, (Int, Int)] = State(createState)

//    val results: (Int, Int) = State.run(firstStateAction)(this)._1


//    for
//      input <- inputs
//      state <- State.
//    yield state
