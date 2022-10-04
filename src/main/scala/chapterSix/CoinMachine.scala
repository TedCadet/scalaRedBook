package chapterSix

// Should I use State Actions
enum Input:
  case Coin, Turn

trait CoinMachine:
  def takeInput: MachineAction

// I could change it: CoinMachine to State[CoinMachine, (Int, Int)]
type MachineAction = Input => (CoinMachine, (Int, Int))
type InputAction = CoinMachine => (CoinMachine, (Int, Int))

