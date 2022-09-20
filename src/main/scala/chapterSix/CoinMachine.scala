package chapterSix

// Should I use State Actions
enum Input:
  case Coin, Turn

trait CoinMachine:
  def takeInput: MachineAction

type MachineAction = Input => CoinMachine
type InputAction = CoinMachine => CoinMachine

