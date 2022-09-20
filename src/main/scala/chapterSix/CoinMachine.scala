package chapterSix

// Should I use State Actions
trait CoinMachine:
  def takeInput(input: Input): CandyCoinMachine