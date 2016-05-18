package decisiontree

trait DecisionTreeNode {
  type DTN = DecisionTreeNode //alias for shorter code

  val name: Option[String] = None
  //Decision combines choices that can be certain Payoffs, uncertain Events or having another Decision
  final case class Decision(override val name: Option[String], choices: List[DTN]) extends DTN
  //Event combines possible Results
  final case class Event(override val name: Option[String], results: List[Result]) extends DTN
  //Result combines outcome of type: known payoff,unknown payoff, decision or event with assigned probabiltiy of this outcome
  final case class Result(override val name: Option[String], outcome: DTN, probability: DTN) extends DTN
  //known values of Payoffs and Probabilities
  final case class Payoff(payoff: Double) extends DTN
  final case class Probability(probability: Double) extends DTN
  //unknown values - various scenarios (environments) can be tested
  final case class UnkPayoff(payoff: String) extends DTN
  final case class UnkProbability(probability: String) extends DTN
}

object DecisionTree extends DecisionTreeNode {
  type Environment = String => Double //alias for environments

  /* Tuple of Decision Tree EV and List of DecisionData (type to store the names and expected values of choices for a given decision).
   * EV is the main thing we are calculating.
   * List of DecisionData could be used to print sth like:
   * "Given decision between Accept Mary's offer with EV of x and Rejecting Mary with EV of y, Accepting should be chosen"
   */
  type TreeResults = Tuple2[Double, List[DecisionData]]

  def solveDecisionTree(tree: DTN, env: Environment): TreeResults = {
    def eval(tree: DTN, env: Environment, decisionTrack: List[DecisionData]): TreeResults = tree match {
      case Decision(name, choices) => chooseBest(choices, env, decisionTrack)
      case Event(name, results) => calcEvent(results, env, decisionTrack)
      case Result(name, payoff, probability) => {
        val ev = eval(payoff, env, decisionTrack)._1 * eval(probability, env, decisionTrack)._1
        Tuple2(ev, decisionTrack)
      }
      case Payoff(payoff) => Tuple2((payoff), decisionTrack)
      case Probability(probability) => Tuple2((probability), decisionTrack)
      case UnkPayoff(payoff) => Tuple2((env(payoff)), decisionTrack)
      case UnkProbability(probability) => Tuple2((env(probability)), decisionTrack)
    }

    //iterate through list of events and return sum of their expected values
    def calcEvent(nodes: List[DTN], env: Environment, decisionTrack: List[DecisionData]): TreeResults = {
      def iterEvent(nodes: List[DTN], env: Environment, decisionTrack: List[DecisionData],
        sumEv: Double): TreeResults = {
        if (nodes.isEmpty) Tuple2(sumEv, decisionTrack)
        else {
          val nodeEv = eval(nodes.head, env, decisionTrack)._1
          iterEvent(nodes.tail, env: Environment, decisionTrack, sumEv + nodeEv)
        }
      }
      iterEvent(nodes, env, decisionTrack, 0)
    }

    /* chooses max EV iterating through list of choices +
     * stores the data of the choices in the decisionTrack for future reference
     */
    def chooseBest(choices: List[DTN], env: Environment, decisionTrack: List[DecisionData]): TreeResults = {
      def iterChoose(choices: List[DTN], env: Environment, decisionTrack: List[DecisionData],
        max: Double, decisionData: DecisionData): TreeResults = {
        if (choices.isEmpty) Tuple2(max, decisionData :: decisionTrack)
        else {

          val evHead = eval(choices.head, env, decisionTrack)._1
          val newMax = Math.max(max, evHead)
          val nameHead = choices.head.name.getOrElse("unnamed choice")
          val appendedData = new DecisionData(decisionData.names :+ nameHead, decisionData.evs :+ evHead)
          iterChoose(choices.tail, env, decisionTrack, newMax, appendedData)
        }
      }
      //set initial max to large negative, as there could be no choice with positive EV
      iterChoose(choices, env, decisionTrack, -1000000, new DecisionData(List(), List()))
    }
    eval(tree, env, List())
  }
}
