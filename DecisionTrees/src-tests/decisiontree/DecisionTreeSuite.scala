package decisiontree

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import DecisionTree._

@RunWith(classOf[JUnitRunner])
class DecisionTreeSuite extends FunSuite {
  /* test case visualised and explained in ReadMe
   * diffrent types of nodes could be tested separately
   * negative payoffs and zero payoffs and probabilities should be incorporated
   */
  val testTree =
    Decision(Some("Offer from ABC"), List(
      Result(Some("Accept ABC"), Payoff(72), Probability(1.00)),
      Event(Some("Reject ABC"), List(
        Result(None,
          Decision(Some("Offer from Mary"), List(
            Result(Some("Accept Mary's offer"), Payoff(80), Probability(1.00)),
            Event(Some("Reject Mary"), List(
              Result(Some("High Salary"), UnkPayoff("high_amount"), UnkProbability("high$")),
              Result(Some("Medium Salary"), Payoff(70), UnkProbability("medium$")),
              Result(Some("Low Salary"), Payoff(60), UnkProbability("low$")))))),
          Probability(0.6)),
        Result(None,
          Event(Some("No offer from Mary"), List(
            Result(Some("High Salary"), Payoff(90), Probability(0.1)),
            Result(Some("Medium Salary"), Payoff(70), Probability(0.5)),
            Result(Some("Low Salary"), Payoff(60), Probability(0.4)))),
          Probability(0.4))))))

  val testEnv: Environment = { case "high_amount" => 90 case "high$" => 0.1 case "medium$" => 0.5 case "low$" => 0.4 }

  test("The final value of TestTree should be 75.2 ") {
    assert(solveDecisionTree(testTree, testEnv)._1 == 75.2)
  }
}