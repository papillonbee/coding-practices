package coding_practices.algorithm

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EggDropSolverTest extends AnyFunSpec
  with Matchers {

  describe("EggDropSolver") {
    val totalEggs: Int = 2
    val totalFloors: Int = 100
    val expectedMinimumNumberOfExperimentsRequired: Int = 14

    it("should solve from bottom up for the minimum number of experiments required for determining the pivotal floor") {
      EggDropSolver.solveBottomUp(totalEggs, totalFloors) shouldEqual
        expectedMinimumNumberOfExperimentsRequired
    }

    it("should solve from top down for the minimum number of experiments required for determining the pivotal floor") {
      EggDropSolver.solveTopDown(totalEggs, totalFloors) shouldEqual
        expectedMinimumNumberOfExperimentsRequired
    }
  }
}
