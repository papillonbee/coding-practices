package coding_practices.algorithm

/**
 * reference: https://github.com/bephrem1/backtobackswe/tree/master/Dynamic%20Programming%2C%20Recursion%2C%20%26%20Backtracking/EggDrop
 * consider below table
 * eggs/floor
 *  , 0, 1, 2, 3, 4, 5, 6
 *  0 -, -, -, -, -, -, -
 *  1 0, 1, 2, 3, 4, 5, 6
 *  2 0, 1, .,
 *  3 0, 1,
 *  4 0, 1,
 *  5 0, 1,
 *  6 0, 1,
 *
 *  . = min(
 *        1 + max(table[2][1], table[1][0]),
 *        1 + max(table[2][0], table[1][1])
 *      ) = 2
 */

object EggDropSolver {

  def solveBottomUp(totalEggs: Int, totalFloors: Int): Int = {
    val emptyMinimumNumberOfExperimentsRequired: Array[Array[Int]] =
      Array.fill[Int](totalEggs + 1, totalFloors + 1)(Int.MaxValue)

    val minimumNumberOfExperimentsRequired: Array[Array[Int]] =
      fillBaseCase(totalEggs, totalFloors, emptyMinimumNumberOfExperimentsRequired)

    (2 to totalEggs).foreach { eggs: Int =>
      (2 to totalFloors).foreach { floor: Int =>
        (1 to floor).foreach { attemptFloor: Int =>
          val minimumNumberOfExperimentsRequiredIfEggBreaks: Int =
            minimumNumberOfExperimentsRequired(eggs - 1)(attemptFloor - 1)
          val minimumNumberOfExperimentsRequiredIfEggDoesNotBreak: Int =
            minimumNumberOfExperimentsRequired(eggs)(floor - attemptFloor)

          val worstMinimumNumberOfExperimentsRequired: Int = 1 + math.max(
            minimumNumberOfExperimentsRequiredIfEggBreaks, minimumNumberOfExperimentsRequiredIfEggDoesNotBreak
          )

          minimumNumberOfExperimentsRequired(eggs)(floor) =
            math.min(minimumNumberOfExperimentsRequired(eggs)(floor), worstMinimumNumberOfExperimentsRequired)
        }
      }
    }

    minimumNumberOfExperimentsRequired(totalEggs)(totalFloors)
  }

  private def fillBaseCase(
    totalEggs: Int,
    totalFloors: Int,
    minimumNumberOfExperimentsRequired: Array[Array[Int]],
  ): Array[Array[Int]] = {
    (1 to totalEggs).foreach { eggs: Int =>
      minimumNumberOfExperimentsRequired(eggs)(0) = 0
      minimumNumberOfExperimentsRequired(eggs)(1) = 1
    }

    (1 to totalFloors).foreach { floor: Int =>
      minimumNumberOfExperimentsRequired(1)(floor) = floor
    }

    minimumNumberOfExperimentsRequired
  }

  def solveTopDown(totalEggs: Int, totalFloors: Int): Int = {
    val minimumNumberOfExperimentsRequired: Array[Array[Int]] =
      Array.fill[Int](totalEggs + 1, totalFloors + 1)(Int.MaxValue)

    recursivelySolveTopDown(totalEggs, totalFloors, minimumNumberOfExperimentsRequired)
  }

  private def recursivelySolveTopDown(
    eggs: Int,
    floor: Int,
    minimumNumberOfExperimentsRequired: Array[Array[Int]],
  ): Int = {
    if (floor == 0 || floor == 1 || eggs == 1) {
      floor
    } else if (minimumNumberOfExperimentsRequired(eggs)(floor) != Int.MaxValue) {
      minimumNumberOfExperimentsRequired(eggs)(floor)
    } else {
      (1 to floor).foreach { attemptFloor: Int =>
        val minimumNumberOfExperimentsRequiredIfEggBreaks: Int =
          recursivelySolveTopDown(eggs - 1, attemptFloor - 1, minimumNumberOfExperimentsRequired)
        val minimumNumberOfExperimentsRequiredIfEggDoesNotBreak: Int =
          recursivelySolveTopDown(eggs, floor - attemptFloor, minimumNumberOfExperimentsRequired)

        val worstMinimumNumberOfExperimentsRequired: Int = 1 + math.max(
          minimumNumberOfExperimentsRequiredIfEggBreaks, minimumNumberOfExperimentsRequiredIfEggDoesNotBreak
        )

        minimumNumberOfExperimentsRequired(eggs)(floor) =
          math.min(minimumNumberOfExperimentsRequired(eggs)(floor), worstMinimumNumberOfExperimentsRequired)
      }

      minimumNumberOfExperimentsRequired(eggs)(floor)
    }
  }
}
