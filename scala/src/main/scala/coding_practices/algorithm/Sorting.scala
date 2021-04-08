package coding_practices.algorithm

import scala.annotation.tailrec

object Sorting {

  def mergeSort[T](list: List[T])(implicit ordering: Ordering[T]): List[T] = {
    if (list.length > 1) {
      val m: Int = list.length / 2
      val firstHalfList: List[T] = mergeSort(list.take(m))
      val secondHalfList: List[T] = mergeSort(list.drop(m))

      merge(firstHalfList, secondHalfList)
    } else {
      list
    }
  }

  @tailrec
  private def merge[T](
    firstList: List[T],
    secondList: List[T],
    mergedList: List[T] = Nil,
    i: Int = 0,
    j: Int = 0,
  )(implicit ordering: Ordering[T]): List[T] = {
    import ordering.mkOrderingOps

    if (i < firstList.length && j < secondList.length) {
      val (elementToBeAppended, nextI: Int, nextJ: Int) = if (firstList(i) < secondList(j)) {
        (firstList(i), i + 1, j)
      } else {
        (secondList(j), i, j + 1)
      }
      merge(firstList, secondList, mergedList :+ elementToBeAppended, nextI, nextJ)
    } else {
      if (i < firstList.length) {
        merge(firstList, secondList, mergedList :+ firstList(i), i + 1, j)
      } else if (j < secondList.length) {
        merge(firstList, secondList, mergedList :+ secondList(j), i, j + 1)
      } else {
        mergedList
      }
    }
  }
}
