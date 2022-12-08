import scala.collection.mutable

@main def day08() =
  val trees = input(8).toIndexedSeq
  val (h, w) = (trees.size, trees.head.size)
  val rows = 0 until h
  val cols = 0 until w

  def fill[A](e: A) = IndexedSeq.fill(h)(mutable.IndexedSeq.fill(w)(e))
  def hyperloop[A](loop: (A, Range, Range) => Unit, col: A, row: A) =
    loop(row, rows, cols)
    loop(col, cols, rows)
    loop(row, rows.reverse, cols.reverse)
    loop(col, cols.reverse, rows.reverse)

  def part1 =
    val visible = fill(false)

    def col(j: Int)(max: Int, i: Int) = row(i)(max, j)
    def row(i: Int)(max: Int, j: Int) =
      visible(i)(j) ||= trees(i)(j) > max
      max.max(trees(i)(j))

    def loop(f: Int => (Int, Int) => Int, r1: Range, r2: Range) = r1.foreach { n =>
      r2.foldLeft(-1)(f(n))
    }

    hyperloop(loop, col, row)
    visible.view.map(_.count(identity)).sum

  /*
    Quadratic algorithm for part 2.
    Using the row [9 6 3 1 4 2 7 8 5] as example and counting how many trees one can see to the right:
    1. Scan the matrix in reverse order (right to left, starting with 5 in the example above)
    2. Initialize a list of 10 elements with the index of the first tree to be scanned (8 in this example)
    3. When you visit a tree of height h at index y, set all elements of the list from 0 to h to y
    4. The difference between the tree index and element h in the list is the number of trees one can see

    In this example, the algorithm goes as:

    5 -> 8 8 8 8 8 8 8 8 8 8
    8 -> 7 7 7 7 7 7 7 7 7 8
    7 -> 6 6 6 6 6 6 6 6 7 8
    2 -> 5 5 5 6 6 6 6 6 7 8
    4 -> 4 4 4 4 4 6 6 6 7 8
    1 -> 3 3 4 4 4 6 6 6 7 8
    3 -> 2 2 2 2 4 6 6 6 7 8
    6 -> 1 1 1 1 1 1 1 6 7 8
    9 -> 0 0 0 0 0 0 0 0 0 0

    Computing the number of tress one can see is a constant operation: just an array lookup followed by a subtraction
    Updating the array is also constant, it takes at most 10 operations regardless of input size
   */
  def part2 =
    val visible = fill(1)

    def col(i: Int, j: Int) = (j, i)
    def row(i: Int, j: Int) = (i, j)

    def loop(f: (Int, Int) => (Int, Int), r1: Range, r2: Range) = r1.foreach { x =>
      r2.foldLeft(IndexedSeq.fill(10)(r2.head)) { case (acc, y) =>
        val (i, j) = f(x, y)
        val h = trees(i)(j) - '0'
        visible(i)(j) *= (y - acc(h)).abs
        IndexedSeq.fill(h + 1)(y) ++ acc.drop(h + 1)
      }
    }

    hyperloop(loop, col, row)
    visible.view.map(_.max).max

  println(part1) // 1796
  println(part2) // 288120
