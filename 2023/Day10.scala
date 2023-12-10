import scala.annotation.tailrec
import scala.collection.mutable

@main def day10() =
  type Direction = (Int, Int)

  case class Tile(i: Int, j: Int):
    def move(d: Direction) = copy(d.head + i, d.last + j)

  val move: Char => Direction => Direction =
    case '|' | '-' => identity
    case 'L' | '7' => _.swap
    case 'J' | 'F' => d => (-d.last, -d.head)

  val tiles = mutable.IndexedSeq.from(input(10).map(_.toCharArray))

  val s = tiles.view.zipWithIndex.map { (line, i) =>
    (line.indexOf('S'), i)
  }.collectFirst {
    case (j, i) if j >= 0 => Tile(i, j)
  }.get

  val start =
    def move(direction: Direction, pipes: String) =
      val tile = s.move(direction)
      tiles
        .unapply(tile.i)
        .flatMap(_.unapply(tile.j))
        .filter(pipes.contains)
        .map(_ => tile -> direction)

    move((-1, 0), "|7F").orElse(
    move((+1, 0), "|LJ")).orElse(
    move((0, -1), "-LF")).orElse(
    move((0, +1), "-7F")).get

  @tailrec
  def loop(tile: Tile, direction: Direction)(acc: Int): Int =
    val next = tiles(tile.i)(tile.j)
    if (next == 'S') acc / 2
    else
      tiles(tile.i)(tile.j) = if ("|LJ".contains(next)) '*' else '#'
      val d = move(next)(direction)
      loop(tile.move(d), d)(acc + 1)

  def part1 = loop.tupled(start)(1)

  def part2 = tiles.foldLeft(0) { (acc, line) =>
    acc + (0 until line.size).foldLeft(0, false) { case ((acc, inside), i) =>
      val c = line(i)
      val inc = if (inside && !"*#".contains(c)) 1 else 0
      (acc + inc, c == '*' != inside)
    }._1
  }

  println(part1) // 6768
  println(part2) // 351
