@main def day07() =
  val Seq(highCard, onePair, twoPairs, threeKind, fullHouse, fourKind, fiveKind) = 0 to 6

  def bids(joker: Char)(wildcard: (String, Seq[Int]) => Seq[Int] = (_, c) => c) = input(7).map { line =>
    val Array(cardz, bid) = line.split(' ')

    val cards = cardz.map:
      case 'A' => '9' + 5
      case 'K' => '9' + 4
      case 'Q' => '9' + 3
      case 'J' => joker
      case 'T' => '9' + 1
      case d => d

    val c = cards
      .collect({ case c if c > '1' => c - '2' })
      .foldLeft(IndexedSeq.fill(13)(0)) { (acc, i) =>
        acc.updated(i, acc(i) + 1)
      }

    val count = wildcard(cardz, c)

    val hand = count.max match
      case 5                           => fiveKind
      case 4                           => fourKind
      case 3 if count.contains(2)      => fullHouse
      case 3                           => threeKind
      case 2 if count.count(2.==) == 2 => twoPairs
      case 2                           => onePair
      case _                           => highCard

    (s"$hand$cards", bid.toInt)
  }.toSeq
  .sortBy(_._1)
  .zipWithIndex
  .map { case ((_, bid), i) =>
    bid * (i + 1)
  }.sum

  def part1 = bids('9' + 2)()

  def part2 = bids('1'){ (cards, count) =>
    val max = count.max
    val j = count.lastIndexWhere(max.==)

    count.updated(j, count(j) + cards.count('J'.==))
  }

  println(part1) // 249748283
  println(part2) // 248029057
