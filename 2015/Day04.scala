@main def day04() =
  val md5 = java.security.MessageDigest.getInstance("MD5")

  def mine(zeroes: Int) =
    val bytes = zeroes / 2
    LazyList.from(1).find { i =>
      val s = "yzbqklnj" + i
      val b = md5.digest(s.getBytes)
      (0 until bytes).forall(b(_) == 0) &&
      (zeroes % 2 == 0 || (b(bytes) & 0xf0) == 0)
    }.get

  def part1 = mine(5)

  def part2 = mine(6)

  println(part1) // 282749
  println(part2) // 9962624
