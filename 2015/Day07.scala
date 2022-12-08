import scala.collection.mutable

@main def day07() =
  def evaluate(overrideB: Option[Int] = None) =
    val state = mutable.Map.empty[String, Int]
    val Seq(dest, instructions: _*) =
      input(7).map(_.split(' ')).toSeq.sortBy(l => (l.last.length, l.last)): @unchecked
    overrideB.foreach { b => instructions.head(0) = b.toString }
    instructions.foreach { tokens =>
      def op(i: Int) =
        val s = tokens(i)
        s.toIntOption.getOrElse(state(s))

      def store(v: Int) = state(tokens.last) = v

      def a = op(0)

      tokens.size match
        case 3 => store(a)
        case 4 => store(65535 - op(1)) // NOT
        case _ =>
          val b = op(2)
          tokens(1) match
            case "AND"    => store(a & b)
            case "OR"     => store(a | b)
            case "LSHIFT" => store(a << b)
            case "RSHIFT" => store(a >> b)
    }
    state(dest.head)

  def part1 = evaluate()

  def part2 = evaluate(Option(evaluate()))

  println(part1) // 46065
  println(part2) // 14134
