object input:
  def apply(day: Int) = io.Source.fromResource(f"day$day%02d.txt").getLines
