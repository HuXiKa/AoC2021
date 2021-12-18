package day3

object B extends App {
  val list = io.Source.stdin.getLines().toList

  val o2Rating = Integer.parseInt(func(0, list, '1'), 2)
  println(o2Rating)

  val co2Scrub = Integer.parseInt(func(0, list, '0'), 2)
  println(co2Scrub)

  def func(ind: Int, rem: List[String], ch: Char): String ={
    rem match {
      case Nil => ???
      case head :: Nil => head
      case ::(head, next) =>
        val list2 = rem.map(_.zipWithIndex.filter(_._1 == '1').map(_._2))
        val f = list2.foldLeft(Array.fill(list.head.length)(0))({
          case (acc, occ) =>
            occ.foreach(o => acc.update(o, acc(o) + 1))
            acc
        })
        val n = list2.length
        val freq = f.map(_ >= n / 2.0).apply(ind)
        println(s"rec call with ${ind + 1} rem $rem f ${f.mkString(":")} freq $freq")
        func(ind + 1, rem.filter(r => (r.charAt(ind) == ch) == freq), ch)
    }
  }

  println(o2Rating * co2Scrub)
}
