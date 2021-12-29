package day21

import scala.util.control.Breaks._

object A extends App {

  val lines = io.Source.stdin.getLines().map(_.split(": ").tail).toList
  val p1 = lines.head.last.toInt
  val p2 = lines.last.last.toInt

  val r = Iterator
    .iterate((1, 1, (0, 0), (p1,p2)))({ case (i, n, (p1s,p2s), (p1,p2)) =>
      println(s"($i,$n,($p1s,$p2s),($p1,$p2)")
      val d = ((i - 1) % 100) + 1
      val p1score = d + d + 1 + d + 2
      val p1newPos = ((p1 + p1score - 1) % 10) + 1

      val p2score = d + 3 + d + 4 + d + 5
      val p2newPos = ((p2 + p2score - 1) % 10) + 1
      (d + 6, n + 1, (p1s + p1newPos,p2s + p2newPos), (p1newPos,p2newPos))})
    .dropWhile(x => x._3._1 < 1000 && x._3._2 < 1000)
    .next()

  println(s"p1 $p1 p2 $p2")
  println(r)

  val diceCount = if(r._3._1 >= 1000) (r._2 - 1) * 6 - 3 else r._2 * 6
  println(diceCount)
  val losingScore = if(r._3._1 >= 1000) r._3._2 - r._4._2 else r._3._1 - r._4._1
  println(losingScore)
  println(diceCount * losingScore)
}
