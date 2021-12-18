package day1

object B extends App {
  val list = io.Source.stdin.getLines().map(_.toInt)
  val res = list.sliding(3).map(_.sum).sliding(2).map(l => l.last - l.head).count(_ > 0)
  println(res)
}
