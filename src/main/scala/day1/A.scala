package day1

object A extends App {
    val list = io.Source.stdin.getLines().map(_.toInt)
    val res = list.sliding(2).map(l => l.last - l.head).count(_ > 0)
    println(res)
}
