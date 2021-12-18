package day6

object A extends App {

    case class F(timer: Int){
        def p = {
            if(timer > 0){
                List(copy(timer - 1))
            } else {
                List(copy(6), F(8))
            }
        }
    }

    val list = io.Source.stdin.getLines().toList.head.split(",").map(_.toInt).map(F).toList

    println(list)

    val c = 80

    val r = (c until 0 by -1).foldLeft(list) { case (l, i)  => l.flatMap(_.p) }

    println(r)
    println(r.length)

}
