package day6

object B extends App {

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


    /*
        f(1, 1) => 1
        f(1, 2) => 2
        f(1, 3) => 2
        f(1, 4) => 2
        f(1, 5) => 2
        f(1, 6) => 2
        f(1, 7) => 2
        f(1, 8) => 2
        f(1, 9) => 3
        f(1, 10) => 2
        f(1, 11) => 4
        f(1, 12) => 2
    */
    val i = 1
    val c = 1
    println(7-(c-i) % 7)

    val r = (c until 0 by -1).foldLeft(list) {
        case (l, i)  =>
            //println((i, l.length))
            l.flatMap(_.p)
    }

    println(r)
    println(r.length)

}
