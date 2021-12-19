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

    val list = io.Source.stdin.getLines().toList.head.split(",").map(_.toInt)//.map(F).toList
    var array = Array.fill(9)(BigInt(0))

    list.foreach(i => array(i) += 1)

    println(array.mkString("Array(", ", ", ")"))

    val c = 256

    val r = (c until 0 by -1).foldLeft(array) {
        case (array, i) =>
            val r = Array.fill(9)(BigInt(0))
            1 until 9 foreach (n => r(n - 1) = array(n))
            if(array(0) > 0) {
                r(8) = array(0)
                r(6) += array(0)
            }
            //println(r.mkString("Array(", ", ", ")"))
            r
    }

    println(r.sum)

}
