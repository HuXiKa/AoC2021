package day9

object A extends App {

    val list = io.Source.stdin.getLines().toArray.map(_.toCharArray.map(_.asDigit))

    println(list.map(_.mkString("(", ", ", ")\r\n")).mkString("(", ", ", ")"))

    var sum = 0
    //val pairs = List((-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1))
    val pairs = Set((-1,0),(0,1),(1,0),(0,-1))

    println(pairs)

    def isLowest(x: Int, y: Int) = {
        val v = list(x)(y)
        val neighbours = pairs.map{
            case (r, c) if x+r >= 0 && x+r< list.length && y+c >= 0 && y+c<list.head.length => list(x+r)(y+c)
            case _ => Int.MaxValue
        }
        println(s"n of ${list(x)(y)} ${neighbours.filter(_ != Int.MaxValue)}")
        neighbours.filter(_ != Int.MaxValue).forall(_ > v)
    }

    for {
        x <- list.indices
        y <- list(0).indices
    } {
        if (isLowest(x, y)) {
            println(s"lowest ${list(x)(y)}")
            sum += list(x)(y) + 1
        }
    }

    val res = list

    println(sum)

}
