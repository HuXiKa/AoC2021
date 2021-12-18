package day9

object B extends App {

    val list = io.Source.stdin.getLines().toArray.map(_.toCharArray.map(_.asDigit))

    println(list.map(_.mkString("(", ", ", ")\r\n")).mkString("(", ", ", ")"))

    var sum = List.empty[(Int, Int)]
    //val pairs = List((-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1))
    val pairs = Set((-1,0),(0,1),(1,0),(0,-1))

    println(pairs)

    def isLowest(x: Int, y: Int) = {
        val v = list(x)(y)
        val neighbours = pairs.map{
            case (r, c) if x+r >= 0 && x+r< list.length && y+c >= 0 && y+c<list.head.length => list(x+r)(y+c)
            case _ => Int.MaxValue
        }
        //        println(s"n of ${list(x)(y)} ${neighbours.filter(_ != Int.MaxValue)}")
        neighbours.filter(_ != Int.MaxValue).forall(_ > v)
    }

    for {
        x <- list.indices
        y <- list(0).indices
    } {
        if (isLowest(x, y)) {
            //            println(s"lowest ${list(x)(y)}")
            sum = sum.appended((x, y))
        }
    }

    val res = sum.map(min => {
        var prev = List(min)
        def f(p: (Int, Int), cur: (Int, Int)): Int = {
            //println(s"min $min p $p cur $cur adj ${(cur._1+p._1, cur._2+p._2)} prev $prev")
            p match {
                case (r, c) if cur._1+r >= 0 && cur._1+r< list.length && cur._2+c >= 0 && cur._2+c<list.head.length && list(cur._1+r)(cur._2+c) != 9 && !prev.contains((cur._1+r, cur._2+c)) =>
                    prev = prev :+ ((cur._1+r, cur._2+c))
                    pairs.map(p => f(p, (cur._1+r,cur._2+c))).max
                case _ => prev.length
            }
        }

        (min, pairs.map(p => f(p, min)).max)

    })


    println(sum)
    println(res)

    println(res.sortBy(_._2).takeRight(3).map(_._2).product)

}
