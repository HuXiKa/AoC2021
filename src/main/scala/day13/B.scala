package day13

object B extends App {

    val lines = io.Source.stdin.getLines().toList
    val (coords, folds) = lines.filter(_.nonEmpty).partition(_.charAt(0).isDigit)
    //println(coords.toList)
    //println(folds.toList)

    val board = new Board(coords.map(_.split(",").map(_.toInt)).map(x => (x(0), x(1))))

    class Board(val numbers: List[(Int, Int)]) {

        def foldUp(y: Int) = {
            val n = for {
                (r, c) <- numbers
            } yield {
                if (c > y) {
                    //println(s"folding ($r,$c) into ($r,${y - (c - y)})")
                    (r, y - (c - y))
                } else {
                    (r, c)
                }
            }
            new Board(n)
        }

        def foldLeft(x: Int) = {
            val n = for {
                (r, c) <- numbers
            } yield {
                if (r > x) {
                    //println(s"folding ($r,$c) into (${x - (r - x)},$c)")
                    (x - (r - x), c)
                } else {
                    (r, c)
                }
            }
            new Board(n)
        }

        def f(fold: String) = {
            fold match {
                case msg if msg.contains("x=") => foldLeft(msg.split("x=").last.toInt)
                case msg if msg.contains("y=") => foldUp(msg.split("y=").last.toInt)
            }
        }

        def d = println(numbers.mkString("Array(", ", ", ")"))
        def p = {
            val n = Array.fill(numbers.map(_._2).max + 1)(Array.fill(numbers.map(_._1).max + 1)('.'))
            for {
                (x, y) <- numbers
            } {
                n(y)(x) = '#'
            }
            print(s"\n\n${n.map(_.mkString("")).mkString("\n")}")
        }
    }

    //val b = board.f(folds.head)
    //println(b.numbers.distinct.size)
    //   b.p

    val r = folds.foldLeft(board){
        case (board, fold) => board.f(fold)
    }
    r.p

}