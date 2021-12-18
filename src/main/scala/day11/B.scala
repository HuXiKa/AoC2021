package day11

object B extends App {

    val board = new Board(io.Source.stdin.getLines().map(_.map(_.asDigit).toArray).toArray)
    val indexes = ((-1 to 1) flatMap (x => (-1 to 1).map(y => (x,y)))).filterNot(_ == (0,0))

    class Board(numbers: Array[Array[Int]]) {
        def d = print(s"\n\n${numbers.map(_.mkString(" ")).mkString("\n")}")

        private def flashing: Boolean = {
            numbers.indices.flatMap { r: Int =>
                numbers.head.indices map { c =>
                    numbers(r)(c) > 9
                }
            }.exists(identity)
        }

        private def f = {
            var seen = Set.empty[(Int, Int)]
            var fc = 0
            while(flashing) {
                numbers.indices foreach { r =>
                    numbers.head.indices foreach { c =>
                        if (numbers(r)(c) > 9) {
                            //println(s"flashing $r $c $seen")
                            fc += 1
                            seen = seen + ((r, c))
                            numbers(r)(c) = 0
                            indexes.foreach {
                                case (x, y) if x + r >= 0 && x + r < numbers.length && y + c >= 0 && y + c < numbers.head.length && !seen.contains((x + r, y + c)) =>
                                    numbers(r + x)(c + y) = numbers(r + x)(c + y) + 1
                                case _ =>
                            }
                        }
                    }
                }
            }
            fc
        }

        def i = {
            numbers.indices foreach { r =>
                numbers.head.indices foreach { c =>
                    numbers(r)(c) += 1
                    /*indexes.foreach{
                        case (x, y) if x+r >= 0 && x+r < numbers.length && y+c >= 0 && y+c< numbers.head.length =>
                          numbers(r+x)(c+y) = numbers(r+x)(c+y) + 1
                        case _ =>
                    }*/
                }
            }
            f
        }
    }

    println(indexes)

    val r = (1 to 500).map(_ => board.i)
    board.d

    println()
    println(r.zipWithIndex.filter{ case (i, i1) => i == 100}.map(_._2 + 1))

}