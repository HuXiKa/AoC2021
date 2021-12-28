package day20

object A extends App {

  val lines = io.Source.stdin.getLines()
  val res = lines.toList
  val map = res.head
  val image = res.drop(2)

  val indexes = ((-1 to 1) flatMap (x => (-1 to 1).map(y => (x, y))))

  class Board(numbers: Array[Array[Int]]) {
    def c = {
      numbers.map(_.sum).sum
    }

    def d = print(s"\n\n${numbers.map(_.mkString(" ")).mkString("\n")}")

    def i(d: Int) = {
      val n = -1 to numbers.length flatMap { r =>
        -1 to numbers.head.length map { c =>
          val n2 = indexes.map {
            case (x, y) if x + r < 0 || x + r >= numbers.length || y + c < 0 || y + c >= numbers.head.length => d
            case (x, y) => numbers(x + r)(y + c)
          }
          //if(r == 2 && c == 2) println(n2)
          val n = Integer.parseInt(n2.mkString(""), 2)
          (r, c) -> map(n)
        }
      }

      //      println(n)

      val a = Array.fill(numbers.length + 2)(Array.fill(numbers.head.length + 2)(0))
      n.foreach { case ((x, y), c) => a(x + 1)(y + 1) = if (c == '#') 1 else 0 }
      new Board(a)
    }
  }

  val board = new Board(image.map(_.map(c => if (c == '#') 1 else 0).toArray).toArray)

  //  board.d
  //  println()

  val r = (0 until 2).foldLeft(board)({
    case (b, i) => b.i(i % 2)
  })

  r.d
  println()
  println(r.c)

}