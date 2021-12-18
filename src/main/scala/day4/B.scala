package day4

object B extends App {
  val lines = io.Source.stdin.getLines()
  val numbers = lines.next().split(",").map(_.toInt)

  val boards = lines.toArray.grouped(6).map(_.tail).map(_.map(_.split("\\D+").filter(_.nonEmpty).map(_.toInt))).map(new Board(_)).toList

  class Board(numbers: Array[Array[Int]]){

    private val hit = Array.fill(5)(Array.fill(5)(false))

    def d = print(s"\n\n${hit.map(_.mkString(" ")).mkString("\n")}")
    def p(n: Int) = {
      numbers.indices foreach { r =>
        numbers.head.indices foreach { c =>
          if(numbers(r)(c) == n){
            hit(r)(c) = true
          }
        }
      }
    }

    def r = {
      var s = 0
      numbers.indices foreach { r =>
        numbers.head.indices foreach { c =>
          if(!hit(r)(c)){
            s += numbers(r)(c)
          }
        }
      }
      s
    }

    def w: Boolean = {
      hit.exists(_.forall(identity)) || (hit.indices).map(r => hit.forall(_.apply(r))).exists(identity)
    }

  }

//  println(numbers.mkString(" "))
    var winners = List.empty[Int]
    var res = (0,0)

    for {
      number <- numbers
      board <- boards
    } yield {
      board.p(number)
      if (board.w && !winners.contains(boards.indexOf(board))) {
        winners = winners :+ boards.indexOf(board)
        //board.d
        println(board.r * number)
        if(boards.size == winners.size){
          res = (board.r,number)
        }
      }
    }

    println(winners)
    println(res)
    println(res._1 * res._2)


//  boards.foreach(_.d)

}

