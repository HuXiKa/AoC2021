package day4

import scala.util.control.Breaks._

object A extends App {
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

  breakable {
    for {
      number <- numbers
      board <- boards
    } yield {
      board.p(number)
      if (board.w) {
        println(board.r * number)
        break()
      }
    }
  }

//  boards.foreach(_.d)

}
