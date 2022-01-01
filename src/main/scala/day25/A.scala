package day25

import java.util
import scala.jdk.CollectionConverters._

object A extends App {

  val lines = io.Source.stdin.getLines()
  val board = new Board(lines.map(_.toCharArray).toArray)

  class Board(val numbers: Array[Array[Char]]) {
    def d = print(s"\n\n${numbers.map(_.mkString(" ")).mkString("\n")}")

    def move(ch: Char, next: (Int, Int) => (Int, Int)) = {
      numbers.indices flatMap { r =>
        numbers.head.indices flatMap { c=>
          if(numbers(r)(c) == ch){
            val (rn,cn) = next(r,c)
            if(numbers(rn)(cn) == '.'){
              List((r,c) -> '.', (rn, cn) -> ch)
            } else List.empty
          }
          else List.empty
        }
      }
    }

    def i = {
      val res = new Board(numbers.transpose.transpose)
      res.move('>', (r,c) => (r, (c + 1) % numbers.head.length)).foreach {
        case ((r,c), ch) => res.numbers(r)(c) = ch
      }
      res.move('v', (r,c) => ((r + 1) % numbers.length, c)).foreach {
        case ((r,c), ch) => res.numbers(r)(c) = ch
      }
      res
    }


  }

  val r = Iterator
    .iterate((board,0))({ case ((b, i)) => {
      //println(i)
      (b.i, i + 1)
    }})
    .sliding(2)
    .dropWhile(i => !i.head._1.numbers.corresponds(i.last._1.numbers){(x,y) => x.corresponds(y){_ == _}})
    .next
    .head

  println(r._2+1)



}
