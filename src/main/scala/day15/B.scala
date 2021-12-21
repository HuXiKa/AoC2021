package day15

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

object B extends App {

    def expand(numbers: Array[Array[Int]]) = {
        val res = Array.fill(numbers.length * 5)(Array.fill(numbers.head.length * 5)(0))
        for {
            r <- 0 to 4
            c <- 0 to 4
            row <- numbers.indices
            col <- numbers.head.indices
        } {
            val rv = numbers.length * r + row
            val cv = numbers.head.length * c + col
            val vv = numbers(row)(col) + r + c
            res(rv)(cv) = vv%10 + (if((r > 0 || c > 0) && vv > 9) 1 else 0)
        }
        res
    }

    val board = new Board(expand(io.Source.stdin.getLines().map(_.map(_.asDigit).toArray).toArray))
    val indexes = List((0,1),(1,0),(-1,0),(0,-1))

    type Coordinate = (Int, Int)
    case class Edge(a: Coordinate, b: Coordinate, costFromStart: Int)

    class Board(numbers: Array[Array[Int]]) {

        def dim = ((numbers.length - 1) -> (numbers.head.length - 1))

        class Node(val r: Int, val c: Int, val n: mutable.Set[Coordinate]) {
            def v = numbers(r)(c)
            def coordinate = (r, c)

            override def toString: String = s"[($r,$c)[${n.mkString(",")}]"
        }

        def d = print(s"\n\n${numbers.map(_.mkString(" ")).mkString("\n")}")

        private def neighbours(edge: Edge) = {
            indexes
              .filter{
                  case (x,y) => x+edge.b._1 >= 0 && x+edge.b._1< numbers.length && y+edge.b._2 >= 0 && y+edge.b._2<numbers.head.length
              }
              .map{
                  case (x,y) => Edge(edge.b, (x+edge.b._1,y+edge.b._2), edge.costFromStart + v(x+edge.b._1,y+edge.b._2))
              }
        }

        lazy val nodes = for {
            r <- numbers.indices
            c <- numbers.head.indices
        } yield {
            (r,c)
        }

        private def v(coordinate: Coordinate) = numbers(coordinate._1)(coordinate._2)

        def dijkstra(from: Coordinate, to: Coordinate) = {
            val queue = new mutable.PriorityQueue[Edge]()((x,y) => y.costFromStart - x.costFromStart)
            val dist = nodes.map(_ -> Int.MaxValue).to(mutable.Map)

            def f(current: Edge, unvisited: mutable.Set[Coordinate]): Int = {
                unvisited -= current.b
                if(unvisited.size % 10000 == 0) println(s"current $current uv ${unvisited.size} queue ${queue.length}")
                if(unvisited.isEmpty || current.b == to) {
                    current.costFromStart
                } else {
                    val ne = neighbours(current)
                    val nef = ne.filter(e => unvisited.contains(e.b))
                    nef.foreach(e => {
                        val d = dist(e.b)
                        if(e.costFromStart < d){
                            dist += e.b -> e.costFromStart
                            queue.enqueue(e)
                        }
                    })
                    val min = queue.dequeue()
                    f(min, unvisited)
                }
            }

            f(Edge(from, from, 0), nodes.to(mutable.Set))

        }
    }


    val dest = board.dim
    val r = board.dijkstra((0,0),dest)
    println(r)

}