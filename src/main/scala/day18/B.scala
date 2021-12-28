package day18

object B extends App {

    val lines = io.Source.stdin.getLines()
    val res = lines.map(parse).toList

    case class Number(n: Int, l: Int)

    def parse(s: String): List[Number] = {
        def f(xs: List[Char], level: Int, acc: List[Number]): List[Number] = {
            xs match {
                case '[' :: t => f(t, level + 1, acc)
                case ']' :: t => f(t, level - 1, acc)
                case h :: t if h.isDigit => f(t, level, acc :+ Number(h.asDigit, level))
                case _ :: t => f(t, level, acc)
                case Nil => acc
            }
        }

        f(s.toList, 0, Nil)
    }

    def explode(xs: List[Number], i: Int): List[Number] = {
        i match {
            case 0 =>
                val Seq(n1, n2) = xs.tail.take(2)
                //        println(s"n1 $n1 n2 $n2")
                List(Number(0, 4), n2.copy(n = n1.n + n2.n)) ++ xs.drop(3)
            case x if x < xs.length - 2 =>
                val Seq(n0, n1, n2, n3) = xs.slice(i - 1, i + 3)
                val mid = List(n0.copy(n = n0.n + n1.n), Number(0, 4), n3.copy(n = n2.n + n3.n))
                xs.take(x - 1) ++ mid ++ xs.drop(x + 3)
            case _ =>
                val Seq(n1, n2, _) = xs.takeRight(3)
                xs.take(i - 1) ++ List(Number(n1.n + n2.n, 4), Number(0, 4))
        }
    }

    def split(xs: List[Number], i: Int): List[Number] = {
        val num = xs(i)
        val (n1, n2) = ((num.n.toDouble / 2).floor.toInt, (num.n.toDouble / 2).ceil.toInt)
        xs.take(i) ++ List(Number(n1, num.l + 1), Number(n2, num.l + 1)) ++ xs.drop(i + 1)
    }

    //  println(res.head)
    //  println(explode(res.head, 0))
    //  println(res)

    def snailAdd(num: List[Number]) = {
        Iterator
          .iterate((num, false)) { case (num, _) =>
              num.zipWithIndex.find(_._1.l > 4).map(_._2) match {
                  case Some(i) => (explode(num, i), false)
                  case None =>
                      num.zipWithIndex.find(_._1.n >= 10).map(_._2) match {
                          case Some(i) => (split(num, i), false)
                          case None => (num, true)

                      }
              }
          }
          .dropWhile(_._2 == false)
          .next
          ._1
    }

    def p(num: List[Number]) = {
        ("[" * (1 + num.head.l)) + num.sliding(2).flatMap { case
            ::(head, next) => (head.l, next.head.l) match {
            case (h, t) if h == t => head.n + "," + next.head.n + "]"
            case (h, t) if h > t => "," + next.head.n
            case (h, t) if h < t => "]," + ("[" * (t - h + 1))
        }
        case Nil => ""
        }.mkString("")
    }

    def add(x: List[Number], y: List[Number]) = {
        println(s"adding $x and $y")
        snailAdd((x ++ y).map(n => n.copy(l = n.l + 1)))
    }

    val added = res.reduce(add)
    println(added)
    //  println(p(added))

    def m(num: List[Number]): Int = {
        println(s" m of $num")
        num match {
            case h :: Nil => h.n
            case _ =>
                val maxLevel = num.map(_.l).max
                val (head, tail) = num.span(_.l != maxLevel)
                println(s"  head $head tail $tail")
                val maxLeft = tail.head.n
                val maxRight = tail.tail.head.n
                println(s"  maxLeft $maxLeft maxRight $maxRight")
                m((head :+ Number(3 * maxLeft + 2 * maxRight, maxLevel - 1)) ++ tail.drop(2))
        }
    }

    println(m(added))

    val magnitudes = res.combinations(2).flatMap(_.permutations.map(p => m(snailAdd(add(p.head, p.last)))))
    println(magnitudes.toList.max)

}