package day8

import scala.collection.convert.ImplicitConversions.`collection asJava`

object B extends App {

    val list = io.Source.stdin.getLines().toList.map(_.split(" \\| "))
    val signals = list.map(_.head).map(_.split(" ").toList)
    val outputs = list.map(_.last).map(_.split(" ").toList)

    def f(s: List[String]) = {
        val one = s.find(_.length == 2).get
        val seven = s.find(_.length == 3).get
        val four = s.find(_.length == 4).get
        val eight = s.find(_.length == 7).get

        val twoThreeFive = s.filter(_.length == 5)
        val sixNineZero = s.filter(_.length == 6)

        val top = seven diff one

        val three = twoThreeFive.find(_.toCharArray.toSet.containsAll(one.toCharArray.toSet)).get
        val nine = sixNineZero.filter(_.toCharArray.toSet.containsAll(three.toCharArray.toSet)).head
        val six = sixNineZero.find(!_.toCharArray.toSet.containsAll(one.toCharArray.toSet)).get

        val topRight = eight diff six

        val five = twoThreeFive.find(!_.toCharArray.toSet.containsAll(topRight.toCharArray.toSet)).get

        val two = (twoThreeFive diff Seq(three,five)).head

        val zero = (sixNineZero diff Seq(six,nine)).head

        val center = eight diff zero

        val topLeft = nine diff three

        val bottomLeft = six diff five

        val bottomRight = three diff two

        val bottom = eight diff (top + topLeft + topRight + center + bottomLeft + bottomRight)

/*
        println(s"top $top")
        println(s"topRight $topRight")
        println(s"center $center")
        println(s"topLeft $topLeft")
        println(s"bottomLeft $bottomLeft")
        println(s"bottomRight $bottomRight")
        println(s"bottom $bottom")

        println(s"zero $zero")
        println(s"one $one")
        println(s"two $two")
        println(s"three $three")
        println(s"four $four")
        println(s"five $five")
        println(s"six $six")
        println(s"seven $seven")
        println(s"eight $eight")
        println(s"nine $nine")*/

        Array(zero,one,two,three,four,five,six,seven,eight,nine)
    }

    val map = signals.map(f(_).toList).zip(outputs)
    println(map)

    val res = map.map{
        case (strings, value) => value.map(v => strings.indexWhere(_.sorted.equals(v.sorted))).map(_.toString).mkString("").toInt
    }.sum

    println(res)

}
