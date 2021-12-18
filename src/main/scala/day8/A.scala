package day8

object A extends App {

    val list = io.Source.stdin.getLines().toList.map(_.split(" \\| "))
    val signals = list.map(_.head).map(_.split(" ").toList)
    val outputs = list.map(_.last).map(_.split(" ").toList)

    val lengths = Set(2,3,7,4)

    println(outputs)

    val res = outputs.map(_.count(s => lengths.contains(s.length))).sum

    println(res)

}
