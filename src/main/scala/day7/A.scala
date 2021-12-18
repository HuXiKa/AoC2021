package day7

object A extends App {

    val list = io.Source.stdin.getLines().toList.head.split(",").map(_.toInt).toList
    val min = list.min
    val max = list.max

    val best = list.min
    val bestDist = list.min

    val res = (min to max) map (i => {
        (i, list.map(x => Math.abs(x - i)).sum)
    })

    println(res.minBy(_._2))

}
