package day7

object B extends App {

    val list = io.Source.stdin.getLines().toList.head.split(",").map(_.toInt).toList
    val min = list.min
    val max = list.max

    val best = list.min
    val bestDist = list.min

    val res = (min to max) map (i => {
        (i, list.map(x => (1 to Math.abs(x - i)).sum).sum)
    })

    println(res)
    println(res.minBy(_._2))

}
