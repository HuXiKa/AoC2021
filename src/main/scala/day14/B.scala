package day14

object B extends App {

    val lines = io.Source.stdin.getLines().toList
    val (rules, template) = lines.filter(_.nonEmpty).partition(_.contains("->"))

    val ruleMap = rules.map(_.split(" -> ")).map{case Array(a,b) => a -> b}.toMap

    var count = ruleMap.keys.map(_ -> BigInt(0)).toMap
    template.head.sliding(2).foreach(p => count += count.get(p).map(x => p -> (x + 1)).getOrElse(p -> 1))

    val r = 0.until(40).foldLeft(count)({
        case (map, it) =>
            //println(s"it $it")
            //println(s"count $count")
            map.filter{ case (_, i) => i > 0}.foreach{ case (str, i) =>
                val n = ruleMap(str)
                count = count.updated(str, count(str) - i)
                List(str.head + n, n + str.last).foreach(n => {
                    count += count.get(n).map(x => n -> (x + i)).getOrElse(n -> i)
                })
            }
            //println(s"count $count")
            count
    })

    println(ruleMap.values.toList.distinct)
    val res = ruleMap.values.toList.distinct.map(l => l -> count.map {
        case (str, int) if str.startsWith(l) => int
        case _ => BigInt(0)
    }.sum)

    val f = res.map{
        case (str, i) if str.head == template.head.last => (str,i + 1)
        case (str, i) => (str, i)
    }

    println(f)

    println(f.maxBy(_._2)._2 - f.minBy(_._2)._2)

}