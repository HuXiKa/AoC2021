package day14

object A extends App {

    val lines = io.Source.stdin.getLines().toList
    val (rules, template) = lines.filter(_.nonEmpty).partition(_.contains("->"))
    println(template.toList)
    //println(rules.toList)

    val ruleMap = rules.map(_.split(" -> ")).map{case Array(a,b) => a -> b}.toMap
    println(ruleMap)

    val merge = 3 to 10000000 by 3//(2 to 8 by 3 map (i => (i, i + 1)))

    val r = 0.until(10).foldLeft(template.head)({
        case (template, _) =>
            val i = template.sliding(2).map(t => t.head + ruleMap(t) + t.last).mkString("")
            var res = i
            merge.filter(_ < i.length).foreach(m => res = res.patch(m, " ", 1))
            res.filterNot(_ == ' ')
    })


    println(r.length)

    val fmap = r.groupBy(identity).view.mapValues(_.length).toMap
    println(fmap)
    println(fmap.maxBy(_._2)._2 - fmap.minBy(_._2)._2)

/*
    012345678
    NCNNBCCHB
    NCN B CHB

    NBCCNBBBCBHCB
    NBCCNBBBCBHCB
*/
}