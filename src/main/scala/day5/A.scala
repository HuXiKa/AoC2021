package day5

object A extends App {

    case class C(x1: Int, y1: Int, x2: Int, y2: Int)

    def parse(str: String) = {
        val r = str.split("->").flatMap(_.split(",").map(_.trim.toInt))
        C(r(0), r(1), r(2), r(3))
    }

    val N = 1000

    val list = io.Source.stdin.getLines().map(parse)
    val res = Array.fill(N)(Array.fill(N)(0))
    list.toList.filter(c => c.x1 == c.x2 || c.y1 == c.y2).foreach(c => {
        if(c.x1 == c.x2) {
            for {
                y <- Math.min(c.y1, c.y2) to Math.max(c.y1, c.y2)
            } {
                res(c.x1)(y) += 1
            }
        } else {
            for {
                x <- Math.min(c.x1, c.x2) to Math.max(c.x1, c.x2)
            } {
                res(x)(c.y1) += 1
            }
        }
    })
    print(s"\n\n${res.map(_.mkString(" ")).mkString("\n")}")
    var a = for {
        x <- res.indices
        y <- res.head.indices
    } yield {
        res(x)(y) >= 2
    }

    println()
    println(a.count(identity))

}
