package day5

object B extends App {

    case class C(x1: Int, y1: Int, x2: Int, y2: Int)

    def parse(str: String) = {
        val r = str.split("->").flatMap(_.split(",").map(_.trim.toInt))
        C(r(0), r(1), r(2), r(3))
    }

    val N = 1000

    val list = io.Source.stdin.getLines().map(parse)
    val res = Array.fill(N)(Array.fill(N)(0))
    list.foreach(c => {
        println(c)
        if(c.x1 == c.x2) {
            for {
                y <- Math.min(c.y1, c.y2) to Math.max(c.y1, c.y2)
            } {
                res(y)(c.x1) += 1
            }
        } else if(c.y1 == c.y2) {
            for {
                x <- Math.min(c.x1, c.x2) to Math.max(c.x1, c.x2)
            } {
                res(c.y1)(x) += 1
            }
        } else {
            val dx = Math.signum(c.x2 - c.x1).toInt
            val dy = Math.signum(c.y2 - c.y1).toInt
            var y = c.y1
            (c.x1 to c.x2 by dx) foreach(x => {
                res(y)(x) += 1
                y += dy
            })
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
