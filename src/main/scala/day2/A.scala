package day2

object A extends App {
  val list = io.Source.stdin.getLines().map(_.split(" ")).map { case Array(cmd,p) => (cmd,p.toInt) }
  val res = list.foldLeft((0,0))({
    case ((h,d),("forward",p)) => (h+p,d)
    case ((h,d),("up",p)) => (h,d-p)
    case ((h,d),("down",p)) =>(h,d+p)
  })
  println(res._1 * res._2)
}
