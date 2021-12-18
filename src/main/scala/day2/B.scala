package day2

object B extends App {
  val list = io.Source.stdin.getLines().map(_.split(" ")).map { case Array(cmd,p) => (cmd,p.toInt) }
  val res = list.foldLeft((0,0,0))({
    case ((h,d,a),("forward",p)) => (h+p,d+p*a,a)
    case ((h,d,a),("up",p)) => (h,d,a-p)
    case ((h,d,a),("down",p)) =>(h,d,a+p)
  })

  println(res._1 * res._2)
}
