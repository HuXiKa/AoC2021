package day17

object A extends App {

  val lines = io.Source.stdin.getLines()
  val res = lines.toList

  val coords = res.head.split("x=").last.split(", y=")

  case class Probe(x: Int, y: Int, xVelocity: Int, yVelocity: Int){
    def move = {
      copy(x = x + xVelocity, y = y + yVelocity, xVelocity = xVelocity + (if(xVelocity<0) 1 else if(xVelocity>0) -1 else 0), yVelocity = yVelocity - 1)
    }
  }
  case class Target(x1: Int, x2: Int, y1: Int, y2: Int)

  object Target {
    def of(array: Array[Array[Int]]) = array match {
      case Array(Array(x1, x2),Array(y1, y2)) => Target(x1,x2,y1,y2)
    }
  }

  val t = Target.of(coords.map(_.split("\\.\\.").map(_.trim).map(_.toInt)))
  val probes =
    for{
      x <- 1 to t.x2
      y <- t.y1 to -t.y1
    } yield {
      Probe(0, 0, x, y)
    }
  println(probes.length)
  println(t)

  def inTarget(x: Int, y: Int) = x >= t.x1 && x <= t.x2 && y >= t.y1 && y <= t.y2

  def f(probe: Probe): List[(Int, Int)] = {
    (probe.x, probe.y) match {
      case (x,y) if x > t.x2 || y < t.y2 => List((x,y))
      case (x,y) if inTarget(x,y) => List((x,y))
      case (x,y) => List((x,y)) ::: f(probe.move)
    }
  }

  println(probes)
  val r = probes.map(p => (p,f(p))).filter{case (_, t) => inTarget(t.last._1, t.last._2)}
  println(r.map{case (p,t) => p -> t.maxBy(_._2)}.maxBy{ case (probe, tuple) => tuple._2})

}