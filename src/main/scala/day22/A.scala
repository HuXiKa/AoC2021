package day22

object A extends App {

  object Command {

    def of(str: String): Command = {
      def parse(c: String): Range = {
        val Array(a, b) = str.split(s"$c=").last.split(",").head.split("\\.\\.").map(_.toInt)
        Range.inclusive(a, b)
      }

      if (str.startsWith("on")) {
        On(Cuboid(parse("x"), parse("y"), parse("z")))
      } else {
        Off(Cuboid(parse("x"), parse("y"), parse("z")))
      }
    }

  }

  sealed trait Command {
    def c: Cuboid
  }

  case class On(c: Cuboid) extends Command

  case class Off(c: Cuboid) extends Command


  val commands = io.Source.stdin.getLines().map(Command.of).toList.dropRight(2)

  println(commands)

  case class Cuboid(x: Range, y: Range, z: Range) {

    def toRange(value: IndexedSeq[Int]) = {
      Range.inclusive(value.min, value.max)
    }

    def intersection(other: Cuboid): Option[Cuboid] = {
      val nx = x.intersect(other.x)
      val ny = y.intersect(other.y)
      val nz = z.intersect(other.z)
      if (nx.nonEmpty && ny.nonEmpty && nz.nonEmpty) Option(Cuboid(toRange(nx), toRange(ny), toRange(nz))) else None
    }
  }

  def process(xs: List[Command]): Set[(Int, Int, Int)] = {
    val bounds = Cuboid(-50 to 50, -50 to 50, -50 to 50)

    def f(xs: List[Command], acc: Set[(Int, Int, Int)]): Set[(Int, Int, Int)] = xs match {
      case Nil => acc
      case h :: t =>
        h.c.intersection(bounds) match {
          case Some(c) =>
            val next = c.x.flatMap { x => c.y.flatMap(y => c.z.map(z => (x, y, z))) }
            h match {
              case On(_) => f(t, acc ++ next)
              case Off(_) => f(t, acc -- next)
            }
          case None =>
            f(t, acc)
        }
    }

    f(xs, Set.empty)
  }

  val res = process(commands)
  println(res.size)
}
