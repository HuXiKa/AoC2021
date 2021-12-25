package day17

object A extends App {

  val lines = io.Source.stdin.getLines()
  val r = lines.next.map(c => Integer.parseInt(c + "", 16)).map(_.toBinaryString).map(s => "0" * (4 - s.length) + s).mkString("")

  println(r)

  def b2d(s: String) = BigInt(s, 2)

  sealed trait Packet {
    def version: BigInt
  }

  case class Literal(version: BigInt, value: BigInt) extends Packet
  case class Operator(version: BigInt, typeId: BigInt, packets: List[Packet]) extends Packet

  object Packet {
    def parse(xs: String, size: Int = 0, packets: List[Packet] = Nil): (Int, List[Packet]) = {
      val data = xs.drop(size)
      val version = b2d(data.take(3))
      val typeId = b2d(data.substring(3, 6))
      val (s, p) =
        if (typeId == 4) parseLiteral(data, version)
        else parseOperator(data, version, typeId, data(6).asDigit)
      (size + s, packets :+ p)
    }

    def parseLiteral(xs: String, version: BigInt) = {
      val (blocks1, blocks0) = xs.drop(6).grouped(5).toList.span(_.head == '1')
      val value = (blocks1 :+ blocks0.head).map(_.tail).mkString
      val size = 6 + (blocks1.size + 1) * 5
      (size, Literal(version, b2d(value)))
    }

    def parseOperator(xs: String, version: BigInt, typeId: BigInt, lengthId: Int) = {
      val (size, packets) = lengthId match {
        case 1 =>
          val packetCount = b2d(xs.substring(7, 18))
          Iterator
            .iterate((7 + 11, List.empty[Packet]))({ case (sz, p) => parse(xs, sz, p) })
            .drop(packetCount.intValue)
            .next()
        case _ =>
          val packetsSize = b2d(xs.substring(7, 22)) + 7 + 15
          Iterator
            .iterate((7 + 15, List.empty[Packet]))({ case (sz, p) => parse(xs, sz, p) })
            .dropWhile(_._1 < packetsSize)
            .next()
      }
      (size, Operator(version, typeId, packets))
    }
  }

  val (_, packets) = Packet.parse(r)

  println(packets)

  def s(p: Packet): BigInt = p match {
    case Literal(version, value) => version
    case Operator(version, typeId, packets) => version + packets.map(s).sum
  }

  println(s(packets.head))

}