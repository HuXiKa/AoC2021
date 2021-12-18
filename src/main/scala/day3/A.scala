package day3

object A extends App {
  val list = io.Source.stdin.getLines().toList
  val n = list.length
  val list2 = list.map(_.zipWithIndex.filter(_._1 == '1').map(_._2))
  val f = list2.foldLeft(Array.fill(list.head.length)(0))({
    case (acc, occ) =>
      occ.foreach(o => acc.update(o, acc(o) + 1))
      acc
  })
  val flags = f.map(_ > n / 2)
  println(flags.mkString(" "))
  val gamma = Integer.parseInt( flags.map{ case true => '1'; case false => '0' }.mkString, 2 )
  val epsilon = Integer.parseInt( flags.map{ case true => '0'; case false => '1' }.mkString, 2 )
  println(gamma)
  println(epsilon)
  println(gamma * epsilon)
}
