package day10

import scala.collection.mutable

object A extends App {

    val list = io.Source.stdin.getLines()

    def corrupted(str: String) = {
        val stack = new mutable.Stack[Char]
        str.zipWithIndex.flatMap{ case (c, i) => {
            c match {
                case '(' | '{' | '[' | '<' =>
                    stack.push(c)
                    None
                case ')' | '}' | ']' | '>' =>
                    val prev = stack.pop()
                    Some(str, i, (c == ')' && prev != '(') || (c == '}' && prev != '{') || (c == ']' && prev != '[') || (c == '>' && prev != '<'))
            }
        }}.filter(x => identity(x._3))
    }


    val m = Map((')' -> 3), (']' -> 57), ('}' ->  1197), ('>' -> 25137))


val res = list.map(corrupted).filter(_.nonEmpty).map{case Seq((str, i, b)) => str.charAt(i)}

    println(res.map(m.apply).sum)

}