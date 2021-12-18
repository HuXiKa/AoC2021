package day10

import scala.collection.mutable

object B extends App {

    val list = io.Source.stdin.getLines()

    def corrupted(str: String) = {
        val stack = new mutable.Stack[Char]
        str.map{ c => {
            c match {
                case '(' | '{' | '[' | '<' =>
                    stack.push(c)
                    false
                case ')' | '}' | ']' | '>' =>
                    val prev = stack.pop()
                    (c == ')' && prev != '(') || (c == '}' && prev != '{') || (c == ']' && prev != '[') || (c == '>' && prev != '<')
            }
        }}.exists(identity)
    }


    def missing(str: String) = {
        var s = str
        str.zipWithIndex.foreach{case (c,i) => c match {
                case '(' | '{' | '[' | '<' =>
                case ')' =>
                    s = s.updated(s.substring(0, i).lastIndexOf('('), ' ')
                    s = s.updated(s.indexOf(')'), ' ')
                case '}' =>
                    s = s.updated(s.substring(0, i).lastIndexOf('{'), ' ')
                    s = s.updated(s.indexOf('}'), ' ')
                case ']' =>
                    s = s.updated(s.substring(0, i).lastIndexOf('['), ' ')
                    s = s.updated(s.indexOf(']'), ' ')
                case '>' =>
                    s = s.updated(s.substring(0, i).lastIndexOf('<'), ' ')
                    s = s.updated(s.indexOf('>'), ' ')
        }}
        s.filterNot(_.isSpaceChar)
    }

    def score(str: String) = {
        var r = BigInt(0)
        str.reverse.foreach(c => c match {
            case '(' =>
                r = r * 5 + 1
            case '{' =>
                r = r * 5 + 3
            case '[' =>
                r = r * 5 + 2
            case '<' =>
                r = r * 5 + 4
        })
        r
    }

    val res = list.filterNot(corrupted).map(missing).toList
    val scores = res.map(score)
    println(res)
    println(scores)
    println(scores.sorted.toList(scores.length/2))

}