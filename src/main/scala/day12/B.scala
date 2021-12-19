package day12

object B extends App {

    sealed trait Node {
        def children: Set[Node]

        def add(n: Node): Node
        def name: String
        def find(n: String): Option[Node] = {
            if(n == name) Some(this)
            else children.map(_.find(n)).find(_.nonEmpty).flatten
        }

        override def hashCode(): Int = name.hashCode

        override def equals(obj: Any): Boolean = obj.isInstanceOf[Node] && obj.asInstanceOf[Node].name == name

        override def toString: String = {
            p(0)
        }

        def p(depth: Int, limit: Int = Int.MaxValue): String = {
            if(depth <= limit) (("-" * depth) + name) + System.lineSeparator() + children.map(_.p(depth + 1, limit)).mkString("")
            else ""
        }

        def deepAdd(child: Node) = {
            var c = children.filterNot(_.name == child.name)
            c = c + child
            c.fold(Node.of(name))((r, ch) => r.add(ch))
        }
    }

    object Node {
        def parse(nodes: Set[Node]): Node = {
            def f(name: String, p: Set[String]): Node ={
                var r = nodes.find(_.name == name).get
                //println(s"found $r with $p")
                val c = r.children.filterNot(c => p.contains(c.name)).map(c => {
                    //    println(s"looking for $c")
                    val cn = nodes.find(_.name == c.name).map(n => f(n.name, p + name)).get
                    //println(s" found $cn for $r")
                    r = r.deepAdd(cn)
                    //println(s" r now $r")
                    r
                })
                //println(s"all of c $c")
                r
            }

            f("start", Set.empty)
        }

        def of(s: String) = {
            s match {
                case "start" => StartNode(Set.empty)
                case "end" => EndNode(Set.empty)
                case s if s.charAt(0).isUpper => BigNode(Set.empty, s)
                case _ => SmallNode(Set.empty, s)
            }
        }
    }

    case class StartNode(children: Set[Node]) extends Node {
        def add(n: Node) = copy(children = children + n)
        val name = "start"
    }
    case class EndNode(children: Set[Node]) extends Node{
        def add(n: Node) = copy(children = children + n)
        val name = "end"
    }
    case class BigNode(children: Set[Node], name: String) extends Node{
        def add(n: Node) = copy(children = children + n)
    }
    case class SmallNode(children: Set[Node], name: String) extends Node{
        def add(n: Node) = copy(children = children + n)
    }

    var nodes = Set.empty[Node]

    def read(str: String) = {
        val n = str.split("-").map(Node.of)
        val a = n(0)
        val b = n(1)
        nodes.find(_.name == a.name).orElse(Option(a)).foreach(oa => {
            val nn = oa.add(b)
            nodes -= nn
            nodes += nn
        })
        nodes.find(_.name == b.name).orElse(Option(b)).foreach(ob => {
            val nn = ob.add(a)
            nodes -= nn
            nodes += nn
        })
    }

    val lines = io.Source.stdin.getLines().toList.map(read)
    val root = Node.parse(nodes)

    def smallCaves(p: List[String]) = {
        p.filterNot(s => (s == "start") || (s == "end")).filter(_.head.isLower)

    }

    def paths(root: Node) = {
        def f(node: Node, path: List[String]): Set[List[String]] = {
            val p = path :+ node.name
            //            println(s"path ${p.mkString("->")}")
            node.children.flatMap {
                case StartNode(children) => Set(p) // we circled back
                case n@EndNode(children) =>
                    //println(s"path ${p.mkString("->")}")
                    Set(p :+ n.name) // we finished a path
                case n@BigNode(children, name) => f(root.find(name).get, p) // start again
                case n@SmallNode(children, name) =>
                    val m = smallCaves(p).groupBy(identity).view.mapValues(_.size)
                    if(m.forall{ case (str, i) => i <= 1}) // every small cave only visited once
                        f(root.find(name).get, p) // already visited once, but continue
                    else if(m.getOrElse(name, 0) < 1)
                        f(root.find(name).get, p) // already visited once, but continue
                    else
                        Set(p) // already visited
                case n@SmallNode(children, name) => f(root.find(name).get, p) // continue
            }
        }

        f(root, List.empty)
    }

    //println(root.p(0))

    val res = paths(root)

    //    println(root.toString)
    //println(res.filter(_.contains("end")))
    println(res.count(_.contains("end")))

}