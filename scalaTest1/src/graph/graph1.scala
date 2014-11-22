package graph 

import scala.util.matching.Regex
import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.math.Ordering._

  abstract class GraphBase[T, U] {
    case class Edge(n1: Node, n2: Node, value: U) {
      def toTuple = (n1.value, n2.value, value)
    }
    case class Node(value: T) {
      var adj: List[Edge] = Nil
      // neighbors are all nodes adjacent to this node.
      def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
    }

    var nodes: Map[T, Node] = Map()
    var edges: List[Edge] = Nil
    def toTermForm = (nodes map (_._2 .value),edges map (_.toTuple))
    def toAdjacentForm = nodes.map((n)=>(n._1,(List[(T,U)]() /: edges)((acc,x)=> if(x.n1.value  == n._1) (x.n2.value,x.value)::acc else acc ))).toList
    // If the edge E connects N to another node, returns the other node,
    // otherwise returns None.
    def edgeTarget(e: Edge, n: Node): Option[Node]

    override def equals(o: Any) = o match {
      case g: GraphBase[_,_] => ((nodes.keys.toList sameElements g.nodes.keys.toList)  &&
                                 (edges.map(_.toTuple) sameElements g.edges.map(_.toTuple)) )
      case _ => false
    }
    def addNode(value: T) = {
      val n = new Node(value)
      nodes = Map(value -> n) ++ nodes
      n
    }
    
    //doing dfs
    def findPaths(n1:T,n2:T):List[List[T]] = {
      def findPathsR(currNode:Node, currPath:List[T]):List[List[T]] = {
        if(currNode == nodes(n2)) List(currPath)
        else currNode.neighbors.filter((nd:Node) => !currPath.contains(nd.value)).flatMap(n=>findPathsR(n, n.value ::currPath))
      }
    findPathsR(nodes(n1), List(n1)).map(_.reverse)
  }
    
    def findCycles(n1:T):List[List[T]] = nodes(n1).neighbors.flatMap(n=>findPaths(n.value , n1).filter(_ != List(n.value,n1))).map(n1::_) 
    
    def minimalSpanningTree(implicit f:Ordering[U]):Graph[T,U] = {
   		implicit val edgeOrder = f.on((x:Edge)=>x.value)
      
      //candidateEdges ++ 
      def mstR( collectedEdges:List[Edge], candidateEdges:TreeSet[Edge], collectedNodes:Set[Node]):Graph[T,U] =
        if(collectedNodes.size == nodes.size) Graph.termLabel(nodes.values.toList.map(nd=>nd.value ), collectedEdges.map(e=>(e.n1.value ,e.n2.value,e.value)))
        else {
         val currMin =  candidateEdges.min
         val otherEnd = if(collectedNodes(currMin.n1)) currMin.n2 else currMin.n1
         val cns = collectedNodes + otherEnd
         mstR( currMin :: collectedEdges ,(candidateEdges ++ otherEnd.adj).filter(x=> !(cns(x.n1) && cns(x.n2))), cns)
        }
     mstR(List(),TreeSet[Edge]()++nodes.head._2.adj,Set(nodes.head._2 ))
    }
    
    //TODO: some duplicates still coming
    def spanningTrees:List[Graph[T,U]] = {
      
      //candidateEdges ++ 
      def mstR( collectedEdges:List[Edge], candidateEdges:List[Edge], collectedNodes:Set[Node]):List[List[Edge]] =
        if(collectedNodes.size == nodes.size) List(collectedEdges)//collectedEdgess.map(collectedEdges=>Graph.termLabel(nodes.values.toList.map(nd=>nd.value ), collectedEdges.map(e=>(e.n1.value ,e.n2.value,e.value))))
        else {
          candidateEdges.flatMap(edg =>{
        			  val otherEnd = if(collectedNodes(edg.n1)) edg.n2 else edg.n1
        			  val cns = collectedNodes + otherEnd
        			  mstR( edg :: collectedEdges ,(candidateEdges ++ otherEnd.adj).filter(x=> !(cns(x.n1) && cns(x.n2))), cns)
          })
        }
     mstR(List(),List()++nodes.head._2.adj,Set(nodes.head._2 )).map(collectedEdges=>Graph.termLabel(nodes.values.toList.map(nd=>nd.value ), collectedEdges.map(e=>(e.n1.value ,e.n2.value,e.value))))
    }
}
  class Graph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Graph[_,_] => super.equals(g)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else if (e.n2 == n) Some(e.n1)
      else None

    def addEdge(n1: T, n2: T, value: U) = {
      val e = new Edge(nodes(n1), nodes(n2), value)
      edges = e :: edges
      nodes(n1).adj = e :: nodes(n1).adj
      nodes(n2).adj = e :: nodes(n2).adj
    }
  }
  
  class Digraph[T, U] extends GraphBase[T, U] {
    override def equals(o: Any) = o match {
      case g: Digraph[_,_] => super.equals(g)
      case _ => false
    }

    def edgeTarget(e: Edge, n: Node): Option[Node] =
      if (e.n1 == n) Some(e.n2)
      else None

    def addArc(source: T, dest: T, value: U) = {
      val e = new Edge(nodes(source), nodes(dest), value)
      edges = e :: edges
      nodes(source).adj = e :: nodes(source).adj
    }
  }
  
  abstract class GraphObjBase {
    type GraphClass[T, U]
    def addLabel[T](edges: List[(T, T)]) =
      edges.map(v => (v._1, v._2, ()))
    def term[T](nodes: List[T], edges: List[(T,T)]) =
      termLabel(nodes, addLabel(edges))
    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]): GraphClass[T, U]
    def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
      nodes.map(a => (a._1, a._2.map((_, ()))))
    def adjacent[T](nodes: List[(T, List[T])]) =
      adjacentLabel(addAdjacentLabel(nodes))
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]): GraphClass[T, U]
    
    val grphPat2 , singleNodePat, grphEdgePat:Regex
    val grphLabelPat2 , grphEdgeLabelPat:Regex

  def fromString(gStr: String) = {
    val es = (for (r <- grphPat2.findAllIn(gStr) if !r.isEmpty) yield r).map {
      case grphEdgePat(n1, n2) => List(n1, n2)
      case singleNodePat(n1) => List(n1)
    }.toList
    val edgTuples = (es filter (_.size == 2)) map (x => (x(0), x(1)))
    val nodStrs = es.flatten.toSet.toList
    term(nodStrs, edgTuples)

  }
    
   def fromStringLabel(gStr: String) = {
    val es = (for (r <- grphLabelPat2.findAllIn(gStr) if !r.isEmpty) yield r).map {
      case grphEdgeLabelPat(n1, n2,v) => List(n1, n2,v)
      case singleNodePat(n1) => List(n1)
    }.toList
    //println("es --> "+es)
    val edgTuples = (es filter (_.size == 3)) map (x => (x(0), x(1),x(2)))
    val nodStrs = (Set[String]() /: es)( (acc,x)=> x match { case n1::n2::v::Nil => acc ++ Set(n1,n2)
    														 case n1::Nil => acc + n1
    														 case _ => acc}
      ).toList
      termLabel(nodStrs, edgTuples)
    }

  }   
  
  object Graph extends GraphObjBase {
    type GraphClass[T, U] = Graph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
      val g = new Graph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addEdge(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
      val g = new Graph[T, U]
      for ((v, a) <- nodes) g.addNode(v)
      for ((n1, a) <- nodes; (n2, l) <- a) {
        if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
          g.addEdge(n1, n2, l)
      }
      g
    }
    val grphPat2 = """(\w{1}-\w{1})|(\w{1})""".r
    val singleNodePat = """(\w{1})""".r
    val grphEdgePat = """(\w{1})-(\w{1})""".r
    
    val grphLabelPat2 = """(\w{1}-\w{1}/\w{1})|(\w{1})""".r
    val grphEdgeLabelPat = """(\w{1})-(\w{1})/(\w{1})""".r

  }
  
  object Digraph extends GraphObjBase {
    type GraphClass[T, U] = Digraph[T, U]

    def termLabel[T, U](nodes: List[T], edges: List[(T,T,U)]) = {
      val g = new Digraph[T, U]
      nodes.map(g.addNode)
      edges.map(v => g.addArc(v._1, v._2, v._3))
      g
    }
    def adjacentLabel[T, U](nodes: List[(T, List[(T,U)])]) = {
      val g = new Digraph[T, U]
      for ((n, a) <- nodes) g.addNode(n)
      for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
      g
    }
    
     val grphPat2 = """(\w{1}>\w{1})|(\w{1})""".r
    val singleNodePat = """(\w{1})""".r
    val grphEdgePat = """(\w{1})>(\w{1})""".r
    
    val grphLabelPat2 = """(\w{1}>\w{1}/\w{1})|(\w{1})""".r
    val grphEdgeLabelPat = """(\w{1})>(\w{1})/(\w{1})""".r
  }

  
  object GraphTester extends App{
   
    /*
     * 	scala> Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm
		res0: (List[String], List[(String, String, Unit)]) = (List(d, k, h, c, f, g, b),List((h,g,()), (k,f,()), (f,b,()), (g,h,()), (f,c,()), (b,c,())))

		scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm
		res1: List[(String, List[(String, Int)])] = List((m,List((q,7))), (p,List((m,5), (q,9))), (k,List()), (q,List()))
     */
    println("""Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm\n"""+Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm)
    println("Digraph.fromStringLabel(\"[p>q/9, m>q/7, k, p>m/5]\").toAdjacentForm --> "+Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm)
    
    /*
     * scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q")
		res0: List[List[String]] = List(List(p, q), List(p, m, q))
    
		scala> Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k")
		res1: List[List[String]] = List()
     */
    println("Digraph.fromStringLabel(\"[p>q/9, m>q/7, k, p>m/5]\").findPaths(\"p\", \"q\") --> "+Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q"))
    println("Digraph.fromStringLabel(\"[p>q/9, m>q/7, k, p>m/5]\").findPaths(\"p\", \"k\") --> "+Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k"))
    
    /*
     * scala> Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g] ").findCycles("f")
		res0: List[List[String]] = List(List(f, c, b, f), List(f, b, c, f))
     */
    println("Graph.fromString(\"[b-c, f-c, g-h, d, f-b, k-f, h-g]\").findCycles(\"f\") --> "+Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f]").findCycles("f"))

    /*
     * scala> Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree
		res0: Graph[String,Int] = [a-b/1, b-c/2]
     */
    println("Graph.fromStringLabel(\"[a-b/1, b-c/2, a-c/3]\").minimalSpanningTree --> "+Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree.toTermForm)
    
    println(" MST for the big one !! --> "+Graph.termLabel(
  List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
       List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
            ('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
            ('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1))).toTermForm)
            
     /*
      *       scala> Graph.fromString("[a-b, b-c, a-c]").spanningTrees
				res0: List[Graph[String,Unit]] = List([a-b, b-c], [a-c, b-c], [a-b, a-c])
      */       
     println("Graph.fromString(\"[a-b, b-c, a-c]\").spanningTrees --> "+Graph.fromString("[a-b, b-c, a-c]").spanningTrees.map(_.toTermForm+"\n"))       
  }

