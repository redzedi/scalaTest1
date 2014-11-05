package binarytree

sealed abstract class Tree[+T] {
  def isMirrorOf[U >: T](other: Tree[U]): Boolean
  def isSymmetric: Boolean = true
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  def nodeCount: Int
  def leafCount:Int
  def leafList:List[T]
  def internalList:List[T]
  
  def atLevel(lvl:Int):List[T]
  
  def layoutBinaryTree:PositionedNode[T]
  
  def layoutBinaryTree2:PositionedNode[T]
  
  def layoutBinaryTree3:PositionedNode[T]
  
  def height:Int
  
  def stringRep:String
  
  override final def toString = stringRep
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  //override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  
  /*val stringRep = (left,right) match {
    case (End,End) => s"$value"
    case (l:Node[T],End) => s"$value($l,)"
    case (End,r:Node[T]) => s"$value(,$r)"
    case (l:Node[T],r:Node[T]) => s"$value($l,$r)"
    case _ => throw new IllegalArgumentException()
  }*/

  val stringRep = {
    val chldRep = (left stringRep) +","+ (right stringRep)
    if(chldRep == ",") s"$value" else s"$value($chldRep)" 
  }
  
  def isMirrorOf[U >: T](other: Tree[U]) = other match {
    case End => false
    case Node(otherVal, otherL, otherR) => (left isMirrorOf otherL) && (right isMirrorOf otherR)
  }

  override def isSymmetric: Boolean = left isMirrorOf right

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
    if (x < value)
      Node(value, left addValue x, right)
    else
      Node(value, left, right addValue x)

  def nodeCount: Int = left.nodeCount + right.nodeCount +1
 
  def leafCount:Int = (left,right) match {
      case (End,End) => 1
      case _ => left.leafCount + right.leafCount
    }
  
  def leafList:List[T] = (left,right) match {
      case (End,End) => List(value)
      case _ => left.leafList ++ right.leafList
    }
    
    def internalList:List[T] = (left,right) match {
      case (End,End) => List()
      case (End,_) => List(value) ++ right.internalList
      case (_,End) => left.internalList ++ List(value)
      case (Node(_,_,_),Node(_,_,_)) => left.internalList ++ List(value) ++ right.internalList
    }
    
    def atLevel(lvl:Int):List[T] = lvl match {
      case 1 => List(value)
      case _ => left.atLevel(lvl - 1) ++ right.atLevel(lvl - 1)
    }
    
    def layoutBinaryTree:PositionedNode[T] = {
      
      def cPositionNode(nd:Node[T],lvl:Int, inOrderPos:Int):(PositionedNode[T],Int) = nd match {
        case Node(_,End,End) => (PositionedNode(nd, End,End, inOrderPos , lvl),inOrderPos)
        case Node(_,lt:Node[T],End) => {
          val leftPNode = cPositionNode(lt, lvl+1 ,inOrderPos)
          (PositionedNode(nd, leftPNode._1 ,End, leftPNode._2+1 , lvl),leftPNode._2+1)
        }
        case Node(_,End,rt:Node[T]) => {
          val rightPNode = cPositionNode(rt, lvl+1 ,inOrderPos+1)
          (PositionedNode(nd, End, rightPNode._1, inOrderPos , lvl), rightPNode._2)
        }
        case Node(_,lt:Node[T],rt:Node[T]) => {
        val leftPNode = cPositionNode(lt, lvl + 1, inOrderPos)
        val currX = leftPNode._2 + 1
        val rightPNode = cPositionNode(rt, lvl + 1, currX+1)
        (PositionedNode(nd, leftPNode._1, rightPNode._1, currX, lvl), rightPNode._2)
          
        }
      }
      cPositionNode(this,1,1)._1
    }
    
    def layoutBinaryTree2:PositionedNode[T] = {
      val treeHeight = this.height
      
      def cPositionNode(nd:Node[T],lvl:Int, inOrderPos:Int):(PositionedNode[T],Int) = nd match {
        
        case Node(_,End,End) => (PositionedNode(nd, End,End, if(inOrderPos < 0) 1 else inOrderPos , lvl),if(inOrderPos < 0) 1 else inOrderPos)
        
        case Node(_,lt:Node[T],End) => {
        	val hDist = Math.pow(2, treeHeight - lvl-1).toInt 
          val leftPNode = cPositionNode(lt, lvl+1 ,inOrderPos-hDist)
          val currX = if(inOrderPos < 0 ) leftPNode._2 + hDist else inOrderPos
          (PositionedNode(nd, leftPNode._1 ,End, currX , lvl),currX)
        }
        
        case Node(_,End,rt:Node[T]) => {
          val hDist = Math.pow(2, treeHeight - lvl-1).toInt 
          val currX = if(inOrderPos < 0 ) 1 else inOrderPos
          val rightPNode = cPositionNode(rt, lvl+1 ,currX+hDist)
          (PositionedNode(nd, End, rightPNode._1, currX , lvl), currX)
        }
        
        case Node(_,lt:Node[T],rt:Node[T]) => {
        val hDist = Math.pow(2, treeHeight - lvl-1).toInt  
        val leftPNode = cPositionNode(lt, lvl + 1, inOrderPos-hDist)
        val currX = if(inOrderPos < 0 ) leftPNode._2 + hDist else inOrderPos
        val rightPNode = cPositionNode(rt, lvl + 1, currX+hDist)
        (PositionedNode(nd, leftPNode._1, rightPNode._1, currX, lvl), currX)
          
        }
      }
      
      //TODO: remove treeHeight and try thunking it
      cPositionNode(this,1,-1)._1
    }
    
     def layoutBinaryTree3:PositionedNode[T] = {
      
      def cPositionNode(nd:Node[T],lvl:Int, xOfRootOfSiblingTree:Int,htOfSiblingTree:Int,currLvlOfSiblingTree:Int):PositionedNode[T] = nd match {
        
        case Node(_,End,End) => {
          val leftMostPos1 = if(currLvlOfSiblingTree < htOfSiblingTree)
                               (xOfRootOfSiblingTree + Math.pow(2, currLvlOfSiblingTree-1).toInt)+2
                               else if(xOfRootOfSiblingTree > 0 ){println("gotcha !! "+(currLvlOfSiblingTree -htOfSiblingTree)+" -- "+xOfRootOfSiblingTree); xOfRootOfSiblingTree + (currLvlOfSiblingTree -htOfSiblingTree) +1} else 1  
          PositionedNode(nd, End,End, leftMostPos1 , lvl)
        }
        
        case Node(_,End,rt:Node[T]) => {
           val leftMostPos1 = if(currLvlOfSiblingTree < htOfSiblingTree)
                               xOfRootOfSiblingTree + Math.pow(2, currLvlOfSiblingTree-1).toInt+2
                               else if(xOfRootOfSiblingTree > 0 ) xOfRootOfSiblingTree + (currLvlOfSiblingTree -htOfSiblingTree) +1 else 1 
                               
          val rightPNode = cPositionNode(rt, lvl+1 ,if(xOfRootOfSiblingTree < 0) leftMostPos1 else xOfRootOfSiblingTree,htOfSiblingTree, if(currLvlOfSiblingTree < 0) currLvlOfSiblingTree else currLvlOfSiblingTree+1)
          PositionedNode(nd, End, rightPNode, leftMostPos1 , lvl)
        }
        
        
        case Node(_,lt:Node[T],End) => {
          val leftPNode = cPositionNode(lt, lvl+1 ,xOfRootOfSiblingTree,htOfSiblingTree,currLvlOfSiblingTree+1)
          PositionedNode(nd, leftPNode ,End, leftPNode.x +1 , lvl)
        }
        
        
        
        case Node(_,lt:Node[T],rt:Node[T]) => {
        val leftPNode = cPositionNode(lt, lvl + 1, xOfRootOfSiblingTree,htOfSiblingTree,if(currLvlOfSiblingTree < 0) currLvlOfSiblingTree else currLvlOfSiblingTree+1)
        val rightPNode = cPositionNode(rt, lvl + 1,  leftPNode .x,leftPNode.height,0)
        PositionedNode(nd, leftPNode, rightPNode, (leftPNode.x + rightPNode.x )/2, lvl)
          
        }
      }
      
      //TODO: remove treeHeight and try thunking it
      cPositionNode(this,1,-1,-1,-1)
    }

    val height:Int = (if(left.height > right.height) left.height else right.height)+1
    
}

case object End extends Tree[Nothing] {
 // override def toString = "."
  val stringRep = ""

  def isMirrorOf[U >: Nothing](other: Tree[U]) = other match {
    case End => true
    case _ => false
  }

  def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] = Node(x)

  def nodeCount: Int = 0
  
  def leafCount: Int = 0
  
  def leafList:List[Nothing] = List()
  
  def internalList:List[Nothing] = List()
  
  def atLevel(lvl:Int):List[Nothing] = List()
  
  def layoutBinaryTree:PositionedNode[Nothing] = null
  
  def layoutBinaryTree2:PositionedNode[Nothing] = null
  
  def layoutBinaryTree3:PositionedNode[Nothing] = null
  
  val height:Int = 0
  
  val leftMostHDist:Int = 0 
  
  val rightMostHDist:Int = 0
  
}

case class PositionedNode[+T](private val nd:Node[T], val left:Tree[T] , val right:Tree[T], val x: Int, y: Int) extends Tree[T] {
    def value = nd.value
   // def left = nd.left 
   // def right = nd.right
    val stringRep = s"[$x,$y]$nd"
	//override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
	
	def isMirrorOf[U >: Nothing](other: Tree[U]) = other match {
    case End => true
    case _ => false
  }

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = nd.addValue(x)

  def nodeCount: Int = nd.nodeCount
  
  def leafCount: Int = nd.leafCount
  
  def leafList:List[T] = nd.leafList
  
  def internalList:List[T] = nd.internalList
  
  def atLevel(lvl:Int):List[T] = nd.atLevel(lvl)
  
  def layoutBinaryTree:PositionedNode[T] = nd.layoutBinaryTree
  
  def layoutBinaryTree2:PositionedNode[T] = nd.layoutBinaryTree2
  
  def layoutBinaryTree3:PositionedNode[T] = nd.layoutBinaryTree3
  
  val height:Int = nd.height
  
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}



object Tree {
  final def fromList[U <% Ordered[U]](xs: List[U]): Tree[U] = xs.foldLeft(End: Tree[U])((acc: Tree[U], x: U) => acc.addValue(x))

  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }

   def symmetricBalancedTrees[T](n:Int, v:T):List[Tree[T]] = cBalanced(n,v) filter ( t=> ! ( t isSymmetric))
   
    def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
    case n if n < 1 => List(End)
    case 1          => List(Node(value))
    case _ => {
      val fullHeight = hbalTrees(height - 1, value)
      val short = hbalTrees(height - 2, value)
      fullHeight.flatMap((l) => fullHeight.map((r) => Node(value, l, r))) :::
      fullHeight.flatMap((f) => short.flatMap((s) => List(Node(value, f, s), Node(value, s, f))))
    }
  } 

   def minHbalNodes(height:Int):Int = height match{
     case h if h < 1 => 0
     case 1 => 1
     case _ => minHbalNodes(height-2) + minHbalNodes(height-1) + 1
   }
   
   def maxHbalHeight(nodes:Int):Int = (Stream.from(1, 1) takeWhile(minHbalNodes(_) <= nodes) ).last
   
   def minHbalHeight(nodes:Int):Int =
     				if (nodes == 0)
     				  0
     				else
     				  minHbalHeight(nodes / 2) + 1
   
   //def hbalTreesWithNodes(nodes:Int, lbl:String) = hbalTrees(maxHbalHeight(nodes),lbl)
    def hbalTreesWithNodes(nodes:Int, lbl:String) =(minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, lbl)).filter(_.nodeCount == nodes).toList
    
    def completeBinaryTree[T](numOfNodes:Int, v:T):Tree[T] = {
     				  def cBTree(nodeNum:Int):Tree[T]= if(nodeNum > numOfNodes) 
     					  							End
     					  							else
     					  							  Node(v,cBTree(2*nodeNum),cBTree(2*nodeNum + 1))
     				cBTree(1)	  							  
     				}

  def splitExp(str: String): (String, String) = str match {
    case x: String if (x.indexOf(",") == 0) => (str take 0, str drop 1)
    case x: String if (x.indexOf(",") == 1) => (str take 1, str drop 2)
    case _ => {
      val tmp1 = (str map { case '(' => 1; case ')' => -1; case _ => 0 }).scanLeft(0)(_ + _)
      val spanTmp1 = tmp1 span (_ == 0)
      val idx = spanTmp1._1.length + (spanTmp1._2 takeWhile (_ > 0)).length
      //str.splitAt(idx)
      (str take idx, str drop (idx + 1))
    }
  } 
   val leafPat = """(\w)""".r
   val nodePat = """(\w)[(](\S+)[)]""".r 
   val endPat = """""".r
   
   def fromString(treeStr:String):Tree[Char]= treeStr match {
     //"a(b(d,e),c(,f(g,)))" 
     case nodePat(value,left) =>{ val splits = splitExp(left); Node(value.charAt(0),fromString(splits._1),fromString(splits._2 ))}
     case leafPat(value) =>Node(value.charAt(0))
     case endPat(_*) => End
   }
}


object TreeTester extends App {

  //true
  println("binary tree isSymmetric --> " + Node('a', Node('b'), Node('c')).isSymmetric)

  //false
  println("binary tree isSymmetric --> " + Node('a', Node('b'), Node('c', End, Node('d'))).isSymmetric)

  //false
  println("binary tree isSymmetric --> " + Node('a', Node('c', End, Node('d')), Node('c', End, Node('d'))).isSymmetric)
  /*scala> Tree.cBalanced(4, "x")
res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...*/

  /*
     * scala> Tree.fromList(List(3, 2, 5, 7, 1))
		res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
     */
  println(" BST fromList --> " + Tree.fromList(List(3, 2, 5, 7, 1)))

  /*
     * scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
				res4: Boolean = true
				
				scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
				res5: Boolean = false
     */
  println(" BST fromList isSymmetric --> " + Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
  println(" BST fromList isSymmetric --> " + Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric)
  
  /*
   * scala> Tree.symmetricBalancedTrees(5, "x")
			res0: List[Node[String]] = List(T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .))))
   */
  println("Tree.symmetricBalancedTrees(5, \"x\") "+Tree.symmetricBalancedTrees(5, "x"))
  
  /*
   * scala> Tree.hbalTrees(3, "x")
		res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...
   */
  
  //copied sol :)
  
  /*
   * scala> minHbalNodes(3)
			res0: Int = 4
   */
  
  println("minHbalNodes(3) --> "+Tree.minHbalNodes(3))
  
  /*
   * scala> maxHbalHeight(4)
		res1: Int = 3
   */
  println("maxHbalHeight(4) == 3--> "+Tree.maxHbalHeight(4))
  
  /*
   * scala> Tree.hbalTreesWithNodes(4, "x")
		res2: List[Node[String]] = List(T(x T(x T(x . .) .) T(x . .)), T(x T(x . T(x . .)) T(x . .)), ...
   */
  println("Tree.hbalTreesWithNodes(4, \"x\") --> "+Tree.hbalTreesWithNodes(4, "x"))
  
  println("Tree.hbalTreesWithNodes(15, \"x\") --> "+Tree.hbalTreesWithNodes(15, "x").size)
  
  /*
   * scala> Node('x', Node('x'), End).leafCount
     		res0: Int = 1
   */
  
  println(" leafCount --> "+Node('x', Node('x'), End).leafCount)
  
  /*
   * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList
		res0: List[Char] = List(b, d, e)
   */
  println("leafList --> "+Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList)
  
  /*
   * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList
		res0: List[Char] = List(a, c)
   */
  println("internalList --> "+Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)
  
  /*
   * scala> Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)
			res0: List[Char] = List(b, c)
   */
  println("atLevel --> "+Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))
  
  /*
   * scala> Tree.completeBinaryTree(6, "x")
		res0: Node[String] = T(x T(x T(x . .) T(x . .)) T(x T(x . .) .))
   */
  
  println("complete binary tree --> "+Tree.completeBinaryTree(6, "x"))
  
  /*
   * scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
		res0: PositionedNode[Char] = T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))
   */
  println("layoutBinaryTree -->"+Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree)
  
  println("layoutBinaryTree (example on page) --> "+Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q')).layoutBinaryTree)
  
  println("height of binary tree -->"+Node('a', Node('b', End, Node('c')), Node('d')).height)
  
  /*
   * scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2
		res0: PositionedNode[Char] = T[3,1]('a T[1,2]('b . T[2,3]('c . .)) T[5,2]('d . .))
   */
  
  println("layoutBinaryTree2 --> "+Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2)
  println("layoutBinaryTree2 (example on page) --> "+Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q')).layoutBinaryTree2)
  
  /*
   * scala> Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree3
		res0: PositionedNode[Char] = T[2,1]('a T[1,2]('b . T[2,3]('c . .)) T[3,2]('d . .))
   */
  println("layoutBinaryTree3 --> "+Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree3)
  println("layoutBinaryTree3 (example on page) --> "+Tree.fromList(List('n','k','m','c','a','e','d','g','u','p','q')).layoutBinaryTree3)
  
  /*
   * scala> Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString
			res0: String = a(b(d,e),c(,f(g,)))
   */
  println("string representaion of a binary tree[Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End)))] --> "+
      Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString)
      
  /*
   *    scala> Tree.fromString("a(b(d,e),c(,f(g,)))")
			res1: Node[Char] = a(b(d,e),c(,f(g,)))
   */    
      
   println("Tree.fromString(\"a(b(d,e),c(,f(g,)))\") --> "+Tree.fromString("a(b(d,e),c(,f(g,)))"))
   
   println("Tree.fromString(\"a\") --> "+Tree.fromString("a"))
      
}

