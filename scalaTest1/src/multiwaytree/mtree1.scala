package multiwaytree 

import scala.collection.immutable.StringLike

  case class MTree[+T](value: T, children: List[MTree[T]]) {
   // def this(value: T) = this(value, List())
    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
    
    val nodeCount:Int = (1 /: children)((acc,x)=>acc+x.nodeCount)
    
    val internalPathLength:Int = (0 /: (children map (1+2*_.internalPathLength)))(_+_)
    
    val postorder:List[T] =  ( children :\ List[T]())((x,acc) => x.postorder  ::: acc) :+ value 
    
    val lispyTree:String = if(children isEmpty) value.toString else s"($value "+(children map (_.lispyTree)).mkString(" ")+") "
  }

  object MTree {
    def apply[T](value: T):MTree[T] = MTree(value, List())
   // def apply[T](value: T, children: List[MTree[T]]):MTree[T] = MTree(value, children)
    
    //wrong !!
    def string2MTree1(mTreeStr:String):List[MTree[String]] = if(mTreeStr.isEmpty()) List() else mTreeStr.charAt(0) match {
      case '^' => List[MTree[String]]() ::: (if(mTreeStr.isDefinedAt(0))  string2MTree1(mTreeStr.substring(1)) else List())
      case  v => List(MTree[String](v.toString,string2MTree1(mTreeStr.substring(1))))
    }
    
    def string2MTree2(mTreeStr:List[Char]):(List[MTree[String]],List[Char]) = mTreeStr match {
      case '^'::tail => (List[MTree[String]](),tail) 
      case  v :: tail  => (List(MTree[String](v.toString,string2MTree2(tail)._1  )),string2MTree2(tail)._2)
    }
    
    def string2MTree3(mTreeStr:List[Char]) = 
      mTreeStr.foldLeft[Tuple2[List[Tuple2[Int,Char]],List[Tuple2[Int,MTree[Char]]]]](((List[(Int,Char)](),List[(Int,MTree[Char])]())))((acc,x)=> x match {
        case '^' => if(acc._2.isEmpty || acc._2 .head._1-1 != acc._1 .head._1 ) (acc._1.tail, (acc._1.head._1 ,MTree(acc._1.head._2 )) :: acc._2 )
                     else (acc._1.tail, (acc._1.head._1,MTree(acc._1.head._2, (acc._2.takeWhile(_._1-1 == acc._1.head._1)) map (_._2))) :: acc._2 dropWhile (_._1-1 == acc._1.head._1))   
                     case _ => if(acc._1.isEmpty) ((1,x )::acc._1, acc._2 ) else ((acc._1.head._1+1,x )::acc._1, acc._2 )  
                     
      })._2.head._2
      
        implicit def string2MTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    MTree(s(0), splitChildStrings(1).map(string2MTree(_)))
  }
    
   def lispyTreeString2MTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int ={
      
    
    //  println("nextStrBound --. "+s+" nesting "+nesting+" pos--> "+pos+" s(pos) --> "+s(pos))
      if (nesting == 0 ) pos
      else nextStrBound(pos + 1, if (s(pos) == ')') (if(nesting == 0) 0 else nesting - 1) else if(s(pos) == '(') nesting + 1 else nesting)
    }
    def splitChildStrings(pos: Int): List[String] =  
      if (pos >= s.length) Nil
      else {
        val end = if(s(pos) == '(')nextStrBound(pos+1 , 1) else pos+1
        //println("s --> "+s)
        println("substring --> "+s.substring(pos, end ))
        if(s.substring(pos, end ) == ")")
        		splitChildStrings(end)
          else
        s.substring(pos, end ) :: splitChildStrings(end)
      }
    if(s.startsWith("(")){
      val chld = splitChildStrings(2).map(lispyTreeString2MTree(_))
      println("chld --> "+splitChildStrings(2))
    		  MTree(s(1), chld)
    }
     else{println("s-->"+s);MTree(s.charAt(0))}  
  }
      /*mTreeStr match {
      case '^'::tail => (List[MTree[String]](),tail) 
      case  v :: tail  => (List(MTree[String](v.toString,string2MTree2(tail)._1  )),string2MTree2(tail)._2)
    }*/
    
    /*   def string2MTree(mTreeStr:String):List[MTree[Char]] = mTreeStr.substring(1).foldLeft(List[MTree[Char]]())(op)
      case  v+:tail => List(MTree[Char](v,string2MTree(tail)))
    }*/
  }

  object MTree_Tester extends App {
    //import multiwaytree._
    /*
     * scala> MTree('a', List(MTree('f'))).nodeCount
				res0: Int = 2
     */
    import MTree._
    println(" MTree('a', List(MTree('f'))).nodeCount --> "+MTree('a', List(MTree('f'))).nodeCount)
    println("string2MTree1(\"afg^^c^bd^e^^^\") --> "+"afg^^c^bd^e^^^")
    println("string2MTree1(\"afg^^c^bd^e^^^\") --> "+MTree.string2MTree2("afg^^c^bd^e^^^".toList)._1(0))
    println("string2MTree3(\"afg^^c^bd^e^^^\") --> "+MTree.string2MTree3("afg^^c^bd^e^^".toList))
    println("string2MTree3(\"afg^^c^bd^e^^^\") --> "+MTree.string2MTree3("ac^bd^e^^^".toList))
    println("string2MTree3(\"afg^^c^bd^e^^^\") --> "+MTree.string2MTree3("afg^^bd^e^^^".toList))
    println("string2MTree3(\"afg^^c^bd^e^^^\") --> "+MTree.string2MTree3("afg^^c^^".toList))
    println("string2MTree1(\"afg^^c^bd^e^^^\") --> "+MTree.string2MTree3("afg^^^".toList))
    println("string2MTree1(\"afg^^c^bd^e^^^\") --> "+MTree.string2MTree3("g^".toList))
    
    println("\"afg^^c^bd^e^^^\".internalPathLength --> "+"afg^^c^bd^e^^".internalPathLength  )
    
    /*
     * scala> "afg^^c^bd^e^^^".postorder
			res0: List[Char] = List(g, f, c, d, e, b, a)
     */
    println("\"afg^^c^bd^e^^\".postorder --> "+"afg^^c^bd^e^^".postorder)
    
    /*
     scala> MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree
		res0: String = (a (b c))
     */
    
    println("MTree(\"a\", List(MTree(\"b\", List(MTree(\"c\"))))).lispyTree --> "+MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree)
    
    println("\"afg^^c^bd^e^^\".lispyTree --> "+"afg^^c^bd^e^^".lispyTree )
    
    println(MTree.lispyTreeString2MTree("(a(fg)c(bde))").lispyTree )
  }