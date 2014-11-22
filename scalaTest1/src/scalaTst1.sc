object scalaTst1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  for(i <- 1 to 10 if i % 2 == 0) yield i         //> res0: scala.collection.immutable.IndexedSeq[Int] = Vector(2, 4, 6, 8, 10)
  
  List("suman","yeeksie") map(_.substring(1))     //> res1: List[String] = List(uman, eeksie)
  
  val l1 = List(1,2,3,4)                          //> l1  : List[Int] = List(1, 2, 3, 4)
  
  l1.flatMap(l=> l1.map(r=>(l,r)))                //> res2: List[(Int, Int)] = List((1,1), (1,2), (1,3), (1,4), (2,1), (2,2), (2,3
                                                  //| ), (2,4), (3,1), (3,2), (3,3), (3,4), (4,1), (4,2), (4,3), (4,4))
  
  l1.map(l=> l1.map(r=>(l,r)))                    //> res3: List[List[(Int, Int)]] = List(List((1,1), (1,2), (1,3), (1,4)), List((
                                                  //| 2,1), (2,2), (2,3), (2,4)), List((3,1), (3,2), (3,3), (3,4)), List((4,1), (4
                                                  //| ,2), (4,3), (4,4)))
  
  l1.flatMap(l=> l1.flatMap(r=>List(l,r)))        //> res4: List[Int] = List(1, 1, 1, 2, 1, 3, 1, 4, 2, 1, 2, 2, 2, 3, 2, 4, 3, 1,
                                                  //|  3, 2, 3, 3, 3, 4, 4, 1, 4, 2, 4, 3, 4, 4)
  
  l1.flatMap(l=>List(2*l))                        //> res5: List[Int] = List(2, 4, 6, 8)
  
  val l11:List[Any] = List(1,"",true)             //> l11  : List[Any] = List(1, "", true)

 (0/: List(1,2,3))(_+_)                           //> res6: Int = 6
 
 val leafPat = """(\w)""".r                       //> leafPat  : scala.util.matching.Regex = (\w)
  val nodePat = """(\w)[(](\S+)[)]""".r           //> nodePat  : scala.util.matching.Regex = (\w)[(](\S+)[)]
   val endPat = """""".r                          //> endPat  : scala.util.matching.Regex = 
     //"a(b(d,e),c(,f(g,)))"
     //
 def splitExp(str:String):(String,String) = str match{
 case x:String if(x.indexOf(",") == 0) =>(str take 0, str drop 1)
 case x:String if(x.indexOf(",") == 1) =>(str take 1, str drop 2)
 case _ => {
 val tmp1 = (str map {case '(' => 1; case ')' => -1;case _ => 0}).scanLeft (0)(_+_)
 val spanTmp1 = tmp1 span (_==0)
 val idx = spanTmp1._1.length + ( spanTmp1._2 takeWhile (_>0)).length
 (str take idx, str drop (idx +1))
 }
 }                                                //> splitExp: (str: String)(String, String)
 
 splitExp(",f(g,)")                               //> res7: (String, String) = ("",f(g,))
     "a(b(d,e),c(,f(g,)))" match {
     case leafPat(value) =>s"matched leafPat!! $value"
     case nodePat(v,l) =>s"matched nodePat!! $v,left--> "+splitExp(l)._2
     case endPat(_*) => "matched endPat!! "
   }                                              //> res8: String = matched nodePat!! a,left--> c(,f(g,))

 """\w""".r findFirstIn("a")                      //> res9: Option[String] = Some(a)

val grphPat1 = """\[(\w{1}-\w{1}[,])*|(\w{1}-\w{1})*\]""".r
                                                  //> grphPat1  : scala.util.matching.Regex = \[(\w{1}-\w{1}[,])*|(\w{1}-\w{1})*\
                                                  //| ]
val grphPat2 = """(\w{1}-\w{1})|(\w{1})""".r      //> grphPat2  : scala.util.matching.Regex = (\w{1}-\w{1})|(\w{1})
val grphPat3 = """(\w{1}>\w{1}/\w{1})|(\w{1})""".r//> grphPat3  : scala.util.matching.Regex = (\w{1}>\w{1}/\w{1})|(\w{1})
val singleNodePat = """(\w{1})""".r               //> singleNodePat  : scala.util.matching.Regex = (\w{1})
 val grphEdgeLabelPat = """(\w{1})>(\w{1})/(\w{1})""".r
                                                  //> grphEdgeLabelPat  : scala.util.matching.Regex = (\w{1})>(\w{1})/(\w{1})

(for(r <- grphPat2.findAllIn("[a-b,b-c,d]") if !r.isEmpty)yield r ).flatMap((a:String)=> a.split("-").toList).toList
                                                  //> res10: List[String] = List(a, b, b, c, d)
(for(r <- grphPat2.findAllIn("[a-b,b-c,d]"))yield r ).map(_.toString).toList
                                                  //> res11: List[String] = List(a-b, b-c, d)

(for(r <- grphPat3.findAllIn("[a>b/4,b>c/5,d]"))yield r ).map {
      case grphEdgeLabelPat(n1, n2,v) => List(n1, n2,v)
      case singleNodePat(n1) => List(n1)
    }.toList                                      //> res12: List[List[String]] = List(List(a, b, 4), List(b, c, 5), List(d))
 "[a-b,b-c]" match {
  case grphPat1(value) => println(value)
  case _ => println( "de")
 }                                                //> de
 ("b(d,e),c(,f(g,))" map {case '(' => 1; case ')' => -1;case _ => 0}).scanLeft (0)(_+_)
                                                  //> res13: scala.collection.immutable.IndexedSeq[Int] = Vector(0, 0, 1, 1, 1, 1
                                                  //| , 0, 0, 0, 1, 1, 1, 2, 2, 2, 1, 0)
     //"a(b(d,e),c(,f(g,)))"
     
     List('d,'a,'s) indexWhere (_ == 'a)          //> res14: Int = 1
     
     List('d,'a,'s) take 0                        //> res15: List[Symbol] = List()
     
     class Outer{
       class Inner
     }
     
     val o1 = new Outer                           //> o1  : scalaTst1.Outer = scalaTst1$$anonfun$main$1$Outer$1@5282140
     val i1 = new o1.Inner                        //> i1  : scalaTst1.o1.Inner = scalaTst1$$anonfun$main$1$Outer$1$Inner@1e00ae86
                                                  //| 
    import scala.reflect.runtime.{universe => ru}
    
   // val m = ru.runtimeMirror(i1.getClass.getClassLoader)
 
  //m.typeOf
    //val shippingTermSymb = ru.typeOf[Purchase].declaration(ru.newTermName("shipped")).asTerm
   val s = "(a(fg)c(bde))"                        //> s  : String = (a(fg)c(bde))
   def nextStrBound(pos: Int, nesting: Int): Int ={
      
    
     // println("nextStrBound --. "+s+" nesting "+nesting+" pos--> "+pos+" s(pos) --> "+s(pos))
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == ')') nesting - 1 else if(s(pos) == '(') nesting + 1 else nesting)
    }                                             //> nextStrBound: (pos: Int, nesting: Int)Int
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos , 0)+1
        println("s --> "+s)
        println("substring "+s.substring(pos, end ))
        s.substring(pos, end ) :: splitChildStrings(end)
      }                                           //> splitChildStrings: (pos: Int)List[String]
      
//      splitChildStrings(2)

object test{
//these are methods
 def foo() = "this is the normal method"
 def foo(n:Int) = "this is the overloaded method "+n
}

 def recursiveFibonacci(n:Int):Int = n match {
   case 0 => 1
   case 1 => 1
   case _ => recursiveFibonacci(n-1) + recursiveFibonacci(n-2)
 }                                                //> recursiveFibonacci: (n: Int)Int
 
 recursiveFibonacci(11)                           //> res16: Int = 144
 
 import scala.actors._
 
  object evenActor extends Actor{
  var startNum = 2
   def act()={
     receive {
       case "your_turn" => startNum += 2;println(startNum);startNum
     }
   }
  
  }
 // evenActor.start()
 
 def f1(n:Int,ff:Int=>Int):Int = {
    println(n)
    ff(n+1)
 }                                                //> f1: (n: Int, ff: Int => Int)Int
 
 def f2(n:Int,ff:Int=>Int):Int = {
    println(n)
    ff(n+2)
 }                                                //> f2: (n: Int, ff: Int => Int)Int
 
 //val ff1 = f1(1)()()
// f1.
}