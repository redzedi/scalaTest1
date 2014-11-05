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
 
 ("b(d,e),c(,f(g,))" map {case '(' => 1; case ')' => -1;case _ => 0}).scanLeft (0)(_+_)
                                                  //> res10: scala.collection.immutable.IndexedSeq[Int] = Vector(0, 0, 1, 1, 1, 1
                                                  //| , 0, 0, 0, 1, 1, 1, 2, 2, 2, 1, 0)
     //"a(b(d,e),c(,f(g,)))"
}