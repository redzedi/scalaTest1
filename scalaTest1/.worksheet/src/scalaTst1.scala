object scalaTst1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  println("Welcome to the Scala worksheet");$skip(45); val res$0 = 
  
  for(i <- 1 to 10 if i % 2 == 0) yield i;System.out.println("""res0: scala.collection.immutable.IndexedSeq[Int] = """ + $show(res$0));$skip(49); val res$1 = 
  
  List("suman","yeeksie") map(_.substring(1));System.out.println("""res1: List[String] = """ + $show(res$1));$skip(28); 
  
  val l1 = List(1,2,3,4);System.out.println("""l1  : List[Int] = """ + $show(l1 ));$skip(38); val res$2 = 
  
  l1.flatMap(l=> l1.map(r=>(l,r)));System.out.println("""res2: List[(Int, Int)] = """ + $show(res$2));$skip(34); val res$3 = 
  
  l1.map(l=> l1.map(r=>(l,r)));System.out.println("""res3: List[List[(Int, Int)]] = """ + $show(res$3));$skip(46); val res$4 = 
  
  l1.flatMap(l=> l1.flatMap(r=>List(l,r)));System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(30); val res$5 = 
  
  l1.flatMap(l=>List(2*l));System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(41); 
  
  val l11:List[Any] = List(1,"",true);System.out.println("""l11  : List[Any] = """ + $show(l11 ));$skip(25); val res$6 = 

 (0/: List(1,2,3))(_+_);System.out.println("""res6: Int = """ + $show(res$6));$skip(30); 
 
 val leafPat = """(\w)""".r;System.out.println("""leafPat  : scala.util.matching.Regex = """ + $show(leafPat ));$skip(40); 
  val nodePat = """(\w)[(](\S+)[)]""".r;System.out.println("""nodePat  : scala.util.matching.Regex = """ + $show(nodePat ));$skip(25); 
   val endPat = """""".r;System.out.println("""endPat  : scala.util.matching.Regex = """ + $show(endPat ));$skip(465); 
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
 };System.out.println("""splitExp: (str: String)(String, String)""");$skip(22); val res$7 = 
 
 splitExp(",f(g,)");System.out.println("""res7: (String, String) = """ + $show(res$7));$skip(212); val res$8 = 
     "a(b(d,e),c(,f(g,)))" match {
     case leafPat(value) =>s"matched leafPat!! $value"
     case nodePat(v,l) =>s"matched nodePat!! $v,left--> "+splitExp(l)._2
     case endPat(_*) => "matched endPat!! "
   };System.out.println("""res8: String = """ + $show(res$8));$skip(30); val res$9 = 

 """\w""".r findFirstIn("a");System.out.println("""res9: Option[String] = """ + $show(res$9));$skip(90); val res$10 = 
 
 ("b(d,e),c(,f(g,))" map {case '(' => 1; case ')' => -1;case _ => 0}).scanLeft (0)(_+_);System.out.println("""res10: scala.collection.immutable.IndexedSeq[Int] = """ + $show(res$10))}
     //"a(b(d,e),c(,f(g,)))"
}
