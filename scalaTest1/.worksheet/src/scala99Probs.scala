object scala99Probs {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(65); 
  println("Welcome to the Scala worksheet");$skip(173); 
  
  //http://aperiodic.net/phil/scala/s-99/
  //prob #1
  def last(intLst:List[Any]):Any =
    intLst match {
     case List(x) => x
     case _ => last(intLst.tail)
    };System.out.println("""last: (intLst: List[Any])Any""");$skip(32); val res$0 = 
    
    last(List(1,3,5)) == 5;System.out.println("""res0: Boolean = """ + $show(res$0));$skip(58); val res$1 = 
    
    last(List("suman","yashu","mamoni")) == "mamoni";System.out.println("""res1: Boolean = """ + $show(res$1));$skip(154); 
    
    //prob #2
  def penultimate(intLst:List[Any]):Any =
    intLst match {
     case List(x1,x2) => x1
     case _ => penultimate(intLst.tail)
    };System.out.println("""penultimate: (intLst: List[Any])Any""");$skip(39); val res$2 = 
    
    penultimate(List(1,3,5)) == 3;System.out.println("""res2: Boolean = """ + $show(res$2));$skip(64); val res$3 = 
    
    penultimate(List("suman","yashu","mamoni")) == "yashu";System.out.println("""res3: Boolean = """ + $show(res$3));$skip(120); 
    
    //prob #3
    def nth(pos:Int,intLst:List[Any]):Any =
   if(pos == 0) intLst.head else nth(pos-1,intLst.tail);System.out.println("""nth: (pos: Int, intLst: List[Any])Any""");$skip(33); val res$4 = 
    
    nth(1,List(1,3,5)) == 3;System.out.println("""res4: Boolean = """ + $show(res$4));$skip(33); val res$5 = 
    
    nth(2,List(1,3,5)) == 5;System.out.println("""res5: Boolean = """ + $show(res$5));$skip(139); 
    
    //prob #4
    
    def length(xs:List[Any]):Int =
    xs match {
      case hd::tl => 1 + length(tl)
      case List() => 0
    };System.out.println("""length: (xs: List[Any])Int""");$skip(40); val res$6 = 
    
    length(List(1, 1, 2, 3, 5, 8));System.out.println("""res6: Int = """ + $show(res$6));$skip(158); 
    
    //prob #5
    
    def reverse(xs:List[Any]):List[Any] =
     xs match {
       case hd::tl => reverse(tl) :+ hd
       case List() => List()
     };System.out.println("""reverse: (xs: List[Any])List[Any]""");$skip(273); 
     
     def reverse1(xs:List[Any]):List[Any] ={
       def reverse1R(ys:List[Any],acc:List[Any]):List[Any]={
         ys match {
           case Nil => acc
           case hd::tl => reverse1R(tl,hd::acc)
         }
        
       };
        reverse1R(xs,List())
     };System.out.println("""reverse1: (xs: List[Any])List[Any]""");$skip(41); val res$7 = 
    
    reverse(List(1, 1, 2, 3, 5, 8));System.out.println("""res7: List[Any] = """ + $show(res$7));$skip(42); val res$8 = 
    
    reverse1(List(1, 1, 2, 3, 5, 8));System.out.println("""res8: List[Any] = """ + $show(res$8));$skip(69); 
    
    def isPalindrome(xs:List[Any]):Boolean = xs == reverse1(xs);System.out.println("""isPalindrome: (xs: List[Any])Boolean""");$skip(337); 
                                                  
         
     def isPalindrome1(xs:List[Any]):Boolean ={
     def inner(xs2:List[Any]):Boolean = xs2 match {
       case xs1 if xs1.length > 1 =>  xs1.head == xs1.last && inner(xs1.slice(1,xs1.length-2))
       case _ => true
     }
     if(xs.length < 2) false else inner(xs)
     };System.out.println("""isPalindrome1: (xs: List[Any])Boolean""");$skip(37); val res$9 = 
    isPalindrome1(List(1, 2,  2, 1));System.out.println("""res9: Boolean = """ + $show(res$9));$skip(331); 
 
 
 // #7 - flatten(List(List(1, 1), 2, List(3, List(5, 8)))) =>  List[Any] = List(1, 1, 2, 3, 5, 8)
 
 def flatten1(xs:List[Any]):List[Any] = xs.foldLeft( List[Any]())((acc,x1) => x1 match {
  																																		case x2:List[Any] => acc ::: flatten1(x2)
  																																		case _ => acc :+ x1
 });System.out.println("""flatten1: (xs: List[Any])List[Any]""");$skip(61); val res$10 = 
    
    flatten1(List(List(1, 1), 2, List(3, List(5, 8))));System.out.println("""res10: List[Any] = """ + $show(res$10));$skip(354); 
                                                  
                                                  
   // #8 - compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)
   
   def compress(xs:List[Any]):List[Any] = xs.foldLeft(List[Any]())((acc,x1) => if(acc.isEmpty || acc.last != x1)  acc :+ x1 else acc);System.out.println("""compress: (xs: List[Any])List[Any]""");$skip(126); val res$11 = 
                                                   
   compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e));System.out.println("""res11: List[Any] = """ + $show(res$11));$skip(421); 
   // #9 -  pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 
  def pack(xs:List[Any]):List[List[Any]] = xs.foldLeft( List[List[Any]]() :+ List[Any]() )(
  (acc,x1) => if(acc.last.isEmpty || acc.last.head == x1)
     acc.slice(0,acc.length-1) :+ (  acc.last :+ x1 )
       else  acc :+ List[Any](x1) );System.out.println("""pack: (xs: List[Any])List[List[Any]]""");$skip(70); val res$12 = 
   pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e));System.out.println("""res12: List[List[Any]] = """ + $show(res$12));$skip(238); 
   
   // #10 -  encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==  List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   
   def encode(xs:List[Any]):List[Tuple2[Any,Any]] = pack(xs).map( x => (x.length, x.head));System.out.println("""encode: (xs: List[Any])List[(Any, Any)]""");$skip(72); val res$13 = 
   encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e));System.out.println("""res13: List[(Any, Any)] = """ + $show(res$13));$skip(256); 

   // #11 -  encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

   def encodeModified(xs:List[Any]):List[Any] = pack(xs).map( x => if(x.length > 1) (x.length, x.head) else x.head);System.out.println("""encodeModified: (xs: List[Any])List[Any]""");$skip(85); val res$14 = 
        encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e));System.out.println("""res14: List[Any] = """ + $show(res$14));$skip(327); 
                                                  
   // #12 - decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   
   def decode(xs:List[Tuple2[Int,Any]]):List[Any] = xs.foldLeft(List[Any]())((acc,x) => acc ++ (for(i <- 1 to x._1)yield x._2) );System.out.println("""decode: (xs: List[(Int, Any)])List[Any]""");$skip(74); val res$15 = 
   
   decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)));System.out.println("""res15: List[Any] = """ + $show(res$15));$skip(454); 
  // for(i <- 1 to 1)yield 'a
  
  // #13 - encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  
  def  encodeDirect(xs:List[Any]):List[Tuple2[Int,Any]] = xs.foldLeft(List(new Tuple2[Int,Any](0,null)))((acc,x) => if(acc.last._2 == x) acc.slice(0,acc.length-1) :+ new Tuple2(acc.last._1+1, x) else if(acc.last._2 == null) List(new Tuple2(1,x)) else acc :+ new Tuple2(1,x) );System.out.println("""encodeDirect: (xs: List[Any])List[(Int, Any)]""");$skip(77); val res$16 = 
  encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e));System.out.println("""res16: List[(Int, Any)] = """ + $show(res$16));$skip(192); 
  // #14 - duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

   def duplicate(xs:List[Any]):List[Any] = xs.foldLeft(List[Any]())((acc,x) => (acc :+ x) :+ x);System.out.println("""duplicate: (xs: List[Any])List[Any]""");$skip(43); val res$17 = 
   
   duplicate(List('a, 'b, 'c, 'c, 'd));System.out.println("""res17: List[Any] = """ + $show(res$17));$skip(245); 
  
  // #15 - duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  
   def duplicateN(n:Int,xs:List[Any]):List[Any] = xs.foldLeft(List[Any]())((acc,x) => acc ++ (for(i <- 1 to n)yield x));System.out.println("""duplicateN: (n: Int, xs: List[Any])List[Any]""");$skip(47); val res$18 = 
   
   duplicateN(3, List('a, 'b, 'c, 'c, 'd));System.out.println("""res18: List[Any] = """ + $show(res$18));$skip(39); 
   def sum1(x:Int,y:Int,z:Int) = x+y+z;System.out.println("""sum1: (x: Int, y: Int, z: Int)Int""");$skip(402); 
   
 //  val add1 = sum _

 // #16 - drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 
  def drop(n:Int,xs:List[Any]):List[Any] = xs.foldLeft(List[Tuple2[Int,Any]]())((acc,x) => if(((acc.length + 1) % n) != 0) acc :+ ((acc.length + 1),x) else acc :+ ((acc.length + 1),null)).map((tpl) => if(tpl._2 != null) tpl._2).filter((x) => !x.isInstanceOf[Unit]);System.out.println("""drop: (n: Int, xs: List[Any])List[Any]""");$skip(61); val res$19 = 
   drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k));System.out.println("""res19: List[Any] = """ + $show(res$19));$skip(88); 
                                                  
                         val kk = ();System.out.println("""kk  : Unit = """ + $show(kk ));$skip(350); 
                         
 // #17 - split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 
     def split(n:Int, xs:List[Any]):Tuple2[List[Any],List[Any]] = xs.foldLeft(Tuple2(List[Any](),List[Any]()))((acc,x) => if(acc._1.length < n) (acc._1 :+ x, acc._2) else (acc._1,acc._2 :+ x) );System.out.println("""split: (n: Int, xs: List[Any])(List[Any], List[Any])""");$skip(65); val res$20 = 
      split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k));System.out.println("""res20: (List[Any], List[Any]) = """ + $show(res$20));$skip(276); 
// #18 - slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g)
   
   def slice(start:Int, end:Int, xs:List[Any]) = xs.foldLeft((0,List[Any]()))((acc,x) => if(acc._1 >= start && acc._1 < end) (acc._1+1,acc._2 :+ x) else (acc._1+1,acc._2 ) )._2;System.out.println("""slice: (start: Int, end: Int, xs: List[Any])List[Any]""");$skip(65); val res$21 = 
   slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k));System.out.println("""res21: List[Any] = """ + $show(res$21));$skip(403); 
  // #19 - rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
 
  def rotate1(n:Int, xs:List[Any]) = ((res:Tuple2[List[Any], List[Any]]) => res._2 ++: res._1)(xs.foldLeft((List[Any](),List[Any]()))( (acc,x) => if( (n > 0 && acc._1.length < n) || (n < 0 && acc._1.length < (xs.length + n))) (acc._1 :+ x, acc._2) else (acc._1 , acc._2 :+ x)));System.out.println("""rotate1: (n: Int, xs: List[Any])List[Any]""");$skip(63); val res$22 = 
  rotate1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k));System.out.println("""res22: List[Any] = """ + $show(res$22));$skip(64); val res$23 = 
  rotate1(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k));System.out.println("""res23: List[Any] = """ + $show(res$23));$skip(381); 
  // #20 - removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b)
  
  def removeAt(n:Int, xs:List[Any]) = xs.foldLeft(List[Tuple2[Int,Any]]((0,null)))((acc,x) => if(acc.last._2 == null && acc.last._1 == 0) List((0,x)) else acc :+ (acc.last._1+1,x) ).foldLeft(new Tuple2[List[Any],Any](List[Any](),null))((acc,x) => if(x._1 == n) (acc._1, x._2) else (acc._1 :+ x._2, acc._2) );System.out.println("""removeAt: (n: Int, xs: List[Any])(List[Any], Any)""");$skip(88); val res$24 = 
                                                  
   removeAt(1, List('a, 'b, 'c, 'd));System.out.println("""res24: (List[Any], Any) = """ + $show(res$24));$skip(264); 
   
  // #21 - insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd)
  
   def insertAt(newX:Any,pos:Int,xs:List[Any]) = xs.foldLeft((0,List[Any]()))((acc,x) => if(acc._1 == pos) (acc._1+1, (acc._2 :+ newX) :+ x) else (acc._1+1, acc._2 :+ x ) )._2;System.out.println("""insertAt: (newX: Any, pos: Int, xs: List[Any])List[Any]""");$skip(43); val res$25 = 
   insertAt('new, 1, List('a, 'b, 'c, 'd));System.out.println("""res25: List[Any] = """ + $show(res$25));$skip(129); 
   
   // #22 -  range(4, 9) == List(4, 5, 6, 7, 8, 9)
   
   def range(frm:Int,to1:Int) = (for(i <- frm to to1) yield i).toList;System.out.println("""range: (frm: Int, to1: Int)List[Int]""");$skip(15); val res$26 = 
   range(4, 9);System.out.println("""res26: List[Int] = """ + $show(res$26));$skip(260); 
  // #23 - randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)) == List('e, 'd, 'a)
   
    def randomSelect(n:Int, xs:List[Any]):List[Any] = if(n > 0){ val p = removeAt((new java.util.Random()).nextInt(xs.length),xs);randomSelect(n-1,p._1) :+ p._2 } else List();System.out.println("""randomSelect: (n: Int, xs: List[Any])List[Any]""");$skip(54); val res$27 = 
    randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h));System.out.println("""res27: List[Any] = """ + $show(res$27));$skip(116); 
  // #24 lotto(6, 49) == List(23, 1, 17, 33, 21, 37)

    def lotto(n:Int,rangeHi:Int) = randomSelect(n,range(1,n));System.out.println("""lotto: (n: Int, rangeHi: Int)List[Any]""");$skip(18); val res$28 = 
     lotto(6, 49);System.out.println("""res28: List[Any] = """ + $show(res$28));$skip(165); 
     
  // #25 -  randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)) == List('b, 'a, 'd, 'c, 'e, 'f)
   
    def randomPermute(xs:List[Any]) = randomSelect(xs.length , xs);System.out.println("""randomPermute: (xs: List[Any])List[Any]""");$skip(48); val res$29 = 
    randomPermute(List('a, 'b, 'c, 'd, 'e, 'f));System.out.println("""res29: List[Any] = """ + $show(res$29));$skip(468); 
    
    // #26 -  combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) == List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
    
    // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    };System.out.println("""flatMapSublists: [A, B](ls: List[A])(f: List[A] => List[B])List[B]""");$skip(184); 

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    };System.out.println("""combinations: [A](n: Int, ls: List[A])List[List[A]]""");$skip(50); val res$30 = 
    combinations(3, List('a, 'b, 'c, 'd, 'e, 'f));System.out.println("""res30: List[List[Symbol]] = """ + $show(res$30));$skip(317); 
  
  // #28 lsort(List(List(a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
   // 				= List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))


   def lsort(xs:List[List[Any]]) :List[List[Any]] = xs;System.out.println("""lsort: (xs: List[List[Any]])List[List[Any]]""");$skip(43); 

   def sum(x:Int,y:Int,z:Int):Int = x+y+z;System.out.println("""sum: (x: Int, y: Int, z: Int)Int""");$skip(36); 

   val add1 = sum( 1, _:Int,_:Int);System.out.println("""add1  : (Int, Int) => Int = """ + $show(add1 ));$skip(32); 
   
   val add3 = add1(2,_:Int);System.out.println("""add3  : Int => Int = """ + $show(add3 ));$skip(29); val res$31 = 
   
   sum(1,2,3) == add3(3);System.out.println("""res31: Boolean = """ + $show(res$31));$skip(31); val res$32 = 
   
   List(1,2,3,4).map(add3);System.out.println("""res32: List[Int] = """ + $show(res$32));$skip(218); val res$33 = 
   
   List(3,3,5,7).foldRight(List[Tuple2[Int,Int]]())((x,acc)=>
                                if(!acc.isEmpty && (acc.head)._1 == x) (x,(acc.head)._2+1) :: acc.tail
                                else (x,1)::acc)
   import scala.collection.immutable.HashMap;System.out.println("""res33: List[(Int, Int)] = """ + $show(res$33));$skip(124); 
               val mp:HashMap[String,String] =    new HashMap[String,String]();System.out.println("""mp  : scala.collection.immutable.HashMap[String,String] = """ + $show(mp ));$skip(33); val res$34 = 
               mp + ("a" -> "d");System.out.println("""res34: scala.collection.immutable.HashMap[String,String] = """ + $show(res$34));$skip(80); val res$35 = 
               
               
       List('a,'a,'b,'c,'d,'a,'c).groupBy(x=>x);System.out.println("""res35: scala.collection.immutable.Map[Symbol,List[Symbol]] = """ + $show(res$35))}
}