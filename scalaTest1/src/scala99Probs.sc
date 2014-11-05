object scala99Probs {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  //http://aperiodic.net/phil/scala/s-99/
  //prob #1
  def last(intLst:List[Any]):Any =
    intLst match {
     case List(x) => x
     case _ => last(intLst.tail)
    }                                             //> last: (intLst: List[Any])Any
    
    last(List(1,3,5)) == 5                        //> res0: Boolean = true
    
    last(List("suman","yashu","mamoni")) == "mamoni"
                                                  //> res1: Boolean = true
    
    //prob #2
  def penultimate(intLst:List[Any]):Any =
    intLst match {
     case List(x1,x2) => x1
     case _ => penultimate(intLst.tail)
    }                                             //> penultimate: (intLst: List[Any])Any
    
    penultimate(List(1,3,5)) == 3                 //> res2: Boolean = true
    
    penultimate(List("suman","yashu","mamoni")) == "yashu"
                                                  //> res3: Boolean = true
    
    //prob #3
    def nth(pos:Int,intLst:List[Any]):Any =
   if(pos == 0) intLst.head else nth(pos-1,intLst.tail)
                                                  //> nth: (pos: Int, intLst: List[Any])Any
    
    nth(1,List(1,3,5)) == 3                       //> res4: Boolean = true
    
    nth(2,List(1,3,5)) == 5                       //> res5: Boolean = true
    
    //prob #4
    
    def length(xs:List[Any]):Int =
    xs match {
      case hd::tl => 1 + length(tl)
      case List() => 0
    }                                             //> length: (xs: List[Any])Int
    
    length(List(1, 1, 2, 3, 5, 8))                //> res6: Int = 6
    
    //prob #5
    
    def reverse(xs:List[Any]):List[Any] =
     xs match {
       case hd::tl => reverse(tl) :+ hd
       case List() => List()
     }                                            //> reverse: (xs: List[Any])List[Any]
     
     def reverse1(xs:List[Any]):List[Any] ={
       def reverse1R(ys:List[Any],acc:List[Any]):List[Any]={
         ys match {
           case Nil => acc
           case hd::tl => reverse1R(tl,hd::acc)
         }
        
       };
        reverse1R(xs,List())
     }                                            //> reverse1: (xs: List[Any])List[Any]
    
    reverse(List(1, 1, 2, 3, 5, 8))               //> res7: List[Any] = List(8, 5, 3, 2, 1, 1)
    
    reverse1(List(1, 1, 2, 3, 5, 8))              //> res8: List[Any] = List(8, 5, 3, 2, 1, 1)
    
    def isPalindrome(xs:List[Any]):Boolean = xs == reverse1(xs)
                                                  //> isPalindrome: (xs: List[Any])Boolean
                                                  
         
     def isPalindrome1(xs:List[Any]):Boolean ={
     def inner(xs2:List[Any]):Boolean = xs2 match {
       case xs1 if xs1.length > 1 =>  xs1.head == xs1.last && inner(xs1.slice(1,xs1.length-2))
       case _ => true
     }
     if(xs.length < 2) false else inner(xs)
     }                                            //> isPalindrome1: (xs: List[Any])Boolean
    isPalindrome1(List(1, 2,  2, 1))              //> res9: Boolean = true
 
 
 // #7 - flatten(List(List(1, 1), 2, List(3, List(5, 8)))) =>  List[Any] = List(1, 1, 2, 3, 5, 8)
 
 def flatten1(xs:List[Any]):List[Any] = xs.foldLeft( List[Any]())((acc,x1) => x1 match {
  																																		case x2:List[Any] => acc ::: flatten1(x2)
  																																		case _ => acc :+ x1
 })                                               //> flatten1: (xs: List[Any])List[Any]
    
    flatten1(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res10: List[Any] = List(1, 1, 2, 3, 5, 8)
                                                  
                                                  
   // #8 - compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)
   
   def compress(xs:List[Any]):List[Any] = xs.foldLeft(List[Any]())((acc,x1) => if(acc.isEmpty || acc.last != x1)  acc :+ x1 else acc)
                                                  //> compress: (xs: List[Any])List[Any]
                                                   
   compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res11: List[Any] = List('a, 'b, 'c, 'a, 'd, 'e)
   // #9 -  pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 
  def pack(xs:List[Any]):List[List[Any]] = xs.foldLeft( List[List[Any]]() :+ List[Any]() )(
  (acc,x1) => if(acc.last.isEmpty || acc.last.head == x1)
     acc.slice(0,acc.length-1) :+ (  acc.last :+ x1 )
       else  acc :+ List[Any](x1) )               //> pack: (xs: List[Any])List[List[Any]]
   pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res12: List[List[Any]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
                                                  //|  List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   
   // #10 -  encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==  List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   
   def encode(xs:List[Any]):List[Tuple2[Any,Any]] = pack(xs).map( x => (x.length, x.head))
                                                  //> encode: (xs: List[Any])List[(Any, Any)]
   encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res13: List[(Any, Any)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'
                                                  //| e))

   // #11 -  encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

   def encodeModified(xs:List[Any]):List[Any] = pack(xs).map( x => if(x.length > 1) (x.length, x.head) else x.head)
                                                  //> encodeModified: (xs: List[Any])List[Any]
        encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res14: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
                                                  
   // #12 - decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   
   def decode(xs:List[Tuple2[Int,Any]]):List[Any] = xs.foldLeft(List[Any]())((acc,x) => acc ++ (for(i <- 1 to x._1)yield x._2) )
                                                  //> decode: (xs: List[(Int, Any)])List[Any]
   
   decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
                                                  //> res15: List[Any] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e,
                                                  //|  'e)
  // for(i <- 1 to 1)yield 'a
  
  // #13 - encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  
  def  encodeDirect(xs:List[Any]):List[Tuple2[Int,Any]] = xs.foldLeft(List(new Tuple2[Int,Any](0,null)))((acc,x) => if(acc.last._2 == x) acc.slice(0,acc.length-1) :+ new Tuple2(acc.last._1+1, x) else if(acc.last._2 == null) List(new Tuple2(1,x)) else acc :+ new Tuple2(1,x) )
                                                  //> encodeDirect: (xs: List[Any])List[(Int, Any)]
  encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res16: List[(Int, Any)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'
                                                  //| e))
  // #14 - duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

   def duplicate(xs:List[Any]):List[Any] = xs.foldLeft(List[Any]())((acc,x) => (acc :+ x) :+ x)
                                                  //> duplicate: (xs: List[Any])List[Any]
   
   duplicate(List('a, 'b, 'c, 'c, 'd))            //> res17: List[Any] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  
  // #15 - duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  
   def duplicateN(n:Int,xs:List[Any]):List[Any] = xs.foldLeft(List[Any]())((acc,x) => acc ++ (for(i <- 1 to n)yield x))
                                                  //> duplicateN: (n: Int, xs: List[Any])List[Any]
   
   duplicateN(3, List('a, 'b, 'c, 'c, 'd))        //> res18: List[Any] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd,
                                                  //|  'd, 'd)
   def sum1(x:Int,y:Int,z:Int) = x+y+z            //> sum1: (x: Int, y: Int, z: Int)Int
   
 //  val add1 = sum _

 // #16 - drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 
  def drop(n:Int,xs:List[Any]):List[Any] = xs.foldLeft(List[Tuple2[Int,Any]]())((acc,x) => if(((acc.length + 1) % n) != 0) acc :+ ((acc.length + 1),x) else acc :+ ((acc.length + 1),null)).map((tpl) => if(tpl._2 != null) tpl._2).filter((x) => !x.isInstanceOf[Unit])
                                                  //> drop: (n: Int, xs: List[Any])List[Any]
   drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res19: List[Any] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
                                                  
                         val kk = ()              //> kk  : Unit = ()
                         
 // #17 - split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 
     def split(n:Int, xs:List[Any]):Tuple2[List[Any],List[Any]] = xs.foldLeft(Tuple2(List[Any](),List[Any]()))((acc,x) => if(acc._1.length < n) (acc._1 :+ x, acc._2) else (acc._1,acc._2 :+ x) )
                                                  //> split: (n: Int, xs: List[Any])(List[Any], List[Any])
      split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res20: (List[Any], List[Any]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 
                                                  //| 'i, 'j, 'k))
// #18 - slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g)
   
   def slice(start:Int, end:Int, xs:List[Any]) = xs.foldLeft((0,List[Any]()))((acc,x) => if(acc._1 >= start && acc._1 < end) (acc._1+1,acc._2 :+ x) else (acc._1+1,acc._2 ) )._2
                                                  //> slice: (start: Int, end: Int, xs: List[Any])List[Any]
   slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res21: List[Any] = List('d, 'e, 'f, 'g)
  // #19 - rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
 
  def rotate1(n:Int, xs:List[Any]) = ((res:Tuple2[List[Any], List[Any]]) => res._2 ++: res._1)(xs.foldLeft((List[Any](),List[Any]()))( (acc,x) => if( (n > 0 && acc._1.length < n) || (n < 0 && acc._1.length < (xs.length + n))) (acc._1 :+ x, acc._2) else (acc._1 , acc._2 :+ x)))
                                                  //> rotate1: (n: Int, xs: List[Any])List[Any]
  rotate1(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res22: List[Any] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  rotate1(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res23: List[Any] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  // #20 - removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b)
  
  def removeAt(n:Int, xs:List[Any]) = xs.foldLeft(List[Tuple2[Int,Any]]((0,null)))((acc,x) => if(acc.last._2 == null && acc.last._1 == 0) List((0,x)) else acc :+ (acc.last._1+1,x) ).foldLeft(new Tuple2[List[Any],Any](List[Any](),null))((acc,x) => if(x._1 == n) (acc._1, x._2) else (acc._1 :+ x._2, acc._2) )
                                                  //> removeAt: (n: Int, xs: List[Any])(List[Any], Any)
                                                  
   removeAt(1, List('a, 'b, 'c, 'd))              //> res24: (List[Any], Any) = (List('a, 'c, 'd),'b)
   
  // #21 - insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd)
  
   def insertAt(newX:Any,pos:Int,xs:List[Any]) = xs.foldLeft((0,List[Any]()))((acc,x) => if(acc._1 == pos) (acc._1+1, (acc._2 :+ newX) :+ x) else (acc._1+1, acc._2 :+ x ) )._2
                                                  //> insertAt: (newX: Any, pos: Int, xs: List[Any])List[Any]
   insertAt('new, 1, List('a, 'b, 'c, 'd))        //> res25: List[Any] = List('a, 'new, 'b, 'c, 'd)
   
   // #22 -  range(4, 9) == List(4, 5, 6, 7, 8, 9)
   
   def range(frm:Int,to1:Int) = (for(i <- frm to to1) yield i).toList
                                                  //> range: (frm: Int, to1: Int)List[Int]
   range(4, 9)                                    //> res26: List[Int] = List(4, 5, 6, 7, 8, 9)
  // #23 - randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)) == List('e, 'd, 'a)
   
    def randomSelect(n:Int, xs:List[Any]):List[Any] = if(n > 0){ val p = removeAt((new java.util.Random()).nextInt(xs.length),xs);randomSelect(n-1,p._1) :+ p._2 } else List()
                                                  //> randomSelect: (n: Int, xs: List[Any])List[Any]
    randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
                                                  //> res27: List[Any] = List('b, 'd, 'f)
  // #24 lotto(6, 49) == List(23, 1, 17, 33, 21, 37)

    def lotto(n:Int,rangeHi:Int) = randomSelect(n,range(1,n))
                                                  //> lotto: (n: Int, rangeHi: Int)List[Any]
     lotto(6, 49)                                 //> res28: List[Any] = List(6, 5, 3, 2, 1, 4)
     
  // #25 -  randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)) == List('b, 'a, 'd, 'c, 'e, 'f)
   
    def randomPermute(xs:List[Any]) = randomSelect(xs.length , xs)
                                                  //> randomPermute: (xs: List[Any])List[Any]
    randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))   //> res29: List[Any] = List('b, 'd, 'f, 'a, 'e, 'c)
    
    // #26 -  combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) == List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
    
    // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }                                             //> flatMapSublists: [A, B](ls: List[A])(f: List[A] => List[B])List[B]

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }                                             //> combinations: [A](n: Int, ls: List[A])List[List[A]]
    combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) //> res30: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('
                                                  //| a, 'b, 'e), List('a, 'b, 'f), List('a, 'c, 'd), List('a, 'c, 'e), List('a, 
                                                  //| 'c, 'f), List('a, 'd, 'e), List('a, 'd, 'f), List('a, 'e, 'f), List('b, 'c,
                                                  //|  'd), List('b, 'c, 'e), List('b, 'c, 'f), List('b, 'd, 'e), List('b, 'd, 'f
                                                  //| ), List('b, 'e, 'f), List('c, 'd, 'e), List('c, 'd, 'f), List('c, 'e, 'f), 
                                                  //| List('d, 'e, 'f))
  
  // #28 lsort(List(List(a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
   // 				= List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))


   def lsort(xs:List[List[Any]]) :List[List[Any]] = xs
                                                  //> lsort: (xs: List[List[Any]])List[List[Any]]

   def sum(x:Int,y:Int,z:Int):Int = x+y+z         //> sum: (x: Int, y: Int, z: Int)Int

   val add1 = sum( 1, _:Int,_:Int)                //> add1  : (Int, Int) => Int = <function2>
   
   val add3 = add1(2,_:Int)                       //> add3  : Int => Int = <function1>
   
   sum(1,2,3) == add3(3)                          //> res31: Boolean = true
   
   List(1,2,3,4).map(add3)                        //> res32: List[Int] = List(4, 5, 6, 7)
   
   List(3,3,5,7).foldRight(List[Tuple2[Int,Int]]())((x,acc)=>
                                if(!acc.isEmpty && (acc.head)._1 == x) (x,(acc.head)._2+1) :: acc.tail
                                else (x,1)::acc)  //> res33: List[(Int, Int)] = List((3,2), (5,1), (7,1))
   import scala.collection.immutable.HashMap
               val mp:HashMap[String,String] =    new HashMap[String,String]()
                                                  //> mp  : scala.collection.immutable.HashMap[String,String] = Map()
               mp + ("a" -> "d")                  //> res34: scala.collection.immutable.HashMap[String,String] = Map(a -> d)
               
               
       List('a,'a,'b,'c,'d,'a,'c).groupBy(x=>x)   //> res35: scala.collection.immutable.Map[Symbol,List[Symbol]] = Map('d -> Lis
                                                  //| t('d), 'b -> List('b), 'c -> List('c, 'c), 'a -> List('a, 'a, 'a))
}