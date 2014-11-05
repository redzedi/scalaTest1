object scalaWS2 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(61); 
  println("Welcome to the Scala worksheet");$skip(35); 
  
  val aLst  = List('a','b','b');System.out.println("""aLst  : List[Char] = """ + $show(aLst ));$skip(27); val res$0 = 
  
  aLst.map( x => (x,1));System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(754); 
  
   
/*  val initialValue:List[(Char,Int)] = List()
  aLst.foldLeft[List[(Char,Int)]](initialValue)((runningList, currElem)=>{
  println(currElem)
  println(runningList)
   if(runningList.isEmpty)
       (currElem,1) :: initialValue
       else if(!runningList.find( elem => elem._1 == currElem).isEmpty)
        runningList.map( runningLstPair => if(runningLstPair._1 == currElem)
                                               (runningLstPair._1,runningLstPair._2+1)
                                            else
                                              runningLstPair)
           else
             (currElem,1) :: runningList
                                              
  }) */
   
  val unOrderedLst = List(('t', 2), ('e', 1), ('x', 3));System.out.println("""unOrderedLst  : List[(Char, Int)] = """ + $show(unOrderedLst ));$skip(34); val res$1 = 
    unOrderedLst.sortBy(x=> x._2);System.out.println("""res1: List[(Char, Int)] = """ + $show(res$1));$skip(87); val res$2 = 
    
   ( "banana" groupBy (x=>x) map (v=>(v._1,v._2.length)) toList) sortBy (x=>x._1);System.out.println("""res2: List[(Char, Int)] = """ + $show(res$2));$skip(135); 
  def wordOccurrences(w: String): List[(Char,Int)] =
   ((for((k,v) <- w groupBy (x=>x) ) yield (k,v.length)) toList )sortBy (x=>x._1);System.out.println("""wordOccurrences: (w: String)List[(Char, Int)]""");$skip(282); val res$3 = 
   
    List("banana","bunny").foldLeft[List[(Char,Int)]](List())((listSoFar,currElem)=>
    (  (   listSoFar ++ wordOccurrences(currElem)).foldLeft[Map[Char,Int]](Map())(
    (x:Map[Char,Int],y:(Char,Int))=> x updated (y._1, y._2 + x.getOrElse(y._1,0) )).toList sortBy (x=>x._1)));System.out.println("""res3: List[(Char, Int)] = """ + $show(res$3));$skip(284); val res$4 = 
    
     List("bunny","banana").foldLeft[List[(Char,Int)]](List())((listSoFar,currElem)=>
    (  (   listSoFar ++ wordOccurrences(currElem)).foldLeft[Map[Char,Int]](Map())(
    (x:Map[Char,Int],y:(Char,Int))=> x updated (y._1, y._2 + x.getOrElse(y._1,0) )).toList sortBy (x=>x._1)));System.out.println("""res4: List[(Char, Int)] = """ + $show(res$4));$skip(33); 
    
    val l1 = List(1,4,7,10);System.out.println("""l1  : List[Int] = """ + $show(l1 ));$skip(25); 
    val l2 = List(2,3,5);System.out.println("""l2  : List[Int] = """ + $show(l2 ));$skip(94); val res$5 = 
    
    //TODO: merge of 2 sorted list using for
    for(x <- l1; y <- l2 ; if x< y) yield x;System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(50); val res$6 = 
    
    l1 zipAll (l2,Int.MinValue,Int.MaxValue);System.out.println("""res6: List[(Int, Int)] = """ + $show(res$6));$skip(106); 
    //map { case (p1,p2)=> if(p1<p2) (p1,p2) else (p2,p1)}
    
    val ocLsts = List(('a', 2), ('b', 2));System.out.println("""ocLsts  : List[(Char, Int)] = """ + $show(ocLsts ));$skip(364); val res$7 = 
    
   (ocLsts.foldLeft[List[List[(Char,Int)]]](List(List()))((x:List[List[(Char,Int)]],y:(Char,Int))=> x ++ (for(k <- 1 to y._2 ) yield List((y._1,k)))) ) ++:     ( ocLsts.foldLeft[List[(Char,Int)]](List())((x:List[(Char,Int)],y:(Char,Int))=> x ++ (for(k <- 1 to y._2 ) yield(y._1,k))) combinations 2 withFilter (x=> !(x forall (z=> z._1 == x.head._1))) toList);System.out.println("""res7: List[List[(Char, Int)]] = """ + $show(res$7));$skip(50); 
   
   
   val ocLsts1 = List(('a', 2), ('c', 1));System.out.println("""ocLsts1  : List[(Char, Int)] = """ + $show(ocLsts1 ));$skip(186); val res$8 = 
   
   ocLsts1.foldLeft[Map[Char,Int]](ocLsts.toMap)((xMap:Map[Char,Int],yElem:(Char,Int)) => xMap.updated(yElem._1,  (xMap getOrElse (yElem._1,0))-yElem._2 )).filter(k=> k._2>0) toList;System.out.println("""res8: List[(Char, Int)] = """ + $show(res$8));$skip(281); 
                                                  
  /* def permutations(str:String):List[String] = {
       if(str.isEmpty()) List("") else
          
   
   }*/
 // "aemnsy".map(x=> (x,1)).permutations.toList
  val dic = Set("as","my","en","man","men","sean","say","yes","sane");System.out.println("""dic  : scala.collection.immutable.Set[java.lang.String] = """ + $show(dic ));$skip(367); 
   
  /* for{ i <- "aemnsy".permutations
        currWrd = i
        res:List[List[String]] = List()
         j <- 1 to currWrd.length
         if}*/
         
         val dicMap:Map[String,List[String]] = Map("en" -> List("en"),"as" -> List("as") , "my" -> List("my"), "amn" -> List("man"), "asy" -> List("say"), "esy" -> List("yes"), "aens" ->List("sane","sean"));System.out.println("""dicMap  : Map[String,List[String]] = """ + $show(dicMap ));$skip(1060); 
         
         
       /*  def queens(n: Int): List[List[(Int, Int)]] = {
							def placeQueens(k: Int): List[List[(Int, Int)]] =
							if (k == 0)
							List(List())
							else
							for {
							queens <placeQueens(k 1)
                     column <1 to n
                     queen = (k, column)
                    if isSafe(queen, queens)
                   } yield queen :: queens
                   placeQueens(n)
                } */
                
                
         /*   def wrdPerm(wrd:String): List[List[String]] ={
              
              if(wrd.isEmpty)
                 List(List())
               else
                 for{
                   currSol <- wrdPerm(wrd.substring(0,	wrd.length -2))
                   
                 }
            
            } */
            
        
        
        def comb(wrd:String, r:Int):List[String] =
        if(r == 0 ) List("")
        else for( currChar <- wrd.toList; prevWrd = wrd.substring(wrd.indexOf(currChar)+1); combWrd <-comb(prevWrd,r-1)) yield currChar + combWrd;System.out.println("""comb: (wrd: String, r: Int)List[String]""");$skip(40); val res$9 = 
        
        
        comb("abc",2);System.out.println("""res9: List[String] = """ + $show(res$9));$skip(337); 
         
         def perm2(wrd:String,lst:List[List[String]]):List[List[String]] = {
            if(wrd.isEmpty)
              lst
            else if(dicMap contains wrd)
               lst :+ dicMap(wrd)
            else
               for(i <- wrd.toList; j <- perm2(wrd.toList.remove(x=> x == i).mkString,lst))  yield j
         };System.out.println("""perm2: (wrd: String, lst: List[List[String]])List[List[String]]""");$skip(325); 
         
         def perm1(wrds:String):List[String] = {
           if(wrds.isEmpty)
              List("")
            else{
           // println(wrds)
              for(i <- wrds.toList; j <- perm1(wrds.toList.remove(x=> x == i).mkString))  yield (i :: j.toList).mkString
            }
            
         
         };System.out.println("""perm1: (wrds: String)List[String]""");$skip(248); 
         
         def perm(wrds:List[String]):List[List[String]] = {
           if(wrds.isEmpty)
              List(List[String]())
            else
              for(i <- wrds;j <- perm(wrds.remove(x=> x == i))) yield i :: j
         
         };System.out.println("""perm: (wrds: List[String])List[List[String]]""");$skip(43); val res$10 = 
         
         perm(List("yes","man"));System.out.println("""res10: List[List[String]] = """ + $show(res$10));$skip(40); val res$11 = 
         
         perm1("aemnsy") size;System.out.println("""res11: Int = """ + $show(res$11));$skip(42); val res$12 = 
         
         perm2("aemnsy",List());System.out.println("""res12: List[List[String]] = """ + $show(res$12));$skip(41); val res$13 = 
         
          perm2("aens",List());System.out.println("""res13: List[List[String]] = """ + $show(res$13));$skip(41); val res$14 = 
   
       ( "yesman" permutations) size;System.out.println("""res14: Int = """ + $show(res$14))}
}