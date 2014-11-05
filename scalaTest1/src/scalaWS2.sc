object scalaWS2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val aLst  = List('a','b','b')                   //> aLst  : List[Char] = List(a, b, b)
  
  aLst.map( x => (x,1))                           //> res0: List[(Char, Int)] = List((a,1), (b,1), (b,1))
  
   
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
   
  val unOrderedLst = List(('t', 2), ('e', 1), ('x', 3))
                                                  //> unOrderedLst  : List[(Char, Int)] = List((t,2), (e,1), (x,3))
    unOrderedLst.sortBy(x=> x._2)                 //> res1: List[(Char, Int)] = List((e,1), (t,2), (x,3))
    
   ( "banana" groupBy (x=>x) map (v=>(v._1,v._2.length)) toList) sortBy (x=>x._1)
                                                  //> res2: List[(Char, Int)] = List((a,3), (b,1), (n,2))
  def wordOccurrences(w: String): List[(Char,Int)] =
   ((for((k,v) <- w groupBy (x=>x) ) yield (k,v.length)) toList )sortBy (x=>x._1)
                                                  //> wordOccurrences: (w: String)List[(Char, Int)]
   
    List("banana","bunny").foldLeft[List[(Char,Int)]](List())((listSoFar,currElem)=>
    (  (   listSoFar ++ wordOccurrences(currElem)).foldLeft[Map[Char,Int]](Map())(
    (x:Map[Char,Int],y:(Char,Int))=> x updated (y._1, y._2 + x.getOrElse(y._1,0) )).toList sortBy (x=>x._1)))
                                                  //> res3: List[(Char, Int)] = List((a,3), (b,2), (n,4), (u,1), (y,1))
    
     List("bunny","banana").foldLeft[List[(Char,Int)]](List())((listSoFar,currElem)=>
    (  (   listSoFar ++ wordOccurrences(currElem)).foldLeft[Map[Char,Int]](Map())(
    (x:Map[Char,Int],y:(Char,Int))=> x updated (y._1, y._2 + x.getOrElse(y._1,0) )).toList sortBy (x=>x._1)))
                                                  //> res4: List[(Char, Int)] = List((a,3), (b,2), (n,4), (u,1), (y,1))
    
    val l1 = List(1,4,7,10)                       //> l1  : List[Int] = List(1, 4, 7, 10)
    val l2 = List(2,3,5)                          //> l2  : List[Int] = List(2, 3, 5)
    
    //TODO: merge of 2 sorted list using for
    for(x <- l1; y <- l2 ; if x< y) yield x       //> res5: List[Int] = List(1, 1, 1, 4)
    
    l1 zipAll (l2,Int.MinValue,Int.MaxValue)      //> res6: List[(Int, Int)] = List((1,2), (4,3), (7,5), (10,2147483647))
    //map { case (p1,p2)=> if(p1<p2) (p1,p2) else (p2,p1)}
    
    val ocLsts = List(('a', 2), ('b', 2))         //> ocLsts  : List[(Char, Int)] = List((a,2), (b,2))
    
   (ocLsts.foldLeft[List[List[(Char,Int)]]](List(List()))((x:List[List[(Char,Int)]],y:(Char,Int))=> x ++ (for(k <- 1 to y._2 ) yield List((y._1,k)))) ) ++:     ( ocLsts.foldLeft[List[(Char,Int)]](List())((x:List[(Char,Int)],y:(Char,Int))=> x ++ (for(k <- 1 to y._2 ) yield(y._1,k))) combinations 2 withFilter (x=> !(x forall (z=> z._1 == x.head._1))) toList)
                                                  //> res7: List[List[(Char, Int)]] = List(List(), List((a,1)), List((a,2)), List
                                                  //| ((b,1)), List((b,2)), List((a,1), (b,1)), List((a,1), (b,2)), List((a,2), (
                                                  //| b,1)), List((a,2), (b,2)))
   
   
   val ocLsts1 = List(('a', 2), ('c', 1))         //> ocLsts1  : List[(Char, Int)] = List((a,2), (c,1))
   
   ocLsts1.foldLeft[Map[Char,Int]](ocLsts.toMap)((xMap:Map[Char,Int],yElem:(Char,Int)) => xMap.updated(yElem._1,  (xMap getOrElse (yElem._1,0))-yElem._2 )).filter(k=> k._2>0) toList
                                                  //> res8: List[(Char, Int)] = List((b,2))
                                                  
  /* def permutations(str:String):List[String] = {
       if(str.isEmpty()) List("") else
          
   
   }*/
 // "aemnsy".map(x=> (x,1)).permutations.toList
  val dic = Set("as","my","en","man","men","sean","say","yes","sane")
                                                  //> dic  : scala.collection.immutable.Set[java.lang.String] = Set(yes, as, men,
                                                  //|  man, my, en, sean, say, sane)
   
  /* for{ i <- "aemnsy".permutations
        currWrd = i
        res:List[List[String]] = List()
         j <- 1 to currWrd.length
         if}*/
         
         val dicMap:Map[String,List[String]] = Map("en" -> List("en"),"as" -> List("as") , "my" -> List("my"), "amn" -> List("man"), "asy" -> List("say"), "esy" -> List("yes"), "aens" ->List("sane","sean"))
                                                  //> dicMap  : Map[String,List[String]] = Map(esy -> List(yes), as -> List(as), 
                                                  //| asy -> List(say), my -> List(my), en -> List(en), aens -> List(sane, sean),
                                                  //|  amn -> List(man))
         
         
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
        else for( currChar <- wrd.toList; prevWrd = wrd.substring(wrd.indexOf(currChar)+1); combWrd <-comb(prevWrd,r-1)) yield currChar + combWrd
                                                  //> comb: (wrd: String, r: Int)List[String]
        
        
        comb("abc",2)                             //> res9: List[String] = List(ab, ac, bc)
         
         def perm2(wrd:String,lst:List[List[String]]):List[List[String]] = {
            if(wrd.isEmpty)
              lst
            else if(dicMap contains wrd)
               lst :+ dicMap(wrd)
            else
               for(i <- wrd.toList; j <- perm2(wrd.toList.remove(x=> x == i).mkString,lst))  yield j
         }                                        //> perm2: (wrd: String, lst: List[List[String]])List[List[String]]
         
         def perm1(wrds:String):List[String] = {
           if(wrds.isEmpty)
              List("")
            else{
           // println(wrds)
              for(i <- wrds.toList; j <- perm1(wrds.toList.remove(x=> x == i).mkString))  yield (i :: j.toList).mkString
            }
            
         
         }                                        //> perm1: (wrds: String)List[String]
         
         def perm(wrds:List[String]):List[List[String]] = {
           if(wrds.isEmpty)
              List(List[String]())
            else
              for(i <- wrds;j <- perm(wrds.remove(x=> x == i))) yield i :: j
         
         }                                        //> perm: (wrds: List[String])List[List[String]]
         
         perm(List("yes","man"))                  //> res10: List[List[String]] = List(List(yes, man), List(man, yes))
         
         perm1("aemnsy") size                     //> res11: Int = 720
         
         perm2("aemnsy",List())                   //> res12: List[List[String]] = List(List(my), List(my), List(yes), List(en), L
                                                  //| ist(en), List(my), List(yes), List(my), List(my), List(en), List(my), List(
                                                  //| en), List(en), List(en), List(my), List(my), List(say), List(as), List(my),
                                                  //|  List(say), List(my), List(as), List(my), List(my), List(man), List(as), Li
                                                  //| st(as), List(man), List(yes), List(en), List(en), List(say), List(as), List
                                                  //| (yes), List(say), List(as), List(en), List(en), List(sane, sean), List(my),
                                                  //|  List(yes), List(my), List(my), List(say), List(my), List(as), List(yes), L
                                                  //| ist(say), List(as), List(my), List(my), List(as), List(as), List(my), List(
                                                  //| en), List(my), List(en), List(my), List(my), List(man), List(en), List(en),
                                                  //|  List(my), List(my), List(en), List(man), List(en), List(en), List(en), Lis
                                                  //| t(as), List(as), List(man), List(sane, sean), List(as), List(as), List(en),
                                                  //|  List(man), List(en))
         
          perm2("aens",List())                    //> res13: List[List[String]] = List(List(sane, sean))
   
       ( "yesman" permutations) size              //> res14: Int = 720
}