object scalaWS3 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(331); 

 val dicMap:Map[String,List[String]] = Map("en" -> List("en"),"as" -> List("as") , "my" -> List("my"), "amn" -> List("man"), "asy" -> List("say"), "esy" -> List("yes"),
                                           "aens" ->List("sane","sean"), "emn" -> List("men"), "ouy" -> List("you"), "eilov" -> List("olive") );System.out.println("""dicMap  : Map[String,List[String]] = """ + $show(dicMap ));$skip(453); 

/*def comb(wrd:String, r:Int):List[String] =
        if(r == 0 ) List("")
        else for( currChar <- wrd.toList; prevWrd = wrd.substring(wrd.indexOf(currChar)+1); combWrd <-comb(prevWrd,r-1)) yield currChar + combWrd*/
        
        def comb(wrd:String, r:Int):List[String] =
         if(r == 0 ) List("")
         else if(r == wrd.length ) List(wrd)
        else comb(wrd.substring(1),r-1).map(x => wrd.charAt(0)+x) ++: comb(wrd.substring(1),r);System.out.println("""comb: (wrd: String, r: Int)List[String]""");$skip(392); 

def comb1(wrd:String, r:Int):List[String] =
        if(r == 0 ) List("")
        else for( currChar <- wrd.toList; prevWrd = wrd.substring(wrd.indexOf(currChar)+1); combWrd <-comb1(prevWrd,r-1); resWrd = {currChar + combWrd}.toList.sortBy( x => x).mkString/*;
         if ( dicMap.get(resWrd)  != None ||   (resWrd.length == 1))*/  ) yield resWrd;System.out.println("""comb1: (wrd: String, r: Int)List[String]""");$skip(24); val res$0 = //dicMap.getOrElse(resWrd,List(resWrd)).head

         comb("bc",1);System.out.println("""res0: List[String] = """ + $show(res$0));$skip(31); val res$1 = 
        
        comb("abc",2);System.out.println("""res1: List[String] = """ + $show(res$1));$skip(31); val res$2 = 
        
        comb("abc",0);System.out.println("""res2: List[String] = """ + $show(res$2));$skip(147); val res$3 = 
        comb("yesman",3);System.out.println("""res3: List[String] = """ + $show(res$3));$skip(25); val res$4 =  /*map (wrd => wrd.toList.sortBy( x => x).mkString) filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) )*/
        comb("aemnsy",3);System.out.println("""res4: List[String] = """ + $show(res$4));$skip(62); val res$5 = 
        
        //comb("yesman",4)
        comb1("yesman",2);System.out.println("""res5: List[String] = """ + $show(res$5));$skip(36); val res$6 = 
        
         comb1("yesman",3);System.out.println("""res6: List[String] = """ + $show(res$6));$skip(65); val res$7 = 
         
         //comb1("aemnsy",3)
         comb("yesman",1);System.out.println("""res7: List[String] = """ + $show(res$7));$skip(144); val res$8 = 
         comb("yesman",2) map (wrd => wrd.toList.sortBy( x => x).mkString) filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) );System.out.println("""res8: List[String] = """ + $show(res$8));$skip(117); val res$9 = 
         
         comb("aemnsy",3)  filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) ) map dicMap;System.out.println("""res9: List[List[String]] = """ + $show(res$9));$skip(108); val res$10 = 
          comb("aemnsy",2)  filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) ) map dicMap;System.out.println("""res10: List[List[String]] = """ + $show(res$10));$skip(93); val res$11 = 
         comb("aemnsy",4)  filter (resWrd =>  dicMap.contains(resWrd)  ) map dicMap  flatten;System.out.println("""res11: List[String] = """ + $show(res$11));$skip(103); val res$12 = 
         
          comb("aemnsy",1)  filter (resWrd =>  dicMap.contains(resWrd) ) map dicMap  flatten;System.out.println("""res12: List[String] = """ + $show(res$12));$skip(386); 
   
   def allComb(sentence:List[String]):List[String]={
   
     def flatCombForR(wrd:String,r:Int) = comb(wrd,r)  filter (resWrd =>  dicMap.contains(resWrd)  ) map dicMap  flatten
    
    def flatComboForAWord(word:String) = for( currR <- 1 to word.length) yield flatCombForR(word,currR)
     
     flatComboForAWord(sentence.mkString.sortBy(x=> x).mkString).toList.flatten
   
   };System.out.println("""allComb: (sentence: List[String])List[String]""");$skip(1107); 
   
   def sentencePerm(wrds:Set[String]):List[List[String]] ={
   
   //def canAddWord(wrdToAdd:String, currSoln:List[String]):Boolean= !wrds.mkString.toList.foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSoln.mkString.contains(y)) x else x :+ y ).mkString.contains(wrdToAdd)
  
   def canAddWord1(diffLst:List[Char])(wrdToAdd:String):Boolean = (wrdToAdd toList) forall ( x=> diffLst contains x)
   //def canAddWord(wrdToAdd:String, currSoln:List[String]):Boolean= false
 
     def sentencePermInner(currWrds:Set[String]):List[List[String]]=
     if(currWrds.isEmpty)
        List(List())
     else{
        for{
           solns <- sentencePermInner(currWrds.tail)
           wrdToAdd <- wrds
           if canAddWord1(wrds.mkString.toList.foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(solns.mkString.contains(y)) x else x :+ y ))(wrdToAdd)
          // if canAddWord1(List('a','e','m','n','s','y').foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(solns.mkString.contains(y)) x else x :+ y ))(wrdToAdd)
        } yield  wrdToAdd :: solns
   }
   sentencePermInner(wrds)
   };System.out.println("""sentencePerm: (wrds: Set[String])List[List[String]]""");$skip(39); val res$13 = 
   allComb(List("yes","man"))    toSet;System.out.println("""res13: scala.collection.immutable.Set[String] = """ + $show(res$13));$skip(56); val res$14 = 
    
    sentencePerm(allComb(List("yes","man")).toSet);System.out.println("""res14: List[List[String]] = """ + $show(res$14));$skip(46); val res$15 = 
   
    allComb(List("i","love","you")) toSet;System.out.println("""res15: scala.collection.immutable.Set[String] = """ + $show(res$15));$skip(97); val res$16 = 
         
         //comb1("yesman",4)
  
      (dicMap.contains( "emn")   || "emn".length == 1);System.out.println("""res16: Boolean = """ + $show(res$16));$skip(36); val res$17 = 
      
       dicMap get "esy" head;System.out.println("""res17: List[String] = """ + $show(res$17));$skip(68); val res$18 = 
      
       dicMap.contains("ema".toList.sortBy( x=> x).mkString);System.out.println("""res18: Boolean = """ + $show(res$18));$skip(71); val res$19 = 
      
      if ( "esy".length == 1 ||  dicMap.contains("esy") ) "esy";System.out.println("""res19: Any = """ + $show(res$19));$skip(164); 
       
        // dicMap("ema".toList.sortBy( x=> x).mkString).head
        
      val wrdSet = Set("yes", "as", "men", "man", "my",  "en", "sean", "say", "sane");System.out.println("""wrdSet  : scala.collection.immutable.Set[java.lang.String] = """ + $show(wrdSet ));$skip(44); 
      
      val currSolnLst = List("sean");System.out.println("""currSolnLst  : List[java.lang.String] = """ + $show(currSolnLst ));$skip(162); val res$20 = 
               
        wrdSet.mkString.toList.foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSolnLst.mkString.contains(y)) x else x :+ y ).mkString;System.out.println("""res20: String = """ + $show(res$20));$skip(284); 
                                                  
      def canAddWord(wrdToAdd:String, currSoln:List[String]):Boolean= !wrdSet.mkString.toList.foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSoln.mkString.contains(y)) x else x :+ y )
      .mkString.contains(wrdToAdd);System.out.println("""canAddWord: (wrdToAdd: String, currSoln: List[String])Boolean""");$skip(140); 
        
        
        def canAddWord1(diffLst:List[Char])(wrdToAdd:String):Boolean = (wrdToAdd toList) forall ( x=> diffLst contains x);System.out.println("""canAddWord1: (diffLst: List[Char])(wrdToAdd: String)Boolean""");$skip(169); val res$21 = 
      
       canAddWord1(List('a','e','m','n','s','y').foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSolnLst.mkString.contains(y)) x else x :+ y ))("my");System.out.println("""res21: Boolean = """ + $show(res$21));$skip(168); val res$22 = 
      
      canAddWord1(List('a','e','m','n','s','y').foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSolnLst.mkString.contains(y)) x else x :+ y ))("as");System.out.println("""res22: Boolean = """ + $show(res$22));$skip(166); val res$23 = 
      
         canAddWord1(List('a','e','m','n','s','y').foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(List().mkString.contains(y)) x else x :+ y ))("as");System.out.println("""res23: Boolean = """ + $show(res$23));$skip(47); val res$24 = 
        
        canAddWord("my",List("sean"));System.out.println("""res24: Boolean = """ + $show(res$24));$skip(69); val res$25 = 
  
  for(i <- 1 to "sean".length; cmbs <- comb("sean",i)) yield cmbs;System.out.println("""res25: scala.collection.immutable.IndexedSeq[String] = """ + $show(res$25));$skip(70); val res$26 = 
  
   for(i <- 1 to "aesn".length; cmbs <- comb("aesn",i)) yield cmbs;System.out.println("""res26: scala.collection.immutable.IndexedSeq[String] = """ + $show(res$26))}
}