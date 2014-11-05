object scalaWS3 {

 val dicMap:Map[String,List[String]] = Map("en" -> List("en"),"as" -> List("as") , "my" -> List("my"), "amn" -> List("man"), "asy" -> List("say"), "esy" -> List("yes"),
                                           "aens" ->List("sane","sean"), "emn" -> List("men"), "ouy" -> List("you"), "eilov" -> List("olive") )
                                                  //> dicMap  : Map[String,List[String]] = Map(esy -> List(yes), eilov -> List(oli
                                                  //| ve), as -> List(as), asy -> List(say), ouy -> List(you), my -> List(my), en 
                                                  //| -> List(en), emn -> List(men), aens -> List(sane, sean), amn -> List(man))

/*def comb(wrd:String, r:Int):List[String] =
        if(r == 0 ) List("")
        else for( currChar <- wrd.toList; prevWrd = wrd.substring(wrd.indexOf(currChar)+1); combWrd <-comb(prevWrd,r-1)) yield currChar + combWrd*/
        
        def comb(wrd:String, r:Int):List[String] =
         if(r == 0 ) List("")
         else if(r == wrd.length ) List(wrd)
        else comb(wrd.substring(1),r-1).map(x => wrd.charAt(0)+x) ++: comb(wrd.substring(1),r)
                                                  //> comb: (wrd: String, r: Int)List[String]

def comb1(wrd:String, r:Int):List[String] =
        if(r == 0 ) List("")
        else for( currChar <- wrd.toList; prevWrd = wrd.substring(wrd.indexOf(currChar)+1); combWrd <-comb1(prevWrd,r-1); resWrd = {currChar + combWrd}.toList.sortBy( x => x).mkString/*;
         if ( dicMap.get(resWrd)  != None ||   (resWrd.length == 1))*/  ) yield resWrd//dicMap.getOrElse(resWrd,List(resWrd)).head
                                                  //> comb1: (wrd: String, r: Int)List[String]

         comb("bc",1)                             //> res0: List[String] = List(b, c)
        
        comb("abc",2)                             //> res1: List[String] = List(ab, ac, bc)
        
        comb("abc",0)                             //> res2: List[String] = List("")
        comb("yesman",3) /*map (wrd => wrd.toList.sortBy( x => x).mkString) filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) )*/
                                                  //> res3: List[String] = List(yes, yem, yea, yen, ysm, ysa, ysn, yma, ymn, yan,
                                                  //|  esm, esa, esn, ema, emn, ean, sma, smn, san, man)
        comb("aemnsy",3)                          //> res4: List[String] = List(aem, aen, aes, aey, amn, ams, amy, ans, any, asy,
                                                  //|  emn, ems, emy, ens, eny, esy, mns, mny, msy, nsy)
        
        //comb("yesman",4)
        comb1("yesman",2)                         //> res5: List[String] = List(ey, sy, my, ay, ny, es, em, ae, en, ms, as, ns, a
                                                  //| m, mn, an)
        
         comb1("yesman",3)                        //> res6: List[String] = List(esy, emy, aey, eny, msy, asy, nsy, amy, mny, any,
                                                  //|  ems, aes, ens, aem, emn, aen, ams, mns, ans, amn)
         
         //comb1("aemnsy",3)
         comb("yesman",1)                         //> res7: List[String] = List(y, e, s, m, a, n)
         comb("yesman",2) map (wrd => wrd.toList.sortBy( x => x).mkString) filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) )
                                                  //> res8: List[String] = List(my, en, as)
         
         comb("aemnsy",3)  filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) ) map dicMap
                                                  //> res9: List[List[String]] = List(List(man), List(say), List(men), List(yes))
                                                  //| 
          comb("aemnsy",2)  filter (resWrd =>  dicMap.contains(resWrd) || (resWrd.length == 1) ) map dicMap
                                                  //> res10: List[List[String]] = List(List(as), List(en), List(my))
         comb("aemnsy",4)  filter (resWrd =>  dicMap.contains(resWrd)  ) map dicMap  flatten
                                                  //> res11: List[String] = List(sane, sean)
         
          comb("aemnsy",1)  filter (resWrd =>  dicMap.contains(resWrd) ) map dicMap  flatten
                                                  //> res12: List[String] = List()
   
   def allComb(sentence:List[String]):List[String]={
   
     def flatCombForR(wrd:String,r:Int) = comb(wrd,r)  filter (resWrd =>  dicMap.contains(resWrd)  ) map dicMap  flatten
    
    def flatComboForAWord(word:String) = for( currR <- 1 to word.length) yield flatCombForR(word,currR)
     
     flatComboForAWord(sentence.mkString.sortBy(x=> x).mkString).toList.flatten
   
   }                                              //> allComb: (sentence: List[String])List[String]
   
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
   }                                              //> sentencePerm: (wrds: Set[String])List[List[String]]
   allComb(List("yes","man"))    toSet            //> res13: scala.collection.immutable.Set[String] = Set(yes, as, men, man, my, 
                                                  //| en, sean, say, sane)
    
    sentencePerm(allComb(List("yes","man")).toSet)//> res14: List[List[String]] = List()
   
    allComb(List("i","love","you")) toSet         //> res15: scala.collection.immutable.Set[String] = Set(you, olive)
         
         //comb1("yesman",4)
  
      (dicMap.contains( "emn")   || "emn".length == 1)
                                                  //> res16: Boolean = true
      
       dicMap get "esy" head                      //> res17: List[String] = List(yes)
      
       dicMap.contains("ema".toList.sortBy( x=> x).mkString)
                                                  //> res18: Boolean = false
      
      if ( "esy".length == 1 ||  dicMap.contains("esy") ) "esy"
                                                  //> res19: Any = esy
       
        // dicMap("ema".toList.sortBy( x=> x).mkString).head
        
      val wrdSet = Set("yes", "as", "men", "man", "my",  "en", "sean", "say", "sane")
                                                  //> wrdSet  : scala.collection.immutable.Set[java.lang.String] = Set(yes, as, m
                                                  //| en, man, my, en, sean, say, sane)
      
      val currSolnLst = List("sean")              //> currSolnLst  : List[java.lang.String] = List(sean)
               
        wrdSet.mkString.toList.foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSolnLst.mkString.contains(y)) x else x :+ y ).mkString
                                                  //> res20: String = ymmmyy
                                                  
      def canAddWord(wrdToAdd:String, currSoln:List[String]):Boolean= !wrdSet.mkString.toList.foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSoln.mkString.contains(y)) x else x :+ y )
      .mkString.contains(wrdToAdd)                //> canAddWord: (wrdToAdd: String, currSoln: List[String])Boolean
        
        
        def canAddWord1(diffLst:List[Char])(wrdToAdd:String):Boolean = (wrdToAdd toList) forall ( x=> diffLst contains x)
                                                  //> canAddWord1: (diffLst: List[Char])(wrdToAdd: String)Boolean
      
       canAddWord1(List('a','e','m','n','s','y').foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSolnLst.mkString.contains(y)) x else x :+ y ))("my")
                                                  //> res21: Boolean = true
      
      canAddWord1(List('a','e','m','n','s','y').foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(currSolnLst.mkString.contains(y)) x else x :+ y ))("as")
                                                  //> res22: Boolean = false
      
         canAddWord1(List('a','e','m','n','s','y').foldLeft[List[Char]](List())((x:List[Char],y:Char)=> if(List().mkString.contains(y)) x else x :+ y ))("as")
                                                  //> res23: Boolean = true
        
        canAddWord("my",List("sean"))             //> res24: Boolean = false
  
  for(i <- 1 to "sean".length; cmbs <- comb("sean",i)) yield cmbs
                                                  //> res25: scala.collection.immutable.IndexedSeq[String] = Vector(s, e, a, n, s
                                                  //| e, sa, sn, ea, en, an, sea, sen, san, ean, sean)
  
   for(i <- 1 to "aesn".length; cmbs <- comb("aesn",i)) yield cmbs
                                                  //> res26: scala.collection.immutable.IndexedSeq[String] = Vector(a, e, s, n, a
                                                  //| e, as, an, es, en, sn, aes, aen, asn, esn, aesn)
}