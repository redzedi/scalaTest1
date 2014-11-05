package arithmetic 
  class S99Int(val start: Int) {
    import S99Int._
   
    //P31 (**) Determine whether a given integer number is prime.
    def isPrime:Boolean = ((for( i <- 2 until start if start % i == 0) yield i) toList) isEmpty
    def mod(other:S99Int):S99Int = start % other.start
    //P33 (*) Determine whether two positive integer numbers are coprime.
    def isCoPrimeTo(other:S99Int):Boolean = gcd(this,other) == 1
    //P34 (**) Calculate Euler's totient function phi(m).
    def totient:Int = (for(i <- 1 to start if this.isCoPrimeTo(i)) yield i) length
    
    
    
   def primeFactors:List[Int] = {
      val sqrt:Int = Math.floor( Math.sqrt(this.start) ) toInt
      val primesTillSqrt = sieveOfErastothenes(sqrt)
      def primeCalc(num:Int,currPrimes:List[Int]):List[Int]= {
       if(num == 0 || currPrimes.isEmpty)
        if(num==0) List() else List(num)
        else 
          if(num % currPrimes.head == 0){
       // 	  println("prime factor "+currPrimes.head)
        	  currPrimes.head :: primeCalc(num / currPrimes.head,primesTillSqrt)
          }
           else
             primeCalc(num,currPrimes.tail)
      }
     // println("sqrt "+sqrt)
     // println("primesTillSqrt "+primesTillSqrt)
      primeCalc(this.start,primesTillSqrt)
    }
    
    def primeFactorMultiplicity = primeFactors.foldRight(List[Tuple2[Int,Int]]())((x,acc)=>
                                if(!acc.isEmpty && (acc.head)._1 == x) (x,(acc.head)._2+1) :: acc.tail
                                else (x,1)::acc)  
                                
    def goldbach:Tuple2[Int,Int] = {
      assert(this.start % 2 == 0)
      val primes = sieveOfErastothenes(this.start)
    //  println(" (primes zip primes.reverse) "+(primes zip primes.reverse))
      primes.foldLeft(Map[Int,Int]())((acc:Map[Int,Int],x:Int) =>
        									if(x <= this.start/2) (acc + (x -> -1))
        									else 
        									  if(acc.contains(this.start-x)){ /*println("found "+x);*/ acc +(this.start-x -> x)} 
        									  else acc + (x -> -1)).filter(x=>x._2 != -1).headOption.getOrElse(null) 
      //((primes zip primes.reverse) filter (x=>(x._1+x._2) == this.start)).foldLeft(List[Int]())((acc,x)=>acc ++ List(x._1,x._2))
     // ((primes zip primes.reverse) filter (x=>(x._1+x._2) == this.start)).firstOption.getOrElse(null)
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    
    //P32 (**) Determine the greatest common divisor of two positive integer numbers.
    def gcd(a:S99Int,b:S99Int):Int = if(b.start == 0 ) a.start else gcd(b, a mod b)
    
    def sieveOfErastothenes(x:Int):List[Int] = {
      def sieve(xs:List[Int]):List[Int]= xs match {
        case Nil => List()
        case hd::tl => hd :: sieve( tl.filter(currE => currE%hd != 0))
      }
    		sieve((for(i <- 2 to x) yield i) toList)
    }
    
    def listPrimesinRange(r:Range):List[Int] = sieveOfErastothenes(r.end).filter(x=>x >= r.start)
    //phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ..
    
    def phi(m:Int) = m.primeFactorMultiplicity.foldLeft(1)((acc:Int,x:Tuple2[Int,Int]) => acc*(x._1-1)*Math.pow(x._1,x._2-1).toInt)
     
   def printGoldbachList(r:Range)={
      for(i <- r if i%2 == 0;val gi = i.goldbach)
        println("%d = %d + %d".format(i,gi._1,gi._2))
    } 
    def printGoldbachListLimited(r:Range, stp:Int)={
      for(i <- 2*stp to r.end if  (i%2 == 0);val gi = i.goldbach )
        if(gi._1 > stp && gi._2 > stp) println("%d = %d + %d".format(i,gi._1,gi._2))
    }
    
    
    def main(args:Array[String]){
      println("is 7 prime --> "+7.isPrime)
	println("is 13 prime --> "+13.isPrime)              
	println("is 1024 prime --> "+1024.isPrime)
	
	println("gcd(36,63) --> "+gcd(36,63))
	println("35.isCoprimeTo(64) --> "+ 35.isCoPrimeTo(64))
	println("10.totient --> "+10.totient)
	
	println("primes till 100 --> "+sieveOfErastothenes(100))
	
	 //315.primeFactors == List(3, 3, 5, 7)
	println("315.primeFactors --> "+315.primeFactors)
	
	//315.primeFactorMultiplicity == List((3,2), (5,1), (7,1))
	
	println("315.primeFactorMultiplicity --> "+315.primeFactorMultiplicity)
	
	//phi function by old method
	println("10090.totient --> "+10090.totient)
	println("10090.primeFactorMultiplicity --> "+10090.primeFactors)
	println("10090.primeFactorMultiplicity --> "+10090.primeFactorMultiplicity)
	//phi function by new method
	println("phi(10090) --> "+phi(10090))
	
	//listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31)
	println("listPrimesinRange(7 to 31) --> "+listPrimesinRange(7 to 31))
	//28.goldbach == (5,23)
	println("28.goldbach --> "+28.goldbach)
	
	/*scala> printGoldbachList(9 to 20)
		10 = 3 + 7
		12 = 5 + 7
		14 = 3 + 11
		16 = 3 + 13
		18 = 5 + 13
		20 = 3 + 17
		*/
	
	printGoldbachList(9 to 20)
	
	/*printGoldbachListLimited(1 to 2000, 50)
		992 = 73 + 919
		1382 = 61 + 1321
		1856 = 67 + 1789
		1928 = 61 + 1867
*/
	printGoldbachListLimited(1 to 2000, 50)
    }
  }
  


/*object IntTest extends App{
    import arithmetic._
   implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
	println("is 7 prime --> "+7.isPrime)
	println("is 13 prime --> "+13.isPrime)
	println("is 1024 prime --> "+1024.isPrime)
}*/