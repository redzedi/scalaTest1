package logic{  

  class S99Boolean(private val innerBool:Boolean){
   import S99Boolean._
  def unary_! = !this.innerBool   
  def and(b:Boolean) = innerBool  & b 
  def or(b:Boolean) = innerBool  | b
  def xor(b:Boolean) = innerBool  ^ b
  def nor(b:Boolean) = !or(b)
  def nand(b:Boolean) = !and(b)
  def impl(b:Boolean) = !this or b
  def equiv(b:Boolean) = ( this impl b) and (b impl innerBool)//and(impl(a,b), impl(b,a) )
  
  }
  
  object S99Boolean{
    
    implicit def booleanToS99Boolean(innerBool:Boolean):S99Boolean = new S99Boolean(innerBool)
  }
  
  object S99Logic {
  
  def and(a:Boolean,b:Boolean) = a & b
  def or(a:Boolean,b:Boolean):Boolean = a | b
  def xor(a:Boolean,b:Boolean):Boolean = a ^ b
  def nor(a:Boolean,b:Boolean):Boolean = !or(a,b)
  def nand(a:Boolean,b:Boolean):Boolean = !and(a,b)
  def impl(a:Boolean,b:Boolean):Boolean = or(!a,b)
  def equiv(a:Boolean,b:Boolean):Boolean = and(impl(a,b), impl(b,a) )
  
  //implicit def booleanToS99Boolean(innerBool:Boolean):S99Boolean = new S99Boolean(innerBool)
  def table2(f:(Boolean,Boolean)=>Boolean):String=
    " A    B   result \n" + ( ( for{
       a <- List(true,false)
       b <- List(true,false)
  } yield "%b  %b %b".format(a,b,f(a,b)) ) mkString("\n"))
  
  def gray1(n:Int):List[String] = {
    if( n==1)
      List("0","1")
     else{
    	 val grayN_MINUS_1= gray1(n-1)
    	 (grayN_MINUS_1 map {"0"+_}) ++ ((grayN_MINUS_1 reverse) map {"1"+_})
     }
  }
  
  
  
  
  }
 object logicTest extends App{
  import logic.S99Logic._
  import logic.S99Boolean._
    
   
    /** #46
     * scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
A     B     result
true  true  true
true  false true
false true  false
false false false
     */
    println(table2((a: Boolean, b: Boolean) => and(a, or(a, b))))
    
    println(table2((a: Boolean, b: Boolean) => a and (a or !(b))))
    
  /**
   * scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
		A     B     result
		true  true  true
		true  false true
		false true  false
		false false false
   */
    
    /*
     * scala> gray(3)
	 *	res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
     */
    println("gray1(3) --> "+gray1(3))
   /* 
    scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
	res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
*/
    import logic.Huffman._
    println("huffman code -->"+convert(until(singleton,combine)(makeOrderedLeafList(List(('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5))))).map((a:(Char,List[Int]))=>(a._1 ,a._2  mkString "")))
}

}