object scalaWS1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = new Rational(1,3)                       //> x  : Rational = 1/3
  val y = new Rational(5,7)                       //> y  : Rational = 5/7
  val z = new Rational(3,2)                       //> z  : Rational = 3/2
  
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  
  x - y - z                                       //> res2: Rational = -79/42
  
  y + y                                           //> res3: Rational = 10/7
   x < y                                          //> res4: Boolean = true
   
   x max y                                        //> res5: Rational = 5/7
   
  val wholeNum = new Rational(2,1)                //> wholeNum  : Rational = 2/1
}

class Rational(x:Int , y:Int){
require(y!=0," denom must be nonzero")

def this(x:Int) =  this(x,1)

private def gcd(a:Int, b:Int) :Int = {
  //println("gcd called !!")
  if(b==0) a else gcd(b, a%b)
}

private val g = gcd(x,y)
 val numer = x/g
 val denom = y/g
 
 def max(that:Rational)= if(this < that ) that else this
 
 def <(that:Rational) = numer*that.denom < denom*that.numer
 
 def +(that:Rational)=
 new Rational( numer*that.denom + denom*that.numer, denom*that.denom)
 
 def unary_- :Rational = new Rational(-numer,denom)
 
 def -(that:Rational) =  this +  -that
   
override   def toString = numer +"/"+denom
}