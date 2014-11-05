object scalaWS1 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(61); 
  println("Welcome to the Scala worksheet");$skip(31); 
  
  val x = new Rational(1,3);System.out.println("""x  : Rational = """ + $show(x ));$skip(28); 
  val y = new Rational(5,7);System.out.println("""y  : Rational = """ + $show(y ));$skip(28); 
  val z = new Rational(3,2);System.out.println("""z  : Rational = """ + $show(z ));$skip(13); val res$0 = 
  
  x.numer;System.out.println("""res0: Int = """ + $show(res$0));$skip(10); val res$1 = 
  x.denom;System.out.println("""res1: Int = """ + $show(res$1));$skip(15); val res$2 = 
  
  x - y - z;System.out.println("""res2: Rational = """ + $show(res$2));$skip(11); val res$3 = 
  
  y + y;System.out.println("""res3: Rational = """ + $show(res$3));$skip(9); val res$4 = 
   x < y;System.out.println("""res4: Boolean = """ + $show(res$4));$skip(15); val res$5 = 
   
   x max y;System.out.println("""res5: Rational = """ + $show(res$5));$skip(39); 
   
  val wholeNum = new Rational(2,1);System.out.println("""wholeNum  : Rational = """ + $show(wholeNum ))}
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