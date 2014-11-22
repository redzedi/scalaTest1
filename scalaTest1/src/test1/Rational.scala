package test1

class Rational(val numer:Int,
  val deno:Int) extends Ordered[Rational] {
  
  require(deno != 0)
   
  //def this(n:Int, d:Int) {}
  
  def this(n:Int) = this(n,1)
  
  def *(that:Rational) = new Rational(numer*that.numer,deno*that.deno);
 // def *(that:Int) = new Rational(n*that,d);
  
  override def toString = numer +"/"+deno
  
  def compare(that: Rational): Int = this.numer * that.deno - that.numer*this.deno
  
}

class Rational1(val n:Int, val d:Int, val str:String) extends Rational(n,d){
  override def toString = str+super.toString+str
}


object testRational extends App {
	implicit def IntToRational(x:Int) = new Rational(x)
   val t = new Rational1(2,3,"|")
	println("t.numer "+t.numer)
   println(t+" created t")
   println(" t * t "+t*t)
   println(" t * t "+t*2)
   println(" t * t "+2*t)
   
   val half = new Rational(1,2)
	println("t < half --> "+ (t < half))
	println("t >= half --> "+ (t >= half))
	
  // val t1 = new Rational(2,0)
   //println(t1+" created")
}