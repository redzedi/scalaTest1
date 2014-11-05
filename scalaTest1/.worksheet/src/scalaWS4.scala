object scalaWS4 {

case class Pos(x: Int, y: Int) {
    /** The position obtained by changing the `x` coordiante by `d` */
    def dx(d: Int) = copy(x = x + d, y)

    /** The position obtained by changing the `y` coordiante by `d` */
    def dy(d: Int) = copy(x, y = y + d)
  }
  case class Block(b1: Pos, b2: Pos) {

    // checks the requirement mentioned above
    require(b1.x <= b2.x && b1.y <= b2.y, "Invalid block position: b1=" + b1 + ", b2=" + b2)
    };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(521); val res$0 = 
  
  Block(Pos(1,2),Pos(1,3)) == Block(Pos(1,2),Pos(1,4));System.out.println("""res0: Boolean = """ + $show(res$0));$skip(88); 
                                                  
    val l1 = List("suman","yeeksie");System.out.println("""l1  : List[java.lang.String] = """ + $show(l1 ));$skip(32); val res$1 = 
    
    l1.map(_.substring(1));System.out.println("""res1: List[java.lang.String] = """ + $show(res$1))}
}