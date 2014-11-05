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
    }
  
  Block(Pos(1,2),Pos(1,3)) == Block(Pos(1,2),Pos(1,4))
                                                  //> res0: Boolean = false
                                                  
    val l1 = List("suman","yeeksie")              //> l1  : List[java.lang.String] = List(suman, yeeksie)
    
    l1.map(_.substring(1))                        //> res1: List[java.lang.String] = List(uman, eeksie)
}