package actors

import scala.actors._
import scala.actors.Actor._

object evenActor extends Actor{
  var startNum = 2
   def act()={
    loop{
       react {
     
       case "your_turn" => println(startNum);startNum += 2; if(startNum <= 100) oddActor ! "your_turn" else this.exit()
     }
    }
   }
  
  }

object oddActor extends Actor{
  var startNum = 1
   def act()={
	  loop{
       react {
     
       case "your_turn" => println(startNum);startNum += 2;if(startNum < 103) evenActor ! "your_turn" else exit()
     }
   }
   }
  
  }

object ActorTst extends App {
  
  val oddActor1:Actor = actor {
	  loop{
       react {
     
       case ("your_turn",startNum:Int,even) => println(startNum);if(startNum < 103) evenActor1 ! ("your_turn",startNum+2,even) else exit()
       //    case ("your_turn",startNum:Int,nextFn:(Function[_,Unit] ))=> println(startNum);if(startNum < 103) nextFn((fn:Function[_,Unit])=>oddActor1 ! ("your_turn",startNum+2,fn)) else exit()
      //  case ("your_turn",startNum:Int,nextFn:(Function[Int,Unit] ))=> println(startNum);if(startNum < 103) nextFn(startNum+2) else exit()
     }
   }
   }
  
  val evenActor1:Actor = actor {
	  loop{
       react {
     
       case ("your_turn",odd:Int,startNum:Int) => println(startNum);if(startNum < 103) oddActor1 ! ("your_turn",odd,startNum+2) else exit()
         //case ("your_turn",startNum:Int,nextFn:Function[Function[a,t],Unit])=> println(startNum);if(startNum < 103) nextFn.apply((fn:Function[a,t])=>self.!(("your_turn",startNum+2,fn))) else exit()
     }
   }
   }

  //evenActor1.start()
  //oddActor1.start()
 // oddActor1 ! ("your_turn",1,(x:Int)=>oddActor1 ! ("your_turn",x,2))
  oddActor1 ! ("your_turn",1,2)
  //evenActor ! "your_turn"
}