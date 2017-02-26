import io.threadcso._
/**
  * A solution to the problem will extend this class, which simply
  * provides a standard test-rig.
*/

trait SleepingTutor
{
  /** Protocol for tutor to wait for students to arrive */
  def TutorWait: Unit 
  /** Protocol for student to arrive and wait for tutorial */
  def Arrive: Unit  
  /** Protocol for students to receive tutorial */
  def ReceiveTute: Unit  
  /** Protocol for tutor to end tutorial */
  def EndTeach: Unit
  
  val start = nanoTime
  def now   = (nanoTime-start)/1E9 // in seconds
  def log(me: String, item: String) =  
      println(f"$now%6.5f $me%-5s $item%s")

  val random = new scala.util.Random

  def Student(me: String) = proc(s"Student $me") {
    while (true) {
      sleep(random.nextInt(3)*Sec)
      log(me, "arrives"); Arrive
      log(me, "ready");   ReceiveTute
      log(me, "leaves")
    }
  }

  def Tutor = proc("Tutor") {
    while (true) {
      log("Tutor", "waiting");   TutorWait
      log("Tutorial", "starts"); 
      sleep(random.nextInt(3)*Sec)
      log("Tutorial", "ends");   EndTeach; 
      sleep(random.nextInt(3)*Sec)
    }
  }

  val System = Tutor || Student("Ada") || Student("Bob")

  def main(args: Array[String]) = 
      System()
}

/**
  * To show what happens when the protocols are null.
*/


