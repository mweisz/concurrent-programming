import io.threadcso._

object MyTutor extends SleepingTutor {
  private val monitor = new Monitor

  // The number of students that are present
  private var studentPresentCount: Int = 0

  // Indicates if the lecture is still ongoing
  private var isLectureOver: Boolean = false

  // Condititions for the students and teacher to get notified on
  private val studentsCanLeave, lectureCanStart = monitor.newCondition


  def TutorWait = monitor withLock  {
    // The tutor is waiting until at least two students are preseent
    while(studentPresentCount < 2) { lectureCanStart.await() }

    // Both students have arrived.
    // The tutor's process can continue
  }

  def Arrive = monitor withLock {
    // Indicate that a new lecture is ongoing
    isLectureOver = false

    // Increase student count by one
    studentPresentCount += 1

    // If the student is the second one to arrive
    // signal the tutor that the lecture can start
    if (studentPresentCount == 2) {
      lectureCanStart.signal() 
    }  
  }

  def ReceiveTute = monitor withLock {
    // Students are waiting until the lecture is over
    while (!isLectureOver) { studentsCanLeave.await() }

    // Now they can leave
    studentPresentCount -= 1
  }

  def EndTeach  = monitor withLock {
    // The lecture is over now
    isLectureOver = true

    // Signal the students that they can leave. 
    studentsCanLeave.signalAll() 
  }
}