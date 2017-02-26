import io.threadcso._

object MyTutor extends SleepingTutor {
  private val monitor = new Monitor
  var studentPresentCount: Int = 0
  var isLectureOver: Boolean = false

  val studentsCanLeave, lectureCanStart = monitor.newCondition


  def TutorWait = monitor withLock  {
    while(studentPresentCount < 2) { lectureCanStart.await() }
  }
  def Arrive = monitor withLock {
    isLectureOver = false
    studentPresentCount += 1
    if (studentPresentCount == 2) {
      lectureCanStart.signal() // tutor to check presentCount
    }
  }
  def ReceiveTute = monitor withLock {
    while (!isLectureOver) { studentsCanLeave.await() }

    // Now I can leave
    studentPresentCount -= 1
  }
  def EndTeach = monitor withLock {
    isLectureOver = true
    studentsCanLeave.signalAll() // the students that they can leave
  }
}