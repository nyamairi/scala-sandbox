import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Random

object CountDownLatchSample extends App {
  val random = new Random()

  val promises: Seq[Promise[Int]] = for {i <- 1 to 3} yield Promise[Int]

  val futures: Seq[Future[Int]] = for {i <- 1 to 8} yield Future {
    val waitMilliSec = random.nextInt(1000)
    Thread.sleep(waitMilliSec)
    waitMilliSec
  }

  val counter = new AtomicInteger(0)
  futures.foreach { f =>
    f.onSuccess { case waitMilliSec =>
      val index = counter.getAndIncrement()
      if (index < promises.length) {
        promises(index).success(waitMilliSec)
      }
    }
  }

  promises.foreach { p =>
    p.future.onSuccess { case waitMilliSec =>
      println(waitMilliSec)
    }
  }

  Thread.sleep(5000)
}
