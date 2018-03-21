package fpinscala.parallelism

import java.util
import java.util.concurrent._

import fpinscala.SimpleBooleanTest

import scala.util.Try

object ParTest extends App with SimpleBooleanTest{
  case class ReturnValueAtGivenMillis[A](get: A, millis: Long) extends Future[A] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???
      override def isCancelled: Boolean = ???
      override def isDone: Boolean = ???
      override def get(timeout: Long, unit: TimeUnit): A = {
        val start = System.currentTimeMillis()
        Thread.sleep(millis)
        val end = System.currentTimeMillis()
        if (end - start > unit.convert(timeout, TimeUnit.MILLISECONDS)) throw new TimeoutException("Timed out")
        get
      }
  }

  val es = Executors.newSingleThreadExecutor()

  override def run: Unit = {

    val parA = (es: ExecutorService) => ReturnValueAtGivenMillis(1, 30)
    val parB = (es: ExecutorService) => ReturnValueAtGivenMillis(2, 30)
    val map2WithTimeoutTest: Boolean = Par.map2WithTimeout(parA, parB, 100, TimeUnit.MILLISECONDS)(_+_)(es).get == Par.unit(3)(es).get
    val map2WithTimeoutTest2: Boolean = Try {
      Par.map2WithTimeout(parA, parB,  20, TimeUnit.MILLISECONDS)(_+_)(es).get
    }.toEither match {
      case Left(toe: TimeoutException) => true
      case _ => false
    }
    println((map2WithTimeoutTest && map2WithTimeoutTest2) + ": map2 with timeout")

    val asyncFTest = Par.asyncF((a: Int) => a*2)(2)(es).get == 4
    println(asyncFTest + ": asyncF")

    val par1 = Par.lazyUnit(1)
    val par2 = Par.lazyUnit(2)
    val sequenceTest = Par.sequence(List(par1, par2))(es).get == List(1,2)
    println(sequenceTest + ": sequence")

    val parFilterTest = Par.parFilter(List(1,2,3))(_ => true)(es).get == List(1,2,3)
    println(parFilterTest + ": parFilter")
  }

  run
  es.shutdown()
}
