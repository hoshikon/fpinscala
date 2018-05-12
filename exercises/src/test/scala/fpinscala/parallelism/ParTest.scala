package fpinscala.parallelism

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

  override def run: Unit = {

    val es = Executors.newFixedThreadPool(10)

    val parA = (es: ExecutorService) => ReturnValueAtGivenMillis(1, 30)
    val parB = (es: ExecutorService) => ReturnValueAtGivenMillis(2, 30)
    val map2WithTimeoutTest: Boolean = Par.map2WithTimeout(parA, parB, 100, TimeUnit.MILLISECONDS)(_+_)(es).get == Par.unit(3)(es).get
    val map2WithTimeoutTest2: Boolean = Try {
      Par.map2WithTimeout(parA, parB,  20, TimeUnit.MILLISECONDS)(_+_)(es).get
    }.toEither match {
      case Left(toe: TimeoutException) => true
      case _ => false
    }
    printTest((map2WithTimeoutTest && map2WithTimeoutTest2), "map2 with timeout")

    val asyncFTest = Par.asyncF((a: Int) => a*2)(2)(es).get == 4
    printTest(asyncFTest, "asyncF")

    val par1 = Par.lazyUnit(1)
    val par2 = Par.lazyUnit(2)
    val sequenceTest = Par.sequence(List(par1, par2))(es).get == List(1,2)
    printTest(sequenceTest, "sequence")

    val parFilterTest = Par.parFilter(List(1,2,3,4,5,6,7))(_ => true)(es).get == List(1,2,3,4,5,6,7)
    printTest(parFilterTest, "parFilter")

//===This will trigger deadlock===
//    val a = Par.lazyUnit(42 + 1)
//    val S = Executors.newFixedThreadPool(1)
//    println(Par.equal(S)(a, Par.fork(a)) + "")

//===any fixed-size thread pool can be made to deadlock given this implementation of fork===
//    def triggerDeadLock(n: Int): Unit = {
//      val fixedThreadPool = Executors.newFixedThreadPool(n)
//
//      def recursiveCall(n: Int): Par[Int] = {
//        if (n > 0) Par.fork { recursiveCall(n-1) }
//        else Par.lazyUnit(0)
//      }
//
//      recursiveCall(n)(fixedThreadPool).get
//      fixedThreadPool.shutdown()
//    }
//
//    triggerDeadLock(20)

    val choices = List(Par.unit("a"), Par.unit("b"), Par.unit("c"))
    val choiceNTest = Par.choiceN(Par.unit(1))(choices)(es).get == "b"
    printTest(choiceNTest, "choiceN")

    val choiceWithChoiceNTest = Par.choiceWithChoiceN(Par.unit(false))(Par.unit("true"), Par.unit("false"))(es).get == "false"
    printTest(choiceWithChoiceNTest, "choice with choiceN")


    val choicesMap = Map("one" -> Par.unit(1), "two" -> Par.unit(2), "three" -> Par.unit(3))
    val choiceWithGeneralTest = Par.choiceWithGeneral(Par.unit(true))(Par.unit("true"), Par.unit("false"))(es).get == "true"
    val choiceNWithGeneralTest = Par.choiceNWithGeneral(Par.unit(1))(choices)(es).get == "b"
    val choiceMapTest = Par.choiceMap(Par.unit("two"))(choicesMap)(es).get == 2
    val choiceGeneralTest = choiceWithGeneralTest && choiceNWithGeneralTest && choiceMapTest

    printTest(choiceGeneralTest, "more general choice")

    val joinTest = Par.join(Par.unit(Par.unit("x")))(es).get == "x"
    printTest(joinTest, "join")

    val flatMapWithJoinTest = Par.flatMapWithJoin(Par.unit("1"))(x => Par.unit(x.toInt))(es).get == 1
    printTest(flatMapWithJoinTest, "flatMap with join")

    val joinWithFlatMapTest = Par.joinWithFlatMap(Par.unit(Par.unit("1")))(es).get == "1"
    printTest(joinWithFlatMapTest, "join with flatMap")

    es.shutdown()
  }

  run
}
