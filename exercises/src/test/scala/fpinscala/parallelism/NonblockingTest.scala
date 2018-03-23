package fpinscala.parallelism

import java.util.concurrent.Executors

import fpinscala.SimpleBooleanTest
import fpinscala.parallelism.{Nonblocking => NB}

import scala.util.Try

object NonblockingTest extends App with SimpleBooleanTest {
  val es = Executors.newFixedThreadPool(1)

  override def run: Unit = {
    val runButCatchErrorTest = Try {
      NB.Par.runButCatchError(es)(NB.Par.unit[Int]{throw new Exception("throwing exception, as you can see")})
    }.map(_ => false).getOrElse(true)

    println(runButCatchErrorTest + ": run but catch error")
  }

  run
  es.shutdown()
}
