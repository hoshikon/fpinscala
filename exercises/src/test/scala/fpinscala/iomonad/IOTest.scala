package fpinscala.iomonad

import fpinscala.SimpleBooleanTest

object IOTest  extends App with SimpleBooleanTest {
  override def run: Unit = {
    //This will trigger StackOverflow
    //IO3.runConsoleFunction0(IO3.freeMonad.forever(IO3.Console.printLn("Hello")))()

    //This will run forever without StackOverflow
    //IO3.runConsole(IO3.freeMonad.forever(IO3.Console.printLn("World")))
  }
  run
}
