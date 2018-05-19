package fpinscala.monoids

import java.util.concurrent.Executors

import fpinscala.SimpleBooleanTest
import fpinscala.monoids.Monoid._
import fpinscala.parallelism.Nonblocking.Par

object MonoidTest extends App with SimpleBooleanTest {
  override def run: Unit = {
    val foldRightTest = Monoid.foldRight(List("a","b","c"))("|")((a, b) => a + b) == "abc|"
    printTest(foldRightTest, "foldRight")

    val foldLeftTest = Monoid.foldLeft(List("a","b","c"))("|")((b, a) => b + a) == "|abc"
    printTest(foldLeftTest, "foldLeft")

    val foldMapVTest = Monoid.foldMapV(Vector("a", "b", "c"), stringMonoid)(identity) == "abc"
    printTest(foldMapVTest, "foldMapV")

    val es = Executors.newFixedThreadPool(10)
    val vec = Vector("a", "b", "c", "d", "e", "f", "g")
    val parFoldMapTest = Par.run(es)(Monoid.parFoldMap(vec, stringMonoid)(identity)) == "abcdefg"
    printTest(parFoldMapTest, "parFoldMap")
    es.shutdown()

    val orderedTest = ordered(Vector(1,2,3,4,5,6,7,8)) && !ordered(Vector(1,2,3,4,5,3))
    printTest(orderedTest, "ordered")


    val wcMonoidTest = {
      import wcMonoid._
      val wcs = Seq(
        Stub(""),
        Stub("a"),
        zero,
        Part("", 3, "b"),
        Part("c", 4, ""),
        Part("d", 0, "e"),
        Part("f", 5, "g")
      )

      val law1 = op(zero, Stub("a")) == op(Stub("a"), zero) && op(zero, Part("a", 2, "b")) == op(Part("a", 2, "b"), zero)
      val law2 = (for {
          wc1 <- wcs
          wc2 <- wcs
          wc3 <- wcs
        } yield op(wc1, op(wc2, wc3)) == op(op(wc1, wc2), wc3)).forall(identity)

      law1 && law2
    }
    printTest(wcMonoidTest, "wcMonoid")

    val countTest = {
      val lines = Seq(
        "hello I'm yasu",
        " I am a software developer! ",
        "oh wait is this really working!?",
        "wow, this is, awesome",
        "two  spaces   or   even   three   ",
        "Bacon ipsum dolor amet pig beef ribs pancetta, cupim meatball tongue swine pork belly landjaeger. Hamburger doner beef ham hock short ribs landjaeger shankle. Bresaola short loin flank venison landjaeger. Filet mignon flank doner tail ham hock."
      )
      val counts = Seq(3, 5, 6, 4, 5, 37)

      lines.map(count) == counts
    }

    printTest(countTest, "count")


    def foldableTest[A[_]](foldable: Foldable[A], input: A[String], expected: (String, String, Int) = ("|123", "123|", 6)) = {
      foldable.foldLeft[String, String](input)("|")(_+_) == expected._1 &&
      foldable.foldRight[String, String](input)("|")(_+_) == expected._2 &&
      foldable.foldMap[String, Int](input)(_.toInt)(intAddition) == expected._3
    }

    val listFoldableTest = foldableTest(ListFoldable, List("1", "2", "3"))
    val indexedSeqFoldableTest = foldableTest(IndexedSeqFoldable, Vector("1", "2", "3"))
    val streamFoldableTest = foldableTest(StreamFoldable, Stream("1", "2", "3"))
    val treeFoldableTest = foldableTest(TreeFoldable, Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))))
    val optionFoldableTest = foldableTest(OptionFoldable, Some("123"), ("|123", "123|", 123))

    printTest(listFoldableTest, "ListFoldable")
    printTest(indexedSeqFoldableTest, "IndexedSeqFoldable")
    printTest(streamFoldableTest, "StreamFoldable")
    printTest(treeFoldableTest, "TreeFoldable")
    printTest(optionFoldableTest, "OptionFoldable")

  }

  run
}
