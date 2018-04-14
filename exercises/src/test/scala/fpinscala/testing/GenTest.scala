package fpinscala.testing

import fpinscala.SimpleBooleanTest
import fpinscala.datastructures.Tree
import fpinscala.state.RNG
import fpinscala.state.RNG.Simple

object GenTest extends App with SimpleBooleanTest {

  private def tryNTimes[A](n: Int, g: Gen[A], rng: RNG)(f: A => Boolean): Boolean =
    (0 to n).foldLeft((true, rng))((acc, _) => {
      val (bool, r) = acc
      val (a, r2) = g.sample.run(r)
      (bool && f(a), r2)
    })._1

  override def run: Unit = {
//    val intList = Gen.listOf(Gen.choose(0, 100))
//    val propReverse = forAll(intList) { ns =>
//      sum(ns.reverse.reverse) == sum(ns)
//    }
//
//    val randInt = Gen.choose(0, 100)
//    val intListSameValue = Gen.listOf(randInt)
//    val propSameValue = forAll(intListSameValue) { ns =>
//      sum(ns) == ns.head * ns.length
//    }
//
//    val propMax = forAll(intList)(ns => {
//      val max = max(ns)
//      ns.forAll(_ <= max)
//    })
    implicit val rng = Simple(1)

    val unitTest = Gen.unit(1).value == 1
    println(unitTest + ": unit")

    val chooseTest = tryNTimes(100, Gen.choose(10, 15), rng)(n => n >= 10 && n < 15)
    println(chooseTest + ": choose")

    val listOfNTest = Gen.listOfN(100, Gen.choose(10, 15)).value.forall(n => n >= 10 && n < 15)
    println(listOfNTest + ": list of n")

    val ints = Gen.listOfN(10, Gen.int).value
    println(true + ": int | output => " + ints)

    val booleans = Gen.listOfN(10, Gen.boolean).value
    println(true + ": boolean | output => " + booleans)

    val int = Gen.int
    val toOptionTest = Gen.toOption(int).value == Some(int.value)
    println(toOptionTest + ": toOption")

    val fromOptionTest = Gen.fromOption(Gen.unit[Option[Int]](Some(2))).value == 2
    println(fromOptionTest + ": fromOption")

    val strings = Gen.listOfN(10, Gen.string(3)).value
    println(true + ": string | output => " + strings)

    val flatMapTest = Gen.unit(3).flatMap(n => Gen.unit(n * n)).value == 9
    println(flatMapTest + ": flatMap")

    val listOfGenNTest = Gen.unit(1).listOfN(Gen.unit(3)).value == List(1, 1, 1)
    println(listOfGenNTest + ": list of n with Gen[Int]")

    val unions = Gen.listOfN(10, Gen.union(Gen.unit(1), Gen.unit(2))).value
    println(true + ": union | output => " + unions)

    val weighteds = Gen.listOfN(100, Gen.weighted((Gen.unit(true), 0.1),(Gen.unit(false), 0.9))).value
    val (shouldBe10Percent, shouldBe90Percent) = {
      val map = weighteds.groupBy(identity)
      (map(true).length, map(false).length)
    }
    println(true + ": weighted | output => " + shouldBe10Percent + ":" + shouldBe90Percent + " (this should be around 1:9)")

    val trues = PropWithTag.forAllWithTag("trues", Gen.unit(true))(identity)
    val falses = PropWithTag.forAllWithTag("falses", Gen.unit(false))(identity)
    val ands = trues && falses
    val ors = falses || trues
    val andTest = ands.run(3, rng) == FalsifiedWithTag("falses", "false", 0)
    println(andTest + ": &&")
    val orTest = ors.run(3, rng) == Passed
    println(orTest + ": ||")

    val listOfTest = Gen.listOf(Gen.unit(1)).forSize(3).value == List(1,1,1)
    println(listOfTest + ": SGen.listOf")

    val smallInt = Gen.choose(-10,10)

    val maxProp = Prop.forAll {
      Gen.listOf1(smallInt).flatMap(list => Gen.listOf(smallInt).map(list ++ _))
    } { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    println("max: ")
    Prop.run(maxProp, rng = rng)

    val sortProp = Prop.forAll(Gen.listOf(smallInt)) {
      ns =>
        val sorted = ns.sorted
        sorted.foldLeft((Int.MinValue, true))((acc, n) => {
          (n, acc._2 && (n >= acc._1))
        })._2
    }

    println("sort: ")
    Prop.run(sortProp, rng = rng)

    println("par: ")
    Prop.run(Prop.p4, rng = RNG.Simple(1))
    println("par.fork: ")
    Prop.run(Prop.pFork, rng = RNG.Simple(2))

    Prop.shutdownAllPools

    object ListProps {
      val listOfIntGen: SGen[List[Int]] = Gen.listOf(int)
      val isEven = (i: Int) => i%2 == 0
      val takeWhileProp =
        Prop.forAll(listOfIntGen)(ns => ns.takeWhile(isEven).forall(isEven)) &&
        Prop.forAll(listOfIntGen)(ns => ns.startsWith(ns.takeWhile(isEven))) &&
        Prop.forAll(listOfIntGen)(ns => ns.takeWhile(isEven) ::: ns.dropWhile(isEven) == ns)

      val takeProp =
        Prop.forAll(listOfIntGen.flatMap(l => SGen.choose(0, l.length + 1).map((l, _)))){ case (ns, i) => ns.take(i).lengthCompare(i) == 0 } &&
        Prop.forAll(listOfIntGen.flatMap(l => SGen.choose(0, l.length + 1).map((l, _)))){ case (ns, i) => ns.startsWith(ns.take(i)) } &&
        Prop.forAll(listOfIntGen.flatMap(l => SGen.choose(0, l.length + 1).map((l, _)))){ case (ns, i) => ns.take(i) ::: ns.drop(i) == ns }

      val dropProp =
        Prop.forAll(listOfIntGen.flatMap(l => SGen.choose(0, l.length + 1).map((l, _)))){ case (ns, i) => ns.drop(i).lengthCompare(ns.length - i) == 0 } &&
        Prop.forAll(listOfIntGen.flatMap(l => SGen.choose(0, l.length + 1).map((l, _)))){ case (ns, i) => ns.endsWith(ns.drop(i)) } &&
        Prop.forAll(listOfIntGen.flatMap(l => SGen.choose(0, l.length + 1).map((l, _)))){ case (ns, i) => ns.take(i) ::: ns.drop(i) == ns }

      val filterProp =
        Prop.forAll(listOfIntGen){ ns => ns.filter(isEven).forall(isEven) } &&
        Prop.forAll(listOfIntGen){ ns => ns.toSet == (ns.filter(isEven) ::: ns.filterNot(isEven)).toSet }

      val allProps = takeWhileProp && takeProp && dropProp && filterProp
    }

    println("list prop: ")
    Prop.run(ListProps.allProps, rng = rng)

    object StreamProps {
      import fpinscala.laziness.Stream
      val streamOfIntGen: SGen[Stream[Int]] = Gen.streamOf(int)
      val isEven = (i: Int) => i%2 == 0
      val takeWhileProp =
        Prop.forAll(streamOfIntGen)(ns => ns.takeWhile(isEven).forAll(isEven)) &&
        Prop.forAll(streamOfIntGen)(ns => ns.takeWhile(isEven).filter(n => !isEven(n)) == Stream.empty) &&
        Prop.forAll(streamOfIntGen)(ns => ns.startsWith(ns.takeWhile(isEven)))

      val takeProp =
        Prop.forAll(streamOfIntGen ** SGen.choose(0, 100)){ case (ns, i) => ns.startsWith(ns.take(i)) } &&
        Prop.forAll(streamOfIntGen ** SGen.choose(0, 100)){ case (ns, i) => ns.take(i).toList.lengthCompare(i) <= 0 }

      val dropProp =
        Prop.forAll(streamOfIntGen ** SGen.choose(0, 100)){ case (ns, i) => ns.hasSubsequence(ns.drop(i)) || ns.headOption.isEmpty } &&
        Prop.forAll(streamOfIntGen ** SGen.choose(0, 100)){ case (ns, i) => ns.take(i).append(ns.drop(i)).toList == ns.toList }

      val filterProp =
        Prop.forAll(streamOfIntGen){ ns => ns.filter(isEven).forAll(isEven) } &&
        Prop.forAll(streamOfIntGen){ ns => ns.toList.toSet == ns.filter(isEven).append(ns.filter(!isEven(_))).toList.toSet }

      val unfoldProp =
        Prop.forAll(SGen.choose(0, 100)) {n => Stream.unfold(n)(s => Some(s, s*s)).take(3).toList == List(n, n*n, n*n*n*n)}

      val allProps = takeWhileProp && takeProp && dropProp && filterProp && unfoldProp
    }

    println("stream prop: ")
    Prop.run(StreamProps.allProps, rng = rng)

    val treeFoldProp =
      Prop.forAll(Gen.treeOf(Gen.int)){ tree => Tree.size(tree) == Tree.fold(tree)(_ => 1, (trA, trB) => Tree.size(trA) + Tree.size(trB) + 1)} &&
      Prop.forAll(Gen.treeOf(Gen.int)){ tree => Tree.maximum(tree) == Tree.fold(tree)(identity, (trA, trB) => Tree.maximum(trA) max Tree.maximum(trB)) }

    println("tree fold prop: ")
    Prop.run(treeFoldProp, rng = rng)

    import fpinscala.errorhandling.{Option, Some, None, Either, Right, Left}
    val optionSequenceProp =
      Prop.forAll(Gen.listOf(int)){ l => Option.sequence(l.map(Some.apply)) == Some(l) } &&
      Prop.forAll(Gen.listOf(int)){ l => Option.sequence(l.map(Some.apply) :+ None) == None }

    println("option sequence prop: ")
    Prop.run(optionSequenceProp, rng = rng)

    val eitherSequenceProp =
      Prop.forAll(Gen.listOf(int)){ l => Either.sequence(l.map(Right.apply)) == Right(l) } &&
      Prop.forAll(Gen.listOf(int)){ l => Either.sequence(l.map(Right.apply) :+ Left(2)) == Left(2) }

    println("either sequence prop: ")
    Prop.run(eitherSequenceProp, rng = rng)

  }

  run
}
