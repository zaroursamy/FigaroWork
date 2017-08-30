/*
 * Test.scala 
 * A simple Figaro test program.
 * 
 * Created By:      Michael Reposa (mreposa@cra.com)
 * Creation Date:   Aug 6, 2014
 * 
 * Copyright 2013 Avrom J. Pfeffer and Charles River Analytics, Inc.
 * See http://www.cra.com or email figaro@cra.com for information.
 * 
 * See http://www.github.com/p2t2/figaro for a copy of the software license.
 */

import com.cra.figaro.algorithm.factored.{ProbQueryVariableElimination, VariableElimination}
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.{Normal, Uniform}
import com.cra.figaro.library.atomic.discrete.{AtomicUniform, SwitchingFlip}
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.compound._

object Test {



  def main(args: Array[String]) {


    def first() = {
      // Modèle qui produit la valeur BLANC avec une probabilité 1
      val test = Constant("BLANC")
      println(test)

      // Echantillon de taille 1000 du modèle test
      val algorithm = Importance(1000, test)
      algorithm.start()
      println(algorithm.probability(test, "BLANC"))
      println(algorithm.probability(test, "NOIR"))

      println("*********")
      val bernouilli: AtomicFlip = Flip(0.7)
      val algoBern = Importance(1000, bernouilli)
      algoBern.start()
      println(algoBern.probability(bernouilli, true))
      println(algoBern.probability(bernouilli, false))

      println("*********")
      val uniform: AtomicSelect[String] = Select(0.2 -> "zero deux", 0.3 -> "zero trois", 0.5 -> "zero cinq")
      val algUni = Importance(1000, uniform)
      algUni.start()
      List("", "zero deux", "ok", "zero trois", "zero", "zero cinq").foreach(x => println(algUni.probability(uniform, x)))

      println("*********")
      val trueUniform: Flip = Flip(Uniform(0, 1))
      val algTU = Importance(100, trueUniform)
      algTU.start()
      Console println algTU.probability(trueUniform, true)

      println("*********")
      val mixed: If[String] = If(Flip(0.7), Constant("A"), Select(0.1 -> "A", 0.9 -> "B"))
      val algoMix = Importance(100, mixed)
      algoMix.start()
      println(algoMix.probability(mixed, "B"))


      println("*********")
      val chain: Chain[Double, Int] = Chain(Uniform(0, 1), (x: Double) => if (x > 0.9) Constant(1) else Select(0.2 -> 2, 0.8 -> 4))
      val algoChain = Importance(100, chain)
      algoChain.start()
      algoChain.sample()._2.foreach(println)

      println("*********")
      val apply = Apply(Select(0.6 -> "a", 0.4 -> "b"), (i: String) => i + "=")
      val con = Container(Flip(0.4), Flip(0.2), Flip(0.9))

      println("*********")
      val b = Flip(0.001)
      val alarm = If(b, Flip(0.9), Flip(0.1))

      val x = b
      val y: Eq[b.Value] = b === b
      println(y)
      val yBis = Flip(0.1) === Flip(0.1)
      println(y)

    }

    def second() = {

      import com.cra.figaro.language._
      import com.cra.figaro.library.compound.CPD


      val (burglary, earthquake) = (Flip(0.01), Flip(0.0001))

      val unDeNonPipe = Flip(0.17)
      (1 to 10).foreach { b =>
        val alg = Importance(b, unDeNonPipe)
        alg.start()
        println(s"Nombre d'apparition du 6 en ${b} iterations: " + alg.probability(unDeNonPipe, true) * b)
      }

      val alarm: CPD2[Boolean, Boolean, Boolean] = CPD(
        arg1 = burglary,
        arg2 = earthquake,
        clauses = (false, false) -> Flip(0.001),
        (false, true) -> Flip(0.1),
        (true, false) -> Flip(0.9),
        (true, true) -> Flip(0.99)
      )




      //      (1 to 1000).foreach{num =>
      //        val algAlarm = Importance(num, alarm)
      //
      //        algAlarm.start()
      //        algAlarm.sample()._2.foreach(x => println("Is there an alarm? : "+x._1.value))
      //        println(s"probability to get an alarm in ${num} iterations: "+algAlarm.probability(alarm, true))
      //      }


      val calls: CPD1[Boolean, Boolean] = CPD(alarm, Seq(true -> Flip(0.001), false -> Flip(0.7)): _*)

      val x1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val x2 = Flip(0.6)
      val x3 = Constant(5)
      val x4 = Flip(0.8)

      val y = RichCPD(x1, x2, x3, x4,
        (OneOf(1, 2), *, *, *) -> Constant("1,2"), (*, OneOf(true), *, OneOf(true)) -> Constant("true true"))


      // Conditions on value element can be
      val cond = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      // seules les valeurs 2 et 4 sont possibles à l'issue
      cond.setCondition((i: Int) => i % 2 == 0)
      // on peut rajouter une condition: seules les valeurs 4 sont finalement possibles
      cond.addCondition((i: Int) => i > 2)
      // on spécifie que ca ne peut que renvoyer 4) => ca supprimer toutes les anciennes conditions
      cond.observe(4)
      //CONTRAINTES: on a dix fois plus d'appel que de non appel
      calls.setConstraint((b: Boolean) => if (b) 0.1 else 0.001)


    }

    def firm = {
      import com.cra.figaro.language._
      import com.cra.figaro.library.atomic._
      import com.cra.figaro.library.compound.If

      class Firm {
        val efficient = Flip(0.3)
        val bid = If(efficient, continuous.Uniform(5, 15),
          continuous.Uniform(10, 20))
      }

      // instanciation de 20 firmes
      val firms: Array[Firm] = Array.fill(20)(new Firm)
      // le winner est choisit uniformément parmis les firmes instanciées
      val winner: AtomicUniform[Firm] = discrete.Uniform(firms: _*)
      // on prend le bid de la firme gagnante
      val winningBid: Chain[Firm, Double] = Chain(winner, (f: Firm) => f.bid)
      // un bid de 5 a un poids de 15, un bid de 19 a un poids de 1 => un bid de 5 est 15 fois plus probable qu'un bid de 19
      winningBid.setConstraint((d: Double) => 20 - d)

      (1 to 20).foreach(x => println(s"winner: ${winningBid.generateValue()}"))

    }

    def person = {
      class Person {
        val smokes = Flip(0.6)

        val alice, bob, clara = new Person
        val friends = alice -> bob :: bob -> clara :: Nil

        clara.smokes.observe(true) // on spécifie que clara fume

        def smokingInfluence(t: (Boolean, Boolean)) = if (t._1 == t._2) 3.0 else 1.0

        for ((p1, p2) <- friends) {
          val x = ^^(p1.smokes, p2.smokes)

          ^^(p1.smokes, p2.smokes).setConstraint(smokingInfluence)
        }

      }
    }

    def third() = {


      case class Source(name: String)

      abstract class Sample {
        val fromSource: Element[Source]
      }

      case class Pair(source: Source, sample: Sample) {
        val isTheRightSource: Apply1[Source, Boolean] = Apply(sample.fromSource, (s: Source) => s == source)
        val distance: If[Double] = If(isTheRightSource,
          Normal(0.0, 1.0),
          Uniform(0.0, 10.0))
      }

      val source1 = new Source("Source 1")
      val source2 = new Source("Source 2")
      val sample1 = new Sample {
        val fromSource = Select(0.5 -> source1, 0.5 -> source2)
      }

      val pair1 = Pair(source1, sample1)
      val pair2 = Pair(source2, sample1)

      pair1.distance.setCondition((d: Double) => d > 0.15 && d < 0.25)
      pair2.distance.setCondition((d: Double) => d > 1.45 && d < 2.55)


    }

    def actors = {
      import com.cra.figaro.language._

      case class Actor(name: String) {
        val famous = Flip(0.1)
      }
      case class Movie(name: String) {
        val quality = Select(0.3 -> 'low, 0.5 -> 'medium, 0.2 -> 'high)
      }

      class Apparence(actor: Actor, movie: Movie) {

        def probAward(quality: Symbol, famous: Boolean) = (quality, famous) match {
          case ('low, false) => 0.001
          case ('low, true) => 0.01
          case ('medium, false) => 0.01
          case ('medium, true) => 0.05
          case ('high, false) => 0.05
          case ('high, true) => 0.2
        }

        val award = SwitchingFlip(
          Apply(movie.quality,
            actor.famous,
            probAward))
      }


      val actor1 = new Actor("sylvester")
      val actor2 = new Actor("jason")
      val actor3 = new Actor("arnold")

      val movie1 = new Movie("Rambo")
      val movie2 = new Movie("Expandables 1")

      val ap1 = new Apparence(actor1, movie1)
      val ap2 = new Apparence(actor2, movie2)
      val ap3 = new Apparence(actor3, movie2)


      val aps: Seq[Apparence] = ap1 :: ap2 :: ap3 :: Nil
      actor3.famous.observe(true)
      movie2.quality.observe('high)

      // Ensure that exactly one appearance gets an award.
      def uniqueAwardCondition(awards: List[Boolean]) = awards.count((b: Boolean) => b) == 1

      val allAwards: Element[List[Boolean]] = Inject(aps.map(_.award): _*)

      allAwards.setCondition(uniqueAwardCondition)


    }

    def actors2 = {
      import com.cra.figaro.language._

      class Actor {
        var movies: List[Movie] = Nil
        lazy val skillful = Flip(0.1)
        lazy val qualities = Container(movies.map(_.quality): _*)
        lazy val numGoodMovies = qualities.count(_ == 'high)
        lazy val famous = Chain(numGoodMovies, (n: Int) =>
          if (n >= 2) Flip(0.8) else Flip(0.1))
      }

      class Movie {
        var actors: List[Actor] = Nil
        lazy val skills = Container(actors.map(_.skillful): _*)
        lazy val actorsAllGood = skills.exists(b => b)
        lazy val probLow = Apply(actorsAllGood, (b: Boolean) => if (b) 0.2; else 0.5)
        lazy val probHigh = Apply(actorsAllGood, (b: Boolean) => if (b) 0.5; else 0.2)
        lazy val quality = Select(probLow -> 'low, Constant(0.3) -> 'medium, probHigh -> 'high)
      }

    }

    def cars = {
      import com.cra.figaro.language._

      abstract class Engine extends ElementCollection {
        val power: Element[Symbol]
      }
      class V8 extends Engine {
        val power: AtomicSelect[Symbol] = Select(0.8 -> 'low, 0.2 -> 'high)("power", this)
      }
      class V6 extends Engine {
        val power = Select(0.8 -> 'low, 0.2 -> 'high)("power", this)
      }
      object MySuperEngine extends V8 {
        override val power = Select(1.0 -> 'high)("pwover", this)
      }

      class Car extends ElementCollection {
        val engine = com.cra.figaro.library.atomic.discrete.Uniform[Engine](new V8, new V6, MySuperEngine)("engine", this)
        val speed = CPD(get[Symbol]("engine.power"),
          'high -> Constant(90.0),
          'medium -> Constant(80.0),
          'low -> Constant(70.0))
      }

      (0 to 100).map { x => (new Car).speed.generateValue()
      }.groupBy(x => x)
        .map(x => (x._1, x._2.size))
        .foreach(println)

    }

    def components = {

      import com.cra.figaro.language._
      import com.cra.figaro.util.MultiSet
      class Component extends ElementCollection {
        val f = Select(0.2 -> 2, 0.3 -> 3, 0.5 -> 5)("f", this)
      }

      val comp1 = new Component
      comp1.f.setCondition(x => x <= 3)
      val comp2 = new Component

      def makeComponent(): AtomicSelect[Component] = Select(0.1 -> comp1, 0.2 -> comp2, 0.7 -> new Component)


      class Container extends ElementCollection {

        val components: MakeList[Component] = MakeList(Select(0.5 -> 1, 0.5 -> 2), makeComponent)("f", this)

        val sum: Aggregate[Int, Int] = getAggregate((xs: MultiSet[Int]) => (0 /: xs) (_ + _))("components.f")

        // si un component apparait deux fois dans la liste, avec valeur 1, alors sum=1 et pas   2 : il ne compte qu'une fois
      }
    }

  }
}


object Chapitre5 {

  def printcustom(x:AnyRef) = {
    println("***")
    println(x)
    println("***")
  }

  def main(args: Array[String]): Unit = {

    import com.cra.figaro.algorithm._
    val values: Values = Values()


    val e1: AtomicFlip = Flip(0.7)

    printcustom("possible values e1 " + values(e1)) // Set(true, false)

    val e2: If[Int] = If(e1, Select(0.2 -> 2, 0.8 -> 8), Select(0.5 -> 1, 0.3 -> 3, 0.2 -> 2))

    val v2: Set[Int] = Values()(e2)
    println("***")
    println(s"e2 possible values: ${values(e2)}")
    println(s"e2 possible values: $v2")
    println("***")

    val f1: AtomicSelect[Double] = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
    val f2: Flip = Flip(f1)
    val f3: If[Int] = If(f2, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
    f3.setCondition((i: Int) => i == 2)


    /* variable elimination */
    val ve: ProbQueryVariableElimination = VariableElimination(f2)

    val ve3 = VariableElimination(f3)

    ve.start()
    val distribF2 = ve.distribution(f2)

    printcustom("Distribution f2: ")
    distribF2.foreach(println)


    val probF2 = ve.probability(f2, (b: Boolean) => b) // Calcule la probabilité que f2 soit vrai
    printcustom("P(f2=true) = " + probF2)
    ve.kill()

    ve3.start()
    val probF3 = ve3.probability(f3, (i: Int) => i == 2)
    printcustom("P(f3=2) = " + probF3)
    ve3.kill()


    /* Belief propagation */

    import com.cra.figaro.algorithm.factored.beliefpropagation._
    import com.cra.figaro.language._
    val e1BP = Select(0.25 -> 0.3, 0.25 -> 0.5, 0.25 -> 0.7, 0.25 -> 0.9)
    val e2BP = Flip(e1BP)
    val e3BP = If(e2BP, Select(0.3 -> 1, 0.7 -> 2), Constant(2))
//    e3BP.setCondition((i: Int) => i == 2)
    val bp = BeliefPropagation(100, e3BP)

    println("bp")

    bp.start()
    printcustom("P(e3BP=2) = " + bp.probability(e3BP, 2))
    bp.kill()

    println("Importance")
    val imp = Importance(5000, e2BP)
    imp.start()
    Thread.sleep(1000)
    imp.stop()
    printcustom("P(e2BP=true) = " + imp.probability(e2BP, true))
    imp.kill() // kill the thread

    /* MCMC */






  }
}