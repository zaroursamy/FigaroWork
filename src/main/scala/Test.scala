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

import java.util.UUID

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.continuous.{Normal, Uniform}
import com.cra.figaro.library.atomic.discrete.AtomicUniform
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

      val arrSources = Array.fill(200)(new Source(math.random.toString)).toList
      val arrSamples - new Sample{
        override val fromSource: Element[Source] = Select((1 to 200).map(x => 1/200->arrSources(x)).toSeq:_*)
      }

      val source1 = new Source("Source 1")
      val source2 = new Source("Source 2")
      val sample1 = new Sample {
        val fromSource = Select(0.5 -> source1, 0.5 -> source2)
      }

      val pair1 = Pair(source1, sample1)

    }
  }
}