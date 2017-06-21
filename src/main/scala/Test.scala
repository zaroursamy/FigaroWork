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

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.compound._
import spire.random.Gaussian

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

      val alarm: CPD2[Boolean, Boolean, Boolean] = CPD(
        arg1 = burglary,
        arg2 = earthquake,
        clauses = (false, false) -> Flip(0.001),
        (false, true) -> Flip(0.1),
        (true, false) -> Flip(0.9),
        (true, true) -> Flip(0.99)
      )


      val calls: CPD1[Boolean, Boolean] = CPD(alarm, Seq(true -> Flip(0.001), false -> Flip(0.7)): _*)

      val x1 = Select(0.1 -> 1, 0.2 -> 2, 0.3 -> 3, 0.4 -> 4)
      val x2 = Flip(0.6)
      val x3 = Constant(5)
      val x4 = Flip(0.8)

      val y = RichCPD(x1, x2, x3, x4,
        (OneOf(1, 2), *, *, *) -> Constant("1,2"), (*, OneOf(true), *, OneOf(true)) -> Constant("true true"))


    }
  }
}