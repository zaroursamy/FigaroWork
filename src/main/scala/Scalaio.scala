import com.cra.figaro.algorithm._
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language._
import com.cra.figaro.library.compound.{CPD, If}

/**
  * Created by zarour on 22/08/2017.
  */
object Scalaio {

  def main(args: Array[String]): Unit = {

    case class Data(an9: Symbol, loc: Symbol, nbClic: Int)

    val (d1, d2, d3) = (Data('plombier, 'paris, 1), Data('plombier, 'marseille, 2), Data('boulanger, 'marseille, 3))

    val data = Seq(d1,d2,d3)


    val an9 = Select(data.count(_.an9=='plombier).toDouble/3 -> 'plombier, 0.6 -> 'boulanger)

    val loc = If(Flip(0.2), 'marseille, 'paris)
    //If(Flip(0.8), "paris", "marseille")
    //
    val isClicked = CPD(
      arg1 = an9,
      arg2 = loc,
      clauses = ('plombier, 'marseille) -> Flip(0.6),
      ('boulanger, 'paris) -> Flip(0.5),
      ('boulanger, 'marseille) -> Flip(0.55),
      ('plombier, 'paris) -> Flip(0.5)
    )
    //
    //    /* Quelle est la probabilité d'etre cliqué sur mon site ? */
    //    val alg = Importance(1000, isClicked)
    //    alg.start()
    //    Thread.sleep(1000)
    //
    //    alg.stop()
    //    println("proba d'etre clique = " + alg.probability(isClicked, true))
    val mixed = Flip(0.2)
    val mixedBis = If(mixed, true, false)
    val mixedAll = CPD(arg1 = mixed, arg2 = mixedBis, clauses = (true, false) -> Flip(0.9), (false, true) -> Flip(0.1), (true, true) -> Flip(0.2), (false, false) -> Flip(0.9))


    //    an9.observe('boulanger)
    loc.observe('marseille)
    val algoMix = BeliefPropagation(10000, isClicked)
    println(Values()(isClicked))
    algoMix.start()
    println(algoMix.probability(isClicked, true))


  }
}
