import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Constant, Flip}
import com.cra.figaro.library.compound.CPD

/**
  * Created by zarour on 22/08/2017.
  */
object Scalaio {

  def main(args: Array[String]): Unit = {
    val clic = Flip(0.3)
    val photo = Flip(0.8)

    val called = CPD(clic, photo,
      (true, true) -> Flip(0.8),
      (true, false) -> Flip(0.6),
      (false, true) -> Flip(0.001), //referencement google
      (false, false) -> Constant(false))

    val alg = Importance(1000, called)

    photo.observe(true)
    alg.start()
    Thread.sleep(1000)
    println(alg.probability(called, true))
    alg.kill()
  }
}
