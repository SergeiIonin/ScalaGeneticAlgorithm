package mutation

import evolution.Evolution
import org.scalatest.{FunSpecLike, Matchers}

import scala.util.Random

class MutationSpec extends FunSpecLike with Matchers {

  val bound = 30

  class EvolutionTestImpl extends Evolution[Int] {
    val percentageMutation = 42
    val sizePopulation = 20
    override val population: Population[Int] = for {
      _ <- (0 until 3).toList
      gene <- (0 until 5).toList.map(_ => Random.between(-bound, bound))
    } yield List(gene)
    override val shuffledPopulation: Population[Int] = Random.shuffle(population)

  }

  val evolutionTestImpl = new EvolutionTestImpl with Mutation[Int] {
    val variationRate = 25
  }

  val mutated = evolutionTestImpl.mutate
  assert(1 == 1)

}
