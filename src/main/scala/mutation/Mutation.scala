package mutation

import evolution.Evolution

import scala.util.Random

trait Mutation[Gene] {
  evolution: Evolution[Gene] =>

  def variationRate: Int // value in percent

  def randomness: Any

  def mutate: Population
}

trait MutationInt extends Mutation[Int] {
  evolution: Evolution[Int] =>

  def randomness: Int = Random.between(-1, 2) // note that second param is maxExclusive !!!

  def mutate = subPopulationMutation.map(sample => sample.map(gene => gene + randomness*(gene*variationRate/100)))

}

trait MutationFloat extends Mutation[Float] {
  evolution: Evolution[Float] =>

  def randomness: Float = 1 + Random.between(-variationRate.toFloat/100, variationRate.toFloat/100)

  def mutate = subPopulationMutation.map(sample => sample.map(_*randomness))

}

trait MutationDouble extends Mutation[Double] {
  evolution: Evolution[Double] =>

  def randomness: Double = 1 + Random.between(-variationRate.toFloat/100, variationRate.toFloat/100)

  def mutate = subPopulationMutation.map(sample => sample.map(_*randomness))
}

