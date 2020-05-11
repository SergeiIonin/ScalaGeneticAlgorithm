package evolution

import samples.Sample

import scala.util.Random

trait Evolution[Gene] extends Sample[Gene] {
  def percentageMutation: Int
  def percentageCrossing: Int = 100 - percentageMutation
  def sizePopulation: Int
  def population: Population[Gene]

  val sizeSubPopulationMutation = percentageMutation*sizePopulation/100
  val sizeSubPopulationCrossing = sizePopulation - sizeSubPopulationMutation
  def shuffledPopulation: Population[Gene] //= Random.shuffle(population)

  def subPopulationMutation: Population[Gene] =
    shuffledPopulation.take(sizeSubPopulationMutation)

  def subPopulationCrossing: Population[Gene] =
    shuffledPopulation.takeRight(sizeSubPopulationCrossing)
}


trait EvolutionDouble extends Evolution[Double]

trait EvolutionFloat extends Evolution[Float]

trait EvolutionInt extends Evolution[Int]
