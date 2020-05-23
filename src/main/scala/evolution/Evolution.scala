package evolution

import samples.Sample

trait Evolution[Gene] extends Sample[Gene] {
  def percentageMutation: Int
  def percentageCrossing: Int = 100 - percentageMutation
  def sizePopulation: Int
  def population: Population

  lazy val sizeSubPopulationMutation = (sizePopulation*percentageMutation)/100
  lazy val sizeSubPopulationCrossing = sizePopulation - sizeSubPopulationMutation
  def shuffledPopulation: Population

  def subPopulationMutation: Population = shuffledPopulation.take(sizeSubPopulationMutation)

  def subPopulationCrossing: Population = shuffledPopulation.takeRight(sizeSubPopulationCrossing)
}

trait EvolutionDouble extends Evolution[Double]

trait EvolutionFloat extends Evolution[Float]

trait EvolutionInt extends Evolution[Int]
