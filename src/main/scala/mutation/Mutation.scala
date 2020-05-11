package mutation

import akka.actor.{Actor, ActorLogging}
import evolution.{Evolution, EvolutionDouble, EvolutionFloat, EvolutionInt}

import scala.util.Random

trait Mutation[Gene] {
  evolution: Evolution[Gene] =>

  def variationRate: Float // value in percent

  def randomnessFloat: Float = 1 + Random.between(-variationRate/100, variationRate/100)
  def randomnessDouble: Double = 1 + Random.between(-variationRate/100, variationRate/100)
  def randomnessInt: Int = 1 + Random.between(-variationRate.toInt/100, variationRate.toInt/100)

  def mutate: Population[AnyVal] = evolution match {
    case _: EvolutionInt => mutateInt(subPopulationMutation.asInstanceOf[Population[Int]])
    case _: EvolutionFloat => mutateFloat(subPopulationMutation.asInstanceOf[Population[Float]])
    case _: EvolutionDouble => mutateDouble(subPopulationMutation.asInstanceOf[Population[Double]])
    case _ => throw new IllegalArgumentException("Attempt to try mutation with unimplemented type")
  }

  private def mutateInt(population: Population[Int]) =
    for {
      sample <- population
      gene <- sample
    } yield List(gene * randomnessInt)

  private def mutateFloat(population: Population[Float]) =
    for {
      sample <- population
      gene <- sample
    } yield List(gene * randomnessFloat)

  private def mutateDouble(population: Population[Double]) =
    for {
      sample <- population
      gene <- sample
    } yield List(gene * randomnessDouble)


}


