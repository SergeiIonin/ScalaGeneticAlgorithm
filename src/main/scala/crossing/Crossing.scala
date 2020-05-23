package crossing

import evolution.Evolution

import scala.annotation.tailrec
import scala.util.Random

trait Crossing[Gene] {
  evolution: Evolution[Gene] =>

  def sampleSize: Int = if (subPopulationCrossing.nonEmpty) population.head.length else 0

  lazy val subPopulationIndexes = List.range(0, subPopulationCrossing.length)

  // todo it's also possible that one should prefer genes of the samples with better fitness function
  lazy val genesEmpty = List.fill(sampleSize)(0)
  def indexOfSamplesPerGene() = genesEmpty.map(_ => Random.between(0, subPopulationCrossing.length))

  lazy val sampleIndexToSample: Map[Int, Sample] = (subPopulationIndexes zip subPopulationCrossing).toMap

  def cross = {
    subPopulationCrossing.map(sample => {
      val indexes = indexOfSamplesPerGene()
      getCrossedSample(sample, indexes)
    }
    )
  }

  private def getCrossedSample(sample: Sample, indexes: List[Int]) = {
    @tailrec
    def iter(indexesIter: List[Int], indexGene: Int, sampleAccum: Sample): Sample = indexesIter match {
      case x::xs => {
        iter(xs, indexGene-1, sampleIndexToSample(x)(indexGene)::sampleAccum)
      }
      case Nil => sampleAccum
    }
    iter(indexes, sampleSize-1, Nil)
  }



}
