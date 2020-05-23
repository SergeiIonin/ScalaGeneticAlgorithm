package helper

import evolution.Evolution
import scala.util.Random

trait PopulationHelper[Gene] extends Evolution[Gene] {
  sealed case class Bounds(lower: Gene, upper: Gene)
  def boundsTuples: List[Bounds]
  def randomGene(bounds: Bounds): Gene
  def percentageMutation: Int //42
  def sizePopulation: Int //20
  def sampleLen: Int //6
  def isGeneWithinBounds(gene: Gene, bounds: Bounds): Boolean

  lazy val boundsMap: Map[Int, Bounds] = boundsTuples.zipWithIndex.map(x => x._2 -> x._1).toMap

  lazy val samplesIndexes = List.range(0, sizePopulation)
  lazy val genesIndexes = List.range(0, sampleLen)
  override lazy val population: List[List[Gene]] =
    samplesIndexes
      .map(_ => genesIndexes
        .map(geneIndex => {
          val bounds = boundsMap(geneIndex)
          randomGene(bounds)
        }))
  lazy val shuffledIndexes = Random.shuffle(samplesIndexes)
  override lazy val shuffledPopulation: Population = shuffledIndexes.map(population(_))
  def checkBounds(population: Population) = {
    population.foreach {
      sample => {
        val geneToIndex = sample.zipWithIndex.toMap
        geneToIndex.foreach(x => {
          val gene = x._1
          val index = x._2
          val bounds: Bounds = boundsMap(index)
          assert(isGeneWithinBounds(gene, bounds))
        })
      }
    }
  }

}

abstract class PopulationHelperInt(val percentageMutation: Int, val sizePopulation: Int,
    val sampleLen: Int) extends PopulationHelper[Int] {
  override val boundsTuples: List[Bounds] =
    List(Bounds(0, 9), Bounds(10, 19),
      Bounds(20, 29), Bounds(30, 39),
      Bounds(40, 49), Bounds(50, 59))

  override def randomGene(bounds: Bounds): Int = Random.between(bounds.lower, bounds.upper)
  override def isGeneWithinBounds(gene: Int, bounds: Bounds): Boolean =
    gene >= bounds.lower && gene <= bounds.upper

}

abstract class PopulationHelperFloat(val percentageMutation: Int, val sizePopulation: Int,
    val sampleLen: Int) extends PopulationHelper[Float] {
  override val boundsTuples: List[Bounds] =
    List(Bounds(0F, 9F), Bounds(10F, 19F),
      Bounds(20F, 29F), Bounds(30F, 39F),
      Bounds(40F, 49F), Bounds(50F, 59F))

  override def randomGene(bounds: Bounds): Float = Random.between(bounds.lower, bounds.upper)
  override def isGeneWithinBounds(gene: Float, bounds: Bounds): Boolean =
    gene >= bounds.lower && gene <= bounds.upper
}
