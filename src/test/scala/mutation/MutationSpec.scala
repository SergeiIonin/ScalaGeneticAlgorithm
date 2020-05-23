package mutation

import helper.{PopulationHelperFloat, PopulationHelperInt}
import org.scalatest.{Matchers, WordSpec}

class MutationSpec extends WordSpec with Matchers {

  val lowerBoundInt = -30
  val upperBoundInt = 30
  val variationRate = 35

  val lowerBoundFloat: Float = -30
  val upperBoundFloat: Float = 30
  val percentageMut = 42
  val sizePop = 20


  case class MutationIntTestImpl(variationRate: Int = variationRate)
    extends PopulationHelperInt(percentageMutation = 42, sizePopulation = 20, sampleLen = 6)
      with MutationInt {
    lazy val updBoundsMap = boundsMap.mapValues(v => Bounds(v.lower-(v.lower*variationRate).abs/100,
      v.upper+(v.upper*variationRate).abs/100))
    def checkUpdBounds(population: Population): Unit = {
      population.foreach {
        sample => {
          val geneToIndex = sample.zipWithIndex.toMap
          geneToIndex.foreach(x => {
            val gene = x._1
            val index = x._2
            val bounds: Bounds = updBoundsMap(index)
            assert(isGeneWithinBounds(gene, bounds))
          })
        }
      }
    }
  }

  val mutationIntTestImpl = MutationIntTestImpl()

  s"each gene in the sample lays within the bounds {$lowerBoundInt .. $upperBoundInt " +
    s"in case of MutationInt}" in {
    mutationIntTestImpl.checkBounds(mutationIntTestImpl.population)
  }

  s"each gene in the mutated sample lays within the extended bounds in case of MutationInt" in {
    val mutatedPopulation = mutationIntTestImpl.mutate
      mutatedPopulation.length shouldBe (sizePop*percentageMut)/100
    mutationIntTestImpl.checkUpdBounds(mutatedPopulation)
  }

  case class MutationFloatTestImpl(variationRate: Int = variationRate)
    extends PopulationHelperFloat(percentageMutation = 42, sizePopulation = 20, sampleLen = 6)
      with MutationFloat {
    lazy val updBoundsMap = boundsMap.mapValues(v => Bounds(v.lower-(v.lower*variationRate).abs/100,
      v.upper+(v.upper*variationRate).abs/100))
    def checkUpdBounds(population: Population): Unit = {
      population.foreach {
        sample => {
          val geneToIndex = sample.zipWithIndex.toMap
          geneToIndex.foreach(x => {
            val gene = x._1
            val index = x._2
            val bounds: Bounds = updBoundsMap(index)
            assert(isGeneWithinBounds(gene, bounds))
          })
        }
      }
    }
  }

  val mutationFloatTestImpl = MutationFloatTestImpl()

  s"each gene in the sample lays within the bounds {$lowerBoundFloat .. $upperBoundFloat " +
    s"in case of MutationFloat}" in {
    mutationFloatTestImpl.checkBounds(mutationFloatTestImpl.population)
  }

  s"each gene in the mutated sample lays within the extended bounds in case of MutationFloat" in {
    val mutatedPopulation = mutationFloatTestImpl.mutate
    mutatedPopulation.length shouldBe (sizePop*percentageMut)/100
    mutationFloatTestImpl.checkUpdBounds(mutatedPopulation)
  }



  // todo this test will always success, why???

  /*  "each gene in the sample" should {
      s"be within the bounds {$lowerBound .. $upperBound } " in {
        mutationTestImpl.population.foreach {
          sample => sample.foreach(gene => assert((gene >= (lowerBound+3) && gene <= upperBound)))
        }
      }
    }*/

}
