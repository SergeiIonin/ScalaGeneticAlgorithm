package mutation

import crossing.Crossing
import helper.PopulationHelperInt
import org.scalatest.{Matchers, WordSpec}

class CrossingSpec extends WordSpec with Matchers {

  object CrossingIntTest extends PopulationHelperInt(percentageMutation = 42, sizePopulation = 20,
    sampleLen = 6) with Crossing[Int]

  val crossingIntTest = CrossingIntTest

  s"each gene in the sample lays within the bounds {lowerBoundInt .. upperBoundInt} for each gene" in {
    crossingIntTest.checkBounds(crossingIntTest.population)
  }

  s"each gene in the crossed sample lays within the extended bounds in case of MutationInt" in {
    val crossedPopulation = crossingIntTest.cross
      crossedPopulation.length shouldBe crossingIntTest.sizeSubPopulationCrossing
    val initPopulationSlice = crossingIntTest.population.takeRight(crossingIntTest.sizeSubPopulationCrossing)
    assert(!(crossedPopulation eq  initPopulationSlice))
    crossingIntTest.checkBounds(crossedPopulation)
  }

}
