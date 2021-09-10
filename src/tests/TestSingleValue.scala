package tests

import genetics.GeneticAlgorithm
import genetics.geometry.SingleValue
import org.scalatest.FunSuite

class TestSingleValue extends FunSuite {

  val EPSILON: Double = 0.05

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithm Finds a Single Number 50.0") {
    val hiddenNumber = 50.0
    val numberOfGenes = 1
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), numberOfGenes)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic Algorithm Finds a Single Number 0.0") {
    val hiddenNumber2 = 0.0
    val numberOfGenes2 = 1
    val computed2 = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber2), numberOfGenes2)
    println(computed2.value)
    assert(equalDoubles(hiddenNumber2, computed2.value))
  }
  test("Genetic Algorithm Finds a Single Number -50.0") {
    val hiddenNumber3 = -50.0
    val numberOfGenes3 = 1
    val computed3 = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber3), numberOfGenes3)
    println(computed3.value)
    assert(equalDoubles(hiddenNumber3, computed3.value))
  }
  test("Genetic Algorithm Finds a Single Number -33.33333") {
    val hiddenNumber3 = -33.3333
    val numberOfGenes3 = 1
    val computed3 = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber3), numberOfGenes3)
    println(computed3.value)
    assert(equalDoubles(hiddenNumber3, computed3.value))
  }
}
