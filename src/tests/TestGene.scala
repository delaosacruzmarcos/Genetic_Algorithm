package tests
import org.scalatest._
import genetics.Animal
import genetics.GeneticAlgorithm
class TestGene extends FunSuite {

  test("Test for random number generation"){
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
    println(GeneticAlgorithm.starterGene())
  }

  test("Testing the started population one gene"){
    val oneGenePopulation: List[Animal[Any]] = GeneticAlgorithm.starterPopulation(1)
    val individualOne: Unit = (for(j <- 0 to oneGenePopulation.length-1)
      print(oneGenePopulation(j).Genes))
  }
  test("Testing the started population two gene"){
    val twoGenePopulation: List[Animal[Any]] = GeneticAlgorithm.starterPopulation(2)
    val individualTwo: Unit = (for(j <- 0 to twoGenePopulation.length-1)
      print(twoGenePopulation(j).Genes))
  }
  test("Testing the started population three gene"){
    val threeGenePopulation: List[Animal[Any]] = GeneticAlgorithm.starterPopulation(3)
    val individualThree: Unit = (for(j <- 0 to threeGenePopulation.length-1)
      print(threeGenePopulation(j).Genes))
  }

  test("Cross over function one gene"){
    val Genes1: List[Double] = List(6)
    val Genes2: List[Double] = List(4)
    val Animal1: Animal[Any] = new Animal(Genes1)
    val Animal2: Animal[Any] = new Animal(Genes2)

    val offspring3: Animal[Any] = Animal1.Crossover(Animal2)
    assert(offspring3.Genes == List(5))
  }

  test("Cross over function two gene"){
    val Genes1: List[Double] = List(6,5)
    val Genes2: List[Double] = List(4,7)
    val Animal1: Animal[Any] = new Animal(Genes1)
    val Animal2: Animal[Any] = new Animal(Genes2)

    val offspring3: Animal[Any] = Animal1.Crossover(Animal2)
    assert(offspring3.Genes == List(5,6))
  }

  test("Cross over function three gene"){
    val Genes1: List[Double] = List(6,-6,0)
    val Genes2: List[Double] = List(4,-2,0.4)
    val Animal1: Animal[Any] = new Animal(Genes1)
    val Animal2: Animal[Any] = new Animal(Genes2)

    val offspring3: Animal[Any] = Animal1.Crossover(Animal2)
    assert(offspring3.Genes == List(5,-4,0.2))
  }

}
