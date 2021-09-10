package genetics
import scala.util.Random
object GeneticAlgorithm {

  // Generates random doubles between -100 and 100 that can be called to create a list of genes
  def starterGene(): Double = {
    // 1.) Start by generating a fixed number of random “animals”
    if (Random.nextBoolean()) {
      val gene: Double = Random.nextDouble() * 100
      gene
    }
    else {
      val gene: Double = Random.nextDouble() * -100
      gene
    }
  }

  // Generates a starter population of 30 animals with the amount of random genes specified by numberOfGenes
  // call starter gene to generate genes
  def starterPopulation[T](numberOfGenes: Int): List[Animal[T]] = {
    val Population: List[Animal[T]] = (for (i <- 0 to 119) yield {
      val listOfGenes: List[Double] = (for (j <- 0 until numberOfGenes) yield {
        starterGene()
      }).toList
      val ancestorAnimal: Animal[T] = new Animal(listOfGenes)
      ancestorAnimal
    }).toList
    Population
  }


  /** geneticHelper
   * the recursive call of the genetic algo function
   * Also a recursive function
   * */

  def geneticHelper[T](incubator: List[Double] => T, costFunction: T => Double,
                       numberOfGenes: Int, Generation: List[Animal[T]], RecursiveDepth: Int): T = {
    // Base case ~ via number of recursions (more recursions more accuracy longer time)
    if (RecursiveDepth == 0) {

      val oldeGeneration: List[Animal[T]] = Generation
      val oldeGenerationIncubated: List[T] = (for (i <- 0 to oldeGeneration.length -1) yield {
        incubator(oldeGeneration(i).Genes)
      }).toList

      // 2.) using the cost function on the list of incubated to get a list of cost values
      val oldGenerationCost: List[Double] = (for (j <- 0 to oldeGenerationIncubated.length -1) yield {
        costFunction(oldeGenerationIncubated(j))
      }).toList

      val bestCost: Double = oldGenerationCost.min
      val bestIndex: Int = oldGenerationCost.indexOf(bestCost)

      val bestAnimal: Animal[T] = oldeGeneration(bestIndex)
      incubator(bestAnimal.Genes)
    }
    //----------------------finding the best mating pair from the given generation ----------------------
    // 1.) Send each animal to the incubator and then the cost function
    else {
      val oldGeneration: List[Animal[T]] = Generation


      val oldGenerationIncubated: List[T] = (for (i <- 0 to 119) yield {
       // println(oldGeneration(i).Genes,i)
        incubator(oldGeneration(i).Genes)
      }).toList


      // 2.) using the cost function on the list of incubated to get a list of cost values
      val oldGenerationCost: List[Double] = (for (j <- 0 to 119) yield {
        costFunction(oldGenerationIncubated(j))
      }).toList


      // 3.) find the index of the smallest 2 cost values, so I can match it with the animals in the oldGeneration
      //   3A.) Sort the oldGenerationCost to find the best two costs (lowest value) and store them in variables
      val maleCost: Double = oldGenerationCost.min
      val femaleCost: Double = oldGenerationCost.sorted.apply(1)
      //   3B.) Use the cost variables to get their indexes from the original cost list
      val maleIndex: Int = oldGenerationCost.indexOf(maleCost)
      val femaleIndex: Int = oldGenerationCost.indexOf(femaleCost)
      //   3D.) Using the indexes get the two animals from the original generation list
      val bestMale: Animal[T] = oldGeneration(maleIndex)
      val bestFemale: Animal[T] = oldGeneration(femaleIndex)

      // ---------------------- crossover and mutation to get new generation ----------------------
      // 1.) cross over the bestMale with the bestFemale create offspring
      val offspring: Animal[T] = bestMale.Crossover(bestFemale)

      // 2.) get a list of each animal mutated 9 times
      val maleMutated: List[Animal[T]] = (for (a <- 0 to 39) yield {
        val mutatedMale: Animal[T] = new Animal(bestMale.Mutation())
        mutatedMale
      }).toList


      val femaleMutated: List[Animal[T]] = (for (b <- 0 to 39) yield {
        val mutatedFemale: Animal[T] = new Animal(bestFemale.Mutation())
        mutatedFemale
      }).toList

      val offspringMutated: List[Animal[T]] = (for (c <- 0 to 39) yield {
        val mutatedOffspring: Animal[T] = new Animal(offspring.Mutation())
        mutatedOffspring
      }).toList


      // 3.) Combining Lists along with best original animals to form new generation
      val females: List[Animal[T]] = bestFemale :: femaleMutated


      val males: List[Animal[T]] = bestMale :: maleMutated


      val offsprings: List[Animal[T]] = offspring :: offspringMutated


      // 3A.) Combining the generations list to make a new generation
      val newGeneration: List[Animal[T]] = females ::: males ::: offsprings
      val newRecursiveDepth: Int = RecursiveDepth - 1


      //---------------------- recursive call on new population ----------------------
      val BestAnimal: T = geneticHelper[T](incubator: List[Double] => T, costFunction: T => Double,
        numberOfGenes: Int, newGeneration, newRecursiveDepth)
      BestAnimal
    }
  }

    /**
     * Uses a genetic algorithm to optimize a generic problem
     *
     * @param incubator     Determines how instances of type T are created from a List of Doubles (genes)
     * @param costFunction  Determines the cost for a given instance of T
     * @param numberOfGenes The size of the List expected by the incubator
     * @tparam T The type to be optimized
     * @return An instance of T with minimal cost
     */


    def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {

      // Creates a starter population from random genes and runs them through the cost function to get generation 1
      val Generation: List[Animal[T]] = starterPopulation(numberOfGenes)
      // Set the RecursiveDepth (number of times through the recursion to be 6 for now)
      val RecursiveDepth: Int = 500
      // The holy grail answer returned :)
      val bestAnimal: T = geneticHelper(incubator: List[Double] => T, costFunction: T => Double,
        numberOfGenes: Int, Generation: List[Animal[T]], RecursiveDepth: Int)
      bestAnimal
    }
  }
