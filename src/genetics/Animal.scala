package genetics
import scala.util.Random
class Animal[T](genes: List[Double]) {

  val Genes: List[Double] = this.genes

  //takes seed gene and returns another gene and multiplies or divides it by a value 0.0 to 1.0
  def mutationRandom1(seed: Double): Double = {
    if (Random.nextBoolean()) {
      val newGene: Double = (new Random().nextDouble() * 10) + seed
      newGene
    }
    else {
      val newGene: Double = (new Random().nextDouble() * -10) + seed
       newGene
    }
  }

  def mutationRandom2(seed: Double): Double = {
    if (Random.nextBoolean()) {
      val newGene: Double = (new Random().nextDouble() * 1) + seed
      newGene
    }
    else {
      val newGene: Double = (new Random().nextDouble() * -1) + seed
      newGene
    }
  }

  // creates a list of mutated genes from the genes of the animal it was called upon
  def Mutation(): List[Double] ={
    val newGenes: List[Double] = (for (i <- 0 to this.genes.length-1) yield{
      val seed: Double = this.genes(i)
      if (Random.nextBoolean()){
        mutationRandom1(seed)
      }
      else{
        mutationRandom2(seed)
      }
    }).toList
    newGenes
  }

  //Sum up corresponding pairs of genes then divide by two, to get a new gene
  //record each new average gene and add them to a list of the offspring's genes
  //Returns a Animal called offspring that contains offspring genes
  def Crossover(mate: Animal[T]): Animal[T] ={
    val numberOfGenes: Int = mate.Genes.length
    val offspringGenes: List[Double] = (for(gene <- 0 to numberOfGenes-1) yield{
      (mate.Genes(gene) + this.Genes(gene))/2.0
    }).toList
    val offspring: Animal[T] = new Animal (offspringGenes)
    offspring
  }
}
