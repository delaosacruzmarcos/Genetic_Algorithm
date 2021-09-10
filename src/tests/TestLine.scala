package tests

import genetics.GeneticAlgorithm
import genetics.geometry.{Line,Point}
import org.scalatest.FunSuite

class TestLine extends FunSuite {

  val EPSILON: Double = 0.1

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithm Estimates Linear Regression - Sample Case 1") {
    val point1: Point = new Point(-2.0,1.0)
    val point2: Point = new Point(-1.0,3.0)
    val point3: Point = new Point(0.0,5.0)
    val point4: Point = new Point(1.0,7.0)
    val point5: Point = new Point(2.0,9.0)
    val Points: List[Point] = List(point1,point2,point3,point4,point5)
    val computed: Line = GeneticAlgorithm.geneticAlgorithm(Line.incubator, Line.costFunction(Points), numberOfGenes = 2)
    // y = 2x + 5
     println(computed.toString + " should be y = 2x + 5")
    assert(equalDoubles(2, computed.slope))
    assert(equalDoubles(5, computed.yIntercept))
  }
// the line is represented by two doubles slope and y-intercept
// gene algo will create a double for each
// test that the doubles returned is close to what I expected

  test("Genetic Algorithm Estimates Linear Regression - Sample Case 2") {
    //  Points: (-4.2, 24.957), (-1.2, 13.453), (-0.3, 11.508),
    //  (1.3, 6.088), (2.5, 1.0), (4.0, -3.047), (5.0, -7.839)
    val point1: Point = new Point(-4.2,24.957)
    val point2: Point = new Point(-1.2,13.453)
    val point3: Point = new Point(-0.3,11.508)
    val point4: Point = new Point(1.3,6.088)
    val point5: Point = new Point(2.5,1.0)
    val point6: Point = new Point(4.0,-3.047)
    val point7: Point = new Point(5.0,-7.839)
    val Points: List[Point] = List(point1,point2,point3,point4,point5,point6,point7)
    val regressionLine : Line = Line.estimateCoefficients(Points)
    val computed: Line = GeneticAlgorithm.geneticAlgorithm(Line.incubator, Line.costFunction(Points), numberOfGenes = 2)
    //  To ensure that you have the cost function set up properly,
    //  check that your algorithm finds the best fit line of -3.483x + 10.122
    println(computed.toString + " should be " + regressionLine)
    assert(equalDoubles(-3.483, computed.slope))
    assert(equalDoubles(10.122, computed.yIntercept))
  }

  test("Genetic Algorithm Estimates Linear Regression - Sample Case 3") {
    //  Points: (-10.0, -36.4), (-5.1, -17.3), (0.5, -2.2), (1.7, 2.6), (10.0, 26.6)
    val point1: Point = new Point(-10.0,-36.4)
    val point2: Point = new Point(-5.1,-17.3)
    val point3: Point = new Point(0.5,-2.2)
    val point4: Point = new Point(1.7,2.6)
    val point5: Point = new Point(10.0,26.6)
    val Points: List[Point] = List(point1,point2,point3,point4,point5)
    //  The best fit line is not given for this test case: I calculated y = 3.095x - 3.725
    val regressionLine : Line = Line.estimateCoefficients(Points)
    val computed: Line = GeneticAlgorithm.geneticAlgorithm(Line.incubator, Line.costFunction(Points), numberOfGenes = 2)
    println(computed.toString + " should be " + regressionLine)

    assert(equalDoubles(3.104, computed.slope))
    assert(equalDoubles(-3.540, computed.yIntercept))
  }
  test("Flat Line"){
    val point1: Point = new Point(-10.0, 5.0)
    val point2: Point = new Point(-5.1,5.0)
    val point3: Point = new Point(0.5,5.0)
    val point4: Point = new Point(1.7,5.0)
    val Points: List[Point] = List(point1,point2,point3,point4)
    val regressionLine : Line = Line.estimateCoefficients(Points)
    val computed: Line = GeneticAlgorithm.geneticAlgorithm(Line.incubator, Line.costFunction(Points), numberOfGenes = 2)
    // y = 0x + 5
    println(computed.toString + " should be " + regressionLine)
    assert(equalDoubles(0, computed.slope))
    assert(equalDoubles(5, computed.yIntercept))
  }
}
