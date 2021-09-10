package tests
import org.scalatest._
import genetics.geometry._
class TestCost extends FunSuite{


  val EPSILON: Double = 0.05

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Testing Incubator"){
    val genes: List[Double] = List(2,4)
    val incIn: Line = Line.incubator(genes)
    val incout: Line = new Line(2,4)
    assert(incIn.slope==incout.slope)
    assert(incIn.yIntercept==incout.yIntercept)
  }
  test(" Testing the getX and getY methods"){
    val point1: Point = new Point(-2.0,1.0)
    val point2: Point = new Point(-1.0,3.0)
    val point3: Point = new Point(0.0,5.0)
    val point4: Point = new Point(1.0,7.0)
    val point5: Point = new Point(2.0,9.0)
    val Points: List[Point] = List(point1,point2,point3,point4,point5)
    assert(Line.getX(Points) == List(-2.0,-1.0,0.0,1.0,2.0))
    assert(Line.getY(Points) == List(1.0,3.0,5.0,7.0,9.0))
  }
}
