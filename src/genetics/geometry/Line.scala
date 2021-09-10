package genetics.geometry


object Line {

//--------------------------------Finding the linear regression equation----------------------------------

  // Getting all the x values
  def getX(points: List[Point]): List[Double] = {
    val x: List[Double] = (for (i <- 0 to points.length - 1) yield {
      points(i).x
    }).toList
    x
  }

  // Finding average x
  def meanX(x:List[Double]): Double = {
    val lengthX: Double = x.length.toDouble
    val mean: Double = x.sum / lengthX
    mean
  }


    // Getting all the y values
  def getY(points: List[Point]): List[Double] = {
    val y: List[Double] = (for (i <- 0 to points.length-1) yield{
      points(i).y
    }).toList
    y
  }
  // Finding average y
  def meanY(y:List[Double]): Double = {
    val lengthY: Double = y.length.toDouble
    val mean: Double = y.sum / lengthY
    mean
  }

  // Calculating cross deviation
  def crossDeviation(x: List[Double], y: List[Double], meanX: Double, meanY: Double): Double ={
    val SumXtimesY: Double = (for (i <- 0 to x.length - 1) yield {
      val Multi: Double = x(i) * y(i)
      Multi
    }).toList.sum
    val SumMean: Double = x.length.toDouble * meanX * meanY
    val cross: Double = SumXtimesY -SumMean
    cross
  }
  // Calculating deviation about x
  def deviationOfX(x: List[Double], meanX: Double): Double = {
    val SumXtimesX: Double = (for (i <- 0 to x.length - 1) yield {
      val Multi: Double = x(i) * x(i)
      Multi
    }).toList.sum
    val SumMean: Double = x.length.toDouble * meanX * meanX
    val cross: Double = SumXtimesX -SumMean
    cross
  }


  // calculating slope b_1
  def calcSlope(x: List[Double], y: List[Double], meanX: Double, meanY: Double): Double = {
    val SS_xy: Double = crossDeviation(x,y,meanX,meanY)
    val SS_xx: Double  = deviationOfX(x,meanX)
    val ret: Double = SS_xy / SS_xx
    ret
  }
  // calculating y-intercept b_0
  def calcIntercept(x: List[Double], y: List[Double], meanX: Double, meanY: Double): Double = {
    val b1: Double = calcSlope(x,y,meanX,meanY)
    val ret: Double = meanY - b1*meanX
    ret
  }


  def estimateCoefficients(points: List[Point]): Line = {
    val xlist: List[Double] = getX(points)
    val ylist: List[Double] = getY(points)
    val Xmean: Double = meanX(xlist)
    val Ymean: Double = meanY(ylist)
    val crossDev: Double = crossDeviation(xlist,ylist,Xmean,Ymean)
    val devX: Double = deviationOfX(xlist,Xmean)
    val Slope: Double = calcSlope(xlist,ylist,Xmean,Ymean)
    val Intercept: Double = calcIntercept(xlist,ylist,Xmean,Ymean)
    val retLine: Line = new Line(Slope,Intercept)
    retLine
  }

  // Calculate the distance the line values vary
  def costCalc(points: List[Point], guessedline: Line): Double = {
    val regressionLine: Line = estimateCoefficients(points)
    val costSlope: Double = math.abs(regressionLine.slope - guessedline.slope)
    val costInter: Double = math.abs(regressionLine.yIntercept - guessedline.yIntercept)
    val cost: Double = costSlope + costInter
    cost
  }

  def costFunction(points: List[Point]): (Line) => Double = {
    (Checkline: Line) => {
      costCalc(points,Checkline)
    }
  }


//--------------------------------Used to calculate the slope and y Intercept-----------------------------------------------

  //Takes the x values of a list of points and x2-x1 to get x value for bottom of the slope
  def slopeFriendlyX(points: List[Point]): List[Double] = {
    val lengthOfPoints: Int = points.length
    val pointX: List[Double] = (for (i <- 0 to lengthOfPoints-2) yield {
      val singlePointX1: Double = points(i).x
      val singlePointX2: Double = points(i+1).x
      val x: Double = singlePointX2 - singlePointX1
      x
    }).toList
    pointX
  }

  //Takes the y values of a list of points and y2-y1 to get a y value for the top of the slope
  def slopeFriendly(points: List[Point]): List[Double] = {
    val lengthOfPoints: Int = points.length
    val pointY: List[Double] = (for (j <- 0 to lengthOfPoints-2) yield {
      val singlePointY1: Double = points(j).y
      val singlePointY2: Double = points(j+1).y
      val y: Double = singlePointY2 - singlePointY1
      y
    }).toList
    pointY
  }



//----------------------------------------slope and y Intercept--------------------------------

  // Takes a list of points and finds the slope(average)
//  def findSlope(points: List[Point]): Double ={
//    val pointY: List[Double] = slopeFriendly(points)
//    val pointX: List[Double] = slopeFriendlyX(points)
//    val lengthOfPoints: Int = points.length
//    val slopes: List[Double] = (for(k <- 0 to pointY - 1) yield {
//      val slope: Double = pointY(k) / pointX(k)
//      slope
//    }).toList
//    val averageSlope: Double = slopes.sum / lengthOfPoints.toDouble
//    averageSlope
//  }

  // Takes a list of points and finds the y intercept using the slope from the findSlope method
//  def findYIntercept(points: List[Point]): Double ={
//    val slope: Double = findSlope(points)
//    val lengthOfPoints: Int = points.length
//    val bs: List[Double] = (for(l <- 0 to lengthOfPoints-1) yield {
//      val b: Double = points(l).y - (slope * points(l).x)
//      b
//    }).toList
//    val YIntercept: Double = bs.sum / bs.length
//    YIntercept
//  }
//
//  // -----------------------------Correct Line Generator-----------------------------------------------
//  // Creates a line given by the slope and the yIntercept of a particular list of points
//
//  def lineGenerator(points: List[Point]): Line = {
//    val slope: Double = findSlope(points)
//    val YIntercept: Double = findYIntercept(points)
//    val correctLine: Line = new Line(slope,YIntercept)
//************************************************************************************************
//    println("This is the correctLine generated " + correctLine.toString)
//      correctLine
//  }

  // -----------------------------Used to calculate the Cost-----------------------------------------------
  // Used to find the distance between the line at it's closest point and the point given
  // Used to find the closest point on the line

  //used to find the line tangent to the given line. Using the given point I am testing
  def tangentLine (line: Line, givenPoint: Point): Line = {
   val tangentSlope: Double = -(1 / line.slope)
   val reciprocal: Double = 1/line.slope
   val Intercept: Double = givenPoint.y + (reciprocal * givenPoint.x)
   val tangentLine: Line = new Line(tangentSlope,Intercept)
    tangentLine
  }

  def pointOfIntersection(line:Line, tangentLine: Line): Point = {
    val tanSlope: Double = tangentLine.slope
    val tanIntercept: Double = tangentLine.yIntercept
    val lineSlope: Double = line.slope
    val lineIntercept: Double = line.yIntercept
    val b: Double = tanIntercept - lineIntercept
    val m: Double = lineSlope / tanSlope
    val pointX: Double = b / m
    val pointY: Double = (lineSlope * pointX) + lineIntercept
    val retPoint: Point = new Point(pointX,pointY)
    retPoint
  }

  //used to find the distance between the closest point on the line and the point given
  // Applies the distance formula
  def perpendicularDistance(pointOfIntersection: Point, givenPoint: Point): Double = {
    val lX: Double = pointOfIntersection.x
    val lY: Double = pointOfIntersection.y
    val gX: Double = givenPoint.x
    val gY: Double = givenPoint.y
 //  Using the Distance formula
    val distance: Double = Math.sqrt(math.pow(gX-lX,2) + math.pow(gY-lY,2) * 1.0)
    distance
  }



//-----------------------------Funtions used to simplify cost function-----------------------------------------------

  def distanceToLine(givenLine: Line, points: List[Point]): Double = {
    val costList: List[Double] = (for (i <- 0 to points.length-1) yield {
    val tangentLinei: Line = Line.tangentLine(givenLine, points(i))
    val pointOfIntersectioni: Point = pointOfIntersection(givenLine, tangentLinei)
    val costOnePoint: Double = perpendicularDistance(pointOfIntersectioni, points(i))
      costOnePoint
  }).toList
    costList.sum
  }





  def incubator(genes: List[Double]): Line = {
    val slope: Double = genes.head
    val yIntercept: Double = genes(1)
    val newLine: Line = new Line(slope,yIntercept)
    newLine
  }

}


class Line(val slope: Double, val yIntercept: Double) {
//set string equal to the string to check the output for testing
  override def toString: String = {
    f"($slope%1.3f*x + $yIntercept%1.3f)"
  }

}
