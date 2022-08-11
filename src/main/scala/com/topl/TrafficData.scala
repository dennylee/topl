package com.topl

import scala.annotation.tailrec

// assumes for each traffic measure, they contain the exact same available road segments
case class TrafficData(private val trafficMeasurements: List[TrafficMeasurement]) {

  // for a given road segment the transit time will be recalculated by averaging the same road segment between
  // adjacent time measurements of TrafficMeasurements
  /*
  Example:

  Time 1:
  [A1] ---(3)---> [A2] ---(10)---> [A3]

  Time 2:
  [A1] ---(1)---> [A2] ---(5)---> [A3]

  Time 3:
  [A1] ---(7)---> [A2] ---(2)---> [A3]

  Recalculate the "average" across the different times for each road segment

  Time 1:
  [A1] ---((3+1)/2)---> [A2] ---((10+5)/2)---> [A3]

  Time 2:
  [A1] ---((1+3+7)/3)---> [A2] ---((10+5+2)/3)---> [A3]

  Time 3:
  [A1] ---((1+7)/2)---> [A2] ---((5+2)/2)---> [A3]

   */
  lazy val data: List[TrafficMeasurement] = {
    @tailrec
    def performAvgCalculation(list: List[TrafficMeasurement], firstIteration: Boolean, acc: List[TrafficMeasurement]): List[TrafficMeasurement] = {
      list match {
        case head :: Nil if firstIteration =>
          // single element
          acc :+ head
        case head :: last :: Nil if firstIteration =>
          // at two elements
          val avgRoadMeasurements = head.measurements.map(rm => {
            val nextRoad = last.roadsByIntersections((rm.toRoad().start, rm.toRoad().end))
            rm.copy(transitTime = (rm.transitTime + nextRoad.transitTime) / 2)
          })
          val newTm = head.copy(measurements = avgRoadMeasurements)
          performAvgCalculation(head :: last :: Nil, false, acc :+ newTm)
        case head :: right :: remain if firstIteration =>
          // at least two elements in first iteration
          val avgRoadMeasurements = head.measurements.map(rm => {
            val nextRoad = right.roadsByIntersections((rm.toRoad().start, rm.toRoad().end))
            rm.copy(transitTime = (rm.transitTime + nextRoad.transitTime) / 2)
          })
          val newTm = head.copy(measurements = avgRoadMeasurements)
          performAvgCalculation(head :: right :: remain, false, acc :+ newTm)
        case left :: curr :: right :: remain =>
          // at least 3 elements after first iteration
          val avgRoadMeasurements = curr.measurements.map(rm => {
            val prevRoad = left.roadsByIntersections((rm.toRoad().start, rm.toRoad().end))
            val nextRoad = right.roadsByIntersections((rm.toRoad().start, rm.toRoad().end))
            rm.copy(transitTime = (prevRoad.transitTime + rm.transitTime + nextRoad.transitTime) / 3)
          })
          val newTm = curr.copy(measurements = avgRoadMeasurements)
          performAvgCalculation(curr :: right :: remain, false, acc :+ newTm)
        case left :: last :: Nil =>
          // last two elements after first iteration
          val avgRoadMeasurements = last.measurements.map(rm => {
            val prevRoad = left.roadsByIntersections((rm.toRoad().start, rm.toRoad().end))
            rm.copy(transitTime = (prevRoad.transitTime + rm.transitTime) / 2)
          })
          val newTm = last.copy(measurements = avgRoadMeasurements)
          acc :+ newTm
      }
    }

    val orderedData = trafficMeasurements.sortBy { tm => tm.measurementTime }(Ordering[Double])
    performAvgCalculation(orderedData, true, List())
  }

  def getData(atTime: Double): TrafficMeasurement = {
    data.find(tm => atTime <= tm.measurementTime) match {
      case Some(entry) => entry
      case None => data.last // assumes there is at least one data (based on existing sample data)
    }
  }

  lazy val allIntersections: Set[Intersection] = {
    trafficMeasurements.flatMap(tm => tm.data.values).flatMap(r => List(r.start, r.end)).toSet
  }

  lazy val startTime = data.minBy(tm => tm.measurementTime).measurementTime // assumes at least one element
}

case class TrafficMeasurement(measurementTime: Double, measurements: List[RoadMeasurement]) {
  lazy val data: Map[(Intersection, Intersection), Road] = measurements.map(rm => {
    val (start, end, road) = rm.data
    (start, end) -> road
  }).toMap

  // a map intersection (key) of its possible roads (value) out of the intersection
  lazy val roadsDepartureIntersectionMap: Map[Intersection, List[Road]] = measurements.map(rm => {
    val (start, _, road) = rm.data
    start -> road
  }).groupBy(_._1)
    .map { case (k,v) => (k,v.map(_._2))}

  // a map intersection (key) of its possible roads (value) into the intersection
  lazy val roadsArrivalIntersectionMap: Map[Intersection, List[Road]]  = measurements.map(rm => {
    val (_, end, road) = rm.data
    end -> road
  }).groupBy(_._1)
    .map { case (k,v) => (k,v.map(_._2))}

  lazy val roadsByIntersections: Map[(Intersection, Intersection), Road] = measurements.map(rm => {
    val road = rm.toRoad()
    (road.start, road.end) -> road
  }).toMap
}

case class RoadMeasurement(startAvenue: String, startStreet: String, transitTime: Double, endAvenue: String, endStreet: String) {
  lazy val data: (Intersection, Intersection, Road) = {
    val road = toRoad()
    (road.start, road.end, road)
  }

  def toRoad(): Road = {
    Road(Intersection(startAvenue, startStreet), Intersection(endAvenue, endStreet), transitTime)
  }
}

case class Intersection(avenue: String, street: String) {
  override def toString: String = s"$avenue$street"
}
case class Road(start: Intersection, end: Intersection, transitTime: Double) {
  override def toString: String = s"$start -> $end (${transitTime})"
}
case class Route(totalTransitTime: Double, path: List[Road])



