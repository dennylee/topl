package com.topl

import scala.io.Source
import scala.util.{Failure, Success, Using}
import io.circe.parser._
import io.circe.generic.auto._

object Main {
  def main(args: Array[String]): Unit = {
    require(args.length == 3, "Usage: <traffic data file> <start intersection> <end intersection>")
    val trafficData = ingestTrafficData(args(0))
    val guide = new TrafficGuideService(trafficData)

    val (start, end) = readIntersections(args(1), args(2))
    System.out.println(s"Starting intersection: $start")
    System.out.println(s"Ending intersection:   $end")

    val route = guide.findQuickestRoute(start, end)
    route match {
      case None => System.out.println("No possible quickest route possible.")
      case Some(r) =>
        System.out.println(s"Total (avg) transit time: ${r.totalTransitTime}")
        System.out.println(s"Route:")
        r.path.foreach(road => System.out.println(road))
    }
   }

  def readIntersections(startInput: String, endInput: String): (Intersection, Intersection) = {
    val start = parseIntersection(startInput)
    val end = parseIntersection(endInput)

    (start, end)
  }

  def ingestTrafficData(file: String): TrafficData = {
    val trafficData = Using(Source.fromFile(file)) { bf =>
      System.out.println(s"Ingesting traffic data: $file")
      val data = bf.getLines().mkString
      decode[TrafficData](data)
    }

    trafficData match {
      case Success(Right(data)) => data
      case Success(Left(e)) => throw new Exception("Invalid traffic data.", e)
      case Failure(e) => throw new IllegalArgumentException(s"Unable to read file '$file''", e)
    }
  }

  private def parseIntersection(value: String): Intersection = {
    require(value.contains(','), s"Intersection value must have comma delimiter between avenue and street ($value).  Eg. 'a,1'")

    value match {
      case s"${avenue},${street}" => Intersection(avenue.trim, street.trim)
    }
  }
}
