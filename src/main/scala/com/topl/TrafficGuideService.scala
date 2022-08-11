package com.topl

import scala.annotation.tailrec

class TrafficGuideService(private val trafficData: TrafficData) {

  def findQuickestRoute(start: Intersection, end: Intersection): Option[Route] = {
    @tailrec
    def findQuickestRoute(starts: Set[Intersection], end: Intersection, unvisited: Set[Intersection], visited: Map[Intersection, Route]): Option[Route] = {
      (starts.isEmpty, unvisited.isEmpty) match {
        case (true, true) =>
          // we want to remove the starting time to give us the transit time duration
          visited.get(end).map(r => r.copy(totalTransitTime = r.totalTransitTime - trafficData.startTime))
        case _ =>
          val possibleRoads = starts.flatMap(s => {
            // the visited map contains the shortest route to a particular intersection
            val shortestRoute = visited.getOrElse(s, Route(trafficData.startTime, List()))

            // find the corresponding traffic data for at the time of that shortest route intersection
            val curr = trafficData.getData(shortestRoute.totalTransitTime)

            // get all possible roads leaving out of intersection
            curr.roadsDepartureIntersectionMap.getOrElse(s, List())
          })

          // update the visited map
          val newVisitedMap = possibleRoads.foldLeft(visited)((acc, r) => {
            (acc.get(r.start), acc.get(r.end)) match {
              case (None, _) =>
                acc + (r.start -> Route(trafficData.startTime, List())) // shouldn't occur
              case (Some(shortestFromStartRoute), shortestFromEndRoute) =>
                val newPossibleShortestTime = shortestFromStartRoute.totalTransitTime + r.transitTime
                (shortestFromStartRoute, shortestFromEndRoute) match {
                case (_, None) =>
                  // none existed, so we have found a route to this intersection, add to visited map
                  acc + (r.end -> Route(newPossibleShortestTime, shortestFromStartRoute.path :+ r))
                case (_, Some(v)) if newPossibleShortestTime < v.totalTransitTime =>
                  // found a shorter route
                  acc + (r.end -> Route(newPossibleShortestTime, shortestFromStartRoute.path :+ r))
                case _ =>
                  acc
              }
            }
          })

          // remove the unvisited
          val newUnvisited = starts.foldLeft(unvisited)((acc, s) => acc - s)

          // each possible roads end intersection will be the next starting intersection to
          // find the shortest route if we haven't visited it yet (to avoid cycles)
          val newStarts = possibleRoads
            .map(_.end)
            .filterNot(i => visited.contains(i)) // avoid cycles

          findQuickestRoute(newStarts, end, newUnvisited, newVisitedMap)
      }
    }

    if (start == end) Some(Route(0, List()))
    else findQuickestRoute(Set(start), end, trafficData.allIntersections - start - end, Map(start -> Route(trafficData.startTime, List())))
  }
}
