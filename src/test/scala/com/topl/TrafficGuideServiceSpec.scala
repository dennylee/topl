package com.topl

import org.scalatest.funsuite.AnyFunSuite

class TrafficGuideServiceSpec extends AnyFunSuite {

  // test shortest route algorithm

  test("when no possible paths to intersection will return Option.empty") {
    val a1_a2 = RoadMeasurement("a", "1", 5, "a", "2")
    val tm = TrafficMeasurement(6, List(a1_a2))
    val td = TrafficData(List(tm))
    val start = Intersection("a", "1")
    val end = Intersection("z", "99")

    val guide = new TrafficGuideService(td)
    val results = guide.findQuickestRoute(start, end)

    assertResult(Option.empty)(results)
  }

  test("when start intersection == end intersection will have 0 transit time and no route paths") {
    val td = TrafficData(List())
    val start = Intersection("a", "1")
    val end = Intersection("a", "1")

    val guide = new TrafficGuideService(td)
    val route = guide.findQuickestRoute(start, end)

    assertResult(0)(route.get.totalTransitTime)
    assertResult(true)(route.get.path.isEmpty)
  }

  test("when only one road path will take that path") {
    val a1_a2 = RoadMeasurement("a", "1", 5, "a", "2")
    val tm = TrafficMeasurement(6, List(a1_a2))
    val td = TrafficData(List(tm))
    val start = Intersection("a", "1")
    val end = Intersection("a", "2")

    val guide = new TrafficGuideService(td)
    val route = guide.findQuickestRoute(start, end)

    assertResult(5)(route.get.totalTransitTime)
    assertResult(List(a1_a2.toRoad()))(route.get.path)
  }

  test("when two possible routes will take the correct quickest route") {
    val a1_a2 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1 = RoadMeasurement("a", "1", 9, "b", "1")
    val a2_b2 = RoadMeasurement("a", "2", 10, "b", "2")
    val b1_b2 = RoadMeasurement("b", "1", 1, "b", "2")
    val tm = TrafficMeasurement(0, List(a1_a2, a1_b1, a2_b2, b1_b2))
    val td = TrafficData(List(tm))
    val start = Intersection("a", "1")
    val end = Intersection("b", "2")

    val guide = new TrafficGuideService(td)
    val route = guide.findQuickestRoute(start, end)

    assertResult(10)(route.get.totalTransitTime)
    assertResult(List(
      a1_b1.toRoad(),
      b1_b2.toRoad()
    ))(route.get.path)
  }

  test("when traffic data has a cycle, then it should pick the quickest route that doesn't contain the cycle") {
    val a2_a1 = RoadMeasurement("a", "2", 1, "a", "1")
    val b1_b2 = RoadMeasurement("b", "1", 1, "b", "2")
    val c1_c2 = RoadMeasurement("c", "1", 4, "c", "2")
    val a1_b1 = RoadMeasurement("a", "1", 1, "b", "1")
    val b2_a2 = RoadMeasurement("b", "2", 1, "a", "2")
    val b1_c1 = RoadMeasurement("b", "1", 1, "c", "1")
    val b2_c2 = RoadMeasurement("b", "2", 3, "c", "2")
    val tm = TrafficMeasurement(100, List(a2_a1, b1_b2, c1_c2, a1_b1, b2_a2, b1_c1, b2_c2))

    val td = TrafficData(List(tm))
    val start = Intersection("a", "1")
    val end = Intersection("c", "2")

    val guide = new TrafficGuideService(td)
    val route = guide.findQuickestRoute(start, end)

    assertResult(5)(route.get.totalTransitTime)
    assertResult(List(
      a1_b1.toRoad(),
      b1_b2.toRoad(),
      b2_c2.toRoad()
    ))(route.get.path)
  }

  // test with different time measurement

  test("when a single route with different time measurement") {
    val a1_a2_0 = RoadMeasurement("a", "1", 5, "a", "2")
    val a2_a3_0 = RoadMeasurement("a", "2", 2, "a", "3")
    val a3_a4_0 = RoadMeasurement("a", "3", 8, "a", "4")
    val tm0 = TrafficMeasurement(0, List(a1_a2_0, a2_a3_0, a3_a4_0))

    val a1_a2_1 = RoadMeasurement("a", "1", 1, "a", "2")
    val a2_a3_1 = RoadMeasurement("a", "2", 5, "a", "3")
    val a3_a4_1 = RoadMeasurement("a", "3", 10, "a", "4")
    val tm1 = TrafficMeasurement(3, List(a1_a2_1, a2_a3_1, a3_a4_1))

    val a1_a2_2 = RoadMeasurement("a", "1", 15, "a", "2")
    val a2_a3_2 = RoadMeasurement("a", "2", 1, "a", "3")
    val a3_a4_2 = RoadMeasurement("a", "3", 1, "a", "4")
    val tm2 = TrafficMeasurement(7, List(a1_a2_2, a2_a3_2, a3_a4_2))

    val td = TrafficData(List(tm0, tm1, tm2))
    val start = Intersection("a", "1")
    val end = Intersection("a", "4")

    val guide = new TrafficGuideService(td)
    val route = guide.findQuickestRoute(start, end)

    assertResult(3d + (8d/3d) + (11d/2d))(route.get.totalTransitTime)
    assertResult(List(
      a1_a2_0.toRoad().copy(transitTime = 3d),
      a2_a3_1.toRoad().copy(transitTime = 8d/3d),
      a3_a4_2.toRoad().copy(transitTime = 11d/2d)
    ))(route.get.path)
  }

  test("when there are two distinct quickest route will take either routes") {
    val a1_a2_0 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1_0 = RoadMeasurement("a", "1", 1, "b", "1")
    val a2_b2_0 = RoadMeasurement("a", "2", 10, "b", "2")
    val b1_b2_0 = RoadMeasurement("b", "1", 10, "b", "2")
    val tm0 = TrafficMeasurement(1, List(a1_a2_0, a1_b1_0, a2_b2_0, b1_b2_0))

    val a1_a2_1 = RoadMeasurement("a", "1", 10, "a", "2")
    val a1_b1_1 = RoadMeasurement("a", "1", 10, "b", "1")
    val a2_b2_1 = RoadMeasurement("a", "2", 1, "b", "2")
    val b1_b2_1 = RoadMeasurement("b", "1", 5, "b", "2")
    val tm1 = TrafficMeasurement(100, List(a1_a2_1, a1_b1_1, a2_b2_1, b1_b2_1)) // regardless which path is taken, the time travel will have to take this data set

    val td = TrafficData(List(tm0, tm1))
    val start = Intersection("a", "1")
    val end = Intersection("b", "2")

    val guide = new TrafficGuideService(td)
    val route = guide.findQuickestRoute(start, end)

    // even though it'll always pick the same one, but just incase if the algorithm is changed to run with parallelism,
    // then results can vary, so to handle for that it'll check answer to be either
    assertResult(13)(route.get.totalTransitTime)
    assert(route.get.path == List(
      a1_a2_0.toRoad().copy(transitTime = (a1_a2_0.transitTime + a1_a2_1.transitTime)/2d),
      a2_b2_1.toRoad().copy(transitTime = (a2_b2_0.transitTime + a2_b2_1.transitTime)/2d)
    ) ||
      route.get.path == List(
        a1_b1_0.toRoad().copy(transitTime = (a1_b1_0.transitTime + a1_b1_1.transitTime)/2d)),
      b1_b2_1.toRoad().copy(transitTime = (b1_b2_0.transitTime + b1_b2_1.transitTime)/2d)
    )
  }

  test("when two possible routes with changing road measurement time will take the correct quickest route") {
    val a1_a2_0 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1_0 = RoadMeasurement("a", "1", 9, "b", "1")
    val a2_b2_0 = RoadMeasurement("a", "2", 10, "b", "2")
    val b1_b2_0 = RoadMeasurement("b", "1", 1, "b", "2")
    val tm0 = TrafficMeasurement(3, List(a1_a2_0, a1_b1_0, a2_b2_0, b1_b2_0))

    // which ever the first road taken, it'll have to use the next set of traffic data
    val a1_a2_1 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1_1 = RoadMeasurement("a", "1", 9, "b", "1")
    val a2_b2_1 = RoadMeasurement("a", "2", 1, "b", "2")
    val b1_b2_1 = RoadMeasurement("b", "1", 20, "b", "2")
    val tm1 = TrafficMeasurement(10, List(a1_a2_1, a1_b1_1, a2_b2_1, b1_b2_1))

    val td = TrafficData(List(tm0, tm1))

    val start = Intersection("a", "1")
    val end = Intersection("b", "2")

    val guide = new TrafficGuideService(td)
    val route = guide.findQuickestRoute(start, end)

    assertResult(10.5)(route.get.totalTransitTime)
    assertResult(List(
      a1_a2_0.toRoad().copy(transitTime = (a1_a2_0.transitTime + a1_a2_1.transitTime)/2d),
      a2_b2_1.toRoad().copy(transitTime = (a2_b2_0.transitTime + a2_b2_1.transitTime)/2d)
    ))(route.get.path)
  }

}
