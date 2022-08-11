package com.topl

import org.scalatest.funsuite.AnyFunSuite

class TrafficDataSpec extends AnyFunSuite{

  // test avg calculation algorithm

  test("when single traffic measurement will not perform avg traffic measurement calculation") {
    val a1_a2 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1 = RoadMeasurement("a", "1", 9, "b", "1")
    val a2_b2 = RoadMeasurement("a", "2", 10, "b", "2")
    val b1_b2 = RoadMeasurement("b", "1", 1, "b", "2")
    val tm = TrafficMeasurement(6, List(a1_a2, a1_b1, a2_b2, b1_b2))
    val td = TrafficData(List(tm))

    assertResult(List(tm))(td.data)
  }

  test("when two traffic measurement will perform the avg traffic measurement calculation") {
    val a1_a2_0 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1_0 = RoadMeasurement("a", "1", 9, "b", "1")
    val a2_b2_0 = RoadMeasurement("a", "2", 10, "b", "2")
    val b1_b2_0 = RoadMeasurement("b", "1", 1, "b", "2")
    val tm_0 = TrafficMeasurement(6, List(a1_a2_0, a1_b1_0, a2_b2_0, b1_b2_0))

    val a1_a2_1 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1_1 = RoadMeasurement("a", "1", 1, "b", "1")
    val a2_b2_1 = RoadMeasurement("a", "2", 20, "b", "2")
    val b1_b2_1 = RoadMeasurement("b", "1", 11, "b", "2")
    val tm_1 = TrafficMeasurement(60, List(a1_a2_1, a1_b1_1, a2_b2_1, b1_b2_1))
    val td = TrafficData(List(tm_0, tm_1))

    // expected
    val a1_a2_0e = RoadMeasurement("a", "1", (a1_a2_0.transitTime + a1_a2_1.transitTime) / 2, "a", "2")
    val a1_b1_0e = RoadMeasurement("a", "1", (a1_b1_0.transitTime + a1_b1_1.transitTime) / 2, "b", "1")
    val a2_b2_0e = RoadMeasurement("a", "2", (a2_b2_0.transitTime + a2_b2_1.transitTime) / 2, "b", "2")
    val b1_b2_0e = RoadMeasurement("b", "1", (b1_b2_0.transitTime + b1_b2_1.transitTime) / 2, "b", "2")
    val tm_0e = TrafficMeasurement(6, List(a1_a2_0e, a1_b1_0e, a2_b2_0e, b1_b2_0e))

    val a1_a2_1e = RoadMeasurement("a", "1", (a1_a2_1.transitTime + a1_a2_0.transitTime) / 2, "a", "2")
    val a1_b1_1e = RoadMeasurement("a", "1", (a1_b1_1.transitTime + a1_b1_0.transitTime) / 2, "b", "1")
    val a2_b2_1e = RoadMeasurement("a", "2", (a2_b2_1.transitTime + a2_b2_0.transitTime) / 2, "b", "2")
    val b1_b2_1e = RoadMeasurement("b", "1", (b1_b2_1.transitTime + b1_b2_0.transitTime) / 2, "b", "2")
    val tm_1e = TrafficMeasurement(60, List(a1_a2_1e, a1_b1_1e, a2_b2_1e, b1_b2_1e))

    assertResult(List(tm_0e, tm_1e))(td.data)
  }

  test("when three traffic measurement will perform the avg traffic measurement calculation") {
    val a1_a2_0 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1_0 = RoadMeasurement("a", "1", 9, "b", "1")
    val a2_b2_0 = RoadMeasurement("a", "2", 10, "b", "2")
    val b1_b2_0 = RoadMeasurement("b", "1", 1, "b", "2")
    val tm_0 = TrafficMeasurement(6, List(a1_a2_0, a1_b1_0, a2_b2_0, b1_b2_0))

    val a1_a2_1 = RoadMeasurement("a", "1", 8, "a", "2")
    val a1_b1_1 = RoadMeasurement("a", "1", 10, "b", "1")
    val a2_b2_1 = RoadMeasurement("a", "2", 2, "b", "2")
    val b1_b2_1 = RoadMeasurement("b", "1", 1, "b", "2")
    val tm_1 = TrafficMeasurement(60, List(a1_a2_1, a1_b1_1, a2_b2_1, b1_b2_1))

    val a1_a2_2 = RoadMeasurement("a", "1", 5, "a", "2")
    val a1_b1_2 = RoadMeasurement("a", "1", 1, "b", "1")
    val a2_b2_2 = RoadMeasurement("a", "2", 20, "b", "2")
    val b1_b2_2 = RoadMeasurement("b", "1", 11, "b", "2")
    val tm_2 = TrafficMeasurement(90, List(a1_a2_2, a1_b1_2, a2_b2_2, b1_b2_2))
    val td = TrafficData(List(tm_0, tm_1, tm_2))

    // expected
    val a1_a2_0e = RoadMeasurement("a", "1", (a1_a2_0.transitTime + a1_a2_1.transitTime) / 2, "a", "2")
    val a1_b1_0e = RoadMeasurement("a", "1", (a1_b1_0.transitTime + a1_b1_1.transitTime) / 2, "b", "1")
    val a2_b2_0e = RoadMeasurement("a", "2", (a2_b2_0.transitTime + a2_b2_1.transitTime) / 2, "b", "2")
    val b1_b2_0e = RoadMeasurement("b", "1", (b1_b2_0.transitTime + b1_b2_1.transitTime) / 2, "b", "2")
    val tm_0e = TrafficMeasurement(6, List(a1_a2_0e, a1_b1_0e, a2_b2_0e, b1_b2_0e))

    val a1_a2_1e = RoadMeasurement("a", "1", (a1_a2_0.transitTime + a1_a2_1.transitTime + a1_a2_2.transitTime) / 3, "a", "2")
    val a1_b1_1e = RoadMeasurement("a", "1", (a1_b1_0.transitTime + a1_b1_1.transitTime + a1_b1_2.transitTime) / 3, "b", "1")
    val a2_b2_1e = RoadMeasurement("a", "2", (a2_b2_0.transitTime + a2_b2_1.transitTime + a2_b2_2.transitTime) / 3, "b", "2")
    val b1_b2_1e = RoadMeasurement("b", "1", (b1_b2_0.transitTime + b1_b2_1.transitTime + b1_b2_2.transitTime) / 3, "b", "2")
    val tm_1e = TrafficMeasurement(60, List(a1_a2_1e, a1_b1_1e, a2_b2_1e, b1_b2_1e))

    val a1_a2_2e = RoadMeasurement("a", "1", (a1_a2_1.transitTime + a1_a2_2.transitTime) / 2, "a", "2")
    val a1_b1_2e = RoadMeasurement("a", "1", (a1_b1_1.transitTime + a1_b1_2.transitTime) / 2, "b", "1")
    val a2_b2_2e = RoadMeasurement("a", "2", (a2_b2_1.transitTime + a2_b2_2.transitTime) / 2, "b", "2")
    val b1_b2_2e = RoadMeasurement("b", "1", (b1_b2_1.transitTime + b1_b2_2.transitTime) / 2, "b", "2")
    val tm_2e = TrafficMeasurement(90, List(a1_a2_2e, a1_b1_2e, a2_b2_2e, b1_b2_2e))

    assertResult(List(tm_0e, tm_1e, tm_2e))(td.data)
  }
}
