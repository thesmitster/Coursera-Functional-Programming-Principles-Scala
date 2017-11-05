package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val su1 = union(s1, s2)
    val su2 = union(s2, s3)
    val suAll = union(su1, su2)
    val i1 = intersect(su1, su2)
    val d1 = diff(su1, s2)
    val f1 = filter(suAll, (x: Int) => x % 2 == 1)
    val m3times = map(suAll, (x: Int) => 3 * x)
  }

  test("singletonSet") {

    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 0), "Broken Singleton")
    }
  }

  test("union") {
    new TestSets {
      assert(contains(su1, 1), "Union 1")
      assert(contains(su1, 2), "Union 2")
      assert(!contains(su1, 3), "Union 3")
    }
  }

  test("intersection") {
    new TestSets {
      assert(contains(i1, 2), "should contain 2")
      assert(!contains(i1, 1), "should not contain 1")
      assert(!contains(i1, 3), "should not contain 3")
    }
  }

  test("difference") {
    new TestSets {
      assert(contains(d1, 1), "should contain 1")
      assert(!contains(d1, 2), "should not contain 2")
    }
  }

  test("filter") {
    new TestSets {
      assert(contains(f1, 1), "should contain 1")
      assert(contains(f1, 3), "should contain 3")
      assert(!contains(f1, 2), "should not contain 2")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(suAll, (x: Int) => x == x), "suAll identity")
      assert(!forall(suAll, (x: Int) => x % 2 == 1), "not all are odd")
      assert(forall(suAll, (x: Int) => x > 0), "all are greater than 0")
      assert(!forall(suAll, (x: Int) => x < 3), "not all are less than 3")
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(suAll, (x: Int) => x < 2), "values less than 2 should exist")
      assert(!exists(suAll, (x: Int) => x < 0), "values less than 0 should not exist")
    }
  }

  test("map") {
    new TestSets {
      assert(contains(m3times, 3), "3 is a member of this mapping")
      assert(contains(m3times, 6), "6 is a member of this mapping")
      assert(contains(m3times, 9), "9 is a member of this mapping")
      assert(!contains(m3times, 1), "1 is not a member of this mapping")
    }
  }
}
