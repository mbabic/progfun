package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {
  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = (x:Int) => (x > 2)

  }

  test("Singleton set tests.") {
    new TestSets {
      assert(contains(s1, 1), "Singleton set should contain expected element")
      assert(contains(s2, 2), "Singleton set should contain expected element")
      assert(contains(s3, 3), "Singleton set should contain expected element")
      assert(!contains(s1, 2), "contains should return false for element not " + 
          "in singleton set.")
      assert(!contains(s1, 3), "contains should return false for element not " + 
          "in singleton set.")
      assert(!contains(s2, 1), "contains should return false for element not " + 
          "in singleton set.")
      assert(!contains(s2, 3), "contains should return false for element not " + 
          "in singleton set.")
      assert(!contains(s3, 1), "contains should return false for element not " + 
          "in singleton set.")
      assert(!contains(s3, 2), "contains should return false for element not " + 
          "in singleton set.")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("Set intersection contains only common elements.") {
    new TestSets {
      assert(!contains(intersect(s1, s4), 2), "Intersection should not " +
          "include elements not in either set")
      assert(!contains(intersect(s1, s4), 11), "Intersection should not " + 
          "include elements in one set but not the other.")
      assert(contains(intersect(s3, s4), 3), "Intersection should include " +
          "common elements between sets.")
    }
  }
  
  test("`map` tests") {
    new TestSets {
    	val identMapSet = map(s1, x => x)
    	assert(contains(identMapSet, 1), "map() using identity function " +
    	    "should return set with element in original set.")
    	assert(!contains(identMapSet, 4), "map() using identity function " +
    	    "should return set with no elements which were not in original set.")
    	    
    	val squareMapSet = map(s2, x => x * x)
    	assert(contains(squareMapSet, 4), "map() using squaring function " +
    	    "should return set with element which is square of element in" +
    	    "the original set.")
    	assert(!contains(squareMapSet, 2), "map() using squaring function " +
    	    "should return set with no elements which are not the square of " +
    	    "an element in the original set")  	
    }
  }
}
