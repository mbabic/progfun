package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * Container for static methods of the tweet class.
 */
object Tweet {
  def maxRetweets(a: Tweet, b: Tweet): Tweet =
    if (a.retweets < b.retweets) b
    else a
    
  def minRetweets(a: Tweet, b: Tweet): Tweet =
    if (a.retweets < b.retweets) a
    else b
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   */
  def filter(p: Tweet => Boolean): TweetSet = 
    filterAcc(p, new Empty)
    
  /**
   * Boolean flag indicating whether the TweetSet is empty or not.
   */
  def isEmpty: Boolean

  /**
   * Returns the twe
   */
  
  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   */
   def union(that: TweetSet): TweetSet
   
  /**
   * Returns the tweet from this set which has the greatest retweet count.
   */
  def mostRetweeted: Tweet
  
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   */
  def descendingByRetweet: TweetList


  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  
  val isEmpty = true

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet =
    that
    
  def mostRetweeted(): Tweet =
  	throw new NoSuchElementException("No most retweeted tweet in empty tweet " +
  	  "set")
    
  def descendingByRetweet(): TweetList = Nil  
  
  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  val isEmpty = false
  
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = 
    if (p(elem)) left filterAcc (p, right filterAcc(p, acc incl elem))
    else left filterAcc(p, right filterAcc(p, acc))
  
  
  def union(that: TweetSet): TweetSet = 
  	filterAcc(x => true, that)

  def descendingByRetweet(): objsets.TweetList =
  	new Cons(mostRetweeted, 
  			(this remove mostRetweeted) descendingByRetweet)
  
  /**
   * Idea behind implementation: the most retweeted tweet will be the max of
   * the retweets of the elem of the given set, the most retweeted tweet in
   * the left subtree and the most the retweeted tweet in the right subtree.
   */
  lazy val mostRetweeted: Tweet = {
      // Only one tweet in the set, so it must necessarily be the one with the
      // most retweets.
      if ((left isEmpty) && (right isEmpty)) elem
      else if (left isEmpty) Tweet.maxRetweets(right mostRetweeted, elem)
      else if (right isEmpty) Tweet.maxRetweets(left mostRetweeted, elem)
      else Tweet.maxRetweets(Tweet.maxRetweets(left mostRetweeted, right mostRetweeted), elem)
  }
  	
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val tweets = TweetReader.allTweets
  
  lazy val googleTweets: TweetSet = 
  	TweetReader.allTweets filter (x => 
  		(google.exists (y => x.text contains y)))
  	
  lazy val appleTweets: TweetSet = 
  	TweetReader.allTweets filter (x => 
  		(apple.exists (y => x.text contains y)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = 
    (googleTweets union appleTweets) descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
