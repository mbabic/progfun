package recfun
import common._
import scala.collection.immutable.Stack

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   * Given column and row number of element in pascal's triangle, returns
   * the value of that element.
   */
  def pascal(c: Int, r: Int): Int = {
    
    if (c == 0 || r == 0 || c == r) 1
    // c cannot be greater than r in pascal's triangle, so this handles the
    // edge case that we are calculating the value of an entry on the right
    // hand edge of the triangle
    else if (c > r) 0
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   * Parentheses Balancing: write a recursive function which verifies the 
   * balancing of parentheses.
   * Idea behind the solution:
   * 	-> keep a stack of parentheses thus far seen
   *    -> 
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], stack: Stack[Char]): Boolean = {
      if (chars.isEmpty) stack.isEmpty
      else if (chars.head == '(') balanceHelper(chars.tail, stack :+ '(')
      else if (chars.head == ')' && !stack.isEmpty)
        balanceHelper(chars.tail, stack pop)
      else if (chars.head == ')' && stack.isEmpty) 
        // Encountered a mismatched ')', return false immediately.
        false
      else
        // Element at head of list is not '(' or ')', continue iteration
        // over string
        balanceHelper(chars.tail, stack)        
    }
    balanceHelper(chars, new Stack())
  }

  /**
   * Exercise 3
   * Write a recursive function that counts how many different ways you can make
   * change for an amount, given a list of coin denominations.
   * Implementation idea:
   *   - We cannot make change for money < 0 or when we have no coins
   *   - There is exactly one way to make change for money == 0 which is to
   *     simply return no change
   *   - For money > 0, we can make change a number of ways as follows:
   *     (i) If we use the coin at the head of the list, then we count the
   *         number of ways we can make change using all the coins in the list
   *         for the amount of money left to make change for (money - value of
   *         coin that we used)
   *     (ii)We can alternatively choose not to use the coin at the head of the
   *         list.  Then the number of ways that we can make change is equal to
   *         the number of ways we can make change for money with the rest
   *         of the coins in the list.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
