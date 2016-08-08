// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package recfun

object Main {

  def main(args: Array[String]) {
    print("Pascal's Triangle\n")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      print("\n")
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balance(c: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        c == 0
      }  else if (c < 0) {
        false
      } else if (chars.head == '(') {
        balance(c + 1, chars.tail)
      } else if (chars.head == ')') {
        balance(c - 1, chars.tail)
      } else {
        balance(c, chars.tail)
      }
    }

    balance(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]): Int = {
      if (money < 0) {
        0
      } else if (money == 0) {
        1
      } else if (coins.isEmpty) {
        0
      } else {
        countChangeIter(money - coins.head, coins) + countChangeIter(money, coins.tail)
      }
    }

    if (money == 0) {
      0
    } else if (coins.isEmpty) {
      0
    } else {
      countChangeIter(money, coins)
    }
  }
}
