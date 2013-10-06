/**
 * Copyright (c) 2013 Lauri Fjällström
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package com.github.lfjallstrom.validation

import org.scalatest.FunSpec
import com.github.lfjallstrom.validation._

class ValidationSpec extends FunSpec {
  describe("Validation") {
    it("creates successes") {
      assert(1.success === Success(1))
    }

    it("creates failures") {
      assert("fail".failure === Failure("fail" :: Nil))
    }

    it("has getOrElse method") {
      assert(1.success.getOrElse(2) === 1)
      assert(1.failure.getOrElse(2) === 2)
    }
  
    it("has orElse method") {
      assert(1.success.orElse(2.success) === 1.success)
      assert(1.failure.orElse(2.success) === 2.success)
    }
  
    it("can be mapped") {
      val result1 = (1.success.map(_ + 1))
      assert(result1 === Success(2))

      val result2 = ("fail".failure[Int].map(_ + 1))
      assert(result2 === "fail".failure)
    }

    it("can be flatMapped") {
      val result1 = 1.success.flatMap(v => (v + 1).success)
      assert(result1 === 2.success)
      
      val result2 = 1.success.flatMap(v => (v + 1).failure)
      assert(result2 === 2.failure)
      
      val result3 = "fail".failure[Int].flatMap(v => (v + 1).success)
      assert(result3 === "fail".failure)
    }

    it("supports for comprehensions") {
      val result1 = for {
        v1 <- 1.success
        v2 <- 2.success
      } yield { v1 + v2 }
      assert(result1 === 3.success)

      val result2 = for {
        v1 <- 1.success
        v2 <- "fail".failure[Int]
      } yield { v1 + v2 }
      assert(result2 == "fail".failure)
    }

    it("can be combined with other validations to form validation applications") {
      val sum2 = (a: Int, b: Int) => a + b
      val sum3 = (a: Int, b: Int, c: Int) => a + b + c
      val sum4 = (a: Int, b: Int, c: Int, d: Int) => a + b + c + d
      val sum5 = (a: Int, b: Int, c: Int, d: Int, e: Int) => a + b + c + d + e
      val sum6 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int) => a + b + c + d + e + f
      val sum7 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int) => a + b + c + d + e + f + g
      val sum8 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) => a + b + c + d + e + f + g + h
      val sum9 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i:Int) => a + b + c + d + e + f + g + h + i
      val sum10 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i:Int, j: Int) => a + b + c + d + e + f + g + h + i + j
      val sum11 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i:Int, j: Int, k: Int) => a + b + c + d + e + f + g + h + i + j + k
      val sum12 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i:Int, j: Int, k: Int, l: Int) => a + b + c + d + e + f + g + h + i + j + k + l
      val sum13 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i:Int, j: Int, k: Int, l: Int, m: Int) => a + b + c + d + e + f + g + h + i + j + k + l + m
      val sum14 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i:Int, j: Int, k: Int, l: Int, m: Int, n: Int) => a + b + c + d + e + f + g + h + i + j + k + l + m + n
      val sum15 = (a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i:Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int) => a + b + c + d + e + f + g + h + i + j + k + l + m + n + o

      val result1 = (1.success and 2.success) { sum2 }
      assert(result1 === 3.success)
      
      val result2 = (1.failure and 2.failure) { sum2 }
      assert(result2 === Failure(1 :: 2 :: Nil))

      val result3 = (1.success and 2.success and 3.success) { sum3 }
      assert(result3 === 6.success)

      val result4 = (1.failure and 2.failure and 3.failure) { sum3 }
      assert(result4 === Failure(1 :: 2 :: 3 :: Nil))

      val result5 = (1.success and 2.success and 3.success and 4.success) { sum4 }
      assert(result5 === 10.success)
      
      val result6 = (1.failure and 2.failure and 3.failure and 4.failure) { sum4 }
      assert(result6 === Failure(1 :: 2 :: 3 :: 4 :: Nil))

      val result7 = (1.success and 2.success and 3.success and 4.success and 5.success) { sum5 }
      assert(result7 === 15.success)

      val result8 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure) { sum5 }
      assert(result8 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: Nil))

      val result9 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success) { sum6 }
      assert(result9 === 21.success)

      val result10 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure) { sum6 }
      assert(result10 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil))

      val result11 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success) { sum7 }
      assert(result11 === 28.success)

      val result12 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure) { sum7 }
      assert(result12 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil))

      val result13 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success) { sum8 }
      assert(result13 === 36.success)

      val result14 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure) { sum8 }
      assert(result14 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil))

      val result15 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success and 9.success) { sum9 }
      assert(result15 === 45.success)

      val result16 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure and 9.failure) { sum9 }
      assert(result16 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil))

      val result17 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success and 9.success and 10.success) { sum10 }
      assert(result17 === 55.success)

      val result18 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure and 9.failure and 10.failure) { sum10 }
      assert(result18 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: Nil))

      val result19 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success and 9.success and 10.success and 11.success) { sum11 }
      assert(result19 === 66.success)

      val result20 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure and 9.failure and 10.failure and 11.failure) { sum11 }
      assert(result20 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: Nil))

      val result21 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success and 9.success and 10.success and 11.success and 12.success) { sum12 }
      assert(result21 === 78.success)

      val result22 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure and 9.failure and 10.failure and 11.failure and 12.failure) { sum12 }
      assert(result22 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: Nil))

      val result23 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success and 9.success and 10.success and 11.success and 12.success and 13.success) { sum13 }
      assert(result23 === 91.success)

      val result24 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure and 9.failure and 10.failure and 11.failure and 12.failure and 13.failure) { sum13 }
      assert(result24 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: Nil))

      val result25 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success and 9.success and 10.success and 11.success and 12.success and 13.success and 14.success) { sum14 }
      assert(result25 === 105.success)

      val result26 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure and 9.failure and 10.failure and 11.failure and 12.failure and 13.failure and 14.failure) { sum14 }
      assert(result26 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: Nil))

      val result27 = (1.success and 2.success and 3.success and 4.success and 5.success and 6.success and 7.success and 8.success and 9.success and 10.success and 11.success and 12.success and 13.success and 14.success and 15.success) { sum15 }
      assert(result27 === 120.success)

      val result28 = (1.failure and 2.failure and 3.failure and 4.failure and 5.failure and 6.failure and 7.failure and 8.failure and 9.failure and 10.failure and 11.failure and 12.failure and 13.failure and 14.failure and 15.failure) { sum15 }
      assert(result28 === Failure(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: Nil))
    }
  }
}
