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

sealed abstract class Validation[+E, +V] extends Product with Serializable {
  final def map[B](f: V => B) : Validation[E, B] = {
    this match {
      case Success(v) => Success(f(v))
      case Failure(e) => Failure(e)
    }
  }

  final def flatMap[B, EE >: E](f: V => Validation[EE, B]) : Validation[EE, B] = {
    this match {
      case Success(v) => f(v)
      case Failure(e) => Failure(e)
    }
  }

  final def getOrElse[B >: V](default: => B): B = {
    this match {
      case Success(v) => v
      case _ => default
    }
  }

  final def orElse[B >: V, EE >: E](default: => Validation[EE, B]): Validation[EE, B] = {
    this match {
      case _ : Success[_] => this
      case _ => default
    }
  }

  final def <*>[B, EE >: E](f: Validation[EE, V => B]) : Validation[EE, B] = {
    (this, f) match {
      case (Success(vaa), Success(vb)) => Success(vb(vaa))
      case (Failure(e), Success(_)) => Failure(e)	
      case (Success(_), Failure(e)) => Failure(e)
      case (Failure(e), Failure(ee)) => Failure(ee ::: e)
    }
  }
}

final case class Success[V](value: V) extends Validation[Nothing, V]
final case class Failure[E](errors: List[E]) extends Validation[E, Nothing]

private[validation] sealed abstract class ValidationOps[T] {
  protected val value: T
  
  def success[E] : Validation[E, T] = Success(value)
  def failure[V] : Validation[T, V] = Failure(value :: Nil)
}

private[validation] sealed abstract class ValidationApplyOps[E, A] {
  val va : Validation[E, A]
  
  def and[B](vb: Validation[E, B]) = new ValidationBuilder[E, A, B] {
    val a = va
    val b = vb
  }
}

private[validation] sealed abstract class ValidationBuilder[E, A, B] {
  val a: Validation[E, A]
  val b: Validation[E, B]
  
  def apply[T](f: (A, B) => T) : Validation[E, T] = {
    b <*> (a map f.curried)
  }

  def and[C](vc: Validation[E, C]) = new ValidationBuilder3[C] {
    val c = vc
  }

  sealed abstract class ValidationBuilder3[C] {
    val c : Validation[E, C]
    
    def apply[T](f: (A, B, C) => T) : Validation[E, T] = {
      c <*> (b <*> (a map f.curried))
    }
    
    def and[D](vd: Validation[E, D]) = new ValidationBuilder4[D] {
      val d = vd
    }
    
    sealed abstract class ValidationBuilder4[D] {
      val d : Validation[E, D]
      
      def apply[T](f: (A, B, C, D) => T) : Validation[E, T] = {
        d <*> (c <*> (b <*> (a map f.curried)))
      }
      
      def and[EE](ve: Validation[E, EE]) = new ValidationBuilder5[EE] {
        val e = ve
      }
      
      sealed abstract class ValidationBuilder5[EE] {
        val e: Validation[E, EE]
        
        def apply[T](f: (A, B, C, D, EE) => T) : Validation[E, T] = {
          e <*> (d <*> (c <*> (b <*> (a map f.curried))))
        }
        
        def and[F](vf: Validation[E, F]) = new ValidationBuilder6[F] {
          val ff = vf
        }
        
        sealed abstract class ValidationBuilder6[F] {
          val ff : Validation[E, F]
          
          def apply[T](f: (A, B, C, D, EE, F) => T) : Validation[E, T] = {
            ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried)))))
          }
          
          def and[G](vg: Validation[E, G]) = new ValidationBuilder7[G] {
            val g = vg
          }
          
          sealed abstract class ValidationBuilder7[G] {
            val g : Validation[E, G]
            
            def apply[T](f: (A, B, C, D, EE, F, G) => T) : Validation[E, T] = {
              g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried))))))
            }
            
            def and[H](vh: Validation[E, H]) = new ValidationBuilder8[H] {
              val h = vh
            }

            sealed abstract class ValidationBuilder8[H] {
              val h : Validation[E, H]
            
              def apply[T](f: (A, B, C, D, EE, F, G, H) => T) : Validation[E, T] = {
                h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried)))))))
              }
            
              def and[I](vi: Validation[E, I]) = new ValidationBuilder9[I] {
                val i = vi
              }
              
              sealed abstract class ValidationBuilder9[I] {
                val i : Validation[E, I]
            
                def apply[T](f: (A, B, C, D, EE, F, G, H, I) => T) : Validation[E, T] = {
                  i <*> (h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried))))))))
                }
            
                def and[J](vj: Validation[E, J]) = new ValidationBuilder10[J] {
                  val j = vj
                }

                sealed abstract class ValidationBuilder10[J] {
                  val j : Validation[E, J]
            
                  def apply[T](f: (A, B, C, D, EE, F, G, H, I, J) => T) : Validation[E, T] = {
                    j <*> (i <*> (h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried)))))))))
                  }
            
                  def and[J](vk: Validation[E, J]) = new ValidationBuilder11[J] {
                    val k = vk
                  }

                  sealed abstract class ValidationBuilder11[K] {
                    val k : Validation[E, K]
            
                    def apply[T](f: (A, B, C, D, EE, F, G, H, I, J, K) => T) : Validation[E, T] = {
                      k <*> (j <*> (i <*> (h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried))))))))))
                    }
            
                    def and[L](vl: Validation[E, L]) = new ValidationBuilder12[L] {
                      val l = vl
                    }

                    sealed abstract class ValidationBuilder12[L] {
                      val l : Validation[E, L]
            
                      def apply[T](f: (A, B, C, D, EE, F, G, H, I, J, K, L) => T) : Validation[E, T] = {
                        l <*> (k <*> (j <*> (i <*> (h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried)))))))))))
                      }
            
                      def and[M](vm: Validation[E, M]) = new ValidationBuilder13[M] {
                        val m = vm
                      }

                      sealed abstract class ValidationBuilder13[M] {
                        val m : Validation[E, M]
            
                        def apply[T](f: (A, B, C, D, EE, F, G, H, I, J, K, L, M) => T) : Validation[E, T] = {
                          m <*> (l <*> (k <*> (j <*> (i <*> (h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried))))))))))))
                        }
            
                        def and[N](vn: Validation[E, N]) = new ValidationBuilder14[N] {
                          val n = vn
                        }

                        sealed abstract class ValidationBuilder14[N] {
                          val n : Validation[E, N]
            
                          def apply[T](f: (A, B, C, D, EE, F, G, H, I, J, K, L, M, N) => T) : Validation[E, T] = {
                            n <*> (m <*> (l <*> (k <*> (j <*> (i <*> (h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried)))))))))))))
                          }
            
                          def and[O](vo: Validation[E, O]) = new ValidationBuilder15[O] {
                            val o = vo
                          }

                          sealed abstract class ValidationBuilder15[O] {
                            val o : Validation[E, O]
            
                            def apply[T](f: (A, B, C, D, EE, F, G, H, I, J, K, L, M, N, O) => T) : Validation[E, T] = {
                              o <*> (n <*> (m <*> (l <*> (k <*> (j <*> (i <*> (h <*> (g <*> (ff <*> (e <*> (d <*> (c <*> (b <*> (a map f.curried))))))))))))))
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

object `package` {
  import scala.language.implicitConversions

  implicit def toValidationOps[T](t: T) : ValidationOps[T] =
    new ValidationOps[T] { override val value = t }
  
  implicit def toValidationBuilder[E, V](validation: Validation[E, V]) : ValidationApplyOps[E, V] =
    new ValidationApplyOps[E, V] { val va = validation }
}
