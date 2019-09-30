package com

package object example {
  type ErrorOr[+A] = Either[List[String], A]

  object ErrorOr {
    def singleError(s: String): ErrorOr[Nothing] = Left(List(s))
    def errors(e: List[String]): ErrorOr[Nothing] = Left(e)
    def success[A](v: A): ErrorOr[A] = Right(v)
  }
}
