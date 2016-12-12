package object typeclass {
  trait Bounded[A] {
    def minBound: A
    def maxBound: A
  }
  implicit class BoundedOps[A](a: A)(implicit bounded: Bounded[A]) {
    def minBound: A = bounded.minBound
    def maxBound: A = bounded.maxBound
  }

  trait Enum[A] {
    def succ: A => A
    def pred: A => A
  }
  implicit class EnumOps[A](a: A)(implicit enum: Enum[A]) {
    def succ: A = enum.succ(a)
    def pred: A = enum.pred(a)
  }

  trait Circular[A] {
    def bounded: Bounded[A]
    def enum: Enum[A]

    def next: A => A = a =>
      if (a == bounded.maxBound) bounded.minBound else enum.succ(a)
    def previous: A => A = a =>
      if (a == bounded.minBound) bounded.maxBound else enum.pred(a)
  }
  implicit class CircularOps[A](a: A)(implicit circular: Circular[A]) {
    def next: A = circular.next(a)
    def previous: A = circular.previous(a)
  }
}
