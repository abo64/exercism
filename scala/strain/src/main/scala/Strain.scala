object Strain {
  def keep[T](ts: Seq[T], f: T => Boolean): Seq[T] =
    ts.foldRight(Seq.empty[T]) { case (t, ys) =>
      if (f(t)) t +: ys else ys
    }

  def discard[T](ts: Seq[T], f: T => Boolean): Seq[T] =
    keep(ts, !f(_:T))
}