case class CustomSet[+T] private(ts: T*) {
  require(ts.distinct == ts, s"must only contain unique elements: $ts")
}

object CustomSet {
  def fromList[T](ts: Seq[T]): CustomSet[T] =
    new CustomSet(ts.distinct:_*)

  def toList[T](set: CustomSet[T]): List[T] =
    set.ts toList

  def empty[T](set: CustomSet[T]): Boolean =
    set.ts.isEmpty

  def singleton[T](set: CustomSet[T]): Boolean =
    set.ts.size == 1

  def size[T](set: CustomSet[T]): Int =
    set.ts.size

  def member[T](set: CustomSet[T], t: T): Boolean =
    set.ts contains t

  def insert[T](set: CustomSet[T], t: T): CustomSet[T] =
    CustomSet.fromList(set.ts :+ t)

  def delete[T](set: CustomSet[T], t: T): CustomSet[T] =
    CustomSet.fromList(set.ts filterNot (_ == t))

  def union[T](set1: CustomSet[T], set2: CustomSet[T]): CustomSet[T] =
    CustomSet.fromList(set1.ts ++ set2.ts)

  def difference[T](set1: CustomSet[T], set2: CustomSet[T]): CustomSet[T] =
    CustomSet.union(
        CustomSet.fromList(set1.ts filterNot set2.ts.contains),
        CustomSet.fromList(set2.ts filterNot set1.ts.contains))

  def intersection[T](set1: CustomSet[T], set2: CustomSet[T]): CustomSet[T] =
    CustomSet.fromList(set1.ts intersect set2.ts)

  def isSubsetOf[T](set1: CustomSet[T], set2: CustomSet[T]): Boolean =
    set1.ts forall set2.ts.contains

  def isDisjointFrom[T](set1: CustomSet[T], set2: CustomSet[T]): Boolean =
    CustomSet.empty(CustomSet.intersection(set1, set2))
}