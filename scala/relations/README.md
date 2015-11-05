# Relations

A (one-to-many) relation of type T can be represented as a function from T to a Set of T's.
For an example, imagine a "friends" relation in a social network.

Write a program that answers the question whether t1 is related to t2, assuming the relation is transitive (if t1 is related to t2 and t2 is related to t2 then t1 is also related to t3) but not symmetric (if t1 is related to t2 then t2 is not necessarily related to t1), up to a specified search depth.

In a second stepp assume that relations are stored in some data store.
Retrieval from that store is asynchronous and non-blocking, represented by a `scala.concurrent.Future` as wrapper around the Set.

The Scala exercises assume an SBT project scheme. The exercise solution source
should be placed within the exercise directory/src/main/scala. The exercise
unit tests can be found within the exercise directory/src/test/scala.

To run the tests simply run the command `sbt test` in the exercise directory.

For more detailed info about the Scala track see the [help
page](http://help.exercism.io/getting-started-with-scala.html).
