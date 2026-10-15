package tasks

import org.junit.*
import org.junit.Assert.*
import Optionals.*
import Task10.*
import Task10.Optional.*

class OptionalsTest:
  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertTrue(OptionalInt.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = OptionalInt.Maybe(0)
    assertFalse(OptionalInt.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = OptionalInt.Maybe(0)
    assertEquals(0, OptionalInt.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertEquals(1, OptionalInt.orElse(empty, 1))

  /** Task 10: do test for map **/
  @Test def mapShouldTransformValueWhenPresent(): Unit =
    val opt = Maybe(5)
    val result = map(opt)(_ > 2)
    assertEquals(Maybe(true), result)

  @Test def mapShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Empty()
    val result = map(empty)(_ > 8)
    assertEquals(Empty(), result)

  @Test def filterShouldKeepValueWhenPredicateIsTrue(): Unit =
    val opt = Maybe(5)
    val result = filter(opt)(_ > 2)
    assertEquals(Maybe(5), result)

  @Test def filterShouldDropValueWhenPredicateIsFalse(): Unit =
    val opt = Maybe(5)
    val result = filter(opt)(_ > 8)
    assertEquals(Empty(), result)

  @Test def filterShouldRemainEmptyWhenAlreadyEmpty(): Unit =
    val empty: Optional[Int] = Empty()
    val result = filter(empty)(_ > 2)
    assertEquals(Empty(), result)
