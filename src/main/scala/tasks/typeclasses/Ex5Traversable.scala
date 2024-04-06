package u04lab
import u03.Sequences.* 
import Sequence.*
import u03.Optionals.*
import u03.Lambda.And
import u04lab.Ex5Traversable.logAll

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)

  trait Traversable[T[_]]:
    def logAll[A](a: T[A]): Unit
  
  def logAll[A, T[A]: Traversable](a: T[A]): Unit = 
    summon[Traversable[T]].logAll(a)

object TraversableGivenInstances:
  import Ex5Traversable.*

  given Traversable[Optional] with
    def logAll[A](a: Optional[A]): Unit = a match
      case Optional.Just(v) => log(v)
      case _ => ()

  given Traversable[Sequence] with
    def logAll[A](a: Sequence[A]): Unit = a match
      case Sequence.Cons(h, t) => log(h); logAll(t)
      case _ => ()

@main def tryTraversable() =
  import Ex5Traversable.*
  import TraversableGivenInstances.{*, given}

  logAll(Optional.Just(45))
  logAll(Optional.Empty())
  logAll(Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil()))))
  logAll(Sequence.Nil())







  
