package tasks.adts

import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional if stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]
  
  object StackImpl extends StackADT:
    type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Sequence.Nil()

    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = Sequence.Cons(a, stack)
      def pop(a: A): Optional[(A, Stack[A])] = stack match
        case Sequence.Nil() => Optional.Empty()
        case Sequence.Cons(h, t) => Optional.Just(h, t)
      
      def asSequence(): Sequence[A] = stack 