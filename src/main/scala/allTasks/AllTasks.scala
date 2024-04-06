package allTasks

import u03.Sequences.Sequence
import u03.Optionals.Optional
import u03.Sequences.Sequence.filter

object AllTasks:
    
    // Task 1:
    object Ex1ComplexNumbers:

        trait ComplexADT:
            type Complex
            def complex(re: Double, im: Double): Complex
            extension (complex: Complex)
                def re(): Double
                def im(): Double
                def sum(other: Complex): Complex
                def subtract(other: Complex): Complex
                def asString(): String

        object BasicComplexADT extends ComplexADT:

            // Change assignment below: should probably define a case class and use it?
            case class ComplexImpl(re: Double, im: Double)
            opaque type Complex = ComplexImpl
            def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
            extension (complex: Complex)
                def re(): Double = re
                def im(): Double = im
                def sum(other: Complex): Complex = other match
                    case ComplexImpl(re1, im1) => ComplexImpl(re1+re, im1+im)
                def subtract(other: Complex): Complex = other match
                    case ComplexImpl(re1, im1) => ComplexImpl(re-re1, im-im1)
                def asString(): String = complex match //TODO: modify with one case
                    case _ => if im == 0 then "" + re 
                            else if re == 0 then im + "i"
                            else if im < 0 then "" + re + " - " + Math.abs(im) + "i"
                            else "" + re + " + " + im + "i"

    // Task 2:
    object SchoolModel:

        trait SchoolModule:
            type School
            type Teacher
            type Course
            def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School
            def school(): School
            extension (school: School)
                def addTeacher(name: String): School
                def addCourse(name: String): School
                def teacherByName(name: String): Optional[Teacher]
                def courseByName(name: String): Optional[Course]
                def nameOfTeacher(teacher: Teacher): String
                def nameOfCourse(course: Course): String
                def setTeacherToCourse(teacher: Teacher, course: Course): School
                def coursesOfATeacher(teacher: Teacher): Sequence[Course]
            
        object SchoolModelADT extends SchoolModule:

            case class CourseImpl(name: String)
            case class TeacherImpl(name: String, courses: Sequence[Course])
            case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

            type School = SchoolImpl
            type Teacher = TeacherImpl
            type Course = CourseImpl

            def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)

            def school(): School = SchoolImpl(Sequence.Nil(), Sequence.Nil())

            extension (school: School)

                def addTeacher(name: String): School = school match
                    case SchoolImpl(teachers, courses) => SchoolImpl(Sequence.Cons(TeacherImpl(name, Sequence.Nil()), teachers), courses)

                def addCourse(name: String): School = school match
                    case SchoolImpl(teachers, courses) => SchoolImpl(teachers, Sequence.Cons(CourseImpl(name), courses))

                def teacherByName(name: String): Optional[Teacher] = school match
                    case SchoolImpl(teachers, courses) => teachers match
                    case Sequence.Cons(h, t) => if nameOfTeacher(h) == name then Optional.Just(h) else SchoolImpl(t, courses).teacherByName(name)
                    case _ => Optional.Empty()

                def courseByName(name: String): Optional[Course] = school match
                    case SchoolImpl(teachers, courses) => courses match
                    case Sequence.Cons(h, t) => if nameOfCourse(h) == name then Optional.Just(h) else SchoolImpl(teachers, t).courseByName(name)
                    case _ => Optional.Empty()

                def nameOfTeacher(teacher: Teacher): String = teacher match 
                    case TeacherImpl(name, _) => name

                def nameOfCourse(course: Course): String = course match
                    case CourseImpl(name) => name
                
                def setTeacherToCourse(teacher: Teacher, course: Course): School = 
                    require(!Optional.isEmpty(school.teacherByName(nameOfTeacher(teacher))) & !Optional.isEmpty(school.courseByName(nameOfCourse(course))) 
                    & (filter(school.coursesOfATeacher(teacher))(v => nameOfCourse(v) == nameOfCourse(course)) == Sequence.Nil()))

                    var updatedTeacher: Teacher = TeacherImpl(nameOfTeacher(teacher), Sequence.Cons(course, coursesOfATeacher(teacher)))

                    school match
                    case SchoolImpl(teachers, c) => SchoolImpl(Sequence.Cons(updatedTeacher, filter(teachers)(v => nameOfTeacher(v) != nameOfTeacher(teacher))), c)
                    
                def coursesOfATeacher(teacher: Teacher): Sequence[Course] = school.teacherByName(nameOfTeacher(teacher)) match
                    case Optional.Just(TeacherImpl(_, courses)) => courses
                    case _ => Sequence.Nil()

    //Task 3:
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

    //Task 4:
    object Ex4Summables:
        import Sequence.*

        def sumAllInt(seq: Sequence[Int]): Int = seq match
            case Cons(h, t) => h + sumAllInt(t)
            case _ => 0

        trait Summable[A]:
            def sum(a1: A, a2: A): A
            def zero: A

        def sumAll[A: Summable](seq: Sequence[A]): A = 
            val summable = summon[Summable[A]]
            seq match
            case Cons(h, t) => summable.sum(h, sumAll(t))
            case _ => summable.zero
            

        given Summable[Int] with
            def sum(a1: Int, a2: Int): Int = a1 + a2
            def zero: Int = 0
        
        // write givens for Summable[Double] and Summable[String]

        given Summable[Double] with
            def sum(a1: Double, a2: Double): Double = a1 + a2
            def zero: Double = 0.0

        given Summable[String] with
            def sum(a1: String, a2: String): String = a1 + a2
            def zero: String = ""

    //Task 5:
    object Ex5Traversable:

        def log[A](a: A): Unit = println("The next element is: "+a)

        trait Traversable[T[_]]:
            def logAll[A](a: T[A]): Unit
        
        def logAll[A, T[A]: Traversable](a: T[A]): Unit = 
            summon[Traversable[T]].logAll(a)

        object TraversableGivenInstances:

            given Traversable[Optional] with
                def logAll[A](a: Optional[A]): Unit = a match
                case Optional.Just(v) => log(v)
                case _ => ()

            given Traversable[Sequence] with
                def logAll[A](a: Sequence[A]): Unit = a match
                case Sequence.Cons(h, t) => log(h); logAll(t)
                case _ => ()

    //Task 6:
    object Ex6TryModel:
        import u04.monads.Monads.Monad

        private enum TryImpl[A]:
            case Success(value: A)
            case Failure(exception: Throwable)

        opaque type Try[A] = TryImpl[A]

        def success[A](value: A): Try[A] = TryImpl.Success(value)
        def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
        def exec[A](expression: => A): Try[A] = try success(expression) catch failure(_)

        extension [A](m: Try[A]) 
            def getOrElse[B >: A](other: B): B = m match
            case TryImpl.Success(value) => value
            case TryImpl.Failure(_) => other

        given Monad[Try] with
            override def unit[A](value: A): Try[A] = success(value)
        
            extension [A](m: Try[A]) 

                override def flatMap[B](f: A => Try[B]): Try[B] = m match
                    case TryImpl.Success(a) => f(a) 
                    case TryImpl.Failure(exception) => failure(exception)
