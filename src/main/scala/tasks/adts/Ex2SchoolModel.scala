package tasks.adts
import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(teacher: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
    
  object SchoolModelImpl extends SchoolModule:

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
      def teacherByName(name: String): Optional[Teacher] = 

        def _teacherByName(l: Sequence[Teacher]): Optional[Teacher] = l match
          case Sequence.Cons(teacher, _) if teacher.name == name => Optional.Just(teacher)
          case Sequence.Cons(_, t) => _teacherByName(t)
          case _ => Optional.Empty()
        
        school match
          case SchoolImpl(teachers, _) => _teacherByName(teachers)

      def courseByName(name: String): Optional[Course] = 

        def _courseByName(l: Sequence[Course]): Optional[Course] = l match
          case Sequence.Cons(CourseImpl(n), _) if n == name => Optional.Just(CourseImpl(n))
          case Sequence.Cons(_, t) => _courseByName(t)
          case _ => Optional.Empty()
        
        school match
          case SchoolImpl(_, courses) => _courseByName(courses)
          
      def nameOfTeacher(teacher: Teacher): String = ???
      def nameOfCourse(teacher: Course): String = ???
      def setTeacherToCourse(teacher: Teacher, course: Course): School = ???
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???
