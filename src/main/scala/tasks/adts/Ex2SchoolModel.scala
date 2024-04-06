package tasks.adts
import u03.Sequences.*
import u03.Optionals.*
import u02.AlgebraicDataTypes.Person
import u03.Sequences.Sequence.filter

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
        require(!Optional.isEmpty(school.teacherByName(nameOfTeacher(teacher))) & !Optional.isEmpty(school.courseByName(school.nameOfCourse(course))))
        
        def updateTeachers(l: Sequence[Teacher]): Sequence[Teacher] = teacher match
          case TeacherImpl(name, courses) => Sequence.Cons(TeacherImpl(name, Sequence.Cons(course, courses)), filter(l)(v => nameOfTeacher(v) != name))

        school match
          case SchoolImpl(teachers, c) => SchoolImpl(updateTeachers(teachers), c)
          
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = school.teacherByName(nameOfTeacher(teacher)) match
        case Optional.Just(TeacherImpl(_, courses)) => courses
        case _ => Sequence.Nil()
        
      

      
