package tasks.adts

import u03.Sequences.Sequence
import org.junit.Assert.*
import org.junit.*
import u03.Optionals.Optional
import tasks.adts.SchoolModel.SchoolModelADT.*
import org.junit.jupiter.api.BeforeAll

class SchoolModelTest:

    var mySchool: School = school().addTeacher("Marco").addTeacher("Franco").addCourse("PPS").addCourse("PCD")

    @Test
    def addTeacherTest() =
        assertEquals(Optional.Just(TeacherImpl("Marco", Sequence.Nil())), mySchool.teacherByName("Marco"))
        assertEquals(Optional.Empty(), mySchool.teacherByName("Andrea"))

    @Test
    def addCourseTest() =
        assertEquals(Optional.Just(CourseImpl("PPS")), mySchool.courseByName("PPS"))
        assertEquals(Optional.Empty(), mySchool.courseByName("WEB"))
    
    @Test
    def setTeacherToCourseTest() =
        var teachers: Sequence[Teacher] = Sequence.Cons(TeacherImpl("Marco", Sequence.Cons(CourseImpl("PPS"), Sequence.Nil())), Sequence.Nil())
        var newSchool: School = school(teachers, Sequence.Cons(CourseImpl("PPS"), Sequence.Nil()))
        assertThrows(classOf[IllegalArgumentException], () => mySchool.setTeacherToCourse(TeacherImpl("Andrea", Sequence.Nil()), CourseImpl("PPS")))
        assertEquals(newSchool.teacherByName("Marco"), mySchool.setTeacherToCourse(TeacherImpl("Marco", Sequence.Nil()), CourseImpl("PPS")).teacherByName("Marco"))

    @Test
    def getCourseOfTeacherTest() =
        var courses: Sequence[Course] = Sequence.Cons(CourseImpl("PPS"), Sequence.Nil())
        var teacher: Teacher = TeacherImpl("Marco", Sequence.Nil())
        assertEquals(courses, mySchool.setTeacherToCourse(teacher, CourseImpl("PPS")).coursesOfATeacher(teacher))