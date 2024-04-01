package tasks.adts

import u03.Sequences.Sequence
import org.junit.Assert.*
import org.junit.*
import u03.Optionals.Optional
import tasks.adts.SchoolModel.SchoolModule
import tasks.adts.SchoolModel.SchoolModelADT.TeacherImpl
import tasks.adts.SchoolModel.SchoolModelADT.school
import tasks.adts.SchoolModel.SchoolModelADT.CourseImpl
import tasks.adts.SchoolModel.SchoolModelADT.Teacher

class SchoolModelTest:

    @Test
    def addTeacherTest() =
        assertEquals(Optional.Just(TeacherImpl("Marco", Sequence.Nil())), school().addTeacher("Marco").teacherByName("Marco"))
        assertEquals(Optional.Empty(), school().addTeacher("Marco").teacherByName("Franco"))

    @Test
    def addCourseTest() =
        assertEquals(Optional.Just(CourseImpl("PPS")), school().addCourse("PPS").courseByName("PPS"))
        assertEquals(Optional.Empty(), school().addCourse("PPS").courseByName("PCD"))
    
    @Test
    def setTeacherToCourseTest() =
        var teachers: Sequence[Teacher] = Sequence.Cons(TeacherImpl("Marco", Sequence.Cons(CourseImpl("PPS"), Sequence.Nil())), Sequence.Nil())
        assertEquals(school(teachers, Sequence.Cons(CourseImpl("PPS"), Sequence.Nil())), school().setTeacherToCourse(TeacherImpl("Marco", Sequence.Nil()), CourseImpl("PPS")))