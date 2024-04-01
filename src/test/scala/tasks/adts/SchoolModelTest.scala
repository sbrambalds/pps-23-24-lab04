package tasks.adts

import tasks.adts.SchoolModel.SchoolModelImpl.SchoolImpl
import u03.Sequences.Sequence
import tasks.adts.SchoolModel.SchoolModelImpl.TeacherImpl
import org.junit.Assert.*
import org.junit.*
import u03.Optionals.Optional
import tasks.adts.SchoolModel.SchoolModule
import tasks.adts.SchoolModel.SchoolModelImpl.*

class SchoolModelTest:

    @Test
    def addTeacherTest() =
        assertEquals(Optional.Just(TeacherImpl("Marco", Sequence.Nil())), school().addTeacher("Marco").teacherByName("Marco"))

    @Test
    def addCourseTest() =
        assertEquals(Optional.Just(CourseImpl("PPS")), school().addCourse("PPS").courseByName("PPS"))