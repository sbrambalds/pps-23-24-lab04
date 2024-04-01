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
        var mySchool: School = school().addTeacher("Marco")
        assertEquals(Optional.Just(TeacherImpl("Marco", Sequence.Nil())), mySchool.teacherByName("Marco"))

