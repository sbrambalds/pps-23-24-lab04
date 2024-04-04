package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

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
      
