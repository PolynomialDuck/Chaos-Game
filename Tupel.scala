package MandelbrotSet
import java.awt.{Color=>JColor}
class Tupel(var r: Double, var g: Double, var b: Double){
    def +(a: Double, b: Double, c: Double): Tupel = {
       new Tupel(a+r,b+g,c+b)
    }
    def addTupel(t: Tupel): Tupel = {
       new Tupel(r+t.r, g+t.g, b+t.b) 
    }
    def -(a: Double, b: Double, c: Double): Tupel = {
       new Tupel(r-a,g-b,b-c) 
    }
    def *(a: Double, b: Double, c: Double): Tupel = {
       new Tupel(r*a, g*b, b*c) 
    }
    def /(a: Double, b: Double, c: Double): Tupel = {
       new Tupel(r/a, g/b, b/c) 
    }
    def jColor(): JColor = {
        var tempR: Int = valColor(r.toInt)
        var tempG: Int = valColor(g.toInt)
        var tempB: Int = valColor(b.toInt)
        return new JColor(tempR,tempG,tempB)
    }
    def valColor(x: Int = 0): Int = {
        if(0<=x && x<=255){
            return x
        }
        else if(x<0){
            return 0
        }
        else if(x>255){
            return 255
        }
        else return 0
        }
}