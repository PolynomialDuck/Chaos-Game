package MandelbrotSet

class Complex(var re: Double, var im: Double){
    def +(c: Complex): Complex = { 
        val tempRe = re 
        val tempIm = im
        new Complex((tempRe+c.re),(tempIm+c.im))
    }
    def -(c: Complex): Complex = {
        val tempRe = re 
        val tempIm = im 
        new Complex ((tempRe-c.re),(tempIm-c.im))
    }
    def *(c: Complex): Complex = {
        val tempRe = re 
        val tempIm = im
        new Complex((tempRe*c.re-tempIm*c.im),(tempRe*c.im+tempIm*c.re))
    }
    def /(c: Complex): Complex = {
        val tempRe = re 
        val tempIm = im
        new Complex(((tempRe*c.re+tempIm*c.im)/(c.re*c.re+c.im*c.im)),((tempIm*c.re-tempRe*c.im)/(c.re*c.re+c.im*c.im)))
    }
    def magni: Double = (scala.math.sqrt((re*re)+(im*im)))
    def theta = scala.math.atan2(re, im)
    override def toString =
        re + (if(im<0)"-"+(-im) else "+"+im) + "i"
}
