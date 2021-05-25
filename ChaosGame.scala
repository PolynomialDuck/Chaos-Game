package MandelbrotSet

import introprog.PixelWindow
import java.awt.{Color => JColor}
import java.util.concurrent.CompletionException

object Color{
    var setTupel = new Tupel(0,0,0)
    var backTupel = new Tupel(0,0,255)
    var divTupel = new Tupel(255,0,0)
    var divColor = new JColor(0,0,0)
}

object Window{
    var xLength: Int = 1001
    var yLength: Int = 1001
    var accuracy: Int = 450
    var reCenter: Double = 1.75
    var imCenter: Double = 1
    val windowSize = (xLength,yLength)

    val window = new PixelWindow(windowSize._1,windowSize._2, "Fractal", Color.backTupel.jColor())
    def complexValue(p: (Int, Int)): Complex = {
    val x: Double = p._1.toDouble
    val y: Double = p._2.toDouble
    new Complex( ((x/accuracy)-reCenter), (-(y/accuracy)+imCenter))
    }
    def makeChoice(msg: String = "null", options: Vector[String]): Int = {
        options.indices.foreach(i => println(i+": "+options(i)))
        println(" ")
        val select = io.StdIn.readLine(msg).toInt
        println(" ")
        return select
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

object Math{
    var ConstantOriented: Boolean = true
    var cTemp: Complex = new Complex(0,0)
    var constant: Complex = new Complex(0,0)
    var divType: Double = 0.0
    var divWeight: Double = 0.00575
    var divRate: Double = 25
    
    def checkRekursivFormel(c: Complex): Boolean = {
        if(ConstantOriented){
            divType = 0.0
            cTemp = new Complex(0,0)
            for(i <- 1 to 1000){
                cTemp=(cTemp*cTemp)+c
                if(i<divRate){
                divType+= cTemp.magni
                }
            }
            if (cTemp.magni<2){
                return true
            }
            else{
                Color.divColor = ((Color.backTupel*(divType*divWeight,divType*divWeight,divType*divWeight)).addTupel(Color.divTupel*(1/(divType*divWeight), 1/(divType*divWeight), 1/(divType*divWeight)))).jColor()
                return false
            }
        }
        else{
            divType = 0.0
            cTemp = c
            for(i <- 1 to 1000){
                cTemp=cTemp*cTemp+constant
                if(i<divRate){
                    divType+= cTemp.magni
                }
            }
            if (cTemp.magni<2){
                return true
            }
            else {
                Color.divColor = ((Color.backTupel*(divType*divWeight,divType*divWeight,divType*divWeight)).addTupel(Color.divTupel*(1/(divType*divWeight), 1/(divType*divWeight), 1/(divType*divWeight)))).jColor()
                return false
            }
        }
    }
}

object Main {
    def drawMandelbrot(): Unit = {
        for(y <- 0 to Window.yLength-1){
            for(x <- 0 to Window.xLength-1){
                if(Math.checkRekursivFormel(Window.complexValue(x,y))){
                    Window.window.setPixel(x,y,Color.setTupel.jColor())
                    Thread.sleep(0)
                    }
                    else{
                        if(Color.divColor != new JColor(0,0,0)){
                            Window.window.setPixel(x,y,Color.divColor)
                        }
                        else{
                            Window.window.setPixel(x,y,Color.backTupel.jColor())
                        }
                    }
                }
            }
        }
    def main(args: Array[String]): Unit = {
        var tempAns: Int = 0
        var tempRe: Double = 0.0
        var tempIm: Double = 0.0
        drawMandelbrot()
        var quit = false
        while(!quit){
            try{
               tempAns=Window.makeChoice("Pick a choice: ", Vector("Change to Mandelbrotset", "Change to Juliaset", "Change Color of Background", "Change Color of Set","Change the Color of the Divergence Rate","Change the Weight of Divergence","Change the Fog Divergence","Change Height","Change Width","Change Accuracy", "Change Centerpoint", "Quit"))
            } catch {
                case e: Exception => println("Error: Couldn't understand the input, try again.")
                tempAns=(-1)
            }
            if(0<=tempAns && tempAns<=11){
                if(tempAns==0){
                    if(Math.ConstantOriented==true){
                        println("You are already viewing the MandelbrotSet!")
                    }
                    else{ 
                    Window.xLength = (Window.accuracy*Window.reCenter).toInt
                    Window.yLength = (Window.accuracy*Window.imCenter).toInt
                    Math.ConstantOriented = true
                    try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                } 
                }
                else if(tempAns==1){
                            println("Enter constant for the JuliaSet")
                            try {
                            tempRe = io.StdIn.readLine("Real Part = ").toDouble
                            } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempRe=0
                            }
                            try{
                            tempIm = io.StdIn.readLine("Complex Part = ").toDouble
                            } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                tempIm=0
                            }
                            Math.constant=new Complex(tempRe,tempIm)
                            Math.ConstantOriented = false
                            try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==2){
                        println("Choose a color using the RGB system...")
                        try{
                        Color.backTupel.r = Window.valColor(io.StdIn.readLine("Red = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.backTupel.r=0
                        }
                        try{
                        Color.backTupel.g = Window.valColor(io.StdIn.readLine("Green = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.backTupel.g=0
                        }
                        try{
                        Color.backTupel.b = Window.valColor(io.StdIn.readLine("Blue = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.backTupel.b=0
                        }
                        try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==3){
                        println("Choose a color using the RGB system...")
                        try{
                        Color.setTupel.r = Window.valColor(io.StdIn.readLine("Red = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.setTupel.r=0
                        }
                        try{
                        Color.setTupel.g = Window.valColor(io.StdIn.readLine("Green = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.setTupel.g=0
                        }
                        try{
                        Color.setTupel.b = Window.valColor(io.StdIn.readLine("Blue = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.setTupel.b=0
                        }
                        try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                    }
                    else if(tempAns==4){
                        println("Choose a color using the RGB system...")
                        try{
                        Color.divTupel.r = Window.valColor(io.StdIn.readLine("Red = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.divTupel.r=0
                        }
                        try{
                        Color.divTupel.g = Window.valColor(io.StdIn.readLine("Green = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.divTupel.g=0
                        }
                        try{
                        Color.divTupel.b = Window.valColor(io.StdIn.readLine("Blue = ").toInt)
                        } catch{ 
                                case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                                Color.divTupel.b=0
                        }
                        try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                    }
                else if(tempAns==5){
                    try{
                        Math.divWeight = io.StdIn.readLine("Divergence Weight = ").toDouble
                    } catch{
                        case e: Exception => println("Error: Couldn't understand the input. Returning value 0.5...")
                        Math.divWeight = 0.5
                    }
                    try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==6){
                    try {
                    Math.divRate = io.StdIn.readLine("Divergence Rate = ").toDouble
                    } catch{
                        case e: Exception => println("Error: Couldn't understand the input. Returning value 25...")
                        Math.divWeight = 25
                    }
                    try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==7){
                    try {
                    Window.yLength = io.StdIn.readLine("Height = ").toInt
                    } catch{
                        case e: Exception => println("Error: Couldn't understand the input. Returning value 501...")
                        Window.yLength = 501
                    }
                    try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==8){
                    try {
                    Window.xLength = io.StdIn.readLine("Width = ").toInt
                    } catch{
                        case e: Exception => println("Error: Couldn't understand the input. Returning value 501...")
                        Window.xLength = 501
                    }
                    try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==9){
                    try {
                    Window.accuracy = io.StdIn.readLine("Accuracy = ").toInt
                    } catch{
                        case e: Exception => println("Error: Couldn't understand the input. Returning value 501...")
                        Window.xLength = 100
                    }
                    try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==10){
                    try {
                    Window.reCenter = io.StdIn.readLine("Choose a real part for the centerpoint... ").toDouble
                    } catch{
                        case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                        Window.reCenter = 0
                    }
                    try {
                    Window.imCenter = io.StdIn.readLine("Choose a complex part for the centerpoint... ").toDouble
                    } catch{
                        case e: Exception => println("Error: Couldn't understand the input. Returning value 0...")
                        Window.imCenter = 0
                    }
                    try{
                    Window.window.fill(0,0,Window.windowSize._1,Window.windowSize._2, Color.backTupel.jColor())
                    drawMandelbrot()
                    }catch{
                        case e: Exception => println("An error occured, couldn't paint the set...")
                    }
                }
                else if(tempAns==11){
                    quit = true
                    Window.window.hide()
                }
                }
                else println("Error: The Input did not represent a choice")
            } 
        }
    }
