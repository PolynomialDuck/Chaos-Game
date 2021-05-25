package MandelbrotSet

import introprog.PixelWindow.Event
import introprog.PixelWindow
import java.awt.{Color => JColor}
import java.util.concurrent.CompletionException

object Color{
    var black = new Tupel(0,0,0)
    var red = new Tupel(255,0,0)
    var green = new Tupel(0,255,0)
    var blue = new Tupel(0,0,255)
}

object Window{
    var nextGenerationDelay = 0
    var quit = false;
    var play = false
    val EventMaxWait = 1
    var currPos = new Complex(0,0);
    var dots: Vector[Complex] = Vector(); 
    var xLength: Int = 1001
    var yLength: Int = 1001
    val windowSize = (xLength,yLength)

    val window = new PixelWindow(windowSize._1,windowSize._2, "The Chaos Game", Color.blue.jColor())
    
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

        def handleClick(pos: (Int, Int)): Unit = {
        print(pos)
        }

        def handleKey(key: String): Unit = {
        println(key)
        key match {
            case "Enter" => {play = false}
            case _ =>
        }
    }
    def drawStartingDots(): Unit = {
        for(i <- Window.dots.indices){
            Window.window.setPixel(dots(i).re.toInt, dots(i).im.toInt, Color.black.jColor())
            }
        Window.window.setPixel(currPos.re.toInt, currPos.im.toInt, Color.black.jColor())
        }
    def drawDots(): Unit = {
        var temp = -1
        var tempdiff = new Complex(0,0)
        temp = scala.util.Random.nextInt(Window.dots.length-1)
        currPos = new Complex((currPos.re+((dots(temp).re-currPos.re)/2)).toInt, (currPos.im+((dots(temp).im-currPos.im)/2)).toInt)
        Window.window.setPixel(currPos.re.toInt, currPos.im.toInt, Color.black.jColor())
    }

    def loopUntilQuit(): Unit = while  (!quit) {
        val t0 = System.currentTimeMillis
        if (play) drawDots()
        window.awaitEvent(EventMaxWait)
        while(window.lastEventType != PixelWindow.Event.Undefined) {
            window.lastEventType match {
            case Event.KeyPressed => handleKey(Window.window.lastKey)
            case Event.MousePressed => handleClick(Window.window.lastMousePos)
            case Event.WindowClosed => quit = true
            case _ =>
        }
        window.awaitEvent(EventMaxWait)
    }
    val elapsed = System.currentTimeMillis() - t0
    Thread.sleep((Window.nextGenerationDelay - elapsed) max 0)
}
def start(): Unit = {drawStartingDots(); loopUntilQuit()}
}

object Main {
    var i = 1
    var tempAns = -1
    var tempX = -1
    var tempY =  -1
    def main(args: Array[String]): Unit = {
        while(tempAns == -1){
        try{
               tempAns=io.StdIn.readLine("How many starting dots? Number: ").toInt
            } catch {
                case e: Exception => println("Error: Couldn't understand the input, try again.")
                tempAns=(-1)
            }
        }
        
        while(i != tempAns){
            while(tempX < 0 || tempX > Window.xLength || tempY < 0 || tempY > Window.yLength){
        try{
               tempX = io.StdIn.readLine(s"Insert X coordinate between 0 and ${Window.xLength}: ").toInt
               tempY = io.StdIn.readLine(s"Insert Y coordinate between 0 and ${Window.yLength}: ").toInt
               if(tempX < 0 || tempX > Window.xLength || tempY < 0 || tempY > Window.yLength){
                   throw new IllegalArgumentException("Illegal Coordinate")
               }
            } catch {
                case e: Exception => println("Error: Try again.")
                tempX = -1
                tempY = -1
            }
        }
            Window.dots :+ (new Complex(tempX, tempY)) 
            i = (i+1)
        }
        tempX = -1
        tempY = -1
        while(tempX < 0 || tempX > Window.xLength || tempY < 0 || tempY > Window.yLength){
            try{
                println("Choose Coordinates for starting point")
                tempX = io.StdIn.readLine(s"Insert X coordinate between 0 and ${Window.xLength}: ").toInt
                tempY = io.StdIn.readLine(s"Insert Y coordinate between 0 and ${Window.yLength}: ").toInt
                if(tempX < 0 || tempX > Window.xLength || tempY < 0 || tempY > Window.yLength){
                    throw new IllegalArgumentException("Illegal Coordinate")
                }
                } catch {
                    case e: Exception => println("Error: Try again.")
                    tempX = -1
                    tempY = -1
                }
            }
        Window.currPos = new Complex(tempX, tempY)
        def start(): Unit = {Window.drawStartingDots(); Window.loopUntilQuit()}
    }
}
    