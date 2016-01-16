package fractal
import java.awt.image._
import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import java.util.Random

object Painter {
  
  def CreateFractal(width:Int, height:Int, f:complex.Complex=>complex.Complex,
      threshold:Double, numIterations: Int, top:(Double,Double), bottom: (Double,Double)) : Unit = {
    val image = new BufferedImage(width, height,BufferedImage.TYPE_INT_RGB)
    var coordinates:List[(Int,Int)] = Nil
    
    for {y <- 0 until height
    	x <- 0 until width }{
    		coordinates = (x,y)::coordinates
	 }
    coordinates.foreach((c: (Int,Int)) => image.setRGB(c._1,c._2,
        IterationsToColor(CalculateIterations(TransformCoordinates(c,width,height,top,bottom),f, threshold, numIterations))));
    WriteOutImage(image, threshold, numIterations,top,bottom)
  }
  
  var numberOfIterations = 0.0
  def TransformCoordinates(coord: (Int,Int), width:Int, height:Int, top:(Double,Double)
      , bottom:(Double,Double)): (Double,Double)={
    numberOfIterations +=1
    if(numberOfIterations %50000 ==0){println(numberOfIterations/(width*height))}
    //t(-2,-2), b(2,-2)
    val difx = math.abs(bottom._1- top._1)
    val dify = math.abs(bottom._2 -top._2)
    return (bottom._1 + ((coord._1 +0.0)/width)*difx,bottom._2 + ((coord._2+0.0)/height)*dify)
  }
  def WriteOutImage(image: BufferedImage, threshold:Double,
      numIterations:Int,top:(Double,Double),bottom:(Double,Double)): Unit={
    val name = "fractal_"+threshold+"_"+numIterations+"_"+top+bottom+ new Random().nextFloat()+".png"
    println(name)
    ImageIO.write(image, "png", new File(name));
  }
  
  def CalculateIterations(coord:(Double,Double), f:complex.Complex=>complex.Complex,
      threshold:Double, numIterations: Int) : Double = {
    var iterations = 0.0
    var c = new complex.Complex(coord._1 , coord._2 )
    while(iterations<numIterations){
      c = f(c)
     
      if(c.z>threshold){
       
        return iterations/threshold
        }
      iterations+=1
    }
    
    return iterations/threshold
  }
  
  def IterationsToColor(i: Double) : Int ={
    val c = Color.getHSBColor(i.toFloat, 1, 1)
    return c.getRGB()
    }
  
  def RandomFunction(num:Int, pow:Int, seed:Int, const: Double, coeffe: Int) : complex.Complex=>complex.Complex={
    val r = new Random(seed)
    var f:List[complex.Complex=>complex.Complex]= List()
    for{i <- 0 until num}{
    	val power = r.nextInt(1+pow)
    	val coeff = 1+r.nextInt(coeffe)
    	println(power,coeff)
        f= ((c:complex.Complex)=>(c^(power))*coeff)::f
  }
    f= ((c:complex.Complex)=>new complex.Complex(const))::f
    return (c:complex.Complex)=>f.foldLeft(new complex.Complex(0.0))(
        (m: complex.Complex, n: complex.Complex=>complex.Complex) => m + n(c))
  }
 
  def main(args: Array[String]){
    val const = new complex.Complex(1)
    val cf = new complex.ComplexFunctions
    def man(c:complex.Complex):complex.Complex = {return (c^1)*3+new complex.Complex(1,0)}
    //CreateFractal(1000,1000,man,5,50, (1.5,1.5), (-1.5,-1.5))
    for{i <- 0 until 20}{
      //CreateFractal(1000,1000, RandomFunction(2, 5,i,1,5),5,100, (1.5,1.5), (-1.5,-1.5))
      //CreateFractal(1000,1000, RandomFunction(3, 8,i,1,5),5,100, (1.5,1.5), (-1.5,-1.5))
    CreateFractal(3000,3000, RandomFunction(4,12,i,1,4),5,100, (1.5,1.5), (-1.5,-1.5))
    CreateFractal(3000,3000, RandomFunction(3,6,i,1,4),5,100, (1.5,1.5), (-1,-1.5))
    }
  }

}