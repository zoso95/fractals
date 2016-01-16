package complex

class Complex (real: Double, imaginary:Double){
    def this(real:Double) = this(real,0)
    
	val r: Double = real
	val i: Double = imaginary
	val z = math.sqrt(i*i+r*r)
  	val phi = math.atan2(i, r)
	val cfunc = new ComplexFunctions
	
	def +(x: Double): Complex ={
      return new Complex(x+r,i)
    }
	def +(x: Complex): Complex ={
	  return new Complex(r+x.r, i+x.i)
	}
	def -(x: Complex): Complex ={
	  return new Complex(r-x.r, i-x.i)
	}
	def -(x: Double): Complex ={
	  return new Complex(r-x, i)
	}
	def *(x: Complex): Complex ={
	  return new Complex(r*x.r-i*x.i, i*x.r+r*x.i)
	}
	def *(x: Double): Complex ={
	  return new Complex(r*x, i*x)
	}
	def /(x: Complex): Complex ={
	  val d = x.i*x.i+x.r*x.r
	  return new Complex((r*x.r+i*x.i)/d, (i*x.r-r*x.i)/d)
	}
	def /(x: Double): Complex ={
	  return new Complex(r/x, i/x)
	}
	def ^(x: Complex): Complex ={
	  //z^w = e^(wlog(z))
	  return cfunc.e(x*cfunc.log(this))
	}
	def ^(x: Int): Complex ={
	  if(x==1){return this}
	  if(x==0){return new Complex(1,0)}
	  return this*(this^(x-1))
	}
	override def toString(): String = r+"+"+i+"i";
	
}
class ComplexFunctions {
  	def log(x: Complex): Complex = {
	  return new Complex(math.log(x.z),x.phi)
	}
  	def e(x: Complex): Complex = {
  	  //e^(a+i*b) = e^(a)*cos(b)+ie^(a)sin(b)
  	  val e = math.exp(x.r)
  	  return new Complex(e*math.cos(x.i),e*math.sin(x.i))
  	}
}