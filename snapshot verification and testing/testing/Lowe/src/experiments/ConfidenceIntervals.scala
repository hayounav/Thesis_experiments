package ox.cads.experiments

/** Object to calculate mean and confidence interval, given an array
  * of data. */
object ConfidenceIntervals{

  /** Gaussian distribution */
  object Gaussian{
    /** Standard Gaussian pdf */
    def phi(x: Double) : Double = Math.exp(-x*x / 2) / Math.sqrt(2 * Math.PI)

    /** Standard Gaussian cdf using Taylor approximation
      * `0.5 + phi(z)*[z + z^3^/3 + z^5^/3.5 + z^7^/3.5.7 + ... ]`
      * (See [[http://en.wikipedia.org/wiki/Normal_distribution#Numerical_approximations_for_the_normal_CDF Wikipedia article]]). */
    def Phi(z: Double) : Double = 
      if(z < -8.0) 0.0
      else if(z>8.0) 1.0 
      else{
	var sum = 0.0; var term = z; var i = 3; val z2 = z*z
	while(sum+term != sum){
          sum += term; term = term * z2 / i; i += 2
	}
	0.5 + sum * phi(z)
      }

    /** Find z s.t. Phi(z) = y */
    def PhiInverse(y: Double) : Double = {
      var lo = -8.0; var hi = 8.0; val delta = 0.00000001
      // Invariant: Phi(lo) <= y <= Phi(hi)
      while(hi-lo >= delta){
	val mid = (hi+lo)/2
	if(Phi(mid) > y) hi = mid else lo = mid
      }
      (hi+lo)/2
    }
  }

  /** Student T distribution */
  object StudentT{
    // private val sf = new specialFunctions()

    /** Cumulative distribution function for Student's t-distribution */
    def cdf(x: Double, dgrF: Int) = {
      val a = 0.5 * specialFunctions.betai(0.5*dgrF, 0.5, dgrF/(dgrF+x*x))
      if(x>0) 1-a else a
    }

    /** Find z s.t. cdf(z) = y */
    def inverseCdf(y: Double, dgrF: Int) = {
      var lo = -1.0E15; var hi = 1.0E15; val delta = 0.00000001
      // Invariant: cdf(lo) <= y <= cdf(hi)
      while(hi-lo >= delta){
	val mid = (hi+lo)/2
	if(cdf(mid, dgrF) > y) hi = mid else lo = mid
      }
      (hi+lo)/2
    }
  }

  private def square(x: Double) = x*x

  /** Find mean and standard deviation of the observations xs. */
  def meanSD(xs: Array[Double])  : (Double, Double) = {
    val n = xs.size; assert(n>1, "Need at least two measurements")
    val mean = xs.sum / n
    val sd = Math.sqrt(xs.map((xi:Double) => square(xi - mean)).sum / (n-1))
    (mean, sd)
  }

  /** Find mean and variance of the observations xs. */
  def meanVar(xs: Array[Double])  : (Double, Double) = {
    val n = xs.size; assert(n>1, "Need at least two measurements")
    val mean = xs.sum / n
    val v = xs.map((xi:Double) => square(xi - mean)).sum / (n-1)
    (mean, v)
  }
  /** Find mean and confidence intervals with significance alpha for the 
    * observations xs.  I.e. returns a pair (m, s) such that in a proportion
    * 1-alpha of cases, the mean of the underlying distribution lies in the 
    * interval [m-s, m+s]. */
  def apply(xs: Array[Double], alpha: Double) : (Double, Double) = {
    val n = xs.size; assert(n>1, "Need at least two measurements")
    val (mean, sd) = meanSD(xs)
    // val mean = xs.sum / n
    // val sd = Math.sqrt(xs.map((xi:Double) => square(xi - mean)).sum / (n-1))
    if(n >= 30){
      val z = Gaussian.PhiInverse(1.0 - alpha/2)
      (mean, z*sd/Math.sqrt(n))
    }
    else{
      val t = StudentT.inverseCdf(1.0 - alpha/2, n-1)
      (mean, t*sd/Math.sqrt(n))
    }
  }


  /** Find mean and confidence intervals with significance alpha for the ratio
    * between the observations ys and xs.  I.e. returns a triple (m,s0,s1)
    * such that m is the ratio of the means of ys and xs, and in a proportion 
    * 1-alpha of cases, the mean of the expectations of the underlying
    * distribution lies in the interval [m-s0, m+s1].  This assumes that the
    * underlying distributions are normally distributed. */
  def apply(xs: Array[Double], ys: Array[Double], alpha: Double) 
    : (Double, Double, Double) 
  = {
    // Means and SDs of the two distributions
    val (meanX, varX) = meanVar(xs);  val (meanY, varY) = meanVar(ys)
    // Variance of the ratio.
    // [http://en.wikipedia.org/wiki/Fieller's_theorem#Case_1].  TODO: find
    // better reference.
    // val varRatio = 
    //   square(meanx/meany)(varx/square(meanx) + vary/square(meany))
    // val sdRatio = Math.sqrt(varRatio)
    
    // Following is taken from
    // http://link.springer.com/content/pdf/10.3758%2FBF03201412.pdf
    val t = StudentT.inverseCdf(1.0 - alpha/2, xs.length+ys.length-2)
    val t2 = square(t)
    val q = 1 - t2*varX/square(meanX)
    val meanRatio = meanY/meanX; val c = meanRatio/q
    val se0 = varY + square(meanY/meanX)*varX - t2*varX*varY/square(meanX)
    val se = Math.sqrt(se0) / (meanX*q)
    val m1 = c-t*se; val m2 = c+t*se // confidence interval [m1, m2]
    (meanRatio, meanRatio-m1, m2-meanRatio) 
  }
}
