package test

import scala.collection.mutable.ListBuffer

/**
 * Class used to understand how the applyDensityFilter function works
 */
object DensityFilterTest extends App {
  val nelx = 6
  val nely = 6
  val nelz = 6
  val rmin = 1.5
  val rminceil = math.ceil(1.5).toInt
  val distinctUnityScale = ListBuffer.empty[Double]
  val distinctFilterWeight = ListBuffer.empty[Double]

  for(i1 <- 0 until nelx) {
    for(k1 <- 0 until nelz) {
      for(j1 <- 0 until nely) {
        val e1 = i1*nely*nelz + k1*nely+j1

        var unityScale: Double = 0.0

        //Loop over neighbourhood
        val i2max = math.min(i1 + (rminceil+1), nelx)
        val i2min = math.max(i1 - (rminceil-1), 0)

        for(i2 <- i2min until i2max) {

          val k2max = math.min(k1 + (rminceil+1), nelz)
          val k2min = math.max(k1 - (rminceil-1), 0)

          for(k2 <- k2min until k2max) {

            val j2max = math.min(j1 + (rminceil + 1), nely)
            val j2min = math.max(j1 - (rminceil - 1), 0)

            for(j2 <- j2min until j2max) {

              val filterWeight = math.max(0.0, rmin - math.sqrt(
                (i1-i2)*(i1-i2) + (j1-j2)*(j1-j2) + (k1-k2)*(k1-k2)
              ))
              unityScale += filterWeight

              var contains = false
              distinctFilterWeight.foreach(a => if(math.abs(a-filterWeight)<1e-5) contains = true)
              if(!contains) {
                distinctFilterWeight += filterWeight
              }
            }
          }
        }
        var contains = false
        distinctUnityScale.foreach(a => if(math.abs(a-unityScale)<1e-5) contains = true)
        if(!contains) {
          distinctUnityScale += unityScale
        }
//        print(f"(i,j,k)=($i1,$j1,$k1), uS=$unityScale%4.4f\n")
      }
    }
  }
  print(s"Distinct unity scale values:\n")
  distinctUnityScale.foreach(a => print(f"$a%10.10f\n"))

  print("Distinct filter weight values:\n");
  distinctFilterWeight.foreach(a => print(f"$a%10.10f\n"));
}
//Unityscale values
//Corners  : 3.2573593129
//Edge     : 3.9289321881
//Face     : 4.6862915010
//internal : 5.5294372515

//each set of i,j,k values can only take on one of these 4 values (given that rmin=1.5)

//Filter weight values
//Same:             1.5
//Side nNeighbour:  0.5
//Far neighbour:    0.0
//corner neighbour: 0.0857864376