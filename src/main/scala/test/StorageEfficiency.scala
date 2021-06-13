package test

object StorageEfficiency extends App {
  val N = Seq.range(3,31,2).map(_.toDouble)

  N.foreach(n => {
    val bank0 = math.pow(math.ceil(n/2),3)
    val elem = bank0*3*8
    val ndof = math.pow(n,3)*3
    val eff = (ndof/elem)*100
    println(f"N=$n. lem=$elem, ndof=$ndof, eff=$eff%.2f")
  })
  //At N=13 (NEL=12) we reach 80% effienciency
  //At N=29 (NEL=28) we reach 90% effienciency
}
