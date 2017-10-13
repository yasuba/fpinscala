def nano() = {
  println("Getting nano")
  System.nanoTime
}

def delayed(t: => Long) = { // => indicates a by-name parameter
  println("In delayed method")
  lazy val t2: Long = t
  println("Param: "+t2)
  delayedAgain(t2)
}

def delayedAgain(t2: Long) = t2

println(delayed(nano()))