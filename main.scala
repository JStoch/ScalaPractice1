import scala.collection.mutable

@main
def main(): Unit = {
  println(memoizedStirling(5, 3))
}

//calculate stirling number
def stirling (n: Int, m: Int): Int = {
  m match
    case 0 => 0
    case 1 => 1
    case _ => if m==n then 1 else
      if m > n then 0 else stirling(n-1, m-1) + m*stirling(n-1, m)
}

val memStirGlobal = mutable.HashMap[(Int, Int), Int]()

//calculate stirling number using memStirGlobal to memorise previously calculated numbers
def memoizedStirling(n: Int, m: Int): Int = {
  if memStirGlobal.contains((n, m)) then memStirGlobal.apply((n, m))
  else
    var v = 0
    m match
      case 0 => v = 0
      case 1 => v = 1
      case _ => if m == n then v = 1 else if m > n then v = 0 else v = memoizedStirling(n - 1, m - 1) + m * memoizedStirling(n - 1, m)
    memStirGlobal.put((n, m), v)
    v
}

//calculate stirling number using memStir to memorise previously calculated numbers
def memoized_stirling: (Int, Int)=> Int = {
  val memStir = mutable.HashMap[(Int, Int), Int]()

  def memoizedStirling(n: Int, m: Int): Int = {
    if memStir.contains((n, m)) then memStir.apply((n, m))
    else
      println("count"+n+m)
      var v = 0
      m match
        case 0 => v = 0
        case 1 => v = 1
        case _ => if m == n then v = 1 else if m > n then v = 0 else v = memoizedStirling(n - 1, m - 1) + m * memoizedStirling(n - 1, m)
      memStir.put((n, m), v)
      v
  }

  (n, m) => memoizedStirling(n, m)
}

//return a version of the function that uses memoization
def make_memoize[A, B](fun: A => B): A => B = {
  val cache = mutable.HashMap[A, B]()

  (arg: A) => if cache.contains(arg) then cache.apply(arg)
  else
    println("calc")
    val v = fun(arg)
    cache.put(arg, v)
    v
}

//for testing the previous function
def fib(n: Int):Int = {
  if n == 0 || n == 1 then n else fib(n-2) + fib(n-1)
}
