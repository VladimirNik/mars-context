//package mars.explorer
//class OuterClass {
//  import scala.collection.mutable.Set
//  def main(arg1: Int) = {
//    import scala.collection.mutable.ArrayBuffer
//    test
//  }
//  def test = println("Hello, Mars!")
//}
package mars
object MarsTester {
  def main(args: Array[String]) = test(42)
  
  def test(a: Int): Unit = runtimeMacro()
  
  def runtimeMacro(): Unit = ???
}