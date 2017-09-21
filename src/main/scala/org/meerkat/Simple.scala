package org.meerkat

/**
  * Created by sofysmo on 26.12.16.
  */
object Simple {
  type ParserChar = String => (Boolean, String)

  def parseChar(ch: Char): ParserChar = (str: String) => {
    if (str != null && !str.isEmpty)
      if (str.charAt(0) == ch) (true, str.substring(1))
    (false, "Not found symbol " + ch)
  }

  def seq(p1: ParserChar, p2: ParserChar) = (str: String) => {
    val res1 = p1(str)
    if (res1._1) p2(res1._2)
    else (false, res1._2)
  }
    type Recognizer = Int => Any
  val input = new {
    def startWith(t: String, i: Int): Option[Seq[Int]] = ???
  }

  class  success(i: Int){
     def orElse(s: success): success = ???
   }
object success {
  def apply(i: Int): success = new success(i)
}
  case class failure()

  def parserABC = seq(parseChar('A'), seq(parseChar('B'), parseChar('C')))


  def terminal(t: String): Recognizer =
    i => input.startWith(t, i) match {
      case Some(nums) => nums.map(success(_)).reduceLeft(_.orElse(_))
      case None => failure
    }
}

