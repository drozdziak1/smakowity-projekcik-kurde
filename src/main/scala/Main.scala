@main def main: Unit =
  println("Hello world!")

class SierpinskiFractal(width: Int, height: Int, a: (Int, Int), b: (Int, Int), c: (Int, Int), nIter: Int):
  private var _canvas = Canvas(width, height)

  _canvas.validatePoint(a._1, a._2) match
    case Right(msg) =>
      throw IllegalArgumentException("a point invalid: " + msg)
    case Left(()) => ()

  _canvas.validatePoint(b._1, b._2) match
    case Right(msg) =>
      throw IllegalArgumentException("b point invalid: " + msg)
    case Left(()) => ()

  _canvas.validatePoint(c._1, c._2) match
    case Right(msg) =>
      throw IllegalArgumentException("c point invalid: " + msg)
    case Left(()) => ()

  private val _points = List(a, b, c)

  val rand = scala.util.Random

end SierpinskiFractal

class Canvas(x: Int, y: Int):
  if (x < 0 || y < 0)
    throw IllegalArgumentException("Canvas size below zero")

  private var _buf: List[String] = List.fill(x)(" " * y)

  def validatePoint(pointX: Int, pointY: Int): Either[Unit, String] =
    (pointX > 0, pointY > 0, pointX < x, pointY < y) match
      case (true, true, true, true) => Left(())
      case (false, _, _, _) => Right("pointX must be more than 0")
      case (_, false, _, _) => Right("pointY must be more than 0")
      case (_, _, false, _) => Right(s"pointX must be less than x ($x)")
      case (_, _, _, false) => Right(s"pointY must be less than y ($y)")

  def get(getX: Int, getY: Int): Either[String, String] = validatePoint(getX, getY) match
    case Left(()) =>
      val col = _buf.take(getX + 1).last
      Left(col.take(getY + 1).last.toString)
    case Right(err) => Right(err)

  def set(setX: Int, setY: Int, v: Char): Either[Unit, String] = validatePoint(setX, setY) match
    case Left(()) =>
      val (beforeCols, afterCols) = _buf.splitAt(setX)
      val (beforeVals, afterVals) = afterCols.head.splitAt(setY)
      val colUpdated = beforeVals ++ v.toString ++ afterVals.tail
      _buf = beforeCols ++ (colUpdated :: afterCols.tail)
      Left(())
    case Right(err) => Right(err)



end Canvas
