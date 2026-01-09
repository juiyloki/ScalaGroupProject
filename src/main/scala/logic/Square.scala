package logic

case class Square(value: Int, visibility: Boolean) {

  def this() = { this(0, true) }

  def hide: Square = Square(value, false)

  def show: Square = Square(value, true)

  def changeValue(newValue: Int): Square = Square(newValue, visibility)

}