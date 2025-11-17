class Square(private val value: Int, private val visibility: Boolean) {
  
  def this() = { this(0, true) }
  
  def getValue: Int = value
  
  def getVisibility: Boolean = visibility
  
  def hide: Square = new Square(value, false)
  
  def show: Square = new Square(value, true)
  
  def changeValue(newValue: Int): Square = new Square(newValue, visibility)
  
  def isEqual(someValue: Int): Boolean = someValue == value
  
  def copy: Square = new Square(value, visibility)
  
}
