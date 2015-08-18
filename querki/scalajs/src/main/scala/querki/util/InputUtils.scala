package querki.util

import org.querki.jquery._

/**
 * @author jducoeur
 */
object InputUtils {
  /**
   * This is a filter you can apply to RxText, to only allow in characters legal for Space names.
   */
  def spaceNameFilter(evt:JQueryEventObject):Boolean = {
    // TODO: this is quite crude, and doesn't allow Unicode through. We should do better.
    // See if there is a library to really do this sort of keyboard filtering well.
    val key = evt.which
    val c = key.toChar
    
    (c >= 'A' && c <= 'Z') || 
    // Numeric keypad
    (key >= 96 && key <= 105) ||
    // Backspace and Tab and Enter
    key == 8 || key == 9 || key == 13 ||
    // Home and End
    key == 35 || key == 36 ||
    // left and right arrows
    key == 37 || key == 39 ||
    // Del and Ins
    key == 46 || key == 45 ||
    (!(evt.shiftKey.get) && 
      (c >= '0' && c <= '9') || 
      key == 189 || // dash 
      c == ' ')    
  }
}