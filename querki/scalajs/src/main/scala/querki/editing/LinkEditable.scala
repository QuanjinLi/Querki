package querki.editing

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import rx._

import querki.globals._

import querki.display.Gadget
import querki.display.input.InputGadget

class LinkEditable(implicit e:Ecology) extends InputGadget[dom.HTMLSelectElement](e) {
  def values = ???
  
  lazy val inspector = new LinkSelectInspector(elem)
  
  def hook() = {
    
  }
  
  def doRender() = ???
}

class LinkSelectInspector(sel:dom.HTMLSelectElement) {
  def selectedOption = $(sel).find(":selected")
  
  def isSelected = sel.length == 1
  
  def selectedQ(func:JQuery => String):Option[String] = {
    val sel = selectedOption
    if (isSelected)
      Some(func(sel))
    else
      None      
  }
  def selectedValue = selectedQ(_.valueString)
  def selectedDisplay = selectedQ(_.html())
}

class LinkDisplayView(insp:LinkSelectInspector) extends Gadget[dom.HTMLDivElement] {
  def doRender() =
    div(
      insp.selectedDisplay
    )
}
