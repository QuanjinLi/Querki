package querki.conversations

import org.scalajs.dom.html
import org.scalajs.dom.raw.Element
import scalatags.JsDom.all.{input => inp, _}

import org.querki.gadgets._
import org.querki.jquery._

import querki.data.TID
import querki.globals._

private [conversations] class NewConversationGadget(val thingId:TID, target: => html.Div)(implicit val ecology:Ecology)
  extends Gadget[html.Div] with EcologyMember
{
  def doRender() = {
    div(
      new ReplyGadget(None, "Start a new conversation...", thingId, { node => onNewConversation(node) })
    )
  }
  
  def onNewConversation(newNode:ConvNode) = {
    val convGadget = new ConversationGadget(newNode, true, thingId)
    $(target).append(convGadget.render)
  }
}

object NewConversationGadget {
  def fromElem(e:Element)(implicit ecology:Ecology):NewConversationGadget = {
    val thingId = TID($(e).dataString("thingid"))
    val isAbove:Boolean = $(e).dataString("convwhere") == "above"
    lazy val gadget:NewConversationGadget = new NewConversationGadget(thingId, {
      val newConv = div().render
      if (isAbove)
        $(gadget.elem).before(newConv)
      else
        $(gadget.elem).after(newConv)
      newConv
    })
    $(e).replaceWith(gadget.rendered)
    gadget
  }
}
