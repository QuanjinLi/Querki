package models

import querki.globals._
import querki.ql.{Invocation, QLFunction}
import querki.values.{PropAndVal, QLContext, QValue, RequestContext}

class ThingOps(thing:Thing)(implicit e:Ecology) extends PropertyBundleOps(thing) with QLFunction {
  
  def Core = interface[querki.core.Core]
  def Basic = interface[querki.basic.Basic]
  def QL = interface[querki.ql.QL]
  def Renderer = interface[querki.html.HtmlRenderer]
  
  def ApplyMethod = Basic.ApplyMethod
  def DisplayNameProp = Basic.DisplayNameProp
  def NameProp = Core.NameProp
  
  def id = thing.id
  def model = thing.model
  def props = thing.props
  
  def thisAsQValue:QValue = Core.ExactlyOne(Core.LinkType(thing.id))
  
  def localProp[VT, CT](prop:Property[VT, _]):Option[PropAndVal[VT]] = thing.localProp(prop)
  def getPropOpt[VT](prop:Property[VT, _])(implicit state:SpaceState):Option[PropAndVal[VT]] = thing.getPropOpt(prop)
  
  def fullLookupDisplayName:Option[PropAndVal[_]] = {
    val dispOpt = localProp(DisplayNameProp)
    if (dispOpt.isEmpty || dispOpt.get.isEmpty)
      localProp(NameProp)
    else
      dispOpt
  }
  
  def nameOrComputed(implicit request:RequestContext, state:SpaceState):Future[DisplayText] = {
    val localName = fullLookupDisplayName
    def fallback() = DisplayText(id.toThingId.toString)
    if (localName.isEmpty) {
      val computed = for {
        pv <- getPropOpt(Basic.ComputedNameProp)
        v <- pv.firstOpt
      }
        yield Future.successful(QL.process(v, thisAsContext).raw)
      computed.getOrElse(Future.successful(fallback()))
    } else {
      localName.get.renderPlain.map { rend =>
        val rendered = rend.raw
        if (rendered.length() > 0)
          rendered
        else
          fallback()
      }
    }    
  }
  
  def unsafeNameOrComputed(implicit rc:RequestContext, state:SpaceState):Future[String] = {
    val localName = fullLookupDisplayName
    def fallback() = id.toThingId.toString
    if (localName.isEmpty) {
      val computed = for {
        pv <- getPropOpt(Basic.ComputedNameProp)
        v <- pv.firstOpt
      }
        yield Future.successful(QL.process(v, thisAsContext).plaintext)
      computed.getOrElse(Future.successful(fallback()))
    } else {
      localName.get.renderPlain.map { rend =>
        val rendered = rend.plaintext
        if (rendered.length() > 0)
          rendered
        else
          fallback()
      }
    }
  }
  
  /**
   * True iff the other is an ancestor of this Thing via the Model chain.
   */
  def isAncestor(other:OID)(implicit state:SpaceState):Boolean = {
    (other == model) || thing.getModelOpt.map(_.isAncestor(other)).getOrElse(false)
  }
  
  /**
   * Convenience method, to check whether a YesNo Property is non-empty, and is true.
   */
  def ifSet(prop:Property[Boolean, _])(implicit state:SpaceState):Boolean = {
    thing.firstOr(prop, false)
  }
  
  /**
   * Returns true iff this Thing has the IsModel flag set to true on it.
   */
  def isModel(implicit state:SpaceState):Boolean = {
    ifSet(Core.IsModelProp)
  }
  
  def renderProps(implicit request:RequestContext, state:SpaceState):Future[Wikitext] = {
    Renderer.renderThingDefault(thing)
  }
  
  /**
   * Show the default rendering for this Thing, if it has no DisplayTextProp defined.
   * 
   * This mainly exists so that the different Kinds can override it and do their own thing.
   */
  def renderDefault(implicit request:RequestContext, state:SpaceState):Future[Wikitext] = {
    renderProps
  }
  
  /**
   * Every Thing can be rendered -- this returns a Wikitext string that will then be
   * displayed in-page.
   * 
   * If you specify a property, that property will be rendered with this Thing as a context;
   * otherwise, DisplayText will be rendered.
   */
  def render(implicit request:RequestContext, state:SpaceState, prop:Option[Property[_,_]] = None):Future[Wikitext] = 
  {
    val actualProp = 
      if (ifSet(Core.IsModelProp))
        prop.getOrElse(Basic.ModelViewProp)
      else
        prop.getOrElse(Basic.DisplayTextProp)
    val renderedOpt = for (
      pv <- getPropOpt(actualProp);
      if (!pv.isEmpty)
        )
      yield pv.render(thing.thisAsContext.forProperty(pv.prop), Some(thing))
    
    renderedOpt.getOrElse(renderDefault)
  }
  
  /**
   * Called when this Thing is encountered with no method invocation in a QL expression.
   * Subclasses are allowed to override it as make sense.
   * 
   * This basic version returns a Link to this thing.
   * 
   * Callers should generally prefer qlApplyTop instead, since that allows somewhat more
   * powerful operations.
   */
  def qlApply(inv:Invocation):QValue = {
    val context = inv.context
    val paramsOpt = inv.paramsOpt
    
    val applyOpt = getPropOpt(ApplyMethod)(context.state)
    applyOpt match {
      case Some(apply) => {
        val qlText = apply.first
        QL.processMethod(qlText, context.forProperty(apply.prop), Some(inv), Some(thing))
      }
      case None => Core.ExactlyOne(Core.LinkType(thing.id))
    }
  }
  
  /**
   * The wrapper around qlApply(), which is actually called from the outside. Specific Things may
   * override this if they need to return a QLContext instead of simply a QValue.
   */
  def qlApplyTop(inv:Invocation, transformThing:Thing):QLContext = {
    inv.context.nextFrom(qlApply(inv), transformThing)
  }
}
