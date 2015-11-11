package querki.html

import scala.xml.{Attribute, NodeSeq, Null, Text, Xhtml}

import scalatags.Text.all.{id => idAttr, _}
import scalatags.Text.TypedTag

import models.{DisplayText, FieldIds, HtmlWikitext, OID, PropertyBundle, QWikitext, SimplePTypeBuilder, UnknownOID, Wikitext}
import models.Thing.PropMap

import querki.core.URLableType
import querki.ecology._
import querki.globals._
import querki.ql.{InvocationValue, QLParam, QLPhrase}
import querki.util.{HtmlEscape, SafeUrl, XmlHelpers}
import querki.values._

object UIMOIDs extends EcotIds(11) {
  val SectionMethodOID = sysId(43)
  val LinkButtonOID = sysId(51)
  val IconButtonOID = sysId(68)
  val CreateInstanceLinkOID = sysId(69)
  val ShowLinkMethodOID = sysId(95)
  val PropLinkMethodOID = sysId(96)
  
  val ClassMethodOID = moid(1)
  val TooltipMethodOID = moid(2)
  val DataMethodOID = moid(3)
  val PageHeaderPropOID = moid(4)
  val QLButtonOID = moid(5)
  val MixedButtonOID = moid(6)
  val CreateButtonOID = moid(7)
  val ShowSomeOID = moid(8)
  val QLLinkOID = moid(9)
  val QLTreeOID = moid(10)
}

/**
 * TODO: this should probably be merged with HtmlRenderer -- the distinction between them looks pretty
 * artificial from the outside.
 */
class UIModule(e:Ecology) extends QuerkiEcot(e) with HtmlUI with querki.core.MethodDefs {
  import UIMOIDs._

  lazy val Basic = interface[querki.basic.Basic]
  lazy val HtmlRenderer = interface[querki.html.HtmlRenderer]
  lazy val Links = interface[querki.links.Links]
  val Logic = initRequires[querki.logic.Logic]
  lazy val PublicUrls = interface[PublicUrls]
  val QL = initRequires[querki.ql.QL]
  lazy val Tags = interface[querki.tags.Tags]
  
  lazy val ExternalLinkType = Links.URLType
  lazy val NewTagSetType = Tags.NewTagSetType
  lazy val ParsedTextType = QL.ParsedTextType

  /***********************************************
   * TYPES
   ***********************************************/

  /**
   * This is a fake PType, so that code can inject HTML into the pipeline.
   * 
   * Note that this doesn't get registered in System, since it doesn't exist from the User's perspective.
   */
  lazy val RawHtmlType = new SystemType[Wikitext](UnknownOID, models.Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
  {
    def doDeserialize(v:String)(implicit state:SpaceState) = throw new Exception("Can't deserialize ParsedText!")
    def doSerialize(v:Wikitext)(implicit state:SpaceState) = throw new Exception("Can't serialize ParsedText!")
    def doWikify(context:QLContext)(v:Wikitext, displayOpt:Option[Wikitext] = None, lexicalThing:Option[PropertyBundle] = None) = 
      Future.successful(v)
    
    def doDefault(implicit state:SpaceState) = Wikitext("")
    
    // Transient values, so we don't care:
    def doComputeMemSize(v:Wikitext):Int = 0
  }

  def HtmlValue(html:QHtml):QValue = ExactlyOne(RawHtmlType(HtmlWikitext(html)))
  def HtmlValue(str:String):QValue = HtmlValue(QHtml(str))
  def HtmlValue(xml:NodeSeq):QValue = HtmlValue(Xhtml.toXhtml(xml))
  def HtmlValue(tag:TypedTag[_]):QValue = ExactlyOne(RawHtmlType(HtmlWikitext(tag.toString)))
  
  def toWikitext(xml:NodeSeq):Wikitext = HtmlWikitext(QHtml(Xhtml.toXhtml(xml)))

  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  /**
   * This is the abstraction of a single-parameter function that takes some HTML and modifies it. It is
   * mainly intended for use with functions that change the attributes of the HTML.
   */
  abstract class HtmlModifier(oid:OID, name:String, summary:String, details:String) extends InternalMethod(oid, 
    toProps(
      setName(name),
      Summary(summary),
      Details(details))) 
  {
    // Actual Modifier classes should implement this, which does the heart of the work
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLParam]):Future[NodeSeq]
    
    override def qlApply(inv:Invocation):QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
      
      val v = context.value
      if (v.pType != RawHtmlType && v.pType != ParsedTextType)
        throw new PublicException("UI.transform.htmlRequired", name)
      if (paramsOpt.isEmpty)
        throw new PublicException("UI.transform.classRequired", name)
      val params = paramsOpt.get
      
      def contentToUse:InvocationValue[DisplayText] = {
        v.pType match {
          case RawHtmlType => 
            for {
              wikitext <- inv.contextAllAs(RawHtmlType)
            }
              yield wikitext.display
          case ParsedTextType =>
            for {
              wikitext <- inv.contextAllAs(ParsedTextType)
            }
              yield wikitext.span
        }
      }
      
      for {
        parsedParam <- inv.processParamFirstAs(0, ParsedTextType)
        paramText = parsedParam.raw.toString
        content <- contentToUse
        nodes = XmlHelpers.toNodes(content)
        newXmlFuts = nodes.map(node => doTransform(node, paramText, context, params))
        newXml <- inv.fut(Future.sequence(newXmlFuts).map(_.flatten))
        newHtml = QHtml(Xhtml.toXhtml(newXml))
      }
        yield QL.WikitextValue(HtmlWikitext(newHtml))
    }
  }
  
  lazy val classMethod = new HtmlModifier(ClassMethodOID, "_class",
      "Add a class tag to the received HTML value",
      """Usually, to add HTML classes to something (to make them look pretty via CSS), you use the
            |\{\{class:...\}\} mechanism. But that *wraps* the text, inside of a div or span, and while that is
            |usually good enough, sometimes it doesn't do everything you need. So _class provides an alternate way
            |to do this via QL -- given an HTML block, this adds the class to *that* block, instead of wrapping it
            |in another.
            |
            |This is mainly intended for use with _edit, to do something like this:
            |[[_code(""[[My Thing -> My Prop._edit -> _class(""myClass"")]]"")]]
            |This will create an Edit input for My Prop, with myClass attached so you can control its display.
            |
            |This can also be used to add a class to a given text block:
            |[[_code(""[[""Hello world"" -> _class(""myClass"")]]"")]]
            |This will create a paragraph for "hello world" as usual, but will attach "myClass" as a class on that
            |paragraph. (This is less often necessary, but occasionally helpful.)""".stripMargin)
  {
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLParam]):Future[NodeSeq] = 
      Future.successful(HtmlRenderer.addClasses(nodes, paramText))
  }
  
  lazy val tooltipMethod = new HtmlModifier(TooltipMethodOID, "_tooltip",
      "Add a tooltip to the received HTML value",
      """When you have a title, or some other short text like that, you sometimes want a "tooltip" -- a little
      |pop-up -- with a better description of what it means. This function lets you add that.
      |
      |Since _tooltip is a function, you have to use it inside a QL expression, like this:
      |[[_code(""[[""My Thing"" -> _tooltip(""My Thing is a special sort of Thing"")]]"")]]
      |In the long run, you will be able to describe a tooltip without using a QL expression, but
      |for now, this is the way to do it.""".stripMargin)
  {
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLParam]):Future[NodeSeq] = {
      val withClass = HtmlRenderer.addClasses(nodes, "_withTooltip")      
      Future.successful(XmlHelpers.mapElems(withClass)(_ % Attribute("title", Text(paramText), Null)))
    }
  }
  
  lazy val dataMethod = new HtmlModifier(DataMethodOID, "_data",
      "Add HTML5 data to the received HTML value",
      """This is mainly for internal use for now. Similarly to the _class function, this lets
      |you add a data tag to a block. So for example, this:
      |[[_code(""[[""Hello world"" -> _data(""foo"", ""something"")]]"")]]
      |will add a "data-foo" attribute to the block containing Hello world.""".stripMargin)
  {
    def doTransform(nodes:NodeSeq, paramText:String, context:QLContext, params:Seq[QLParam]):Future[NodeSeq] = {
      if (params.length < 2)
        throw new PublicException("UI.transform.dataRequired")
      
      for {
        processed <- context.parser.get.processPhrase(params(1).ops, context)
        dataBlock = processed.value.firstTyped(ParsedTextType).getOrElse(throw new PublicException("UI.transform.dataRequired")).raw
      }
        yield XmlHelpers.mapElems(nodes)(_ % Attribute(s"data-$paramText", Text(dataBlock), Null))
    }
  }
  
  lazy val PageHeaderProperty = new SystemProperty(PageHeaderPropOID, LargeTextType, Optional,
    toProps(
      setName("Page Header"),
      SkillLevel(SkillLevelAdvanced),
      Summary("Allows you to define the top of the page when looking at this Thing"),
      Details("""Normally, Querki displays each Thing with a fairly complex predefined header,
          |which includes its Display Name, Space, Model, edit buttons and so on. This works well
          |for most cases, but if you want more control over the look and feel of your display, you
          |can override that by setting this Property.""".stripMargin)))

  /***********************************************
   * FUNCTIONS
   ***********************************************/

  class SectionMethod extends InternalMethod(SectionMethodOID,
    toProps(
      setName("_section"),
      Summary("Display a List as a Header, followed by its contents"),
      Details("""_section is intended for the common case where you want to display a section
          |on the page if and only if a specific List is non-empty. It looks like this:
          |    My List -> _section(HEADER, DETAILS, EMPTY)
          |Each of the parameters can be any QL phrase, although they are often just text blocks. They are
          |treated as follows:
          |
          |* HEADER is shown first, if the incoming List is non-empty. It gets the entire List as its Context.
          |* DETAILS is shown after the header, also getting the incoming List as its Context.
          |* EMPTY is shown if and only if the List is empty. This lets you show something else if appropriate.
          |It is optional -- you can leave it off.
          |
          |Note that the generated QText will have the HEADER on a separate line from the DETAILS. This is
          |usually what you want. It means that, for example, if you start the HEADER with "###", it will show
          |up as a true header, separate from the DETAILS, but if it is just something like "Contents:", the
          |HEADER and DETAILS will run together, since QText joins ordinary text lines together.""".stripMargin)
    )) 
  {
    override def qlApply(inv:Invocation):QFut = {
      val context = inv.context
      val paramsOpt = inv.paramsOpt
    
      paramsOpt match {
        case Some(params) if (params.length > 0) => {
          val header = params(0).phrase
          val details = if (params.length > 1) Some(params(1).phrase) else None
          val empty = if (params.length > 2) Some(params(2).phrase) else None
          buildSection(context, header, details, empty)
        }
        case _ => Future.successful(QL.ErrorValue("_section requires at least one parameter"))
      }
    }
  
    def buildSection(context:QLContext, header:QLPhrase, detailsOpt:Option[QLPhrase], emptyOpt:Option[QLPhrase]):QFut = {
      val parser = context.parser.get
      val wikitextFut = if (context.isEmpty) {
        parser.contextsToWikitext(emptyOpt.map(empty => Seq(parser.processPhrase(empty.ops, context.root))).getOrElse(Seq.empty))
      } else {
        for {
          processedHeader <- parser.contextsToWikitext(Seq(parser.processPhrase(header.ops, context.forceAsCollection)))
          processedDetails = detailsOpt.map(details => Seq(parser.processPhrase(details.ops, context)))
          detailContents <- processedDetails.map(parser.contextsToWikitext(_, true)).getOrElse(Future.successful(Wikitext("")))
        }
          yield processedHeader + detailContents
      }
      wikitextFut.map(wikitext => QL.WikitextValue(wikitext))
    }
  }

	abstract class ButtonBase(tid:OID, pf:PropMap) extends InternalMethod(tid, pf)
	{
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem
	  
	  def numParams:Int
	  
	  override def qlApply(inv:Invocation):QFut = {
	    val context = inv.context
	    val paramsOpt = inv.paramsOpt
	    
	    paramsOpt match {
	      case Some(params) if (params.length == numParams) => {
	        val urlOpt = context.value.pType match {
	          case pt:URLableType => context.value.firstOpt.flatMap(pt.getURL(context)(_))
	          case _ => None
	        }
	        
	        urlOpt match {
	          case Some(url) => {
              Future.sequence(
                params.map(param => 
                  context.parser.get.processPhrase(param.phrase.ops, context).flatMap(_.value.wikify(context))))
              .map { paramTexts =>
	              HtmlValue(QHtml(generateButton(url, paramTexts).toString))
              }
	          }
	          // Nothing incoming, so cut.
	          // TODO: there is probably a general pattern to pull out here, of "cut processing if the input is empty"
	          case None => Future.successful(EmptyValue(RawHtmlType))
	        }
	      }
	      case None => QL.WarningFut(displayName + " requires " + numParams + " parameters.")
	    }
	  }
	}

	// TODO: once we have named parameters, both _iconButton and _mixedButton should be reprecated, and _linkButton
	// should take named params of "icon=", "label=" and "tooltip=". That would be much cleaner and more consistent.
	class LinkButtonMethod extends ButtonBase(LinkButtonOID,
	    toProps(
	      setName("_linkButton"),
	      Summary("Displays a button that goes to a linked page when you press it."),
	      Details("""    LINK -> _linkButton(LABEL)
	          |_linkButton receives a Link or External Link, and displays that
	          |link as a button. It expects one parameter, which will be the label of the button.""".stripMargin)))
	{
	  val numParams = 1
	  
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
	    <a class="btn btn-primary" href={url}>{params(0).raw}</a>
	  }
	}
	
	class IconButtonMethod extends ButtonBase(IconButtonOID,
	    toProps(
	      setName("_iconButton"),
	      Summary("Displays a button showing an icon, that goes to a linked page when you press it."),
	      Details("""    LINK -> _iconButton(ICON, TOOLTIP)
	          |_iconButton receives a Link or External Link, and displays that
	          |link as a button. The first parameter identifies the icon to use for the button; the second is the
	          |hover text to display as a tooltip. Both parameters are required.
	          |
	          |For icons, you may use anything from the [Bootstrap Glyphicon](http://getbootstrap.com/2.3.2/base-css.html#icons) set.
	          |Just use the name of the icon (in double-double quotes) in the parameter.""".stripMargin)))
	  {
	  val numParams = 2
	  
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
	    <a class="btn btn-default btn-xs btn-primary" href={url} title={params(1).raw}><i class={"glyphicon glyphicon-" + params(0).raw}></i></a>
	  }
	}
	
	class MixedButtonMethod extends ButtonBase(MixedButtonOID,
	    toProps(
	      setName("_mixedButton"),
	      Summary("Displays a button showing an icon and a text label, that goes to a linked page when you press it."),
	      Details("""    LINK -> _mixedButton(ICON, LABEL)
	          |_mixedButton receives a Link or External Link, and displays that
	          |link as a button. The first parameter identifies the icon to use for the button; the second is the
	          |text that follows the icon. Both parameters are required. This is essentially a combo of _iconButton
	          |and _linkButton.
	          |
	          |For icons, you may use anything from the [Bootstrap Glyphicon](http://getbootstrap.com/2.3.2/base-css.html#icons) set.
	          |Just use the name of the icon (in double-double quotes) in the parameter.""".stripMargin)))
	  {
	  val numParams = 2
	  
	  def generateButton(url:String, params:Seq[Wikitext]):scala.xml.Elem = {
	    <a class="btn btn-default btn-sm btn-primary" href={url}><i class={"glyphicon glyphicon-" + params(0).raw}></i> {params(1).raw}</a>
	  }
	}
	
	// TODO: this is very similar to _linkButton, and should be refactored.
	class ShowLinkMethod extends InternalMethod(ShowLinkMethodOID,
	    toProps(
	      setName("_showLink"),
	      Summary("Displays a Link or External Link as a normal HTML link."),
	      Details("""    LINK -> _showLink(LABEL)
	          |This is the most normal way to display a Link or External Link with a chosen label. The
	          |label may be any expression you choose.
	          |
	          |The default behaviour of a Link, if you don't do anything with it, is effectively
	          |"_showLink(Default View)".""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    for {
	      pt <- inv.contextTypeAs[URLableType]
	      elemContext <- inv.contextElements
	      elemV <- inv.opt(elemContext.value.firstOpt)
	      url <- inv.opt(pt.getURL(elemContext)(elemV))
	      paramVal <- inv.processParam(0, elemContext)
	      label <- inv.fut(paramVal.wikify(elemContext))
	      wikitext = QWikitext("[") + label + QWikitext(s"]($url)")
      }
	      yield QValue.make(ExactlyOne, ParsedTextType, wikitext)
	  }
	}
		
	class PropLinkMethod extends InternalMethod(PropLinkMethodOID, 
	    toProps(
	      setName("_propLink"),
	      Summary("""Produces a Link to a specific Property on a Thing."""),
	      Details("""    THING -> PROPERTY._propLink -> EXTERNAL LINK
	          |A common pattern in Querki is to provide alternate "views" for a Thing -- different ways of displaying it.
	          |Typically, you do this by creating another Large Text Property (separate from Default View), which contains
	          |the alternate view, and then linking to that somewhere. This method makes it easy to do so: feed the THING
	          |and PROPERTY into _propLink, and the result is an EXTERNAL LINK which you can then pass to _showLink,
	          |_linkButton or _iconButton.
	          |
	          |NOTE: this currently only works for Things in the local Space, and probably does *not* work correctly in
	          |sub-Spaces yet.
	          |
	          |This will work for any Property Type, even Types that don't really make sense as Views, so use with a bit
	          |of care!""".stripMargin)))
	{
	  override def qlApply(inv:Invocation):QFut = {
	    for (
	      thing <- inv.contextAllThings;
	      prop <- inv.definingContextAsProperty
	    )
	      yield ExactlyOne(ExternalLinkType(thing.toThingId + "?prop=" + prop.toThingId))
	  }
	}
	
  def getCreateInstanceUrl(inv:Invocation):InvocationValue[String] = {
    implicit val state = inv.state
      // First, figure out the linkback if there is one:
      val linkParamFut:Future[String] = inv.definingContext match {
        case Some(definingContext) => {
          val invStr = for {
            lexicalThing <- inv.opt(inv.lexicalThing match { case Some(t:Thing) => Some(t); case _ => None})
            linkProp <- inv.definingContextAsProperty
            fieldId = new FieldIds(None, linkProp)
            backLink <- inv.opt(linkProp.pType match {
              case LinkType => Some(lexicalThing.id.toThingId.toString)
              case NewTagSetType => lexicalThing.linkName
              case _ => None
            })
            backLinkSafe = SafeUrl(backLink)
          }
            yield s"&${fieldId.inputControlId}=$backLinkSafe"
            
          invStr.get.map(_.headOption.getOrElse(""))
        }
        case _ => Future.successful("")
      }
      
      for {
        thing <- inv.contextFirstThing
        url <- inv.fut(linkParamFut.map(PublicUrls.createAndEditUrl(inv.context.request, thing.toThingId) + _))
      }
        yield url
  }
	
  lazy val CreateInstanceLinkMethod = new InternalMethod(CreateInstanceLinkOID,
    toProps(
      setName("_createInstanceLink"),
      Summary("Given a received Model, this produces a Link to create an instance of that Model."),
      Details("""    MODEL -> _createInstanceLink -> _linkButton(LABEL)
	    |This is how you implement a "Create" button. _createInstanceLink takes a MODEL, and produces an External Link to the page to create a new Instance of it.
	    |
	    |You will usually then feed this into, eg, _linkButton or _iconButton as a way to display the Link.
        |
        |    MODEL -> LINK PROPERTY._createInstanceLink -> _linkButton(LABEL)
        |
        |You may optionally specify a Link Property with _createInstanceLink. That means that the newly-created Instance
        |should point back to *this* Thing -- the one where you have the button -- through the specified Link Property.
        |This is very useful for creating "child" Thing easily.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        url <- getCreateInstanceUrl(inv)
      }
        yield ExactlyOne(ExternalLinkType(url))
    }
  }
	
  lazy val CreateButtonFunction = new InternalMethod(CreateButtonOID,
    toProps(
      setName("_createButton"),
      Summary("Becomes a Create button for the received Model"),
      Details("""    MODEL -> LINK PROPERTY._createButton(LABEL)
          |
          |This displays a button, with the given LABEL, if the user is allowed to create Instances of that Model.
          |
          |This button is essentially a shortcut for:
          |
          |    MODEL -> LINK PROPERTY._createInstanceLink -> _linkButton(LABEL)
          |
          |As with [[_createInstanceLink._self]], the LINK PROPERTY is optional. If you give one, it says that
          |Querki should set LINK PROPERTY on the newly-created Thing, pointing back to the Thing that it showing the
          |button.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        url <- getCreateInstanceUrl(inv)
        labelWikitext <- inv.processParamFirstAs(0, QL.ParsedTextType)
        label = labelWikitext.raw.str
        xml = <a class="btn btn-default" href={url}>{label}</a>
      }
        yield QL.WikitextValue(toWikitext(xml))
    }
  }
  
  abstract class ClickableQLBase(oid:OID, pf:PropMap) extends InternalMethod(oid, pf)
  {
    def buildHtml(label:String, core:String):String
    
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextFirstThing
        labelWiki <- inv.processParamFirstAs(0, ParsedTextType)
        label = HtmlEscape.escapeQuotes(labelWiki.raw.str.trim)
        qlRaw <- inv.rawParam(1)
        ql = HtmlEscape.escapeQuotes(qlRaw.reconstructString)
        target <- inv.processParamFirstOr(2, ParsedTextType, Wikitext(""))
        (targetName, targetDiv) =
          if (target.plaintext.length() == 0) {
            val name = "target-" + scala.util.Random.nextInt.toString 
            (name, s"""<div id="$name"></div>""")
          } else {
            (target.raw.str.trim, "")
          }
      }
        yield 
          HtmlValue(
            buildHtml(label, s"""data-thingid="${thing.toThingId}" data-target="$targetName" data-ql="$ql" href="#" """) + targetDiv)
    }
  }

  lazy val QLButton = new ClickableQLBase(QLButtonOID,
    toProps(
      setName("_QLButton"),
      Summary("Shows a button that, when pressed, executes some QL and can show the result"),
      SkillLevel(SkillLevelAdvanced),
      Details("""    THING -> _QLButton(LABEL, QL, TARGET)
          |
          |This function is unusual, in that it is a way to do something only if the user presses a button.
          |It displays a button with the given LABEL; if the user presses that, it evaluates the given QL
          |(using the received THING as its context). 
          |
          |If a TARGET is specified, that should be the id of a div or span to put the results into; if it
          |is not given, the results will be displayed below the button.
          |
          |As an example of how to use this, say you have a complex Model Property that you want to make
          |editable on the Thing's page, but you only want to show it when needed. You can say:
          |
          |    \[[_QLButton(\""Edit My Model Property\"", My Model Property._edit)\]]""".stripMargin)))
	{
    def buildHtml(label:String, core:String):String = {
      s"""<input type="button" value="$label" class="btn btn-primary _qlInvoke" $core></input>"""
    }
	}

  lazy val QLLink = new ClickableQLBase(QLLinkOID,
    toProps(
      setName("_QLLink"),
      Summary("Shows a link that, when clicked, executes some QL and can show the result"),
      SkillLevel(SkillLevelAdvanced),
      Details("""    THING -> _QLLink(LABEL, QL, TARGET)
          |
          |This function is unusual, in that it is a way to do something only if the user clicks a link.
          |It displays a link with the given LABEL; if the user clicks that, it evaluates the given QL
          |(using the received THING as its context).
          |
          |If a TARGET is specified, that should be the id of a div or span to put the results into; if it
          |is not given, the results will be displayed below the link.
          |
          |As an example of how to use this, say you have a complex Model Property that you want to make
          |editable on the Thing's page, but you only want to show it when needed. You can say:
          |
          |    \[[_QLLink(\""Edit My Model Property\"", My Model Property._edit)\]]""".stripMargin)))
  {
    def buildHtml(label:String, core:String):String = {
      s"""<a class="_qlInvoke" $core>$label</a>"""
    }
  }
  
  lazy val ThingTree = new InternalMethod(QLTreeOID,
    toProps(
      setName("_thingTree"),
      SkillLevel(SkillLevelAdvanced),
      Signature(
        expected = (Seq(LinkType), "One or more Things to display as equal nodes in a tree"),
        reqs = Seq(),
        opts = Seq(
          ("text", TextType, Core.QNone, "The text to display for this node. If omitted, will be shown as a link to this Thing as usual."),
          ("children", Basic.QLType, Core.QNone, "A QL expression that produces the children of this node, which should be more _QLTrees. If empty, this node is a leaf."),
          ("id", TextType, Core.QNone, "The jsTree id to use for this node."),
          ("icon", TextType, Core.QNone, "The icon to display for this node. If not specified, no icon will be shown."),
          ("opened", YesNoType, ExactlyOne(Logic.False), "If set to True, children of this node will be displayed immediately.")
        ),
        returns = None
      ),
      Summary("Display a tree node, which will invoke the specified QL code to get its children")))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        (bundle, elem) <- inv.contextBundlesAndContexts
        thing <- inv.opt(bundle.asThing)
        textOpt <- inv.processAsOpt("text", ParsedTextType, elem)
        text <- inv.fut(textOpt.map(Future.successful(_)).getOrElse(elem.value.wikify(elem)))
        phraseOpt <- inv.rawParam("children")
        qlOpt = phraseOpt.map(phrase => HtmlEscape.escapeQuotes(phrase.reconstructString))
        iconOpt <- inv.processAsOpt("icon", ParsedTextType, elem)
        idOpt <- inv.processAsOpt("id", ParsedTextType, elem)
        opened <- inv.processAs("opened", YesNoType, elem)
      }
        yield 
          HtmlValue(
            span(
              cls:="_qlTree",
              iconOpt.map(icon => data.icon := icon.raw.toString),
              data.opened := opened,
              data.thingid := thing.toThingId.toString,
              idOpt.map(id => data.id := id.raw.toString),
              raw(text.raw),
              qlOpt.map(ql => span(cls:="_treeQL", raw(ql), display:="none"))
            )
          )
    }
  }
  
  // TODO: replace this with a QL function! It certainly requires basic math functions, but I'm
  // not sure it needs much else...
  lazy val ShowSomeFunction = new InternalMethod(ShowSomeOID,
    toProps(
      setName("_showSome"),
      Summary("Show some of the contents at a time"),
      SkillLevel(SkillLevelAdvanced),
      Details("""THING -> _showSome(START, LEN, MSG, ALL, DISPLAY)
        |
        |This is a useful but complex function for dealing with long lists. Given the incoming THING,
        |it runs the expression in ALL to compute a bunch of expressions. It produces a LIST of LEN of them
        |with indexes starting at START, feeds that to DISPLAY, and produces the result of that. The whole
        |thing will be finished with MSG; clicking on that produces the next LEN items.
        |
        |The division between ALL and DISPLAY is a bit subtle, and is mainly for efficiency. ALL should
        |contain the code up until the order is clear -- typically until _sort. You *can* put everything
        |into ALL, but it will run more slowly.
        |
        |In the long run, this should be replaced by a cleverer and more automatic mechanism. Also, this
        |may be replaced by a QL function in due course. So consider this experimental; it may go away
        |down the line.""".stripMargin)))
  {
    override def qlApply(inv:Invocation):QFut = {
      for {
        thing <- inv.contextFirstThing
        start <- inv.processParamFirstAs(0, IntType)
        len <- inv.processParamFirstAs(1, IntType)
        msg <- inv.processParamFirstAs(2, ParsedTextType)
        all <- inv.processParam(3)
        done = (start + len >= all.size)
        rawMsg <- inv.rawParam(2)
        rawAll <- inv.rawParam(3)
        rawDisplay <- inv.rawParam(4)
        selectedElems = all.cType.makePropValue(all.cv.drop(start).take(len), all.pType)
        result <- inv.processParam(4, inv.context.next(selectedElems))
        wiki <- inv.fut(result.wikify(inv.context))
        nextButton =
          if (done)
            ""
          else {
            val nextDiv = s"_nextButton${(scala.math.random * 1000000).toInt.toString()}"
            div(idAttr := nextDiv,
              p(b(a(
                cls := "_qlInvoke",
                msg.raw.toString,
                data.thingId := s"${thing.toThingId}",
                data.target := nextDiv,
                data.ql := s"_showSome(${start + len},$len,${rawMsg.reconstructString},${rawAll.reconstructString},${rawDisplay.reconstructString})")))
            ).toString
          }
        complete =
          if (all.size == 0)
            Wikitext("")
          else
            wiki + HtmlWikitext(nextButton)
      }
        yield QL.WikitextValue(complete)
    }
  }
  
  override lazy val props = Seq(
    classMethod,
    tooltipMethod,
    dataMethod,
    PageHeaderProperty,
    
    new SectionMethod,
    new LinkButtonMethod,
    new IconButtonMethod,
    new ShowLinkMethod,
    new PropLinkMethod,
    CreateInstanceLinkMethod,
    CreateButtonFunction,
    QLButton,
    QLLink,
    ThingTree,
    new MixedButtonMethod,
    ShowSomeFunction
  )
}