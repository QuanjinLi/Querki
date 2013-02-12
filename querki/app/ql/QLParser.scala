package ql

import play.api.Logger

import scala.util.parsing.combinator._

import models.system._
import models._
import controllers.RequestContext

/**
 * This is a fake PType, which exists so that we can persist embedded Texts in the pipeline.
 */
object ParsedTextType extends SystemType[Wikitext](OIDs.IllegalOID, () => Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
{
  def doDeserialize(v:String) = throw new Exception("Can't deserialize ParsedText!")
  def doSerialize(v:Wikitext) = throw new Exception("Can't serialize ParsedText!")
  def doRender(context:ContextBase)(v:Wikitext) = v
    
  val doDefault = Wikitext("")
  def wrap(raw:String):valType = Wikitext(raw)
}

// TODO: we've gotten rid of the explicit ct parameter, since it is contained in v.
// Maybe we can do the same for pt?
case class TypedValue(v:PropValue, pt:PType[_]) {
  def ct:Collection = v.coll
  
  def render(context:ContextBase):Wikitext = ct.render(context)(v, pt) 
}
object ErrorValue {
  def apply(msg:String) = {
    try {
      throw new Exception("dummy")
    } catch {
      case e:Exception => Logger.error(s"Displaying error $msg; stack trace:\n${e.getStackTraceString}")  
    }
    TypedValue(ExactlyOne(PlainTextType(msg)), PlainTextType)
  }
}

abstract class ContextBase {
  def value:TypedValue
  def state:SpaceState
  def request:RequestContext
  // Parent matters at rendering time -- we render the final context in the context of its parent.
  // This matter most when (as often), the last context is a Text; it needs to be rendered correctly
  // in its parent context.
  def parent:ContextBase
  
  override def toString = "Context(" + value.v + ")"
}

/**
 * Represents the incoming "context" of a parsed QLText.
 */
case class QLContext(value:TypedValue, request:RequestContext, parentIn:Option[ContextBase] = None) extends ContextBase {
  def state = request.state.getOrElse(SystemSpace.State)
  def parent = parentIn match {
    case Some(p) => p
    case None => this
  }
}

/**
 * This should only be used in cases where we really don't have a context -- generally,
 * displaying outside of a proper page display in a Space. It is unlikely to work for
 * any sophisticated properties, but can be used for PlainText and the like.
 */
case object EmptyContext extends ContextBase {
  def value:TypedValue = throw new Exception("Can't use the contents of EmptyContext!")
  def state:SpaceState = throw new Exception("Can't use the space of EmptyContext!")
  def request:RequestContext = throw new Exception("Can't get the request of EmptyContext!")
  def parent:ContextBase = throw new Exception("EmptyContext doesn't have a parent!")
}

sealed abstract class QLTextPart
case class UnQLText(text:String) extends QLTextPart
sealed abstract class QLStage
case class QLName(name:String) extends QLStage
case class QLTextStage(contents:ParsedQLText) extends QLStage
case class QLPhrase(ops:Seq[QLStage])
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart
case class QLLink(contents:ParsedQLText) extends QLTextPart
case class ParsedQLText(parts:Seq[QLTextPart])

class QLParser(val input:QLText, initialContext:ContextBase) extends RegexParsers {
  
  // Crude but useful debugging of the process tree. Could stand to be beefed up when I have time
  def logContext(msg:String, context:ContextBase) = {
    //Logger.info(msg + ": " + context)
  }
  
  val name = """[a-zA-Z][\w- ]*[\w]""".r
  val unQLTextRegex = """([^\[\"_]|\[(?!\[)|\"(?!\")|_(?!_))+""".r
  // We don't want the RegexParser removing whitespace on our behalf. Note that we need to be
  // very careful about whitespace!
  override val whiteSpace = "".r
  
  def unQLText:Parser[UnQLText] = unQLTextRegex ^^ { UnQLText(_) }
  def qlName:Parser[QLName] = name ^^ { n => QLName(n) }
  def bangAsNewline:Parser[Option[UnQLText]] = (opt("!") ^^ { _ match {
      case Some(exp) => Some(UnQLText("\n"))
      case None => None
    }})
  def qlTextStage:Parser[QLTextStage] = 
    (bangAsNewline <~ "\"\"") ~ qlText ~ ("\"\"" ~> bangAsNewline) ^^ { 
    case newf ~ ParsedQLText(guts) ~ newb => QLTextStage(ParsedQLText((newf ++: guts) ++ newb)) 
  }
  def qlStage:Parser[QLStage] = qlName | qlTextStage
  // TODO: phrase is going to get a *lot* more complex with time:
  def qlPhrase:Parser[QLPhrase] = rep1sep(qlStage, "\\s*->\\s*".r) ^^ { QLPhrase(_) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\\s*\\r?\\n|\\s*;\\s*".r) ^^ { QLExp(_) }
  def qlLink:Parser[QLLink] = qlText ^^ { QLLink(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]" | "__" ~> qlLink <~ "__") ^^ { ParsedQLText(_) }
  
  /**
   * Note that the output here is nominally a new Context, but the underlying type is
   * ParsedTextType. So far, you can't *do* anything with that afterwards other than
   * render it, which just returns the already-computed Wikitext.
   */
  private def processTextStage(text:QLTextStage, context:ContextBase):ContextBase = {
    logContext("processTextStage " + text, context)
    val ct = context.value.ct
    val pt = context.value.pt
    // For each element of the incoming context, recurse in and process the embedded Text
    // in that context.
    val transformed = context.value.v.cv map { elem =>
      val elemContext = QLContext(TypedValue(ExactlyOne(elem), pt), context.request, Some(context))
      ParsedTextType(processParseTree(text.contents, elemContext))
    }
    // TBD: the asInstanceOf here is surprising -- I would have expected transformed to come out
    // as the right type simply by type signature. Can we get rid of it?
    QLContext(TypedValue(ct.makePropValue(transformed.asInstanceOf[ct.implType]), ParsedTextType), context.request, Some(context))
  }
  
  private def processName(name:QLName, context:ContextBase):ContextBase = {
    logContext("processName " + name, context)
    val thing = context.state.anythingByName(name.name)
    val tv = thing match {
      case Some(t) => t.qlApply(context)
      case None => ErrorValue("[UNKNOWN NAME: " + name.name + "]")
    }
    logContext("processName got " + tv, context)
    QLContext(tv, context.request, Some(context))
  }
  
  private def processStage(stage:QLStage, context:ContextBase):ContextBase = {
    logContext("processStage " + stage, context)
    stage match {
      case name:QLName => processName(name, context)
      case subText:QLTextStage => processTextStage(subText, context)
    }
  }
  
  private def processPhrase(ops:Seq[QLStage], startContext:ContextBase):ContextBase = {
    logContext("processPhrase " + ops, startContext)
    (startContext /: ops) { (context, stage) => processStage(stage, context) }
  }
  
  private def processPhrases(phrases:Seq[QLPhrase], context:ContextBase):Seq[ContextBase] = {
    logContext("processPhrases " + phrases, context)
    phrases map (phrase => processPhrase(phrase.ops, context))
  }

  private def contextsToWikitext(contexts:Seq[ContextBase]):Wikitext = {
    (Wikitext("") /: contexts) { (soFar, context) => soFar + context.value.render(context.parent) }
  }
  
  private def linkToWikitext(contents:ParsedQLText, context:ContextBase):Wikitext = {
    contents.parts.length match {
      // Just four underscores, which means render the context right here:
      case 0 => context.value.render(context)
      // There is content, so turn it into a link to the context Thing:
      case _ => {
        val guts = processParseTree(contents, context)
        context.value.pt match {
          case LinkType => {
            // TODO: this is evil. How should it be described instead?
            val l = LinkType.follow(context)(context.value.v.first.elem.asInstanceOf[OID])
            l match {
              case Some(thing) => Wikitext("[") + guts + Wikitext("](" + thing.toThingId + ")")
              case None => guts
            }
          }
          // TODO: we ought to show some sort of error here?
          case _ => guts
        }        
      }
    }
  }
  
  private def processParseTree(parseTree:ParsedQLText, context:ContextBase):Wikitext = {
    logContext("processParseTree " + parseTree, context)
    (Wikitext("") /: parseTree.parts) { (soFar, nextPart) =>
      soFar + (nextPart match {
        case UnQLText(t) => Wikitext(t)
        case QLExp(phrases) => contextsToWikitext(processPhrases(phrases, context))
        case QLLink(l) => linkToWikitext(l, context)
      })
    }
  }
  
  def parse = parseAll(qlText, input.text)
  
  def process:Wikitext = {
    val parseResult = parse
    parseResult match {
      case Success(result, _) => processParseTree(result, initialContext)
      case Failure(msg, next) => { Logger.warn(s"Couldn't parse qlText: $msg at ${next.pos}"); Wikitext("Couldn't parse qlText: " + msg) }
      // TODO: we should probably do something more serious in case of Error:
      case Error(msg, next) => { Logger.error("Couldn't parse qlText: " + msg); Wikitext("ERROR: Couldn't parse qlText: " + msg) }
    }
  }
}