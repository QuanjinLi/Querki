package querki

import models.{PType, PTypeBuilder, Thing, Wikitext}

import querki.core.QLText
import querki.ecology._

import querki.util.PublicException
import querki.values.{ElemValue, EmptyValue, QLContext, QValue}

package object ql {

  /**
   * This trait should be used by any type that can consider itself "code" -- in particular, that wants to be
   * displayable in the _code() method. 
   */
  trait CodeType {
    def code(elem:ElemValue):String
  }

  /**
   * A QLFunction is something that can be called in the QL pipeline. It is mostly implemented
   * by Thing (with variants for Property and suchlike), but can also be anonymous -- for example,
   * when a method returns a partially-applied function.
   */
  trait QLFunction {
    def qlApply(context:QLContext, params:Option[Seq[QLPhrase]] = None):QValue
  }
  
  /**
   * This quietly transforms a returned InvocationValue[QValue] into a QValue, and will typically be
   * invoked at the end of the function.
   */
  implicit def inv2QValue(inv:InvocationValue[QValue])(implicit ecology:Ecology):QValue = {
    inv.getError.getOrElse(inv.get.getOrElse(EmptyValue.untyped))
  }
  
  /**
   * The InvocationValue Monad, which is how we query Invocations inside of a for.
   */
  trait InvocationValue[T] {
    def map[R](f:T => R):InvocationValue[R]
    def flatMap[R](f:T => InvocationValue[R]):InvocationValue[R]
    
    def get:Option[T]
    def getError:Option[QValue]
  }
  
  /**
   * This encapsulates the call information about a single function invocation in QL. In general,
   * common functionality should get wrapped up into here.
   */
  trait Invocation {
    /**
     * How many parameters were actually given?
     */
    def numParams:Int
    
    /**
     * Check the actual Invocation against the provided Signature.
     * 
     * TODO: soon, the Signature will be given as a Property, and checked automatically.
     */
    def ifMatches(sig:Signature)(f:Invocation => QValue):QValue
    
    /**
     * Declare that we expect the received context to be of the given PType or trait, and
     * keep going using that.
     */
    def contextAs[T : scala.reflect.ClassTag](f:T => QValue):QValue
    
    /**
     * Process one of the parameters. Expects that you have already checked that you have
     * all your expected parameters, using ifMatches!
     */
    def oldProcessParam(paramNum:Int, processContext:QLContext = context):QValue
    
    /**
     * Turns an Option value into an InvocationValue, so they can be used in a for comprehension together.
     */
    def withOption[T](opt:Option[T], errOpt:Option[PublicException] = None):InvocationValue[T]

//    /**
//     * If the context's value is of the specified type (which need not be a PType), return
//     * it cast to that type.
//     */
//    def contextTypeAs[T : scala.reflect.ClassTag]:InvocationValue[T]
//    
//    def contextElements:InvocationValue[List[QLContext]]
    
    /**
     * If the received context is of the specified type, returns the first element of that context
     * monadically.
     * 
     * TODO: this method is a bit suspicious -- we should be encouraging use of map semantics instead.
     */
    def contextFirstAs[VT](pt:PType[VT]):InvocationValue[VT]

    /**
     * Returns the first Thing in the received context.
     */
    def contextFirstThing:InvocationValue[Thing]

    /**
     * Get the specified parameter's first value, which should be of the given Type.
     */
    def processParamFirstAs[VT](paramNum:Int, pt:PType[VT], processContext:QLContext = context):InvocationValue[VT]
    
    //////////////
    //
    // These are the raw fields. By and large, you should prefer *not* to use these, and some of them
    // may go away in the long run. If possible, use higher-level functions instead.
    //
    
    /**
     * The "primary" context of this invocation. This is an exact synonym for receivedContext, and is the
     * one you usually care about. 
     */
    def context:QLContext
    
    /**
     * The Context that was passed to this function. This always exists, although a few functions don't care
     * about it.
     */
    def receivedContext:QLContext
    
    /**
     * The parameter list for this invocation, iff there was one.
     */
    def paramsOpt:Option[Seq[QLPhrase]]
  }
  
  trait QL extends EcologyInterface {
    
    /**
     * The primary entry point for processing a body of QLText into Wikitext.
     * 
     * The input text should be a block of QLText (with the text on the "outside"). This parses
     * that, uses the given context and params to process it, and returns the resulting Wikitext.
     */
    def process(input:QLText, ci:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):Wikitext
    
    /**
     * Process a QL Function into a QValue.
     * 
     * The input text should be a block of QL (with any text on the "inside"). This parses that,
     * uses the given context and params to process it, and returns the resulting QValue.
     */
    def processMethod(input:QLText, ci:QLContext, paramsOpt:Option[Seq[QLPhrase]] = None):QValue
    
    def UnknownNameType:PType[String] with PTypeBuilder[String,String]
    def ParsedTextType:PType[Wikitext] with PTypeBuilder[Wikitext,Wikitext]
    
    def WarningValue(msg:String):QValue
    def ErrorValue(msg:String):QValue
    def WikitextValue(wikitext:Wikitext):QValue
    
    def EmptyListCut():QValue
  }

}