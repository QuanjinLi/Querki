package querki.test

import scala.concurrent.{Future}
import scala.concurrent.duration._

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestActors, TestKit, ImplicitSender}
import akka.util.Timeout

import org.scalatest.concurrent.ScalaFutures

import upickle._
import autowire._

import models._

import querki.api._
import querki.globals.Implicits.execContext
import querki.identity.User
import querki.session.UserSessionMessages.UserSessionMsg
import querki.session.messages._
import querki.spaces.messages._
import querki.values.{RequestContext, SpaceState}

/**
 * Root of the Client Simulation test.
 * 
 * The purpose here is to exercise the system from the "outside", pretty much as a real
 * Client would do. So this replaces the ClientController, submits API commands and tests
 * the results. For practical reasons it can't be a pure black box test (we need to wait
 * until things get written to the "database" in some cases), but it mostly takes that
 * viewpoint.
 * 
 * For the moment, we're doing Client Simulation as one big honking test, to avoid creating
 * and shutting down too many Actor Systems. This probably wants to change in the long run,
 * but keep in mind that my ad hoc profiling indicates that a full startup/shutdown cycle takes
 * about 4 seconds, so that'll add up quickly.
 * 
 * Note that we *can't* break this into separate tests: the Actor System is collectively
 * mutable, and certain tests change its state, so they won't be truly separate. 
 */
class ClientSimulation(_system:ActorSystem) extends TestKit(_system) with QuerkiTests with ScalaFutures
{  
  def this() = this(ActorSystem("ClientSimulation"))
  
  override def createActor(props:Props, name:String):Option[ActorRef] = Some(system.actorOf(props, name))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
  
  lazy val ClientApi = interface[querki.api.ClientApi]
  lazy val SpaceOps = interface[querki.spaces.SpaceOps]
  lazy val UserSessionMgr = interface[querki.session.Session]

  case class ClientSimRequestContext(u:Option[User], s:Option[SpaceState], t:Option[Thing], qs:Map[String,Seq[String]] = Map.empty) 
    extends RequestContext(u, UnknownOID, s, None, ecology) 
  {
    def withUpdatedState(newState:SpaceState):RequestContext = copy(s = Some(newState))
    // In principle, we really should have a test renderer here instead:
    def renderer = interface[querki.html.HtmlRenderer]
  
    def queryParam(paramName:String):Seq[String] = qs(paramName)
    def +(state:SpaceState):RequestContext = copy(s = Some(state))
    def +(thing:Thing):RequestContext = copy(t = Some(thing))    
  }
  def ownerRc = ClientSimRequestContext(Some(commonSpace.owner), None, None)

  def askUserSpaceSession[B](rc:RequestContext, msg:SessionMessage)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    SpaceOps.askSpaceManager2(SessionRequest(rc.requesterOrAnon, rc.ownerId, rc.state.get.toThingId, msg))(cb)
  }
  
  def askUserSession[B](rc:RequestContext, msg:UserSessionMsg)(cb: PartialFunction[Any, Future[B]]):Future[B] = {
    akka.pattern.ask(UserSessionMgr.sessionManager, msg)(Timeout(5 seconds)).flatMap(cb)
  }
  
  abstract class LocalClientBase extends autowire.Client[String, upickle.Reader, upickle.Writer] {
    def callSystem(req:Request):Future[String]
    
	override def doCall(req: Request): Future[String] = callSystem(req)
	
	def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
	def write[Result: upickle.Writer](r: Result) = upickle.write(r)
  }
  
  class CommonClient(rc:RequestContext) extends LocalClientBase {
    def callSystem(req:Request) = {
      ClientApi.handleCommonFunction(rc, req).map { 
        case ClientResponse(pickled) => pickled
        case ClientError(msg) => throw new Exception(msg)
      }
    }
  }
  lazy val common = new CommonClient(ownerRc)
  
  class UserSpaceClient(rc:RequestContext) extends LocalClientBase {
    def callSystem(req:Request) = {
	  askUserSpaceSession(rc, ClientRequest(req, rc)) {
	    case ClientResponse(pickled) => Future.successful(pickled)
	    case ClientError(msg) => Future.failed(new Exception(msg))
	    case ThingError(pex, _) => Future.failed(pex)
	  }      
    }
  }
  
  "The Client simulation" should {
    "run a full test" in {
      
      whenReady(common[CommonFunctions].getStandardThings().call()) { stdMap =>
        assert(stdMap("Optional").oid.underlying == Core.Optional.id.toThingId.toString)
        assert(stdMap("Default View").oid.underlying == Basic.DisplayTextProp.id.toThingId.toString)
      }
      
    }
  }
}
