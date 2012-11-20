package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Promise
import play.api.mvc._

import models._

object Application extends Controller {

  val userForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => User(UnknownOID, name))
     ((user: User) => Some((user.name)))
  )
  
  val newSpaceForm = Form(
    mapping(
      "name" -> nonEmptyText
    )((name) => name)
     ((name:String) => Some(name))
  )
  
  def getUser(username:String):Option[User] = User.get(username)
  
  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)

  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }
  
  def withUser(f: User => Request[AnyContent] => Result) = withAuth { username => implicit request =>
    getUser(username).map { user =>
      f(user)(request)
    }.getOrElse(onUnauthorized(request))
  }
  
  /**
   * Helper for asking the SpaceManager for info. Assumes that the process is
   * asynchronous, and buffers the incoming HTTP request accordingly.
   * 
   * @tparam A The type of the expected return message from SpaceManager. This usually names a trait;
   * the actual messages should be derived from that.
   * @param msg The message to send to the SpaceManager.
   * @param cb A partial function that takes the SpaceManager response and produces a result.
   */
  def askSpaceMgr[A](msg:SpaceMgrMsg)(cb: A => Result)(implicit m:Manifest[A]) = {
    Async {
      SpaceManager.ask[A, Result](msg)(cb)
    }
  }
  
  /**
   * Utility method for pages that are basically displaying some aspect of a whole Space.
   * 
   * @param spaceId The stringified OID of the space to display, from the URL
   * @param f A callback that, given the user and the space's state, produces the
   * actual page content
   */
  def withSpaceAndRequest(spaceId:String)(f: (User, SpaceState) => Request[AnyContent] => Result) = withUser { user => implicit request =>
    askSpaceMgr[GetSpaceResponse](GetSpace(OID(spaceId), Some(user.id))) {
      case RequestedSpace(state) => f(user, state)(request)
      case GetSpaceFailed(id) => Ok(views.html.spaces(Some(user), Seq.empty))
    }     
  }
  
  /**
   * Simpler version of withSpaceAndRequest, for the majority of methods that don't actually
   * care about the request.
   */
  def withSpace(spaceId:String)(f: (User, SpaceState) => Result) = withUser { user => implicit request =>
    askSpaceMgr[GetSpaceResponse](GetSpace(OID(spaceId), Some(user.id))) {
      case RequestedSpace(state) => f(user, state)
      case GetSpaceFailed(id) => Ok(views.html.spaces(Some(user), Seq.empty))
    }     
  }
  
  def index = Security.Authenticated(username, request => Ok(views.html.index(None, userForm))) { 
    user => Action {
      Ok(views.html.index(getUser(user), userForm)) 
    }
  }    

  def spaces = withUser { user => implicit request => 
    askSpaceMgr[ListMySpacesResponse](ListMySpaces(user.id)) { 
      case MySpaces(list) => Ok(views.html.spaces(Some(user), list))
    }
  }
    
  def space(spaceId:String) = withSpace(spaceId) { (user, state) =>
    Ok(views.html.thing(Some(user), state, state))
  }
  
  def things(spaceId:String) = withSpace(spaceId) { (user, state) =>
    Ok(views.html.things(Some(user), state))
  }
  
  def thing(spaceId:String, thingId:String) = withSpace(spaceId) { (user, state) =>
    Ok(views.html.thing(Some(user), state, state.anything(OID(thingId))))
  }
  
  def newSpace = withUser { user => implicit request =>
    Ok(views.html.newSpace(user))
  }
  
  def doNewSpace = withUser { user => implicit request =>
    newSpaceForm.bindFromRequest.fold(
      errors => BadRequest(views.html.newSpace(user, Some("You have to specify a legal space name"))),
      name => {
        askSpaceMgr[GetSpaceResponse](CreateSpace(user.id, name)) {
          case RequestedSpace(state) => Redirect(routes.Application.space(state.id.toString))
          // TODO: we'll want more granular failure messages:
          case GetSpaceFailed(id) =>  BadRequest(views.html.newSpace(user, Some("I wasn't able to create the new Space!")))
        }
      }
    )
  }
  
  def login = 
    Security.Authenticated(username, request => Ok(views.html.login(userForm))) { user =>
      Action { Redirect(routes.Application.index) }
    }
  
  def dologin = Action { implicit request =>
    userForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(None, errors, Some("I didn't understand that"))),
      user => {
        val lookedUp = User.get(user.name)
        if (lookedUp.isEmpty)
          BadRequest(views.html.login(userForm, Some("I don't know who you are")))
        else
	      Redirect(routes.Application.index).withSession(Security.username -> user.name)
      }
    )
  }
  
  def logout = Action {
    Redirect(routes.Application.index).withNewSession
  }
}