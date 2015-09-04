package querki.api

import akka.actor._

import upickle._
import autowire._

import models.{AsOID, HtmlWikitext}

import querki.globals._

import querki.core.NameUtils
import querki.data._
import querki.ecology._
import querki.identity.{PublicIdentity, User}
import querki.tags.IsTag
import querki.types.ModelTypeBase
import querki.values.{QLRequestContext, RequestContext}

class ClientApiEcot(e:Ecology) extends QuerkiEcot(e) with ClientApi
{
  
  lazy val AccessControl = interface[querki.security.AccessControl]
  lazy val ApiRegistry = interface[querki.api.ApiRegistry]
  lazy val Basic = interface[querki.basic.Basic]
  lazy val Conventions = interface[querki.conventions.Conventions]
  lazy val DataModelAccess = interface[querki.datamodel.DataModelAccess]
  lazy val Editor = interface[querki.editing.Editor]
  
  var _anonHandler:Option[ActorRef] = None
  lazy val anonHandler = _anonHandler.get

  override def createActors(createActorCb:CreateActorFunc):Unit = {
    _anonHandler = createActorCb(AnonymousApiRouter.actorProps(ecology), "AnonHandler")
  }
  
  override def postInit() = {
    ApiRegistry.registerApiImplFor[CommonFunctions, CommonFunctionsImpl](anonHandler, false)
  }

  implicit def thing2TID(t:Thing) = TID(t.id.toThingId.toString)
  implicit def OID2TID(oid:OID) = TID(oid.toThingId.toString)
  
  def thingInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):Future[ThingInfo] = {
      val user = rc.requesterOrAnon
      val editable = AccessControl.canEdit(state, user, t.id)
      val isModel = t.isModel
      val importedFrom =
        if (t.spaceId == state.id)
          None
        else
          spaceInfo(state.getApp(t.spaceId), rc)
      t.unsafeNameOrComputed(rc, state) map { name =>
        ThingInfo(
          t, 
          t.linkName, 
          name,
          t.model,
          t.kind,
          isModel,
          editable,
          // We allow deleting Properties at this level:
          editable && DataModelAccess.isDeletable(t, allowIfProp = true),
          isModel && AccessControl.canCreate(state, user, t),
          t.isInstanceOf[IsTag],
          importedFrom)
      }
  }
  
  def spaceInfo(topt:Option[SpaceState], rc:RequestContext):Option[SpaceInfo] = {
    topt.map { t => 
      SpaceInfo(
        t, 
        // TODO: NameUtils.toUrl() is inconsistent with SafeUrl: they handle spaces differently.
        // We need to fix this inconsistency!
        t.linkName.map(NameUtils.toUrl(_)), 
        t.unsafeDisplayName,
        t.owner.toThingId.toString,
        t.ownerHandle)
    }
  }
  
  def spaceInfo(info:querki.spaces.messages.SpaceInfo):SpaceInfo = {
    val querki.spaces.messages.SpaceInfo(spaceId, linkName, display, ownerHandle) = info
    SpaceInfo(TID(spaceId.toThingId), Some(linkName), display, "", ownerHandle)
  }
  
  def identityInfo(identity:PublicIdentity):IdentityInfo = {
    IdentityInfo(AsOID(identity.id), identity.name, identity.handle)
  }
  
  def userInfo(uopt:Option[User]):Option[UserInfo] = {
    uopt.map { user =>
      // TODO: this will need adjusting when we have multiple Identities. The mainIdentity should come first:
      val identityInfos = user.identities.map { identity =>
        identityInfo(identity)
      }
      UserInfo(AsOID(user.id), identityInfos)
    }
  }
  
  def requestInfo(rc:RequestContext)(implicit state:SpaceState):RequestInfo = {
    if (AccessControl.canRead(state, rc.requesterOrAnon, state.id)) {
      RequestInfo(
        userInfo(rc.requester), 
        spaceInfo(Some(state), rc), 
        rc.isOwner,
        rc.requesterOrAnon.level)
    } else {
      // Signal that this person doesn't have access to the Space:
      RequestInfo(None, None, false, rc.requesterOrAnon.level, true)
    }
  }
  
  def rootRequestInfo(rc:RequestContext):RequestInfo = {
    RequestInfo(
      userInfo(rc.requester),
      None,
      false,
      rc.requesterOrAnon.level
    )
  }
  
  def propInfo(prop:AnyProp, rc:RequestContext)(implicit state:SpaceState):PropInfo = {
    val typeId = prop.pType match {
      case mt:ModelTypeBase => {
        if (prop.pType.ifSet(Basic.ExplicitProp))
          prop.pType.id
        else
          mt.basedOn
      }
      case _ => prop.pType.id
    }
    PropInfo(
      prop, 
      prop.linkName, 
      prop.displayName, 
      prop.getPropOpt(Core.AppliesToKindProp).flatMap(_.firstOpt),
      prop.cType,
      typeId)
  }
  
  def propValInfo(t:Thing, rc:RequestContext)(implicit state:SpaceState):Future[Seq[PropValInfo]] = {
    def oneProp(prop:AnyProp, v:QValue):Future[PropValInfo] = {
      val prompt = futOpt(prop.getPropOpt(Editor.PromptProp).map(_.renderPlain))
      val renderedV =
        if (v.pType.isInstanceOf[querki.core.IsTextType]) {
          Future.successful(HtmlWikitext(s"<pre><code>${v.cv.map(v.pType.toUser(_)).mkString("\n")}</code></pre>"))
        } else {
          v.wikify(QLRequestContext(rc))
      }
      val tooltip = futOpt(prop.getPropOpt(Conventions.PropSummary).map(_.render(prop.thisAsContext(rc, state, ecology))))
          
      for {
        p <- prompt
        r <- renderedV
        t <- tooltip
      }
        yield PropValInfo(propInfo(prop, rc), p, r, t)
    }
    
    val infoOpts = for {
      prop <- t.localProps
      if (!prop.ifSet(Core.InternalProp))
    }
      yield t.getPropOpt(prop).map(pv => oneProp(prop, pv.v))
    Future.sequence(infoOpts.flatten.toSeq)
  }
}
