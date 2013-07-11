package modules.person

import models._
import models.Space.oidMap
import models.Thing._
import models.system._
import models.system.OIDs._

import ql._

import querki.values._

import identity._

import modules._
// TODO: Hmm. This is a Module-boundaries break. I think we need an interface layer:
import modules.email.EmailAddress

import querki.util._

import controllers.{Contributor, PageEventManager, Publisher, RequestContext}

import play.api.Logger

// TODO: this Module should formally depend on the Email Module. Probably --
// it isn't entirely clear whether statically described Properties really
// require initialization-order dependencies. But I believe that the Person
// object shouldn't be constructed until after the Email Module has been.
class PersonModule(val moduleId:Short) extends modules.Module {

  object MOIDs {
    val PersonOID = oldMoid(1)
    val InviteLinkCmdOID = oldMoid(2)
    val IdentityLinkOID = oldMoid(3)
    val ChromelessInviteLinkOID = oldMoid(4)
    val MeMethodOID = oldMoid(5)
    val SecurityPrincipalOID = oldMoid(6)
    val ChromelessInvitesOID = moid(7)
  }
  import MOIDs._
  
  override def init = {
    PageEventManager.requestReceived += IdentityLoginChecker
  }
  
  override def term = {
    PageEventManager.requestReceived -= IdentityLoginChecker
  }

  /***********************************************
   * EXTERNAL REFS
   ***********************************************/

  lazy val emailAddressProp = Modules.Email.emailAddress
  
  lazy val urlBase = Config.getString("querki.app.urlRoot")
  
  /***********************************************
   * PROPERTIES
   ***********************************************/

  // The actual definition of this method is down below
  lazy val inviteLink = new SingleThingMethod(InviteLinkCmdOID, "Invite Link", """Place this command inside of an Email Message.
When the email is sent, it will be replaced by a link that the recipient of the email can use to log into this Space as a Person.""", doInviteLink(false))

  lazy val chromelessInviteLink = new SingleThingMethod(ChromelessInviteLinkOID, "Plain Invite Link", """Place this command inside of an Email Message.
When the email is sent, it will be replaced by a link that the recipient of the email can use to log into this Space as a Person.
Unlike the ordinary Invite Link command, this one results in a page with no Querki menu bar, just your pages.
(NOTE: this will probably become a paid-users-only feature in the future. Also, this method isn't usually what you want any more;
instead, you usually want to set the Chromeless Invites property on your Space.)""", doInviteLink(true))

  lazy val identityLink = new SystemProperty(IdentityLinkOID, LinkType, Optional,
      toProps(
        setName("Person to Identity Link"),
        InternalProp(true),
        DisplayTextProp("INTERNAL: points from a Space-scoped Person to a System-scoped Identity")))

  lazy val meMethod = new InternalMethod(MeMethodOID,
      toProps(
        setName("_me"),
        DisplayTextProp("If the current user is a Person in the current Space, return that Person")))
  {
    override def qlApply(context:ContextBase, params:Option[Seq[QLPhrase]] = None):TypedValue = {
      val userOpt = context.request.requester
      val personOpt = userOpt.flatMap { user =>
        user match {
          case SpaceSpecificUser(identityId, name, email, spaceId, personId) => Some(LinkValue(personId))
          case _ => None
        }
      }
      personOpt.getOrElse(WarningValue("Not logged into this Space"))
    }
  }
  
  lazy val chromelessInvites = new SystemProperty(ChromelessInvitesOID, YesNoType, ExactlyOne,
      toProps(
        setName("Chromeless Invites"),
        DisplayTextProp("If you set this to Yes on a Space or Thing, then Invite Links pointing to that will show up without Querki chrome." +
        		"(NOTE: this will probably become a paid-users-only feature in the future.)")))

  override lazy val props = Seq(
    inviteLink,
    
    chromelessInviteLink,
    
    identityLink,
    
    meMethod,
    
    chromelessInvites
  )
  
  /***********************************************
   * THINGS
   ***********************************************/
  
  lazy val securityPrincipal = ThingState(SecurityPrincipalOID, systemOID, RootOID,
      toProps(
        setName("Security Principal"),
        DisplayTextProp("""For internal use -- this the concept of a Thing that can be given permissions.""")))
  
  lazy val person = ThingState(PersonOID, systemOID, SecurityPrincipalOID,
      toProps(
        setName("Person"),
        IsModelProp(true),
        // TODO: this is a fugly declaration, and possibly unsafe -- do we have any
        // assurance that modules.Modules.Email has been constructed before this?
        (modules.Modules.Email.MOIDs.EmailPropOID -> Optional.None),
        DisplayTextProp("""
This represents a Person who is using Querki or can be invited to it. You can create a Person in
your Space, and compose an email to invite them to use the Space; you can also create a new Model
to add new Properties for any Person in your Space.
""")))
    
  override lazy val things = Seq(
    securityPrincipal,
    // The Person Model
    person
  )
   
  /***********************************************
   * METHOD CONTENTS
   ***********************************************/
  
  /**
   * Find the Identity that goes with this Person's email address, or create one if there
   * isn't already one.
   */
  private def setIdentityId(t:Thing, context:ContextBase):OID = {
    implicit val s = context.state
    val emailAddr = t.first(emailAddressProp)
    val name = t.first(NameProp)
    // Get the Identity in the database...
    val identity = Identity.getOrCreateByEmail(emailAddr, name)
    // ... then point the Person to it, so we can use it later...
    val req = context.request
    // TODO: we shouldn't do this again if the Person is already pointing to the Identity:
    val changeRequest = ChangeProps(req.requester.get, s.owner, s.id, t.toThingId, toProps(identityLink(identity.id))())
    // TODO: eventually, all of this should get beefed up in various ways:
    // -- ChangeProps should carry the version stamp of t, so that race conditions can be rejected.
    // -- Ideally, we shouldn't send the email until the Identity has been fully established. Indeed, this whole
    //    business really *ought* to be transactional.
    SpaceManager.ask(changeRequest) { resp:ThingResponse =>
      resp match {
        case ThingFound(id, state) => Logger.info("Added identity " + identity.id + " to Person " + t.toThingId)
        case ThingFailed(error, msg, stateOpt) => Logger.error("Unable to add identity " + identity.id + " to Person " + t.toThingId + ": " + msg)
      }
    }
    // ... and finally, return it so we can use it now:
    identity.id
  }
  
  def idHash(personId:OID, identityId:OID):EncryptedHash = {
    Hasher.calcHash(personId.toString + identityId.toString)
  }
  
  val identityParam = "identity"
  val identityName = "identityName"
  val identityEmail = "identityEmail"
  val personParam = "person"
    
  def doInviteLink(chromelessIn:Boolean)(t:Thing, context:ContextBase):TypedValue = {
    implicit val state = context.state
    val personOpt =
      for {
        rootId <- context.root.value.firstAs(LinkType);
        rootThing <- state.anything(rootId);
        if (rootThing.isAncestor(PersonOID))
      }
        yield rootThing
        
    personOpt match {
      case Some(person) => {
        val chromeless = chromelessIn || t.ifSet(chromelessInvites) || state.ifSet(chromelessInvites)
        // Get the Identity linked from this Person. If there isn't already one, make one.
	    val identityProp = person.localProp(identityLink)
	    val identityId = identityProp match {
	      case Some(propAndVal) if (!propAndVal.isEmpty) => propAndVal.first
  	      // This will set identityProp, as well as getting the Identity's OID:
	      case _ => setIdentityId(person, context)
	    }
	    val hash = idHash(person.id, identityId)
	    
	    // TODO: this surely belongs in a utility somewhere -- it constructs the full path to a Thing, plus some paths.
	    // Technically speaking, we are converting a Link to an ExternalLink, then adding params.
	    val url = urlBase + "u/" + state.owner.toThingId + "/" + state.name + "/" + t.toThingId + 
	      "?" + personParam + "=" + person.id.toThingId + "&" +
	      identityParam + "=" + hash +
	      (if (chromeless) "&cl=on" else "")
	    ExactlyOne(ExternalLinkType(url))
      }
      case _ => WarningValue("Invite Link is only defined when sending email")
    }
  }
  
  /***********************************************
   * LOGIN HANDLER
   ***********************************************/
  
  case class SpaceSpecificUser(identityId:OID, name:String, email:EmailAddress, spaceId:OID, personId:OID) extends User {
    val id = UnknownOID
    val identity = Identity(identityId, email)
    val identities = Seq(identity)
  }
  
  /**
   * This is called via callbacks when we are beginning to render a page. It looks to see whether the
   * URL or the Session contains a space-specific Identity that we should be using as the "user".
   * 
   * TODO: make this Identity Space-specific! It should be possible to have different Persons in the
   * Session for different Spaces.
   */
  object IdentityLoginChecker extends Contributor[RequestContext,RequestContext] {
    def notify(rc:RequestContext, sender:Publisher[RequestContext, RequestContext]):RequestContext = {
      val rcOpt =
        for (
          idParam <- rc.firstQueryParam(identityParam);
          state <- rc.state;
          // NOTE: this is messy for backward compatibility. The first clause is the current way things work: the candidate is
          // the value of the "person" param. The orElse is the way it originally worked: the candidate is the Thing being pointed to.
          // TODO: this should be deprecated and removed when the Wedding Site is done with, if not sooner.
          candidate <- rc.firstQueryParam(personParam).flatMap(personThingId => state.anything(ThingId(personThingId))) orElse rc.thing;
          idProp <- candidate.localProp(identityLink);
          emailPropVal <- candidate.getPropOpt(emailAddressProp)(state);
          email = emailPropVal.first;
          name = candidate.displayName;
          identityId = idProp.first;
          if Hasher.authenticate(candidate.id.toString + identityId.toString, EncryptedHash(idParam));
          updates = Seq((identityParam -> identityId.toString), (identityName -> name), (identityEmail -> email.addr), (personParam -> candidate.id.toString));
          // TODO: if there is already a User in the RC, we should *add* to that User rather than
          // replacing it:
          newRc = rc.copy(
              sessionUpdates = rc.sessionUpdates ++ updates,
              requester = Some(SpaceSpecificUser(identityId, name, email, state.id, candidate.id)))
        ) 
          yield newRc
          
      rcOpt.getOrElse {
        // Okay, the URL doesn't have an Identity login. Does the session already have one?
        val session = rc.request.session
        val withIdentityOpt = for (
          existingIdentity <- session.get(identityParam);
          idName <- session.get(identityName);
          idEmail <- session.get(identityEmail);
          personId <- session.get(personParam)
          )
          yield rc.copy(requester = Some(SpaceSpecificUser(OID(existingIdentity), idName, EmailAddress(idEmail), rc.state.get.id, OID(personId))))
        withIdentityOpt.getOrElse(rc)
      }
    }
  }
}