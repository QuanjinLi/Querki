package querki.identity

import upickle._
import rx._
import scalatags.JsDom.all.{name => nm, _}

import querki.comm._
import querki.globals._
import querki.data.UserInfo
import querki.display._
import querki.display.rx._

class UserManagerEcot(e:Ecology) extends ClientEcot(e) with UserAccess {
  
  def implements = Set(classOf[UserAccess])
  
  lazy val controllers = interface[querki.comm.ApiComm].controllers
  lazy val PageManager = interface[querki.display.PageManager]
  lazy val StatusLine = interface[querki.display.StatusLine]
  
  var _user:Option[UserInfo] = None
  def user:Option[UserInfo] = _user
  
  def setUser(user:Option[UserInfo]) = _user = user
  
  def name = _user.map(_.mainIdentity.name).getOrElse("Not logged in")
  
  def login:Future[UserInfo] = {
    val loginPromise = Promise[UserInfo]
    
    val handleInput = GadgetRef[RxText]
    val passwordInput = GadgetRef[RxInput]
    
    val loginDialog = new Dialog("Log in to Querki",
      div(
        handleInput <= new RxText(placeholder := "Handle or email address", width := "80%", nm := "name", id := "name", tabindex := 1),
        passwordInput <= new RxInput("password", placeholder := "Password", width := "80%", nm := "password", id := "password", tabindex := 2),
        p(""),
        new ButtonGadget(ButtonGadget.Normal, "Log in", disabled := Rx{ handleInput.get.isEmpty() || passwordInput.get.isEmpty() }, tabindex := 3)({ () =>
          // We call this one as a raw AJAX call, instead of going through client, since it is a weird case:
          val fut:Future[String] = 
            controllers.LoginController.clientlogin().callAjax("name" -> handleInput.get.text(), "password" -> passwordInput.get.text())
          fut.foreach { result =>
            if (result == "failed") {
              StatusLine.showBriefly(s"That isn't a correct email and password; please try again.")
              loginPromise.failure(new Exception("Wasn't a legal login"))
            } else {
              val info = read[UserInfo](result)
              _user = Some(info)
              loginPromise.success(info)
              PageManager.reload()
            }
          }
        })
      )
    )
    loginDialog.show()
    
    loginPromise.future
  }
}
