package querki.test.mid

import AllFuncs._

/**
  * Functions that provide common setup, to reduce boilerplate for special-purpose tests.
  */
trait SetupFuncs {
  /**
    * This creates the specified normal user, and has them create a Space.
    *
    * @param usernameBase The base of this user's name. This must be unique, so should be based on the suite name.
    *                     Note that this is the display name, and may be arbitrarily long.
    * @param spaceName The name of the Space, which only needs to be unique per-User.
    * @return Nothing, but the State is set up.
    */
  def setupUserAndSpace(usernameBase: String, spaceName: String): TestOp[Unit] = {
    for {
      std <- getStd
      user = TestUser(usernameBase)
      loginResults <- newUser(user)
      space <- createSpace(spaceName)

    }
      yield ()
  }
}
