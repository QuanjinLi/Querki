package querki.data

case class IdentityInfo(oid:String, name:String, handle:String)

case class UserInfo(oid:String, identities:Seq[IdentityInfo], skillLevel:TID) {
  def mainIdentity = identities.head
}
