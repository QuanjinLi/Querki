package querki.admin

import scala.concurrent.duration._

import akka.actor._

import querki.globals._
import querki.time.DateTime

/**
 * Second draft of system monitoring.
 * 
 * Whereas the (now-deleted) first version of this sent a message to all Spaces
 * and told them to self-report, this one expects to receive regular updates from
 * all of the relevant entities. While Querki is modest-sized, this should work
 * well.
 * 
 * In the medium term, we will want to replace this yet again, with a stream-oriented
 * mechanism that is more big-data friendly, probably based on Spark. But one problem
 * at a time.
 * 
 * @author jducoeur
 */
class AdminMonitor(val ecology:Ecology) extends Actor with EcologyMember {
  import AdminMonitor._

  lazy val SystemManagement = interface[querki.system.SystemManagement]
  
  lazy val scheduler = SystemManagement.actorSystem.scheduler
  
  lazy val monitorTimeout = Config.getDuration("querki.admin.monitorTimeout")

  var spaces = Map.empty[OID, SpaceMonitorEvent]
  var users = Map.empty[OID, UserMonitorEvent]
  var watches = Map.empty[ActorPath, MonitorEvent]
  
  def cleanOldEvents() = {
    val now = DateTime.now
    val old = now.minus(monitorTimeout.toMillis)
    spaces = spaces.filter(_._2.sentTime.isBefore(old))
    users = users.filter(_._2.sentTime.isBefore(old))
  }
  
  def watch(evt:MonitorEvent) = {
    val path = sender.path
    if (watches.get(path).isEmpty) {
      watches += (path -> evt)
      context.watch(sender)
    }
  }
  
  def receive = {
    case evt @ SpaceMonitorEvent(spaceId, _, _) => { spaces += (spaceId -> evt); watch(evt) }
    case evt @ UserMonitorEvent(userId) => { users += (userId -> evt); watch(evt) }
    
    case Terminated(mon) => {
      watches.get(mon.path) match {
        case Some(SpaceMonitorEvent(spaceId, _, _)) => spaces -= spaceId
        case Some(UserMonitorEvent(userId)) => users -= userId
        case None => QLog.error(s"AdminMonitor somehow got a Terminated message for unknown Monitor ${mon.path}!")
      }
    }
  }
}

object AdminMonitor {
  def actorProps(ecology:Ecology) = Props(classOf[AdminMonitor], ecology)
}
  
sealed trait MonitorEvent {
  val sentTime = DateTime.now
}
case class SpaceMonitorEvent(spaceId:OID, name:String, nUsers:Int) extends MonitorEvent
case class UserMonitorEvent(userId:OID) extends MonitorEvent