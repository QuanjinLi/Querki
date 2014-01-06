package querki.system

import querki.ecology._
import querki.values.SpaceState

import modules.{Module, ModuleIds}

private[system] trait SystemManagement extends EcologyInterface {
  def setState(state:SpaceState)
}

object SystemMOIDs extends ModuleIds(18)

class SystemEcot(e:Ecology, val moduleId:Short) extends Module(e) with System with SystemManagement {
  def setState(state:SpaceState) = {
    models.system.SystemSpace._state = Some(state)
  }
  
  def SystemState = models.system.SystemSpace.State
}