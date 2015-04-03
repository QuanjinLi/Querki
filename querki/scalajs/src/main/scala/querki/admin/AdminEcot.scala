package querki.admin

import querki.globals._

class AdminEcot(e:Ecology) extends ClientEcot(e) with Admin {

  def implements = Set(classOf[Admin])
  
  lazy val Pages = interface[querki.pages.Pages]

  lazy val statisticsFactory = Pages.registerStandardFactory("_adminStats", { new StatisticsPage(_) })
  
  override def postInit() = {
    // Instantiate the Pages:
    statisticsFactory
  }
}