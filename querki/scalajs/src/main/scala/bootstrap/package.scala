import org.scalajs.jquery._

package object bootstrap {
  implicit def jq2Boostrap(jq:JQuery):BootstrapFacade = jq.asInstanceOf[BootstrapFacade]
}