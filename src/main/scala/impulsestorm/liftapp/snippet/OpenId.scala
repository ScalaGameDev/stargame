package impulsestorm.liftapp {

package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

import lib.ImOpenIDVendor

class OpenId {
  def form(in: NodeSeq): NodeSeq =
    ImOpenIDVendor.showUserBox(in)
}

}
}
