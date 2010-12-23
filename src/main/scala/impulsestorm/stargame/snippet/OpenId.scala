package impulsestorm.stargame {

package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._

import lib.ImOpenIDVendor

class OpenId {
  def form(in: NodeSeq): NodeSeq =
    ImOpenIDVendor.showUserBox(in)
  
  def dedicatedLoginForm(in: NodeSeq): NodeSeq = {
    println("Redir to: " + ImOpenIDVendor.loginRedirect.get.get)
    if(ImOpenIDVendor.loggedIn) {
      S.redirectTo(ImOpenIDVendor.loginRedirect.get.get)
    }
    ImOpenIDVendor.showUserBox(in)
  }
  
}

}
}
