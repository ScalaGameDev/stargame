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
    
  def logoutOnly(in: NodeSeq): NodeSeq = 
    ImOpenIDVendor.showLogoffOnly(in)
  
  def dedicatedLoginForm(in: NodeSeq): NodeSeq = {
    def redirectTo(url: String) = {
      println("Redir to: " + url)
      S.redirectTo(url)
    }
      
    if(ImOpenIDVendor.loggedIn) {
      ImOpenIDVendor.loginRedirect.get.getOrElse("/stargame/")
    }
    ImOpenIDVendor.showUserBox(in)
  }
  
}

}
}
