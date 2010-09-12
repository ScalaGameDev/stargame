package impulsestorm.liftapp.lib

import _root_.scala.xml.NodeSeq

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._

import _root_.net.liftweb.openid.SimpleOpenIDVendor

object ImOpenIDVendor extends SimpleOpenIDVendor {
  /*
  override def dispatchPF: LiftRules.DispatchPF = NamedPF("Login default") {
    case Req(PathRoot :: LogOutPath :: _, "", _) =>
      () => {
        logUserOut()
        Full(RedirectResponse(S.referer openOr "/", S responseCookies :_*))
      }

    case r @ Req(PathRoot :: LoginPath :: _, "", PostRequest)
      if r.param(PostParamName).isDefined =>
      () => {
          Full(OpenIDObject.is.authRequest(r.param(PostParamName).get, "/"+PathRoot+"/"+ResponsePath))
      }
  }*/
  
  override def loginForm: NodeSeq = {
    <form class="openid" method="post" action={"/"+PathRoot+"/"+LoginPath}>
      <table class="openid">
      <tr>
      <td width="60px">
        <ul class="openid providers"> 
          <li class="openid" title="OpenID"><img src="images/openid/openidW.png" alt="icon" /> 
          <span><strong>http://</strong></span></li> 
          <li class="direct" title="Google"> 
          <img src="images/openid/googleW.png" alt="icon" /><span>https://www.google.com/accounts/o8/id</span></li> 
          <li class="direct" title="Yahoo"> 
          <img src="images/openid/yahooW.png" alt="icon" /><span>http://yahoo.com/</span></li> 
          <li class="username" title="Verisign user name"> 
          <img src="images/openid/verisign.png" alt="icon" /><span>http://<strong>username</strong>.pip.verisignlabs.com/</span></li> 
        </ul>
      </td>
      <td>
        <fieldset> 
          <label for="openid_username">Enter your <span>Provider user name</span></label> 
          <div><span></span><input type="text" name="openid_username" /><span></span> 
          <input type="submit" value="Login" /></div> 
        </fieldset> 
        <fieldset>  
          <label for="openid_identifier">Enter your <a class="openid_logo" href="http://openid.net">OpenID</a></label>
          <div><input type="text" name="openid_identifier" /> 
          <input type="submit" value="Login" /></div>
        </fieldset>
      </td>
      </tr>
      </table>
    </form>  
  }
  
  
}
