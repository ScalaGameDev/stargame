package bootstrap.liftweb

import net.liftweb._
import net.liftweb.openid._
import net.liftweb.mongodb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import impulsestorm.liftapp.lib.ImOpenIDVendor
import impulsestorm.liftapp.model.{StarView}

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // mongo db definiton
    MongoDB.defineDb(DefaultMongoIdentifier, 
      MongoAddress(MongoHost("localhost", 27017), "impulsestormGames"))

    // where to search snippet
    LiftRules.addToPackages("impulsestorm.liftapp")
    
    // Build SiteMap
    val entries = List(
      Menu("Home") / "index", // the simple way to declare a menu
      Menu("Login") / "login",
      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	       "Static Content"))) ::: StarView.menus
	  
    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    LiftRules.statelessRewrite.prepend(StarView.rewrites)
    
    LiftRules.dispatch.append(ImOpenIDVendor.dispatchPF)
    
    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => ImOpenIDVendor.currentUser.isDefined)

  }
}
