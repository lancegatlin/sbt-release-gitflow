package sbtrelease

import sbt._
import sbtrelease.gitflow.ReleasePlugin.autoImport.ReleaseKeys._

object Utilities {
  def versionFormatError = sys.error("Version format is not compatible with " + Version.VersionR.pattern.toString)

  class StateW(st: State) {
    def extract = Project.extract(st)
  }
  implicit def stateW(st: State): StateW = new StateW(st)

   def resolve[T](key: ScopedKey[T], extracted: Extracted): ScopedKey[T] =
		Project.mapScope(Scope.resolveScope(GlobalScope, extracted.currentRef.build, extracted.rootProject) )( key.scopedKey )

  object Yes {
    def unapply(s: Option[String]) = s.exists(_.toLowerCase == "y")
  }

  def extractDefault(st: State, default: String): Option[String] = {
    val useDefs = st.get(useDefaults).getOrElse(false)
    if (useDefs) Some(default)
    else None
  }

}

