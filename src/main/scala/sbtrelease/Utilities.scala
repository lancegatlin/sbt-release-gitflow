package sbtrelease

import sbt._

object Utilities {
  def die(msg: String) = sys.error(msg)
  implicit class OptionPML[A](val self: Option[A]) extends AnyVal {
    def getOrDie(msg: String) = self.getOrElse(die(msg))
  }

  implicit class StatePML[A](val self: State) extends AnyVal {
    def getOrDie[T](key: AttributeKey[T]): T = self.get(key).getOrDie(s"AttributeKey ${key.label} is not set")
  }

  def versionFormatError = sys.error("Version format is not compatible with " + Version.VersionR.pattern.toString)

  def resolve[T](key: ScopedKey[T], extracted: Extracted): ScopedKey[T] =
    Project.mapScope(Scope.resolveScope(GlobalScope, extracted.currentRef.build, extracted.rootProject) )( key.scopedKey )
//
//  object Yes {
//    def unapply(s: Option[String]) = s.exists(_.toLowerCase == "y")
//  }


}

