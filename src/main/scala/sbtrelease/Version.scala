package sbtrelease

import sbtrelease.Utilities._

import scala.util.control.Exception._

object Version {
  sealed trait Bump {
    def bump: Version => Version
  }

  object Bump {
    object Major extends Bump { def bump = _.bumpMajor }
    object Minor extends Bump { def bump = _.bumpMinor }
    object Bugfix extends Bump { def bump = _.bumpBugfix }
    object Next extends Bump { def bump = _.bump }

    val default = Next
  }

  val VersionR = """([0-9]+)(?:(?:\.([0-9]+))?(?:\.([0-9]+))?)?([\-0-9a-zA-Z]*)?""".r
  val PreReleaseQualifierR = """[\.-](?i:rc|m|alpha|beta)[\.-]?[0-9]*""".r

  def apply(s: String): Version = {
    allCatch opt {
      val VersionR(maj, min, mic, qual) = s
      Version(maj.toInt, Option(min).map(_.toInt), Option(mic).map(_.toInt), Option(qual).filterNot(_.isEmpty))
    }
  }.getOrElse(versionFormatError)
}

case class Version(major: Int, minor: Option[Int], bugfix: Option[Int], qualifier: Option[String]) {
  def bump : Version = {
    val maybeBumpedPrerelease = qualifier.collect {
      case Version.PreReleaseQualifierR() => withoutQualifier
    }
    def maybeBumpedBugfix = bugfix.map(m => copy(bugfix = Some(m + 1)))
    def maybeBumpedMinor = minor.map(m => copy(minor = Some(m + 1)))
    def bumpedMajor = copy(major = major + 1)

    maybeBumpedPrerelease
      .orElse(maybeBumpedBugfix)
      .orElse(maybeBumpedMinor)
      .getOrElse(bumpedMajor)
  }

  def bumpMajor : Version = copy(major = major + 1, minor = minor.map(_ => 0), bugfix = bugfix.map(_ => 0))
  def bumpMinor : Version = copy(minor = minor.map(_ + 1), bugfix = bugfix.map(_ => 0))
  def bumpBugfix : Version = copy(bugfix = bugfix.map(_ + 1))

  def bump(bumpType: Version.Bump): Version = bumpType.bump(this)

  def withoutQualifier : Version = copy(qualifier = None)
  def asSnapshot : Version = copy(qualifier = Some("-SNAPSHOT"))

  override def toString =
    s"$major${get(minor)}${get(bugfix)}${qualifier.getOrElse("")}"

  private def get(part: Option[Int]) = part.map("." + _).getOrElse("")
}
