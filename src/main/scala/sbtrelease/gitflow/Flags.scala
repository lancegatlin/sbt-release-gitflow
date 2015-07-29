package sbtrelease.gitflow

import sbtrelease.gitflow.ReleasePlugin.autoImport.ReleaseKeys

case class Flags (
  isDryRun: Boolean,
  skipTests: Boolean,
  skipPublish: Boolean,
  useDefs: Boolean,
  skipIfExists: Boolean,
  skipPush: Boolean
)
object Flags {
  def apply(args: Seq[String]) : Flags = {
    import ReleaseKeys._
    Flags(
      isDryRun = args.contains(DryRun),
      skipTests = args.contains(SkipTests),
      skipPublish = args.contains(SkipPublish),
      useDefs = args.contains(WithDefaults),
      skipIfExists = args.contains(SkipIfExists),
      skipPush = args.contains(SkipPush)
    )
  }
}
