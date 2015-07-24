package sbtrelease.gitflow

import sbtrelease.gitflow.ReleasePlugin.autoImport.ReleaseKeys

case class Args (
  isDryRun: Boolean,
  skipTests: Boolean,
  skipPublish: Boolean,
  useDefs: Boolean
)
object Args {
  def apply(args: Seq[String]) : Args = {
    import ReleaseKeys._
    Args(
      isDryRun = args.contains(DryRun),
      skipTests = args.contains(SkipTests),
      skipPublish = args.contains(SkipPublish),
      useDefs = args.contains(WithDefaults)
    )
  }
}
