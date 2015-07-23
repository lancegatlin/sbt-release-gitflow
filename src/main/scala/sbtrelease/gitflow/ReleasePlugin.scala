package sbtrelease.gitflow

import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._
import sbt.complete.Parser
import sbtrelease._

object ReleasePlugin extends AutoPlugin {

  object autoImport {
    // More commonly configured settings
    val releaseVersionBump = settingKey[Version.Bump]("How the version should be incremented")
    val releaseTagComment = taskKey[String]("The comment to use when tagging")
    val versionChangeCommitMessage = taskKey[String]("The commit message to use when version is bumped")
    val releaseTagName = taskKey[String]("The name of the tag. Defaults to 'vX.Y.Z'")
    val releaseCreateProcess = settingKey[Seq[ReleaseStep]]("The release creation process")
    val releaseCloseProcess = settingKey[Seq[ReleaseStep]]("The release merge close process")
    val releaseCrossBuild = settingKey[Boolean]("Whether the release should be cross built")
    val releaseUseGlobalVersion = settingKey[Boolean]("Whether to use a global version")
    val globalVersionString = "version in ThisBuild := \"%s\""
    val versionString = "version := \"%s\""

    // Less commonly used settings
    val releaseSnapshotDependencies = taskKey[Seq[ModuleID]]("Calculate the snapshot dependencies for a build")
    val releaseCalcNextVersion = settingKey[String => String]("Function to compute the next release version from the last version. Defaults to version bump.")
    val releaseVersionFile = settingKey[File]("The file to write the version to. Defaults to version.sbt")
    val releasePublishArtifactsAction = taskKey[Unit]("The action that should be performed to publish artifacts. Defaults to publish")

    // Gitflow configurables
    val gitflowMasterBranchName = settingKey[String]("Branch name for master branch")
    val gitflowDevelopBranchName = settingKey[String]("Branch name for the develop branch")
    val calcGitflowReleaseBranchName = settingKey[String => String]("Function to compute the name of a gitflow release branch from the version. Defaults to 'release/X.Y.Z'")

    object ReleaseKeys {
      val gitflowReleaseBranchName = AttributeKey[String]("Internal. Used to store the computed release branch name")
      val nextVersion = AttributeKey[String]("nextVersion")
      val useDefaults = AttributeKey[Boolean]("releaseUseDefaults")
      val skipTests = AttributeKey[Boolean]("releaseSkipTests")
      val skipPublish = AttributeKey[Boolean]("releaseSkipPublish")
      val cross = AttributeKey[Boolean]("releaseCross")

      val WithDefaults = "with-defaults"
      val SkipTests = "skip-tests"
      val SkipPublish = "skip-publish"
      val CrossBuild = "cross"
      val FailureCommand = "--failure--"
      val releaseParser = (Space ~> WithDefaults | Space ~> SkipTests | Space ~> SkipPublish | Space ~> CrossBuild).*

      def mkReleaseCommand(
        key: String,
        p: SettingKey[Seq[ReleaseStep]],
        parser: Parser[Seq[String]]
      ): Command =
        Command(key)(_ => parser) { (st, args) =>
          val extracted = Project.extract(st)
          val releaseParts = extracted.get(p)
          val crossEnabled = extracted.get(releaseCrossBuild) || args.contains(CrossBuild)
          val startState = st
            .copy(onFailure = Some(FailureCommand))
            .put(useDefaults, args.contains(WithDefaults))
            .put(skipTests, args.contains(SkipTests))
            .put(skipPublish, args.contains(skipPublish))
            .put(cross, crossEnabled)

          val initialChecks = releaseParts.map(_.check)

          def filterFailure(f: State => State)(s: State): State = {
            s.remainingCommands match {
              case FailureCommand :: tail => s.fail
              case _ => f(s)
            }
          }

          val removeFailureCommand = { s: State =>
            s.remainingCommands match {
              case FailureCommand :: tail => s.copy(remainingCommands = tail)
              case _ => s
            }
          }

          val process = releaseParts.map { step =>
            if (step.enableCrossBuild && crossEnabled) {
              filterFailure(ReleaseStateTransformations.runCrossBuild(step.action)) _
            } else filterFailure(step.action) _
          }

          initialChecks.foreach(_(startState))
          Function.chain(process :+ removeFailureCommand)(startState)
        }

      lazy val releaseCreateCommandKey = "releaseCreate"
      val releaseCreateCommand = mkReleaseCommand(
        releaseCreateCommandKey,
        releaseCreateProcess,
        releaseParser
      )
      lazy val releaseCloseCommandKey = "releaseClose"
      val releaseCloseCommand = mkReleaseCommand(
        releaseCloseCommandKey,
        releaseCloseProcess,
        releaseParser
      )
    }

  }

  import ReleaseStateTransformations._
  import autoImport.ReleaseKeys._
  import autoImport._
  import Utilities._

  override def trigger = allRequirements

  override def projectSettings = Seq[Setting[_]](
    gitflowDevelopBranchName := "develop",
    gitflowMasterBranchName := "master",
    calcGitflowReleaseBranchName := { version:String => s"release/$version"},
    releaseSnapshotDependencies := {
      val moduleIds = (managedClasspath in Runtime).value.flatMap(_.get(moduleID.key))
      val snapshots = moduleIds.filter(m => m.isChanging || m.revision.endsWith("-SNAPSHOT"))
      snapshots
    },
  
    releaseVersionBump := Version.Bump.default,
    releaseCalcNextVersion := { ver =>
      Version(ver)
        .map(_.bump(releaseVersionBump.value)
        .asSnapshot.string)
        .getOrElse(versionFormatError)
    },
    releaseUseGlobalVersion := true,
    releaseCrossBuild := false,

    releaseTagName := s"v${(version in ThisBuild).value}",
    releaseTagComment := s"Releasing ${(version in ThisBuild).value}",
    versionChangeCommitMessage := s"Setting version to ${(version in ThisBuild).value}",

    releaseVersionFile := file("version.sbt"),

    releasePublishArtifactsAction := publish.value,

    releaseCreateProcess := Seq[ReleaseStep](
      ensureNoReleaseBranch,
      ensureStagingClean,
      ensureCurrentBranch(_.extract.get(gitflowDevelopBranchName)),
      ensureNotBehindRemote,
      checkoutNewBranch(
        branchName = calcReleaseBranchName
      ),
      checkoutBranch(_.extract.get(gitflowDevelopBranchName)),
      calcNextVersion,
      updateVersionFile(
        newVersion = getNextVersion
      ),
      setVersion(getNextVersion),
      runClean,
      runTest,
      publishArtifacts,
      addAndCommitAll(
        commitMessage = calcVersionChangeCommitMessage
      ),
      pushAllpushTags
    ),

    releaseCloseProcess := Seq[ReleaseStep](
      ensureStagingClean,
      setReleaseBranch(
        findReleaseBranch(searchRemote = false).andThen(_.getOrElse(sys.error("Could not find release branch!")))
      ),
      ensureCurrentBranch(getReleaseBranch),
      ensureNotBehindRemote,
      setVersion(getReleaseBranch.andThen(_.drop("release/".length))),
      checkSnapshotDependencies,
      runClean,
      runTest,
      updateVersionFile(
        newVersion = _.extract.get(version)
      ),
      publishArtifacts,
      addAndCommitAll(
        commitMessage = calcVersionChangeCommitMessage
      ),
      checkoutBranch(_.extract.get(gitflowMasterBranchName)),
      mergeBranch(
        branch = getReleaseBranch,
        flags = Seq("--no-ff","--strategy-option","theirs")
      ),
      tagRelease,
      pushAllpushTags,
      deleteLocalAndRemoteBranch(getReleaseBranch)
    ),

    commands ++= Seq(releaseCreateCommand,releaseCloseCommand)
  )
}
