package sbtrelease.gitflow

import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._
import sbtrelease._
import sbtrelease.gitflow.ReleaseStateTransformations.Helper

object ReleasePlugin extends AutoPlugin {
  import Utilities._

  object autoImport {
    // More commonly configured settings
    val releaseVersionBump = settingKey[Version.Bump]("How the version should be incremented")
    val releaseCalcTagComment = settingKey[Version => String]("Compute the tag comment from version")
    val releaseCalcVersionChangeCommitMessage = settingKey[Version => String]("The commit message to use when version is bumped")
    val releaseCalcTag = settingKey[Version => (String,String)]("Compute the tag name and comment from version. Defaults to 'v' + version")
    val releaseCreateSteps = settingKey[Config => Step[Unit]]("The release creation process")
    val releaseCloseSteps = settingKey[Config => Step[Unit]]("The release close process")
    val releaseUseGlobalVersion = settingKey[Boolean]("Whether to use a global version")

    // Less commonly used settings
    val releaseSnapshotDependencies = taskKey[Seq[ModuleID]]("Calculate the snapshot dependencies for a build")
    val releaseCalcNextSnapshotVersion = settingKey[Version => Version]("Function to compute the next release version from the last version. Defaults to version bump.")
    val releaseVersionFile = settingKey[File]("The file to write the version to. Defaults to version.sbt")
    val releasePublishArtifactsAction = taskKey[Unit]("The action that should be performed to publish artifacts. Defaults to publish")

    // Gitflow configurables
    val gitflowMasterBranchName = settingKey[String]("Branch name for master branch")
    val gitflowDevelopBranchName = settingKey[String]("Branch name for the develop branch")
    val calcGitflowReleaseBranchName = settingKey[Version => String]("Function to compute the name of a gitflow release branch from the version. Defaults to 'release/X.Y.Z'")

    object ReleaseKeys {
      val WithDefaults = "with-defaults"
      val DryRun = "dry-run"
      val SkipTests = "skip-tests"
      val SkipPublish = "skip-publish"
      val CrossBuild = "cross"
      val FailureCommand = "--failure--"
      val releaseArgsParser = (
        Space ~> WithDefaults |
        Space ~> SkipTests |
        Space ~> SkipPublish |
        Space ~> CrossBuild |
        Space ~> DryRun
      ).*.map(Args.apply)
      
      val releaseCreateCommand =
        Command("releaseCreate")(_ => releaseArgsParser) { (s1,args) =>
          val e = Project.extract(s1)
          val f = e.get(releaseCreateSteps)
          val cfg = Config(e,args,s1.log)
          f(cfg)(s1)._1
        }

      val releaseCloseCommand =
        Command("releaseClose")(_ => releaseArgsParser) { (s1,args) =>
          val e = Project.extract(s1)
          val f = e.get(releaseCloseSteps)
          val cfg = Config(e,args,s1.log)
          f(cfg)(s1)._1
        }
      val releaseAbortCommand =
        Command("releaseAbort")(_ => releaseArgsParser) { (s1,args) =>
          import args._

          val e = Project.extract(s1)
          val cfg = Config(e,args,s1.log)
          import cfg._
          val helper = new Helper(cfg)
          import helper._

          val releaseBranch =
            findReleaseBranch(searchRemote = true)
              .getOrDie("Could not find release branch!")
          // Checkout in case its only on remote
          checkoutBranch(releaseBranch)
          checkoutBranch(developBranch)
          deleteLocalAndRemoteBranch(releaseBranch)
          s1
        }
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
    calcGitflowReleaseBranchName := { version:Version => s"release/${version.withoutQualifier}"},
    releaseSnapshotDependencies := {
      val moduleIds = (managedClasspath in Runtime).value.flatMap(_.get(moduleID.key))
      val snapshots = moduleIds.filter(m => m.isChanging || m.revision.endsWith("-SNAPSHOT"))
      snapshots
    },
  
    releaseVersionBump := Version.Bump.default,
    releaseCalcNextSnapshotVersion := {
      _
        .bump(releaseVersionBump.value)
        .asSnapshot
    },
    releaseUseGlobalVersion := true,

    releaseCalcTag := { v => (s"v$v",s"Release $v") },
    releaseCalcVersionChangeCommitMessage := { v => s"Setting version to $v" },

    releaseVersionFile := file("version.sbt"),

    releasePublishArtifactsAction := publish.value,

    releaseCreateSteps := { cfg =>
      import cfg._
      val helper = new Helper(cfg)
      import helper._
      
      ensureNoReleaseBranch()
      ensureStagingClean()
      ensureCurrentBranch(developBranch)
      ensureNoReleaseBranch()
      
      for {
        _ <- runClean()
        _ <- runUpdate()
        _ <- runTest()
        nextSnapshotVersion = calcNextSnapshotVersion(currentVersion)
        releaseBranch = calcReleaseBranch(currentVersion)
        _ = checkoutNewBranch(releaseBranch)
        _ = checkoutBranch(developBranch)
        updatedVersion = suggestNextSnapshotVersion(
          nextSnapshotVersion
        )
        _ = updateVersionFile(updatedVersion)
        _ <- setVersion(updatedVersion)
        _ = addAndCommitVersionFile(
          commitMessage = calcVersionChangeCommitMessage(updatedVersion)
        )
        _ = pushBranch(developBranch)
        _ <- runPublish()
        // Note: end this command on the release branch to allow immediately executing close command
        _ = checkoutBranch(releaseBranch)
      } yield ()
    },

    releaseCloseSteps := { cfg:Config =>
      import cfg._
      val helper = new Helper(cfg)
      import helper._

      ensureStagingClean()
      val releaseBranch = findReleaseBranch(searchRemote = false).getOrDie("Could not find release branch!")
      ensureCurrentBranch(releaseBranch)
      ensureNotBehindRemote()
      checkSnapshotDependencies()
      val releaseVersion = currentVersion.withoutQualifier
      for {
        _ <- runClean()
        _ <- runUpdate()
        _ <- runTest()
        _ = updateVersionFile(releaseVersion)
        _ <- setVersion(releaseVersion)
        _ = addAndCommitVersionFile(
          commitMessage = calcVersionChangeCommitMessage(releaseVersion)
        )
        _ = checkoutBranch(masterBranch)
        _ = mergeBranch(
          branchName = releaseBranch,
          flags = Seq("--no-ff","--strategy-option","theirs")
        )
        (tagName,tagComment) = calcTag(releaseVersion)
        _ = tag(tagName,tagComment)
        _ = pushBranch(masterBranch)
        _ = pushBranch(releaseBranch)
        _ = pushTag(tagName)
        _ = deleteLocalAndRemoteBranch(releaseBranch)
        _ <- runPublish()
      } yield ()
    },
    commands ++= Seq(
      releaseCreateCommand,
      releaseCloseCommand,
      releaseAbortCommand
    )
  )
}
