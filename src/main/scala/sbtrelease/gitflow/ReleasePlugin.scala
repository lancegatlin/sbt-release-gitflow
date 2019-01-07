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
      val SkipIfExists = "skip-if-exists"
      val SkipPush = "skip-push"
      val releaseArgsParser = (
        Space ~> WithDefaults |
        Space ~> SkipTests |
        Space ~> SkipPublish |
        Space ~> DryRun |
        Space ~> SkipIfExists |
        Space ~> SkipPush
      ).*.map(Flags.apply)
      
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

          findReleaseBranch(searchRemote = true, artifactName) match {
            case Some(releaseBranch) =>
              // Checkout in case its only on remote
              checkoutBranch(releaseBranch)
              checkoutBranch(developBranch)
              deleteLocalAndRemoteBranch(releaseBranch)
            case None =>
              log.info(s"No release to abort")
          }
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

      ensureStagingClean()
      ensureCurrentBranch(developBranch)
      ensureNotBehindRemote()

      log.info("Ensuring no release branch is already present... ")
      findReleaseBranch(searchRemote = true,artifactName) match {
        case Some(releaseBranch) =>
          if(flags.skipIfExists) {
            log.info("Skipping creating release branch")
            // Ensure release branch is local and end command in release branch
            ensureBranchIsLocalAndCheckout(releaseBranch)
            for {
              _ <- runClean()
              _ <- runUpdate()
              _ <- runTest()
              _ <- runPublish()
            } yield ()
          } else {
            die(s"Unexpected release branch found: $releaseBranch")
          }
        case _ =>

          for {
            // Run these before making changes to ensure everything is ok to release
            _ <- runClean()
            _ <- runUpdate()
            _ <- runTest()
            nextSnapshotVersion = calcNextSnapshotVersion(currentVersion)
            releaseBranch = calcReleaseBranch(currentVersion)
            updatedVersion = suggestNextSnapshotVersion(
              nextSnapshotVersion
            )
            _ = {
              // Create release branch
              checkoutNewBranch(releaseBranch)
              // Update develop branch with incremented version
              checkoutBranch(developBranch)
              updateVersionFile(updatedVersion)
              addAndCommitVersionFile(
                commitMessage = calcVersionChangeCommitMessage(updatedVersion)
              )
              pushBranch(developBranch)
            }
            _ <- setVersion(updatedVersion)
            // Publish develop
            _ <- runPublish()
            // Note: end this command on the release branch to allow immediately executing close command
            _ = checkoutBranch(releaseBranch)

            _ <- setVersion(currentVersion)
            // Publish release
            _ <- runPublish()
          } yield ()
      }

    },

    releaseCloseSteps := { cfg:Config =>
      import cfg._
      val helper = new Helper(cfg)
      import helper._

      ensureStagingClean()
      val releaseBranch = findReleaseBranch(searchRemote = false,artifactName).getOrDie("Could not find release branch!")
      ensureCurrentBranch(releaseBranch)
      // If skipPush is set then there may not be a tracking remote for the release branch
      // since create was probably called with skipPush
      if(!flags.skipPush) {
        ensureNotBehindRemote()
      }
      checkSnapshotDependencies()
      checkoutBranch(masterBranch)
      ensureNotBehindRemote()
      checkoutBranch(releaseBranch)

      val releaseVersion = currentVersion.withoutQualifier
      for {
        // Run these before making changes to ensure everything is ok to close
        _ <- runClean()
        _ <- runUpdate()
        _ <- runTest()
        (tagName,tagComment) = calcTag(releaseVersion)
        _ = {
          updateVersionFile(releaseVersion)
          addAndCommitVersionFile(
            commitMessage = calcVersionChangeCommitMessage(releaseVersion)
          )
          checkoutBranch(masterBranch)
          mergeBranch(
            branchName = releaseBranch,
            flags = Seq("--no-ff","--strategy-option","theirs")
          )
          tag(tagName,tagComment)
          pushBranch(masterBranch)
          pushBranch(releaseBranch)
          pushTag(tagName)
          deleteLocalAndRemoteBranch(releaseBranch)
        }
        _ <- setVersion(releaseVersion)
        // Publish final artifact from master
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
