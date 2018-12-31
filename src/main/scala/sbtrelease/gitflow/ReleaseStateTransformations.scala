package sbtrelease.gitflow

import sbt.Aggregation.KeyValue
import sbt.Keys._
import sbt.Package.ManifestAttributes
import sbt._
import sbt.std.Transform.DummyTaskMap
import sbtrelease._
import sbtrelease.gitflow.ReleasePlugin.autoImport._

import scala.annotation.tailrec

object ReleaseStateTransformations {
  import Utilities._

  // TODO: get rid of me
  private def runTaskAggregated[T](taskKey: TaskKey[T], state: State): (State, Result[Seq[KeyValue[T]]]) = {
    import EvaluateTask._
    val extra = DummyTaskMap(Nil)
    val extracted = Project.extract(state)
    val config = extractedTaskConfig(extracted, extracted.structure, state)

    val rkey = Utilities.resolve(taskKey.scopedKey, extracted)
    val keys = Aggregation.aggregate(rkey, ScopeMask(), extracted.structure.extra)
    val tasks = Act.keyValues(extracted.structure)(keys)
    val toRun = tasks map { case KeyValue(k,t) => t.map(v => KeyValue(k,v)) } join
    val roots = tasks map { case KeyValue(k,_) => k }


    val (newS, result) = withStreams(extracted.structure, state){ str =>
      val transform = nodeView(state, str, roots, extra)
      runTask(toRun, state,str, extracted.structure.index.triggers, config)(transform)
    }
    (newS, result)
  }

  class Helper(
    val cfg: Config
  ) {
    import cfg._
    import flags._
    import log._
    
    val git = Git
        .detect(baseDirectory,isDryRun)(cfg.log)
        .getOrDie("Working directory is not a git repository")

    def ensureStagingClean() : Unit = {
      info("Ensuring staging is clean... ")
  
      if (git.status.nonEmpty) {
        die("Working directory is dirty.")
      }
      
      info("Starting release process off commit: " + git.currentHash)
    }
    
    def findReleaseBranch(searchRemote: Boolean, artifactName: String) : Option[String] = {
      val result =
        git.localBranches.find(_.startsWith(s"release/${artifactName}")) orElse {
          if(searchRemote) {
            git.remoteBranches.find(_._2.startsWith(s"release/${artifactName}")).map(_._2)
          } else {
            None
          }
        }

      result match {
        case s@Some(releaseBranch) =>
          info(s"Found release branch $releaseBranch")
        case _ =>
      }
      result
    }

    def ensureNotBehindRemote() : Unit = {
      info("Ensuring not behind remote...")
      // Note: throws exception if not tracking
      git.trackingRemote

      if(git.isBehindRemote) {
        die("Upstream has unmerged commits")
      }
    }
    
    def ensureCurrentBranch(branchName: String) : Unit = {
      if(!isDryRun) {
        info(s"Ensuring current branch is $branchName...")
        if(git.currentBranch != branchName) {
          die(s"Must be started from branch $branchName")
        }
      }
    }

    def checkoutBranch(branchName: String) : Unit = {
      info(s"Checking out $branchName...")
  
      if (!git.checkout(branchName)) {
        die(s"Failed to checkout $branchName branch")
      }
    }
    
    def runClean() = Step.unit { s1 =>
      if (!skipTests || !skipPublish) {
        info("Running clean....")
        val extracted = Project.extract(s1)
        val ref = extracted.get(thisProjectRef)
        val s2 = extracted.runAggregated(clean in Global in ref, s1)
        s2
      } else {
        info("Skipping clean...")
        s1
      }
    }
    
    def runUpdate() = Step.unit { s1 =>
      if (
        // Only need to run update if skipping tests but still publishing
        // (test will perform update)
        skipTests && !skipPublish
      ) {
        info("Running update...")
        val extracted = Project.extract(s1)
        val ref = extracted.get(thisProjectRef)
        val s2 = extracted.runAggregated(update in Global in ref, s1)
        s2
      } else {
        info("Skipping update...")
        s1
      }
    }
    
    def runTest() = Step.unit { s1: State =>
      if(!skipTests) {
        info("Running tests...")
        val extracted = Project.extract(s1)
        val ref = extracted.get(thisProjectRef)
        val s2 = extracted.runAggregated(test in Test in ref, s1) 
        s2
      } else {
        info("Skipping running tests...")
        s1
      }
    }

    def ensureBranchIsLocalAndCheckout(branchName: String) : Unit = {
      info(s"Ensuring $branchName is local...")

      if(git.localBranches.contains(branchName)) {
        val remote = git.trackingRemote(branchName)
        git.checkout(branchName)
        info(s"Checked out local branch $branchName tracking $remote/$branchName")
      } else {
        val remote = git.remoteBranches.find(_._2 == branchName).getOrDie(s"Could not find remote branch $branchName")._1
        git.checkoutRemote(remote,branchName)
        info(s"Checked out $branchName locally tracking $remote/$branchName")
      }
    }

    def checkoutNewBranch(branchName: String) : Unit = {
      info(s"Creating branch $branchName...")
      val defaultChoice = if(useDefs) Some("y") else None
      defaultChoice orElse SimpleReader.readLine(s"Create branch $branchName (y/n)? [y] ") match {
        case Some("y") | Some("") =>
        case _ => die("Declined release branch creation")
      }
  
      val remote = git.trackingRemote
      git.checkoutNewBranch(branchName)
      if(!skipPush) {
        git.pushSetUpstream(remote)
      } else {
        info(s"Skipping pushing new branch $branchName...")
      }
    }
  
    def suggestNextSnapshotVersion(suggested: Version) : Version = {
      readVersion(suggested, "Next SNAPSHOT version [%s] : ", useDefs)
    }

    def updateVersionFile(newVersion: Version) : Unit = {
      info(s"Updating version file to $newVersion")
      val versionStr = if(useGlobal) {
        s"""version in ThisBuild := "$newVersion""""
      } else {
        s"""version := "$newVersion""""
      }
      if(!isDryRun) {
        IO.write(versionFile, versionStr + "\n")
      }
    }
  
    def setVersion(newVersion: Version) = Step.unit { s1:State =>
      info(s"Setting current version to $newVersion")
  
      val s2 = reapply(Seq(
        if (useGlobal) version in ThisBuild := newVersion.toString
        else version := newVersion.toString
      ), s1)
      
      s2
    }
    
    def addAndCommitVersionFile(commitMessage: String) : Unit = {
      info(s"Adding $versionFile and committing with message: '$commitMessage'")
      git.add(versionFile.toString)
      git.commit(commitMessage)
    }

    def pushBranch(branchName: String) : Unit = {
      val defaultChoice = if(skipPush) {
        Some("n")
      } else {
        if(useDefs) Some("y") else None
      }

      if (git.hasUpstream) {
        defaultChoice orElse SimpleReader.readLine(s"Push branch $branchName to the remote repository (y/n)? [y] ") match {
          case Some("y") | Some("") =>
            git.pushBranch(branchName)
          case _ => warn(s"Skipping pushing branch $branchName...")
        }
      } else {
        die(s"No upstream branch is configured for the local branch $branchName")
      }
    }
  
    def pushTag(tagName: String) : Unit = {
      val defaultChoice = if(skipPush) {
        Some("n")
      } else {
        if(useDefs) Some("y") else None
      }
      defaultChoice orElse SimpleReader.readLine(s"Push tag $tagName to the remote repository (y/n)? [y] ") match {
        case Some("y") | Some("") =>
          git.pushTag(tagName)
        case _ => warn(s"Skipping push tag $tagName...")
      }
    }

    def runPublish() = Step.unit { st =>
     if (!skipPublish) {
       val currentBranch = git.currentBranch
       info(s"Publishing from branch $currentBranch...")
       val extracted = Project.extract(st)
       val ref = extracted.get(thisProjectRef)
       // getPublishTo fails if no publish repository is set up.
       Classpaths.getPublishTo(extracted.get(publishTo in Global in ref))
       val s2 = extracted.runAggregated(releasePublishArtifactsAction in Global in ref, st)
       s2
     } else {
       info("Skipping publishing artifacts...")
       st
     }
    }
  
    def checkSnapshotDependencies() = Step.unit { st: State =>
      info("Checking for SNAPSHOT dependencies...")
      val e = Project.extract(st)
      val thisRef = e.get(thisProjectRef)
      val (newSt, result) = runTaskAggregated(releaseSnapshotDependencies in thisRef, st)
      val snapshotDeps = result match {
        case Value(value) => value.flatMap(_.value)
        case Inc(cause) => sys.error("Error checking for snapshot dependencies: " + cause)
      }
      if (snapshotDeps.nonEmpty) {
        val useDefaults = if(useDefs) Some("n") else None
        warn("Snapshot dependencies detected:\n" + snapshotDeps.mkString("\n"))
        useDefaults orElse SimpleReader.readLine("Do you want to continue (y/n)? [n] ") match {
          case Some("y") =>
          case _ => sys.error("Aborting release due to snapshot dependencies.")
        }
      }
      newSt
    }

    def mergeBranch(branchName: String, flags: Seq[String] = Nil) : Unit = {
      val currentBranch = git.currentBranch
      info(s"Merging branch $branchName into $currentBranch" )
      val defaultChoice = if(useDefs) Some("y") else None
      defaultChoice orElse SimpleReader.readLine(s"Merge branch $branchName into $currentBranch (y/n) [y] ") match {
        case Some("" | "y" | "Y") =>
          git.merge(branchName,flags)
        case _ =>
          die("Merge declined")
      }
    }
    
    def tag(tagName: String, tagComment: String) : Unit = {
      val defaultChoice = if(useDefs) Some("a") else None
      info(s"Creating tag $tagName...")

      @tailrec
      def findTag(tag: String): String = {
        if (git.tagExists(tag)) {
          defaultChoice orElse SimpleReader.readLine(s"Tag [$tag] exists! (O)verwrite, (a)bort or enter a new tag (o/a/*)? [a] ") match {
            case Some("" | "a" | "A") =>
              die(s"Tag [$tag] already exists. Aborting release!")

            case Some("o" | "O") =>
              warn(s"Overwriting a tag can cause problems if others have already seen the tag (see `${git.commandName} help tag`)!")
              tag

            case Some(newTag) =>
              findTag(newTag)

            case None =>
              die("No tag entered. Aborting release!")
          }
        } else {
          tag
        }
      }
      val pickedTag = findTag(tagName)
      git.tag(pickedTag,tagComment,force = true)

// TODO:
//      reapply(Seq[Setting[_]](
//        packageOptions += ManifestAttributes("git-release-tag" -> pickedTag)
//      ), st)
    }

    def deleteLocalAndRemoteBranch(branch: String) : Unit = {
      info(s"Deleting branch $branch...")

      val defaultChoice = if(useDefs) Some("y") else None
      defaultChoice orElse SimpleReader.readLine(s"Delete branch $branch (y/n)? [y] ") match {
        case Some("y") | Some("") =>
          if(!skipPush) {
            val remote = git.trackingRemote(branch)
            git.deleteRemoteBranch(remote, branch)
          } else {
            info(s"Skipping deleting remote branch $branch...")
          }
          git.deleteLocalBranch(branch)
        case _ =>
      }
    }

  }

  def readVersion(ver: Version, prompt: String, useDef: Boolean): Version = {
    if (useDef) ver
    else SimpleReader.readLine(prompt format ver) match {
      case Some("") => ver
      case Some(input) => Version(input)
      case None => sys.error("No version provided!")
    }
  }

  def reapply(settings: Seq[Setting[_]], state: State): State = {
    val extracted = Project.extract(state)
    import extracted._

    val append = Load.transformSettings(Load.projectScope(currentRef), currentRef.build, rootProject, settings)

    // We don't want even want to be able to save the settings that are applied to the session during the release cycle.
    // Just using an empty string works fine and in case the user calls `session save`, empty lines will be generated.
		val newSession = session.appendSettings( append map (a => (a, List.empty[String])))
		BuiltinCommands.reapply(newSession, structure, state)
  }


  // This is a copy of the state function for the command Cross.switchVersion
   def switchScalaVersion(state: State, version: String): State = {
    val x = Project.extract(state)
    import x._
    state.log.info("Setting scala version to " + version)
    val add = (scalaVersion in GlobalScope := version) :: (scalaHome in GlobalScope := None) :: Nil
    val cleared = session.mergeSettings.filterNot(Cross.crossExclude)
    val newStructure = Load.reapply(add ++ cleared, structure)
    Project.setProject(session, newStructure, state)
  }

   def runCrossBuild(func: State => State): State => State = { state =>
    val x = Project.extract(state)
    import x._
    val versions = Cross.crossVersions(state)
    val current = scalaVersion in currentRef get structure.data
    val finalS = (state /: versions) {
      case (s, v) => func(switchScalaVersion(s, v))
    }
    current.map(switchScalaVersion(finalS, _)).getOrElse(finalS)
  }
}


