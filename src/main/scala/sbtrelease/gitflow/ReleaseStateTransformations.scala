package sbtrelease.gitflow

import sbt.Aggregation.KeyValue
import sbt.Keys._
import sbt.Package.ManifestAttributes
import sbt._
import sbt.std.Transform.DummyTaskMap
import sbtrelease._
import sbtrelease.gitflow.ReleasePlugin.autoImport.ReleaseKeys._
import sbtrelease.gitflow.ReleasePlugin.autoImport._

import scala.annotation.tailrec

object ReleaseStateTransformations {
  import Utilities._

  private def runTaskAggregated[T](taskKey: TaskKey[T], state: State): (State, Result[Seq[KeyValue[T]]]) = {
    import EvaluateTask._
    val extra = DummyTaskMap(Nil)
    val extracted = state.extract
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


  lazy val checkSnapshotDependencies: ReleaseStep = { st: State =>
    st.log.info("Checking for SNAPSHOT dependencies...")
    val thisRef = st.extract.get(thisProjectRef)
    val (newSt, result) = runTaskAggregated(releaseSnapshotDependencies in thisRef, st)
    val snapshotDeps = result match {
      case Value(value) => value.flatMap(_.value)
      case Inc(cause) => sys.error("Error checking for snapshot dependencies: " + cause)
    }
    if (snapshotDeps.nonEmpty) {
      val useDefaults = extractDefault(newSt, "n")
      st.log.warn("Snapshot dependencies detected:\n" + snapshotDeps.mkString("\n"))
      useDefaults orElse SimpleReader.readLine("Do you want to continue (y/n)? [n] ") match {
        case Yes() =>
        case _ => sys.error("Aborting release due to snapshot dependencies.")
      }
    }
    newSt
  }

  // TODO:
//  lazy val removeSnapshotDependencies: ReleaseStep = { st: State =>
//    val thisRef = st.extract.get(thisProjectRef)
//    val (newSt, result) = runTaskAggregated(releaseSnapshotDependencies in thisRef, st)
//    val snapshotDeps = result match {
//      case Value(value) => value.flatMap(_.value)
//      case Inc(cause) => sys.error("Error checking for snapshot dependencies: " + cause)
//    }
//    if (snapshotDeps.nonEmpty) {
//      val useDefaults = extractDefault(newSt, "n")
//      st.log.warn("Snapshot dependencies detected:\n" + snapshotDeps.mkString("\n"))
//      useDefaults orElse SimpleReader.readLine("Do you want to continue (y/n)? [n] ") match {
//        case Yes() =>
//        case _ => sys.error("Aborting release due to snapshot dependencies.")
//      }
//    }
//    newSt
//  }

  lazy val runClean : ReleaseStep = ReleaseStep(
    action = { st: State =>
      if (
        !st.get(skipTests).getOrElse(false) ||
        !st.get(skipPublish).getOrElse(false)
      ) {
        st.log.info("Running clean....")
        val extracted = Project.extract(st)
        val ref = extracted.get(thisProjectRef)
        extracted.runAggregated(clean in Global in ref, st)
      } else {
        st.log.info("Skipping clean...")
        st
      }
    }
  )


  lazy val runTest: ReleaseStep = ReleaseStep(
    action = { st: State =>
      if (!st.get(skipTests).getOrElse(false)) {
        st.log.info("Running tests...")
        val extracted = Project.extract(st)
        val ref = extracted.get(thisProjectRef)
        extracted.runAggregated(test in Test in ref, st)
      } else {
        st.log.info("Skipping running tests...")
        st
      }
    },
    enableCrossBuild = true
  )


  val calcNextVersion : ReleaseStep = { st:State =>
    val extracted = Project.extract(st)

    val useDefs = st.get(useDefaults).getOrElse(false)
    val currentV = extracted.get(version)

    val nextFunc = extracted.get(releaseCalcNextVersion)
    val suggestedNextV = nextFunc(currentV)
    val nextV = readVersion(suggestedNextV, "Next SNAPSHOT version [%s] : ", useDefs)
    
    st.put(nextVersion, nextV)
  }

  def setVersion(newVersion: State => String) : ReleaseStep = { st:State =>
    val nv = newVersion(st)
    st.log.info(s"Setting current version to $nv")
    val useGlobal = st.extract.get(releaseUseGlobalVersion)

    reapply(Seq(
      if (useGlobal) version in ThisBuild := nv
      else version := nv
    ), st)
  }

  def getNextVersion = { st:State =>
    st.get(nextVersion).getOrElse(sys.error("Aborting nextVersion is not set!"))
  }

  def updateVersionFile(newVersion: State => String) : ReleaseStep = { st: State =>
    val v = newVersion(st)
    st.log.info(s"Updating version file to $v")
    val useGlobal = st.extract.get(releaseUseGlobalVersion)
    val versionStr = (if (useGlobal) globalVersionString else versionString) format v
    val file = st.extract.get(releaseVersionFile)
    IO.write(file, versionStr + "\n")
    st
  }

  def git(st: State): Git = {
    Git.detect(st.extract.get(baseDirectory))(st.log) match {
      case None =>
        sys.error("Working directory is not a git repository")
      case Some(git) => git
    }
  }

   lazy val ensureStagingClean : ReleaseStep = { st: State =>
     st.log.info("Ensuring staging is clean... ")

     val status = (git(st).status !!).trim
    if (status.nonEmpty) {
      sys.error("Working directory is dirty.")
    }

    st.log.info("Starting release process off commit: " + git(st).currentHash)
    st
  }

  def findReleaseBranch(searchRemote: Boolean = false) = {st:State =>
    val branches = git(st).allBranches
    val result =
      if(searchRemote) {
        branches.find(_.contains("release/"))
      } else {
        branches.find(_.startsWith("release/"))
      }
    result match {
      case s@Some(releaseBranch) =>
        st.log.info(s"Found release branch $releaseBranch")
      case _ =>
    }
    result
  }

  def setReleaseBranch(branch: State => String) : ReleaseStep = { st: State =>
    val bname = branch(st)
    st.log.info(s"Setting release branch to $bname")
    st.put(gitflowReleaseBranchName, bname)
  }

  def getReleaseBranch = { st:State =>
    st.get(gitflowReleaseBranchName).getOrElse(sys.error("Release branch name not set!"))
  }

  lazy val ensureNoReleaseBranch : ReleaseStep = { st: State =>
    st.log.info("Ensuring no release branch is already present... ")
    findReleaseBranch(searchRemote = true)(st) match {
      case Some(releaseBranch) =>
        sys.error(s"Release branch already exists: $releaseBranch")
      case _ =>
    }
    st
  }

  lazy val ensureNotBehindRemote : ReleaseStep = { st: State =>
    st.log.info("Ensuring not behind remote...")
    // Note: throws exception if not tracking
    git(st).trackingRemote

    if (git(st).isBehindRemote) {
      sys.error("Upstream has unmerged commits (suggested fix: git pull)")
    }
    st
  }

  def ensureCurrentBranch(branchName: State => String) : ReleaseStep = { st:State =>
    val bname = branchName(st)
    st.log.info(s"Ensuring current branch is $bname...")
    if(git(st).currentBranch != bname) {
      sys.error(s"Must be started from branch $bname")
    } 
    st
  }
  
  def checkoutBranch(branchName: State => String) : ReleaseStep = { st: State =>
    val bname = branchName(st)
    st.log.info(s"Checking out $bname...")

    val status = git(st).checkout(bname)
    if (status.nonEmpty) {
      sys.error(s"Could not checkout $bname branch. git returned: $status")
    }
    st
  }

  def calcNonSnapshotVersion = { st:State =>
    val snapshotVersion = st.extract.get(version)
    if(snapshotVersion.endsWith("-SNAPSHOT")) {
      snapshotVersion.dropRight("-SNAPSHOT".length)
    } else {
      sys.error(s"Current snapshotVersion $version must end with '-SNAPSHOT'.")
    }
  }

  def calcReleaseBranchName = { st: State =>
    val calcReleaseBranchName = st.extract.get(calcGitflowReleaseBranchName)
    val nonSnapshotVersion = calcNonSnapshotVersion(st)
    calcReleaseBranchName(nonSnapshotVersion)
  }

  def checkoutNewBranch(branchName: State => String) : ReleaseStep = { st: State =>
    val bname = branchName(st)
    st.log.info(s"Creating branch $bname...")
    val defaultChoice = extractDefault(st, "y")
    defaultChoice orElse SimpleReader.readLine(s"Create branch $bname (y/n)? [y] ") match {
      case Yes() | Some("") =>
      case _ => sys.error("Declined release branch creation")
    }

    val vc = git(st)
    val remote = vc.trackingRemote
    val logger = errToInfoLogger(st.log)
    val result1 = vc.checkoutNewBranch(bname).!!(logger).trim
    if(result1.nonEmpty) {
      sys.error(s"Unexpected git result: $result1")
    }
    val result2 = vc.pushSetUpstream(remote).!!(logger).trim
    val expectedResult2 = s"Branch $bname set up to track remote branch $bname from $remote."
    if(result2 != expectedResult2) {
      sys.error(s"Expected $expectedResult2 but got git result: $result2")
    }
    st
  }
  
  def calcVersionChangeCommitMessage = { st:State =>
    val (_,msg) = st.extract.runTask(versionChangeCommitMessage, st)
    msg
  }
  
  def addAndCommitAll(commitMessage: State => String) : ReleaseStep = { st: State =>
    val cm = commitMessage(st)
    st.log.info(s"Adding all changes and committing with message: '$cm'")
    git(st).add(".") !! st.log
    git(st).commit(cm) ! st.log
    st
  }

  def errToInfoLogger(logger: Logger) = new ProcessLogger {
    def info(s: => String) = logger.info(s)
    def error(s: => String) = logger.info(s)
    def buffer[T](f: => T) = logger.buffer(f)
  }

  lazy val pushAllpushTags : ReleaseStep = { st:State =>
    val defaultChoice = extractDefault(st, "y")

    val vc = git(st)
    if (vc.hasUpstream) {
      defaultChoice orElse SimpleReader.readLine("Push all changes (and tags) to the remote repository (y/n)? [y] ") match {
        case Yes() | Some("") =>
          // Note: git sends output to stderr for whatever reason, redirect it to info here
          val logger = errToInfoLogger(st.log)
          git(st).pushAll.!!(logger)
          git(st).pushTags.!!(logger)
        case _ => st.log.warn("Remember to push the changes yourself!")
      }
    } else {
      st.log.info(s"Changes were NOT pushed, because no upstream branch is configured for the local branch [${git(st).currentBranch}]")
    }
    st
  }

  def deleteLocalAndRemoteBranch(getBranch: State => String) : ReleaseStep = { st:State =>
    val branch = getBranch(st)
    st.log.info(s"Deleting branch $branch...")

    val defaultChoice = extractDefault(st, "y")
    val logger = errToInfoLogger(st.log)
    val vc = git(st)
      defaultChoice orElse SimpleReader.readLine(s"Delete branch $branch (y/n)? [y] ") match {
        case Yes() | Some("") =>
          val currentBranch = vc.currentBranch
          vc.checkout(branch)
          val remote = vc.trackingRemote
          vc.checkout(currentBranch)
          vc.deleteLocalBranch(branch) !! logger
          vc.deleteRemoteBranch(remote, branch) !! logger
        case _ =>
      }
    st
  }

  lazy val tagRelease: ReleaseStep = { st: State =>
    val defaultChoice = extractDefault(st, "a")
    st.log.info("Tagging release...")

    @tailrec
    def findTag(tag: String): Option[String] = {
      if (git(st).existsTag(tag)) {
        defaultChoice orElse SimpleReader.readLine("Tag [%s] exists! Overwrite, keep or abort or enter a new tag (o/k/a)? [a] " format tag) match {
          case Some("" | "a" | "A") =>
            sys.error("Tag [%s] already exists. Aborting release!" format tag)

          case Some("k" | "K") =>
            st.log.warn("The current tag [%s] does not point to the commit for this release!" format tag)
            None

          case Some("o" | "O") =>
            st.log.warn("Overwriting a tag can cause problems if others have already seen the tag (see `%s help tag`)!" format git(st).commandName)
            Some(tag)

          case Some(newTag) =>
            findTag(newTag)

          case None =>
            sys.error("No tag entered. Aborting release!")
        }
      } else {
        Some(tag)
      }
    }

    val (tagState, tag) = st.extract.runTask(releaseTagName, st)
    val (commentState, comment) = st.extract.runTask(releaseTagComment, tagState)
    val tagToUse = findTag(tag)
    tagToUse.foreach(git(commentState).tag(_, comment, force = true) !! commentState.log)


    tagToUse map (t =>
      reapply(Seq[Setting[_]](
        packageOptions += ManifestAttributes("Vcs-Release-Tag" -> t)
      ), commentState)
    ) getOrElse commentState
  }

  lazy val publishArtifacts = ReleaseStep(
    action = runPublishArtifactsAction,
    check = st => {
      // getPublishTo fails if no publish repository is set up.
      val ex = st.extract
      val ref = ex.get(thisProjectRef)
      Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
      st
    },
    enableCrossBuild = true
  )
   lazy val runPublishArtifactsAction = { st: State =>
     if (!st.get(skipPublish).getOrElse(false)) {
       st.log.info("Publishing artifacts...")
       val extracted = st.extract
       val ref = extracted.get(thisProjectRef)
       extracted.runAggregated(releasePublishArtifactsAction in Global in ref, st)
     } else {
       st.log.info("Skipping publishing artifacts...")
       st
     }
  }

  def mergeBranch(branch: State => String, flags: Seq[String] = Nil) : ReleaseStep = { st:State =>
    val bname = branch(st)
    val _git = git(st)
    st.log.info(s"Merging branch $bname to ${_git.currentBranch}" )
    _git.merge(bname,flags) !! st.log
    st
  }

  def readVersion(ver: String, prompt: String, useDef: Boolean): String = {
    if (useDef) ver
    else SimpleReader.readLine(prompt format ver) match {
      case Some("") => ver
      case Some(input) => Version(input).map(_.string).getOrElse(versionFormatError)
      case None => sys.error("No version provided!")
    }
  }

  def reapply(settings: Seq[Setting[_]], state: State): State = {
    val extracted = state.extract
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


