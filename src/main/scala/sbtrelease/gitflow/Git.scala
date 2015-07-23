package sbtrelease.gitflow

import java.io.File

import sbt._

class Git(
  val baseDir: File,
  isDryRun: Boolean
)(implicit logger:Logger) extends {
  val commandName = "git"

  protected def executableName(command: String) = {
    val maybeOsName = sys.props.get("os.name").map(_.toLowerCase)
    val maybeIsWindows = maybeOsName.filter(_.contains("windows"))
    maybeIsWindows.map(_ => command+".exe").getOrElse(command)
  }

  protected val devnull = new ProcessLogger {
    def info(s: => String) {}
    def error(s: => String) {}
    def buffer[T](f: => T): T = f
  }

  private lazy val exec = executableName(commandName)

  def query(args: Any*) : ProcessBuilder =
    Process(exec +: args.map(_.toString),baseDir)

  def mutate(args: Any*) : ProcessBuilder = {
    val cmds = exec +: args.map(_.toString)
    if(isDryRun) {
      Process("echo " +: cmds, baseDir)
    } else {
      logger.info(s"-- ${cmds.mkString(" ")}")
      Process(cmds, baseDir)
    }
  }

  def add(files: String*) : ProcessBuilder =
    mutate(("add" +: files): _*)

  def commit(message: String) : ProcessBuilder =
    mutate("commit", "-m", message)

  private lazy val trackingBranchCmd : ProcessBuilder =
    query("config", "branch.%s.merge" format currentBranch)

  private def trackingBranch: String =
    (trackingBranchCmd !!).trim.stripPrefix("refs/heads/")

  private lazy val trackingRemoteCmd: ProcessBuilder =
    query("config", "branch.%s.remote" format currentBranch)

  def trackingRemote: String = (trackingRemoteCmd !!) trim

  def hasUpstream : Boolean =
    trackingRemoteCmd ! devnull == 0 && trackingBranchCmd ! devnull == 0

  def currentBranch : String =
    (query("symbolic-ref", "HEAD") !!).trim.stripPrefix("refs/heads/")

  def currentHash : String =
    revParse("HEAD")

  def checkout(branch: String) : String =
    (mutate("checkout","-q",branch) !!).trim

  private def revParse(name: String) : String =
    (query("rev-parse", name) !!) trim

  def isBehindRemote : Boolean =
    (query(
      "rev-list",
      "%s..%s/%s".format(currentBranch, trackingRemote, trackingBranch)
    ) !! devnull).trim.nonEmpty

  def tag(name: String, comment: String, force: Boolean = false) : ProcessBuilder =
    mutate("tag", "-a", name, "-m", comment, if(force) "-f" else "")

  def existsTag(name: String) : Boolean =
    query("show-ref", "--quiet", "--tags", "--verify", "refs/tags/" + name) ! devnull == 0

  def checkRemote(remote: String) : ProcessBuilder =
    fetch(remote)

  def fetch(remote: String) : ProcessBuilder =
    mutate("fetch", remote)

  def status : ProcessBuilder = query("status", "--porcelain")

  def pushAll : ProcessBuilder = mutate("push","--all", trackingRemote)

  def pushTags : ProcessBuilder = mutate("push","--tags")

  def allBranches : List[String] =
    query("branch","-a").lines.map(_.substring(2)).toList

  def checkoutNewBranch(branch: String) : ProcessBuilder =
    mutate("checkout","-q","-b",branch)

  def pushSetUpstream(remote: String) : ProcessBuilder =
    mutate("push","-q","--set-upstream",remote,currentBranch)

  def merge(branch: String, flags: Seq[String] = Nil) : ProcessBuilder = {
    val args = Seq("merge","-q") ++ flags :+ branch
    mutate(args:_*)
  }

  def deleteLocalBranch(branch: String) : ProcessBuilder =
    mutate("branch","-d",branch)

  def deleteRemoteBranch(remote: String, branch: String) : ProcessBuilder =
    mutate("push","--delete",remote,branch)
}

object Git {
  def detect(dir: File, isDryRun: Boolean)(implicit logger: Logger) : Option[Git] = {
    Git.isRepository(dir).map(dir => new Git(dir,isDryRun))
  }

  def isRepository(dir: File): Option[File] =
    if (new File(dir, markerDirectory).isDirectory) Some(dir)
    else Option(dir.getParentFile).flatMap(isRepository)

  protected val markerDirectory = ".git"
}

