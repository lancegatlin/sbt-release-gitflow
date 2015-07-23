package sbtrelease.gitflow

import java.io.File

import sbt._

class Git(
  val baseDir: File,
  isDryRun: Boolean
)(implicit logger:Logger) extends {
  val errToInfoLogger = Git.errToInfoLogger(logger)
  implicit val plog : ProcessLogger = logger

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

  private def query(args: Any*)(implicit _logger: ProcessLogger) : String =
    {
      val cmds = exec +: args.map(_.toString)
//      logger.info(s"-- ${cmds.map { c =>
//        if(c.exists(_.isWhitespace)) {
//          s"'$c'"
//        } else {
//          c
//        }
//      }.mkString(" ")}")

      val p = Process(cmds,baseDir)
      val buffer = new StringBuffer
      val pr = p.run(BasicIO(buffer, Some(_logger), false))
      pr.exitValue()
      // Ignore non zero exit values for queries
//      if (code == 0) buffer.toString else sys.error("Nonzero exit value: " + code)
      pr.destroy()
      buffer.toString
    }

  private def mutate(args: Any*)(implicit _logger: ProcessLogger) : String = {
    val cmds = exec +: args.map(_.toString)
    logger.info(s"-- ${cmds.map { c =>
      if(c.exists(_.isWhitespace)) {
        s"'$c'"
      } else {
        c
      }
    }.mkString(" ")}")
    if(!isDryRun) {
      Process(cmds, baseDir) !! _logger
    } else {
      ""
    }
  }

  def add(files: String*) : Unit =
    mutate(("add" +: files): _*)

  def commit(message: String) : Unit =
    mutate("commit", "-m", message)

  def trackingBranch: String =
    query("config", s"branch.$currentBranch.merge").trim.stripPrefix("refs/heads/")

  def trackingRemote(branch: String): String = {
    query("config", s"branch.$branch.remote").trim
  }
  def trackingRemote: String =
    trackingRemote(currentBranch)


  def hasUpstream : Boolean =
    trackingRemote.nonEmpty && trackingBranch.nonEmpty

  def currentBranch : String =
    query("symbolic-ref", "HEAD").trim.stripPrefix("refs/heads/")

  def currentHash : String =
    revParse("HEAD")

  def checkout(branch: String) : Boolean =
    mutate("checkout","-q",branch).isEmpty

  private def revParse(name: String) : String =
    query("rev-parse", name).trim

  def isBehindRemote : Boolean =
    query(
      "rev-list",
      s"$currentBranch..$trackingRemote/$trackingBranch"
    ).trim.nonEmpty

  def tag(name: String, comment: String, force: Boolean = false) : Unit =
    mutate("tag", "-a", name, "-m", comment, if(force) "-f" else "")

  def tagExists(name: String) : Boolean =
    query("show-ref", "--tags", "--verify", "refs/tags/" + name)(devnull).nonEmpty

//  def checkRemote(remote: String) : ProcessBuilder =
//    fetch(remote)

//  def fetch(remote: String) : ProcessBuilder =
//    mutate("fetch", remote)

  def status : String = query("status", "--porcelain")

  def pushAll : Unit =
    mutate("push","--all", trackingRemote)(errToInfoLogger)

  def pushTags : Unit =
    mutate("push","--tags")(errToInfoLogger)

  def allBranches : List[String] =
    query("branch","-a").split('\n').map(_.substring(2)).toList

  def checkoutNewBranch(branch: String) : Unit =
    mutate("checkout","-q","-b",branch).trim

  def pushSetUpstream(remote: String) : Unit = {
    val bname = currentBranch
    mutate("push","-q","--set-upstream",remote,bname)
  }

  def merge(branch: String, flags: Seq[String] = Nil) : Unit = {
    val args = Seq("merge","-q") ++ flags :+ branch
    mutate(args:_*)
  }

  def deleteLocalBranch(branch: String) : Unit =
    mutate("branch","-d",branch)

  def deleteRemoteBranch(remote: String, branch: String) : Unit =
    mutate("push","--delete",remote,branch)(errToInfoLogger)
}

object Git {
  // Note: git sometimes sends output to stderr for whatever reason, redirect it to info here
  def errToInfoLogger(logger: Logger) = new ProcessLogger {
    def info(s: => String) = logger.info(s)
    def error(s: => String) = logger.info(s)
    def buffer[T](f: => T) = logger.buffer(f)
  }

  def detect(dir: File, isDryRun: Boolean)(implicit logger: Logger) : Option[Git] = {
    Git.isRepository(dir).map(dir => new Git(dir,isDryRun))
  }

  def isRepository(dir: File): Option[File] =
    if (new File(dir, markerDirectory).isDirectory) Some(dir)
    else Option(dir.getParentFile).flatMap(isRepository)

  protected val markerDirectory = ".git"
}

