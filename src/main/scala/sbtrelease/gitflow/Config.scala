package sbtrelease.gitflow

import java.io.File

import sbt.{Extracted, Logger}
import sbt.Keys.{name, version}
import sbtrelease.Version
import ReleasePlugin.autoImport._

case class Config(
  log: Logger,
  flags: Flags,
  useGlobal: Boolean,
  developBranch: String,
  masterBranch: String,
  calcReleaseBranch: Version => String,
  baseDirectory: File,
  currentVersion: Version,
  calcNextSnapshotVersion: Version => Version,
  calcVersionChangeCommitMessage: Version => String,
  calcTag: Version => (String,String),
  versionFile: File,
  artifactName: String
)

object Config {
  def apply(e: Extracted, flags: Flags, log: Logger) : Config = {
    val useGlobal = e.get(releaseUseGlobalVersion)
    val developBranch = e.get(gitflowDevelopBranchName)
    val masterBranch = e.get(gitflowMasterBranchName)
    val currentVersion = Version(e.get(version))
    val calcReleaseBranch = e.get(calcGitflowReleaseBranchName)
    val calcNextSnapshotVersion = e.get(releaseCalcNextSnapshotVersion)
    val calcVersionChangeCommitMessage = e.get(releaseCalcVersionChangeCommitMessage)
    val calcTag = e.get(releaseCalcTag)
    val versionFile = e.get(releaseVersionFile)
    val artifactName = e.get(name)

    Config(
      log = log,
      flags = flags,
      useGlobal = useGlobal,
      developBranch = developBranch,
      masterBranch = masterBranch,
      calcReleaseBranch = calcReleaseBranch,
      baseDirectory = new File("."),
      currentVersion = currentVersion,
      calcNextSnapshotVersion = calcNextSnapshotVersion,
      calcVersionChangeCommitMessage = calcVersionChangeCommitMessage,
      calcTag = calcTag,
      versionFile = versionFile,
      artifactName = artifactName
    )
  }
}
