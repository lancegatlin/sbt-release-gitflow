package sbtrelease

import sbt.State

case class ReleaseStep(
  action: State => State,
  check: State => State = identity,
  enableCrossBuild: Boolean = false
)

object ReleaseStep {
  implicit def func2ReleasePart(f: State => State): ReleaseStep = ReleaseStep(f)

  implicit def releasePart2Func(rp: ReleaseStep): State=>State = rp.action
}


