package sbtrelease

import sbt.State

// State monad
trait Step[A] extends (State => (State,A)) { self =>
  def apply(s: State) : (State, A)
  def map[B](f: A => B) : Step[B] = 
    new Step[B] {
      def apply(s: State) : (State, B) = {
        val (s1,a) = self.apply(s)
        (s1,f(a))
      }
    }
  def flatMap[B](f: A => Step[B]) : Step[B] = 
    new Step[B] {
      def apply(s: State) = {
        val (s1,a) = self.apply(s)
        f(a).apply(s1)
      }
    }
}

object Step {
  def unit(f: State => State) : Step[Unit] = new Step[Unit] {
    def apply(s: State) = (f(s),())
  }
  def sideEffect(f: State => Unit) : Step[Unit] = new Step[Unit] {
    def apply(s: State) = (s,f(s))
  }
  def apply[A](f: State => (State,A)) : Step[A] = new Step[A] {
    def apply(s: State) = f(s)
  }
}

