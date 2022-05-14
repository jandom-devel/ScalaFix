# ScalaFix
A Scala library for solving fixpoint equations.

It implements several equation solvers, from simple Kleene's iteration to complex methods involving hierarchical ordering.

Scalafix supports both finite and infinite sets of equations and many techniques for accelerate, approximate or ensure the convergence of the iteration method to a fixpoint, such as widening, narrowing, warrowing, localized widening.

ScalaFix can be used as a backend for a static analyzer and a general-purpose library for finding fixpoints in equation systems.

A set of examples and benchmarks is implemented in ScalaFixExamples
https://github.com/jandom-devel/ScalaFixExamples. 

The compiled code is available on the Sonatype OSSRH (OSS Repository Hosting) https://oss.sonatype.org/ with group it.unich.scalafix and artifact scalafix.
