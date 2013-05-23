/*
 * Copyright (c) 2012, Martin Zuber
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package typechecklib

import typechecklib.Syntax._
import typechecklib.Substitutions._
import typechecklib.Errors._
import typechecklib.Constraints.Constraint


/**
  * The trait for constraint solvers.
  */
trait ConstraintSolver {

  /**
    * Solve the given constraints.
    *
    * @return If all constraints can be solved, a substitution encapsulating the results
    *         of the solved constraints
    */
  def solveConstraints(constraints: List[Constraint]): Either[Error, Substitution]
}


/**
  * The trait for traceable constraint solvers.
  */
trait TraceableConstraintSolver extends ConstraintSolver {

  /**
    * Solve the given constraints step by step.
    *
    * @return Every intermediate result of the constraint solving process
    */
  def traceSolver(constraints: List[Constraint]): List[IntermediateResult]

  /**
    * Solve the given constraints.
    *
    * @return If all constraints can be solved, a substitution encapsulating the results
    *         of the solved constraints
    */
  override def solveConstraints(constraints: List[Constraint]): Either[Error, Substitution] = traceSolver(constraints) match {
    case Nil     => Right(new Substitution)
    case results => results.last match {
      case IntermediateResult(_, Left(error), _, _) => Left(error)
      case IntermediateResult(_, Right(_), σ, _)    => Right(σ)
    }
  }
}


/**
  * A class to store all intermediate information arising during constraint solving.
  */
case class IntermediateResult(val constraint: Constraint, val result: Either[Error, Substitution], val substitution: Substitution, remainingConstraints: List[Constraint])


/**
  * Linear constraint solver.
  *
  * This solver tries to solve all constraints in the given order.
  */
trait LinearConstraintSolver extends ConstraintSolver {

  /**
    *  Solve the generated constraints and return the possibly arisen substitution.
    */
  def solveConstraints(constraints: List[Constraint]): Either[Error, Substitution] = {
    var σ = new Substitution
    var unsolvedConstraints = constraints

    while (!unsolvedConstraints.isEmpty) {
      val constraint = unsolvedConstraints.head

      if (!constraint.isSolveable) {
        return Left(UnsolvableConstraintError(constraint))
      } else {
        /* Evaluate all meta-level type functions in this constraint */
        val evaluatedConstraint = evaluateMetaFun(constraint)
        evaluatedConstraint.solve match {
          case Some(φ) => {
            σ = φ ++ σ
            unsolvedConstraints = σ(unsolvedConstraints.tail)
          }
          case None => return Left(NoSolutionError(evaluatedConstraint))
        }
      }
    }

    Right(σ)
  }
}


/**
  * Traceable linear constraint solver.
  *
  * This solver tries to solve all constraints in the given order.
  */
trait TraceableLinearConstraintSolver extends LinearConstraintSolver with TraceableConstraintSolver {

  /**
    * Solve the given constraints step by step.
    *
    * @return Every intermediate result of the constraint solving process
    */
  def traceSolver(constraints: List[Constraint]): List[IntermediateResult] = {
    var σ = new Substitution
    var unsolvedConstraints = constraints
    var intermediateResults: List[IntermediateResult] = Nil

    while (!unsolvedConstraints.isEmpty) {
      val constraint = unsolvedConstraints.head

      if (!constraint.isSolveable) {
	val errorResult = IntermediateResult(constraint, Left(UnsolvableConstraintError(constraint)), σ, unsolvedConstraints.tail)
        return intermediateResults :+ errorResult
      } else {
        /* Evaluate all meta-level type functions in this constraint */
        val evaluatedConstraint = evaluateMetaFun(constraint)
        evaluatedConstraint.solve match {
          case Some(φ) => {
            σ = φ ++ σ
            unsolvedConstraints = σ(unsolvedConstraints.tail)
	    intermediateResults = intermediateResults :+ IntermediateResult(evaluatedConstraint, Right(φ), σ, unsolvedConstraints)
          }
          case None => {
	    val errorResult = IntermediateResult(evaluatedConstraint, Left(NoSolutionError(evaluatedConstraint)), σ, unsolvedConstraints.tail)
	    return intermediateResults :+ errorResult
	  }
        }
      }
    }

    intermediateResults
  }
}
