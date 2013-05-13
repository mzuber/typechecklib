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
  * Linear cosntraint solver.
  * 
  * This solver tries to solve constraints in the given order.
  */
trait LinearConstraintSolver extends ConstraintSolver {

  /**
    *  Solve the generated constraints and return the possibly arisen
    *  substitution.
    */
  def solveConstraints(constraints: List[Constraint]): Either[Error, Substitution] = {
    var σ = new Substitution
    var unsolvedConstraints = constraints

    while (! unsolvedConstraints.isEmpty) {
      val constraint = unsolvedConstraints.head

      if (! constraint.isSolveable) {
	return Left(UnsolvableConstraintError(constraint))
      }
      else {
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
