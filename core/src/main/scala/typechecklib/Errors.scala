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

import typechecklib.Rules.Judgement
import typechecklib.Constraints.Constraint
import typechecklib.Types.Type


object Errors {

  /**
    * A class for errors.
    */
  abstract class Error

  /**
    * A class for errors occuring during the constraint generation phase.
    */
  abstract class ConstraintGenerationError extends Error

  /**
    * No matching rule has been found for an expression when building the type derivation tree.
    */
  case class NoMatchingRuleError(judgement: Judgement) extends ConstraintGenerationError

  /**
    * A class for errors occuring during the constraint solving phase.
    */
  abstract class ConstraintSolvingError extends Error

  /**
    * A constraint is not yet in a state where it can be solved.
    */
  case class UnsolvableConstraintError(constraint: Constraint) extends ConstraintSolvingError

  /**
    * Finding a solution for a constraint fails.
    */
  case class NoSolutionError(constraint: Constraint) extends ConstraintSolvingError

  /**
    * An error during type checking, i.e. the expected type does not match the inferred one.
    */
  case class TypeCheckError(expectedType: Type, inferredType: Type) extends Error


  /**
    * Apply a function which might result in an Error to all elements of a list
    * and combine the results accordingly.
    */
  def errorMap[S,T](ts: List[T], f: T => Either[Error, S]): Either[Error, List[S]] = {

    def combine[T](e1: Either[Error, T], e2: Either[Error, List[T]]) = e1 match {
      case Left(error) => Left(error)
      case Right(s) => e2 match {
	case Right(t)    => Right(s :: t)
	case Left(error) => Left(error)
      }
    }

    val z: Either[Error, List[S]] = Right(Nil)
    (ts.map(f) :\ z)(combine)
  }

}
