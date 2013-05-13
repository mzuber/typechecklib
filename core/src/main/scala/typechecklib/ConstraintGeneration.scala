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

import typechecklib.Rules._
import typechecklib.Errors._


/**
  * The constraint generation trait.
  */
trait ConstraintGeneration {

  /**
    * Build the derivation tree for a given judgement based on the given type rules.
    */
  def typeDerivation(judgement: Judgement): Either[Error, ConstraintTree] = {

    instantiateRule(judgement) match {
      case None       => Left(NoMatchingRuleError(judgement))
      case Some(rule) => {
	/* Generate constraints for remaining premises and build constraint tree. */
	for (children <- errorMap(rule.premises, (j: Judgement) => typeDerivation(j)).right)
	yield ConstraintTree(rule, children)
      }
    }
  }


  /**
    * Instantiate the corresponding type rule for the given judgement.
    */
  def instantiateRule(judgement: Judgement): Option[Rule]
}


/**
  * A class representing type deductions.
  *
  * A type deduction is a tree of rule instances.
  */
case class ConstraintTree(rule: Rule, children: List[ConstraintTree])
