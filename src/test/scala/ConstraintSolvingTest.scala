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

import scala.reflect.runtime.universe.typeOf

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers._

import typechecklib._
import typechecklib.Types._
import typechecklib.Rules._
import typechecklib.Errors._
import typechecklib.ConstraintGeneration._
import typechecklib.Substitutions.Substitution

import ExampleRules._


/**
  * Test suite for the constraint solver module.
  */
class ConstraintSolvingTest extends FunSuite with BeforeAndAfter with ShouldMatchers {

  /*
   * Reset the name supply before running each test case.
   */
  before {
    NameSupply.reset()
  }
  
  /*
   * Type rules for test cases.
   */
  val rules = List(typeOf[AbsRule], typeOf[VarRule], typeOf[AppRule],typeOf[ConstRule])


  /*
   * Some built-in functions on integers and booleans.
   */
  val context = {
    val int  = BaseType("int")
    val bool = BaseType("bool")

    new Context( Var("+") -> (int --> (int --> int)),
	         Var("-") -> (int --> (int --> int)),
	         Var("*") -> (int --> (int --> int)),
	         Var("/") -> (int --> (int --> int)),
	         Var("<") -> (int --> (int --> bool)),
	         Var(">") -> (int --> (int --> bool)),
	         Var("=") -> (int --> (int --> bool)),
	         Var("true") -> bool,
	         Var("false") -> bool)
  }


  /*
   * Custom Matcher handling the Either values returned by the constraint solvers.
   */
  case class ConstraintSolvingMatcher(expected: Type) extends Matcher[Term] {

    object Traversal extends TopDown
    object Solver extends LinearConstraintSolver

    def apply(term: Term) = {
      val result = {
	val tv = TypeVariable()
	val judgement = Judgement(context, term, tv)

	for ( constraintTree <- typeDerivation(rules, judgement).right ;
	      σ <- Solver.solveConstraints(Traversal.flatten(constraintTree)).right )
	yield σ[Type](tv)
      }

      val failureMessageSuffix = (for (resType <- result.right) yield "'" + term + "' resulted in " + resType + " which did not equal " + expected).fold("failed: %s".format(_), _.toString)

      val negatedFailureMessageSuffix = "'" + term + "' yielded " + expected

      val equals = (for (resType <- result.right) yield resType == expected).right.toOption.getOrElse(false)

      MatchResult(equals,
		  "Solving constraints for " + failureMessageSuffix,
		  "Solving constraints for " + negatedFailureMessageSuffix,
		  "Solving constraints for " + failureMessageSuffix,
		  "Solving constraints for " + negatedFailureMessageSuffix)
    }
  }


  def haveType(ty: Type) = ConstraintSolvingMatcher(ty)


  test("Should solve constraints for constants: 42") {
    Const(42) should haveType(BaseType("int"))
  }

  test("Should solve constraints for application: 3 + 5") {
    App(App(Var("+"), Const(3)), Const(5)) should haveType(BaseType("int"))
  }
    
  test("Should solve constraints for lambda-abstraction: \\ x. x > 1") {
    Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))) should haveType(BaseType("int") --> BaseType("bool"))
  }
}
