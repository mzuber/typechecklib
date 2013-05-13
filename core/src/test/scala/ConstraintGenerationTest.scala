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

import typechecklib._
import typechecklib.Types._
import typechecklib.Rules._

import ExampleRules._


/**
  * Test suite for the constraint generation module.
  */
class ConstraintGenerationTest extends FunSuite with BeforeAndAfter with ReflectionBasedConstraintGeneration {

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
    

  test("Constraint generation for application: 3 + 5") {
    val derivationTree = {
      ConstraintTree(AppRule(context, App(App(Var("+"), Const(3)), Const(5)), TypeVariable("$4")),
                     List(ConstraintTree(AppRule(context, App(Var("+"), Const(3)), TypeVariable("$5")),
                                         List(ConstraintTree(VarRule(context, Var("+"), TypeVariable("$7")), Nil), 
                                              ConstraintTree(ConstRule(context, Const(3), TypeVariable(("$8"))), Nil))), 
                          ConstraintTree(ConstRule(context, Const(5), TypeVariable("$6")), Nil)))
    }

    val judgement = Judgement(context, App(App(Var("+"), Const(3)), Const(5)), TypeVariable())

    assert(typeDerivation(judgement).right.get === derivationTree)
  }


  test("Constraint generation for constants: 42") {
    val derivationTree = ConstraintTree(ConstRule(context, Const(42), TypeVariable("$0") ), Nil)

    val judgement = Judgement(context, Const(42), TypeVariable())

    assert(typeDerivation(judgement).right.get === derivationTree)
  }

                                                                     
  test("Constraint generation for lambda-abstraction: \\ x. x > 1") {
    val derivationTree = {
      ConstraintTree(AbsRule(context, Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))), TypeVariable("$6")),
                     List(ConstraintTree(AppRule(context + (Var("x") -> TypeVariable("$7")), App(App(Var(">"), Var("x")), Const(1)), TypeVariable("$8")),
                                         List(ConstraintTree(AppRule(context + (Var("x") -> TypeVariable("$7")), App(Var(">"), Var("x")), TypeVariable("$9")),
                                                             List(ConstraintTree(VarRule(context + (Var("x") -> TypeVariable("$7")), Var(">"), TypeVariable(("$11"))), Nil), 
								  ConstraintTree(VarRule(context + (Var("x") -> TypeVariable("$7")), Var("x"), TypeVariable(("$12"))), Nil))), 
                                              ConstraintTree(ConstRule(context + (Var("x") -> TypeVariable("$7")), Const(1), TypeVariable(("$10"))), Nil)))))
    }

    val judgement = Judgement(context, Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))), TypeVariable())
    
    assert(typeDerivation(judgement).right.get === derivationTree)
  }

}
