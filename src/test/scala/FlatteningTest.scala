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
import typechecklib.ConstraintGeneration._
import typechecklib.Syntax._
import typechecklib.Constraints._

import ExampleRules._


/**
  * Test suite for the constraint tree traversal module.
  */
class FlatteningTest extends FunSuite with BeforeAndAfter {

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
   * The flattening strategies used in the test cases.
   */
  object DepthFirstPreOrderTraversal extends DepthFirstPreOrder
  object DepthFirstPostOrderTraversal extends DepthFirstPostOrder
  object BreadthFirstTraversal extends BreadthFirst
  object BottomUpTraversal extends BottomUp
  object TopDownTraversal extends TopDown
  

  test("Pre-order depth-first traversal of type derivation for constant: 42") {

    val constraintList = List(TypeVariable("$0") =:= BaseType("int"))

    val judgement = Judgement(context, Const(42), TypeVariable())

    assert(DepthFirstPreOrderTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }
  
  test("Post-order depth-first traversal of type derivation for constant: 42") {

    val constraintList = List(TypeVariable("$0") =:= BaseType("int"))

    val judgement = Judgement(context, Const(42), TypeVariable())

    assert(DepthFirstPostOrderTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Breadth-first traversal of type derivation for constant: 42") {

    val constraintList = List(TypeVariable("$0") =:= BaseType("int"))

    val judgement = Judgement(context, Const(42), TypeVariable())

    assert(BreadthFirstTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Bottom-up traversal of type derivation for constant: 42") {

    val constraintList = List(TypeVariable("$0") =:= BaseType("int"))

    val judgement = Judgement(context, Const(42), TypeVariable())

    assert(BottomUpTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Top-down traversal of type derivation for constant: 42") {

    val constraintList = List(TypeVariable("$0") =:= BaseType("int"))

    val judgement = Judgement(context, Const(42), TypeVariable())

    assert(TopDownTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }


  test("Pre-order depth-first traversal of type derivation for application: 3 + 5") {

    val constraintList = List(TypeVariable("$3") =:= context(Var("+")),
			      TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$1"),
			      TypeVariable("$2") =:= BaseType("int"),
			      TypeVariable("$1") =:= TypeVariable("$2") --> TypeVariable("$0"))

    val judgement = Judgement(context, App(App(Var("+"), Const(3)), Const(5)), TypeVariable())

    assert(DepthFirstPreOrderTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Post-order depth-first traversal of type derivation for application: 3 + 5") {

    val constraintList = List(TypeVariable("$2") =:= BaseType("int"),
			      TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$3") =:= context(Var("+")),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$1"),
			      TypeVariable("$1") =:= TypeVariable("$2") --> TypeVariable("$0"))

    val judgement = Judgement(context, App(App(Var("+"), Const(3)), Const(5)), TypeVariable())

    assert(DepthFirstPostOrderTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Breadth-first traversal of type derivation for application: 3 + 5") {

    val constraintList = List(TypeVariable("$1") =:= TypeVariable("$2") --> TypeVariable("$0"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$1"),
			      TypeVariable("$2") =:= BaseType("int"),
			      TypeVariable("$3") =:= context(Var("+")),
			      TypeVariable("$4") =:= BaseType("int"))

    val judgement = Judgement(context, App(App(Var("+"), Const(3)), Const(5)), TypeVariable())

    assert(BreadthFirstTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Bottom-up traversal of type derivation for application: 3 + 5") {

    val constraintList = List(TypeVariable("$3") =:= context(Var("+")),
			      TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$1"),
			      TypeVariable("$2") =:= BaseType("int"),
			      TypeVariable("$1") =:= TypeVariable("$2") --> TypeVariable("$0"))

    val judgement = Judgement(context, App(App(Var("+"), Const(3)), Const(5)), TypeVariable())

    assert(BottomUpTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Top-down traversal of type derivation for application: 3 + 5") {

    val constraintList = List(TypeVariable("$1") =:= TypeVariable("$2") --> TypeVariable("$0"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$1"),
			      TypeVariable("$3") =:= context(Var("+")),
			      TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$2") =:= BaseType("int"))

    val judgement = Judgement(context, App(App(Var("+"), Const(3)), Const(5)), TypeVariable())

    assert(TopDownTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }


  test("Pre-order depth-first traversal of type derivation for lambda-abstraction: \\ x. x > 1") {

    val constraintList = List(TypeVariable("$5") =:= context(Var(">")),
			      TypeVariable("$6") =:= TypeVariable("$1"),
			      TypeVariable("$5") =:= TypeVariable("$6") --> TypeVariable("$3"),
			      TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$2"),
			      TypeVariable("$0") =:= TypeVariable("$1") --> TypeVariable("$2"))

    val judgement = Judgement(context, Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))), TypeVariable())

    assert(DepthFirstPreOrderTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Post-order depth-first traversal of type derivation for lambda-abstraction: \\ x. x > 1") {

    val constraintList = List(TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$6") =:= TypeVariable("$1"),
			      TypeVariable("$5") =:= context(Var(">")),
			      TypeVariable("$5") =:= TypeVariable("$6") --> TypeVariable("$3"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$2"),
			      TypeVariable("$0") =:= TypeVariable("$1") --> TypeVariable("$2"))

    val judgement = Judgement(context, Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))), TypeVariable())

    assert(DepthFirstPostOrderTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Breadth-first traversal of type derivation for lambda-abstraction: \\ x. x > 1") {

    val constraintList = List(TypeVariable("$0") =:= TypeVariable("$1") --> TypeVariable("$2"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$2"),
			      TypeVariable("$5") =:= TypeVariable("$6") --> TypeVariable("$3"),
			      TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$5") =:= context(Var(">")),
			      TypeVariable("$6") =:= TypeVariable("$1"))

    val judgement = Judgement(context, Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))), TypeVariable())

    assert(BreadthFirstTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Bottom-up traversal of type derivation for lambda-abstraction: \\ x. x > 1") {

    val constraintList = List(TypeVariable("$5") =:= context(Var(">")),
			      TypeVariable("$6") =:= TypeVariable("$1"),
			      TypeVariable("$5") =:= TypeVariable("$6") --> TypeVariable("$3"),
			      TypeVariable("$4") =:= BaseType("int"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$2"),
			      TypeVariable("$0") =:= TypeVariable("$1") --> TypeVariable("$2"))

    val judgement = Judgement(context, Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))), TypeVariable())

    assert(BottomUpTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }

  test("Top-down traversal of type derivation for lambda-abstraction: \\ x. x > 1") {

    val constraintList = List(TypeVariable("$0") =:= TypeVariable("$1") --> TypeVariable("$2"),
			      TypeVariable("$3") =:= TypeVariable("$4") --> TypeVariable("$2"),
			      TypeVariable("$5") =:= TypeVariable("$6") --> TypeVariable("$3"),
			      TypeVariable("$5") =:= context(Var(">")),
			      TypeVariable("$6") =:= TypeVariable("$1"),
			      TypeVariable("$4") =:= BaseType("int"))

    val judgement = Judgement(context, Abs(Var("x"), App(App(Var(">"), Var("x")), Const(1))), TypeVariable())

    assert(TopDownTraversal.flatten(typeDerivation(rules, judgement).right.get) === constraintList)
  }
  
}
