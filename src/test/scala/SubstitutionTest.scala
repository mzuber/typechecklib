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

import org.scalatest.FunSuite

import typechecklib.Substitutions._
import typechecklib.Syntax._
import typechecklib.Types._
import typechecklib.Constraints._


/**
  * Test suite for the substitution module.
  */
class SubstitutionTest extends FunSuite {

  /*
   * Some types and type variables.
   */
  val int = BaseType("Int")
  val bool = BaseType("Bool")
  val α = new TypeVariable("alpha")
  val β = new TypeVariable("beta")


  test("Apply substitution to type variable: {α/Int}(α)") {
    val σ = new Substitution(α -> int)
    assert(int === σ[Type](α))
  }

  test("Apply substitution to type variable: {α/Int}(β)") {
    val σ = new Substitution(α -> int)
    assert(β === σ[Type](β))
  }

  test("Apply substitution to base type: {α/Int}(Bool)") {
    val σ = new Substitution(α -> int)
    assert(bool === σ[Type](bool))
  }

  test("Apply substitution to function type: {α/Int, β/Bool}(α -> β)") {
    val σ = new Substitution(β -> bool, α -> int)
    assert(int --> bool === σ[Type](α --> β))
  }

  test("Apply substitution to function type: {α/Int, β/Bool}(Int -> Bool)") {
    val σ = new Substitution(β -> bool, α -> int)
    assert(int --> bool === σ[Type](int --> bool))
  }

  test("Apply substitution to tuple type: {α/Int, β/Bool}(α ** β)") {
    val σ = new Substitution(β -> bool, α -> int)
    assert(int ** bool === σ[Type](α ** β))
  }

  test("Apply substitution to tuple type: {α/Int, β/Bool}(Int ** Bool)") {
    val σ = new Substitution(β -> bool, α -> int)
    assert(int ** bool === σ[Type](int ** bool))
  }

  test("Apply substitution to type constructor: {α/Int, β/Bool}(Pair[α,β])") {
    val σ = new Substitution(β -> bool, α -> int)
    assert(TypeConstructor("Pair", List(int, bool)) === σ(TypeConstructor("Pair", List(α, β))))
  }

  test("Apply substitution to equality constraint: {α/Int, β/Bool}(α = β)") {
    val σ = new Substitution(β -> bool, α -> int)
    assert(int =:= bool === σ(α =:= β))
  }
}
