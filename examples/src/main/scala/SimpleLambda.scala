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

package examples

import typechecklib._
import typechecklib.Syntax.Ide
import typechecklib.Rules._
import typechecklib.Types._
import typechecklib.Constraints._


/**
  * An example type checker for a simply typed lambda calculus.
  */
object SimpleLambda {

  /* Abstract Syntax */
  abstract class Term
  case class Var(ide: Ide) extends Term
  case class Abs(x: Var, e: Term) extends Term
  case class App(f: Term, e:Term) extends Term


  /* Parent type for all type rules */
  sealed trait SimpleLambdaRules


  /*
   * Typing rule for variable lookup:
   * 
   *         T = Γ(x)
   *       ----------- (Var)
   *        Γ ⊢ x : T
   */
  case class VarRule(ctx: Context, x: Var, t: Type) extends Axiom with SimpleLambdaRules {

    ctx ⊢ x <:> t | t =:= ctx(x)

    override val name = "Var"
  }


  /*
   * Typing rule for lambda abstraction:
   * 
   *      Γ,x:T1 ⊢ e : T2       T = T1 -> T2
   *     ------------------------------------ (Abs)
   *                Γ ⊢ λ x.e : T
   */
  case class AbsRule(ctx: Context, abs: Abs, t: Type) extends Rule with SimpleLambdaRules {
    val t1 = TypeVariable()
    val t2 = TypeVariable()
    val newCtx = ctx + (abs.x -> t1)

    newCtx ⊢ abs.e <:> t2 ==> 
    ctx ⊢ abs <:> t | (t =:= t1 --> t2)
  
    override  val name = "Abs"
  }


  /*
   * Typing rule for application.
   * 
   *    Γ ⊢ f : T1     Γ ⊢ e : T2     T1 = T2 -> T
   *   -------------------------------------------- (App)
   *                  Γ ⊢ (f) e : T
   */
  case class AppRule(ctx: Context, app: App, t: Type) extends Rule with SimpleLambdaRules {
    val t1 = TypeVariable()
    val t2 = TypeVariable()

    List(ctx ⊢ app.f <:> t1, ctx ⊢ app.e <:> t2 ) ==> 
    ctx ⊢ app <:> t | (t1 =:= t2 --> t)

    override val name = "App"
  }


  /*
   * The type checker.
   */
  object LambdaTypeChecker extends TypeChecker with ReflectionBasedConstraintGeneration with DepthFirstPreOrder with LinearConstraintSolver {
    import scala.reflect.runtime.universe.typeOf

    val rules = List(typeOf[VarRule], typeOf[AbsRule], typeOf[AppRule])
  }
}
