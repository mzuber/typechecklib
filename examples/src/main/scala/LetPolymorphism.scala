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
import typechecklib.Syntax.{Ide, MetaFun1}
import typechecklib.Rules._
import typechecklib.Types._
import typechecklib.Constraints._
import typechecklib.Substitutions._
import typechecklib.Unification.unify


/**
  * An example type checker for a simply typed lambda calculus
  * extended with some base types and polymorphic let-bindings.
  */
object LetPolymorphism {
  import SimpleLambda.{Term, Var, Abs, App}

  /* Abstract Syntax */
  case class Const(n: Int) extends Term
  case class Let(x : Var, e1 : Term, e2 : Term) extends Term


  /* Parent type for all type rules */
  sealed trait PolyLetRules


  /*
   * Typing rule for integer values:
   *
   *     T = int
   *   ----------- (Int)
   *    Γ ⊢ n : T
   */
  case class ConstRule(ctx: Context, n: Const, t: Type) extends Axiom with PolyLetRules {
    ctx ⊢ n <:> t | t =:= BaseType("int")
    
    override val name = "const"
  }


  /*
   * Typing rule for polymorphic variable lookup:
   * 
   *        Γ(x) >= T
   *    ----------------- (Var)
   *        Γ ⊢ x : T
   */
  case class PolyVarRule(ctx: Context, x: Var, t: Type) extends Axiom with PolyLetRules {
    ctx ⊢ x <:> t | ctx(x) >= t

    override val name = "PolyVar"
  }


  /*
   * Typing rule for lambda abstraction:
   * 
   *      Γ,x:T1 ⊢ e : T2       T = T1 -> T2
   *     ------------------------------------ (Abs)
   *                Γ ⊢ λ x.e : T
   */
  case class AbsRule(ctx: Context, abs: Abs, t: Type) extends Rule with PolyLetRules {
    val Abs(x, e) = abs

    val t1 = TypeVariable()
    val t2 = TypeVariable()
    val newCtx = ctx + (x -> t1)

    newCtx ⊢ e <:> t2 ==> 
    ctx ⊢ abs <:> t | (t =:= t1 --> t2)
  
    override  val name = "Abs"
  }


  /*
   * Typing rule for application.
   *    Γ ⊢ f : T1     Γ ⊢ e : T2     T1 = T2 -> T
   *   -------------------------------------------- (App)
   *                  Γ ⊢ (f) e : T
   */
  case class AppRule(ctx: Context, app: App, t: Type) extends Rule with PolyLetRules {
    val App(f, e) = app

    val t1 = TypeVariable()
    val t2 = TypeVariable()

    List(ctx ⊢ f <:> t1, ctx ⊢ e <:> t2 ) ==> 
    ctx ⊢ app <:> t | (t1 =:= t2 --> t)

    override val name = "App"
  }


  /*
   * Typing rule for polymorphic lets:
   * 
   *     Γ ⊢ e1 : T1     T2 = gen(Γ,T1)   Γ,x:T2 ⊢ e2 : T3   T = T3
   *    ------------------------------------------------------------- (Let)
   *                      Γ ⊢ let x = e1 in e2 : T
   */
  case class LetRule(ctx: Context, localDef: Let, t: Type) extends Rule with PolyLetRules {
    val Let(x, e1, e2) = localDef

    val t1 = TypeVariable()
    val t2 = TypeVariable()
    val t3 = TypeVariable()
    val bodyCtx = ctx + (x -> t2)

    List(ctx ⊢ e1 <:> t1, bodyCtx ⊢ e2 <:> t3 ) ==>
    ctx ⊢ localDef <:> t | (t2 =:= gen(ctx, t1), t =:= t3)

    /* Lift the 'generalize' function to the meta level */
    def gen(ctx: Context, ty: Type): Type = TypeFunction1(MetaFun1((t: Type) => t.generalize(ctx), ty))

    override val name = "polylet"
  }


  /**
    * Check if a given type is a generic instance of a type scheme.
    */
  case class GenericInstanceConstraint(s: Type, t: Type) extends Constraint {

    def solve: Option[Substitution] = solve(s, t)

    def solve(s: Type, t: Type): Option[Substitution] = (s, t) match {
      /*
       * Check if the second type scheme is a generic instance of the first one.
       */
      case (s: TypeScheme, t: TypeScheme) => {
	val (unifiable, σ) = unify(s.ty, t.ty)
	val validSubstitutionDomain = σ.keySet subsetOf s.tvars.toSet
	val noNameClash = (s.ty.freeVars intersect t.ty.boundVars).isEmpty

	if (unifiable && validSubstitutionDomain && noNameClash)
	  Some(new Substitution())
	else
	  None
      }

      /*
       * Instantiate the type scheme and unify it with the given type.
       */
      case (s: TypeScheme, t: Type) => {
	val σ: Substitution = s.tvars zip freshTVars(s.tvars.length)
	solve(σ[Type](s.ty), t)
      }

      /*
       * Instantiate the type scheme and unify it with the given type.
       */
      case (s: Type, t: TypeScheme) => solve(t, s)

      /*
       * Unify both types.
       */
      case (s: Type, t: Type) => unify(s, t) match {
	case (true, σ)  => Some(σ)
	case (false, _) => None
      }
    }
  }


  /**
    * Syntactic shugar for the generic instance constraint.
    */
  implicit class GenericInstanceConstraintBuilder(ty: Type) {
    def >=(that: Type) = GenericInstanceConstraint(ty, that)
  }



  /**
    * User-defined flattening strategy.
    */
  trait PolyLetTraversal extends TreeTraversal {

    /**
      * Traverse a derivation tree and collect all constraints along the way.
      *
      * When traversing let-binding node in the derivation tree we need to make sure
      * that the generalization of the local definition's type is performed before it
      * is used the first time. That is, the constraint  T2 = gen(Γ,T1)  has to be
      * collected before the constraints generated by the premise for the body of the
      * let-binding. All other constraints can be collected in a regular depth-first
      * fashion.
      */
    def flatten(tree: ConstraintTree): List[AnnotatedConstraint] = tree match {
      /* Collect generalization constraint (c1) before the constraints generated by premise for the body (p2) */
      case ConstraintTree(rule: LetRule, List(p1, p2)) => {
	val List(c1, c2) = rule.constraints
	(flatten(p1) :+ c1) ++ (flatten(p2) :+ c2)
      }
      /* Otherwise: Regular depth-first traversal */
      case ConstraintTree(rule: Rule, children) => children.flatMap(flatten) ++ rule.constraints
    }
  }



  /**
    * The type checker.
    */
  object PolyLetTypeChecker extends TypeChecker with ReflectionBasedConstraintGeneration with PolyLetTraversal with LinearConstraintSolver {
    import scala.reflect.runtime.universe.typeOf

    val rules = List(typeOf[ConstRule], typeOf[PolyVarRule], typeOf[AbsRule], typeOf[AppRule], typeOf[LetRule])

    /**
      * Some built-in functions on integers and booleans.
      */
    val builtIns = {
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
  }
}
