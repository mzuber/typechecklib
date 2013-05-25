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

import scala.language.implicitConversions

import typechecklib.Substitutions._
import typechecklib.Types.Type
import typechecklib.Unification.{unify}


/**
  * The module for constraints in type rules.
  */
object Constraints {

  /**
    * A trait to define error messages in [[typechecklib.Constraints.Constraint Constraints]].
    */
  trait ErrorMessage {
    self: Product =>

    /**
      * Generate the error message.
      */
    def message: String
  }

  
  /**
    * Simple, constant error message.
    */
  case class SimpleMessage(msg: String) extends ErrorMessage {
    def message = msg
  }

  implicit def string2Message(msg: String) = SimpleMessage(msg)

  /**
    * Basic error message for type missmatch.
    */
  case class TypeMissmatch(expectedType: Type, inferredType: Type) extends ErrorMessage {
    def message = "Type missmatch: Couldn't match expected type " + quote(expectedType) + " against inferred type " + quote(inferredType)
  }

  /**
    * Put the given term in `quotes'.
    */
  def quote[T](term: T): String = "`" + term + "'"


  /**
    * A trait for constraints.
    */
  trait Constraint {
    self: Product =>

    /**
      * Solve this constraint.
      */
    def solve(): Option[Substitution]

    /**
      * Check, if this constraint can be solved.
      */
    def isSolveable: Boolean = true

    /**
      * An [[typechecklib.Constraints.ErrorMessage ErrorMessage]] which
      * can be displayed in case the constraint cannot be solve.
      */
    var errorMsg: ErrorMessage = "Error: Could not solve " + this.toString

    /**
      * Attach an error message to a constraint.
      */
    def |(msg: ErrorMessage): this.type = {
      errorMsg = msg
      this
    }

    /**
      * Defines a disjunction of this and the given constraints.
      */
    def ∧(that: Constraint): Or = Or(this, that)

    /**
      * Defines a conjunction of this and the given constraints.
      */
    def ∨(that: Constraint): And = And(this, that)
  }


  /**
    * Equality constraint over type terms.
    */ 
  case class Eq[S <: Type, T <: Type](s: S, t: T) extends Constraint {

    /**
      * Equality constraints are solved via unification.
      */
    def solve() = unify(s, t) match {
      case (false, _) => None
      case (true, σ)  => Some(σ)
    }

    override def toString = s + " = " + t
  }


  /**
    * Syntactic sugar for equality constraints, e.g.
    * {{{
    * val c: Eq = s =:= t
    * }}}
    */
  implicit class EqBuilder[S <: Type](x: S) {
    def =:=[T <: Type](y: T): Eq[S,T] = Eq[S,T](x, y)
  }


  /**
    * Negated constraint
    */ 
  case class Not(c: Constraint) extends Constraint {

    /**
      * Solve the non-negated cosntraint and flip the result.
      */
    def solve() = c.solve match {
      case None => Some(new Substitution)
      case _    => None
    }
  }


  /**
    * Syntactic sugar for negated constraints. 
    */
  object ¬ {
    def apply(c: Constraint): Not = Not(c)
  }


  /**
    * A class for constraint conjunctions.
    */ 
  case class And(a: Constraint, b: Constraint) extends Constraint {

    /**
      * Solve both constraints and combine the results accordingly.
      */
    def solve(): Option[Substitution] = {
      var s1 = a.solve
      if (s1.isDefined)
	return solve(a, b);
      else
	solve(b, a)
    }

    private def solve(x: Constraint, y: Constraint): Option[Substitution] = {
      var s1 = x.solve
      if (s1.isDefined) {
        val f = s1.get
        val s2 = (f(y)).solve
        if (s2.isDefined)
          return Some(s1.get ++ s2.get)
        else None
      } else None
    }
  }


  /**
    * A class for constraint disjunctions.
    */ 
  case class Or(a: Constraint, b: Constraint) extends Constraint {

    /**
      * Solve both constraints and combine the results accordingly.
      */
    def solve(): Option[Substitution] = {
      val s1 = a.solve
      if (s1.isDefined) {
        val s2 = ((s1.get)(b)).solve
        if (s2.isDefined)
          return Some(s1.get ++ s2.get)
        else
          return s1
      }
      else
        return b.solve
    }
  }

}
