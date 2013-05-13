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

import typechecklib.Types.{Type}
import typechecklib.Constraints.{Constraint}

import org.kiama.rewriting.Rewriter._


/**
  * This module provides the library's facilities to define constraint-based type rules.
  */
object Rules {

  /**
    * A class for type rules.
    */
  abstract class Rule {

    /**
      * The premises of this rule.
      */
    var premises: List[Judgement] = null

    /**
      * The rule's conclusion.
      */
    var conclusion: Judgement = null

    /**
      * The constraints of this rule. 
      */
    var constraints: List[Constraint] = Nil

    /**
      * The name of this rule.
      */
    val name: String = "Rule"


    /**
      * Syntactic sugar for defining inference rules.
      * 
      * This class provides methods to define a type rule as following, where
      * ''p,,i,,'' are the rule's premises, ''c'' its conclusion, and ''c,,i,,''
      * the constraints which must be valid for the rule to hold:
      * {{{
      * List(p_1, ... , p_n) ==> c | List(c_1, ... , c_n)
      * }}}
      */
    protected case class RuleBuilder(ps: List[Judgement]) {

      def ==>(c: Judgement) = {
	premises = ps
	conclusion = c
	this
      }

      def |(cs: Constraint*) {
	constraints = cs.toList
      }
    }

    /**
      * Converts a Judgement to a RuleBuilder.
      *
      * This implicit conversion allows us to use a single judgement as the premises of
      * a rule when using the convenience functions defined in the RuleBuilder class.
      */
    protected implicit def judgement2RuleBuilder(judgement: Judgement): RuleBuilder = RuleBuilder(List(judgement))

    /**
      * Converts a Product of Judgements to a RuleBuilder.
      *
      * This implicit conversion allows us to use any Product type to define a
      * collection of premises for a rule, e.g. Tuples, Sequences, and Lists.
      */
    protected implicit def judgements2RuleBuilder(judgements: Product): RuleBuilder = {
      RuleBuilder(collectl{ case j: Judgement => j }(judgements))
    }
  }


  /**
    * A class for axioms, i.e., rules with no premises.
    */
  abstract class Axiom extends Rule {

    premises = Nil

    /**
      * Syntactic sugar for defining axioms.
      *
      * This class allows the user to define an axiom as following, where
      * ''c'' is the rule's conclusion, and ''c,,i,,'' the constraints which
      * must be valid for the rule to hold:
      * {{{
      * c | List(c_1, ... , c_n)
      * }}}
      */
    protected implicit class AxiomBuilder(c: Judgement) {
      
      def |(cs: Constraint*) {
	conclusion = c
	constraints = cs.toList
      }
    }
  }


  /**
    * A class for type judgements.
    */
  case class Judgement(ctx: Context, expr: Any, ty: Type) {
    override def toString = ctx + " ⊢ " +  expr + " <:> " + ty
  }


  /**
    * Syntactic sugar for defining type judgements.
    *
    * This class provides methods to define a [[typechecklib.Rules.Judgement Judgement]]
    * as following, where ''ctx'' is the judgement's context, ''e'' its expressions, and ''t'' its type:
    * {{{
    * ctx |- e <:> t
    * }}}
    */
  implicit class JudgementBuilder(c: Context) {

    var exp: Option[Any] = None

    /**
      * Turnstile operator.
      */
    def ⊢(exp: Any): JudgementBuilder = {
      this.exp = Some(exp)
      this
    }

    /**
      * Turnstile operator (ASCII).
      */
    def |-(exp: Any): JudgementBuilder = ⊢(exp)

    /**
      * Type annotation.
      */
    def <:>(t: Type): Judgement = Judgement(c, exp.get, t)
  }
}
