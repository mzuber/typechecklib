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

import typechecklib.Types.{TypeFunction, TypeFunction1, TypeFunction2}

import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting.Strategy


/**
  * Common functionality for abstract syntax.
  */
object Syntax {

  /**
    * An abstract class which represents identifiers.
    */
  abstract class Ide

  /**
    * A class for simple, String-based identifiers.
    */
  case class SimpleIde(ide: String) extends Ide {
    override def toString = ide 
  }

  
  implicit def string2Ide(ide: String): SimpleIde = SimpleIde(ide)


  /**
    * A class for deferring auxiliary function calls in deduction rules.
    */
  case class MetaFun[S <: Product, T](f: Function1[S, T], args: S) { //why invariant in S?
    def apply(): T = f(args)
  }


  /**
    * A class for deferring unary auxiliary function calls in deduction rules.
    */
  case class MetaFun1[S, T](f: Function1[S, T], arg: S) {
    def apply(): T = f(arg)
  }


  /**
    * A class for deferring binary auxiliary function calls in deduction rules.
    */
  case class MetaFun2[S1, S2, T](f: Function2[S1, S2, T], args: (S1, S2)) {
    def apply(): T = f(args._1, args._2)
  }


  /**
    * A class for deferring ternary auxiliary function calls in deduction rules.
    */
  case class MetaFun3[S1, S2, S3, T](f: Function3[S1, S2, S3, T], args: (S1, S2, S3)) {
    def apply(): T = f(args._1, args._2, args._3)
  }


  /**
    * Evaluate all meta-level auxiliary functions encapsulated in the given term.
    *
    * @param term A term which might encapsulate a meta-level function call.
    * @param eval A function which evaluates meta-level functions of a specific type. Per default,
    *             this function evaluates meta-level functions evaluating to types. This hook can
    *             be used to handle user-defined data types encapsulating meta-level auxiliary functions.
    */
  def evaluateMetaFun[T](term: T, eval: PartialFunction[Any, Any] = evalTypeFunction): T = {
    val evalMetaFun: Strategy = rule(eval)

    everywhere(evalMetaFun)(term).getOrElse(term) match {
      case t: T @unchecked => t
    }
  }

  private def evalTypeFunction: PartialFunction[Any, Any] = {
    case f: TypeFunction1[_]    => f.apply
    case f: TypeFunction2[_, _] => f.apply
    case f: TypeFunction[_]     => f.apply
  }

}
