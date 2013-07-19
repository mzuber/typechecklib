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

import scala.collection.mutable.ListBuffer

import org.kiama.rewriting.Rewriter._

import typechecklib.Types._
import typechecklib.Substitutions._


/**
  * Generic Unification of type terms and values containing types (e.g. lists of types).
  */
object Unification {

  /**
    * Convenience for DefaultUnification.unify
    */
  def unify = DefaultUnification.unify[Type] _


  /**
    * The default implementation of the unify method that works with every type.
    * 
    * Make sure to always use this to call unify to ensure the right implementation is used.
    */
  object DefaultUnification extends Unifiable {
    def unify[T](x: T, y: T): (Boolean, Substitution) = (x, y) match {
      case (x1: Unifiable, y1) => x1.unify(x1, y1)
      case (x1, y1: Unifiable) => y1.unify(x1, y1)
      case (x1, y1) => unifyDefault(x1, y1)
    }
  }


  /**
    * Unification of two values returns a Boolean (signaling success or failure) and
    * a [[typechecklib.Substitutions.Substitution substitution]].
    * 
    * Types that need special handling regarding unification can mix-in this trait to override the default behavior.
    */
  trait Unifiable {

    /**
      * Unify arbitrary objects of the same type.
      */
    def unify[T](x: T, y: T): (Boolean, Substitution)

    /**
      * Default unification for objects that don't mix-in this trait.
      *
      * Don't call directly, instead use [[typechecklib.Unification.DefaultUnification DefaultUnification.unify]].
      */
    protected def unifyDefault(x: Any, y: Any): (Boolean, Substitution) = (x, y) match {
      case (x, y) if x == y            => (true, new Substitution)
      case (x: TypeVariable, y : Type) => unifyVar(x, y)
      case (x: Type, y: TypeVariable)  => unifyVar(y, x)
      case (x, y) if x.getClass() == y.getClass() &&
                     arity(x) == arity(y) => unifyAny(x, y)
      case _ => (false, new Substitution)
    }

    /**
      * Unification of objects that have the same type by trying to unify the children.
      */
    protected def unifyAny(x: Any, y: Any): (Boolean, Substitution) = {
      val xChildren = getChildren(x)
      val yChildren = getChildren(y)

      if (xChildren.isEmpty)
        (x == y, new Substitution)
      else
	unifyLists(xChildren, yChildren)
    }

    /**
      * Unify two lists with elements of the same type by unifying their elements step by step.
      */
    protected def unifyLists[T](list1: List[T], list2: List[T]): (Boolean, Substitution) = (list1, list2) match {
      case (Nil, Nil)    => (true, new Substitution)
      case (Nil, _ :: _) => (false, new Substitution)
      case (_ :: _, Nil) => (false, new Substitution)
      case (x :: xs, y :: ys) => {
	val (b, σ) = unify(x, y)

	if (b)
	  combineUnifier((b, σ), unifyLists(σ(xs), σ(ys)))
	else 
	  (false, new Substitution)
      }
    }

    /**
      *  Unification of a TypeVariable and a Type.
      */
    protected def unifyVar(x: TypeVariable, r: Type): (Boolean, Substitution) = {
      if (x == r) {
        (true, new Substitution)
      } else if (occursIn(x, r)) {
        (false, new Substitution)
      } else {
        val sub = new Substitution
        (true, sub + (x -> r))
      }
    }

    /**
      * Occurs check for two values.
      *
      * @return True, if 'x' occurs in 'y'.
      */
    protected def occursIn[T](x: T, y: T): Boolean = oncebu(
      strategy {
        case a if a == x => Some(a)
      })(y) != None

    /**
      * Number of children of the given value.
      */
    protected def arity(x: Any): Int = {
      var c = 0;
      all(queryf {
        case a => c += 1;
      })(x)
      c
    }

    /**
      * Get the children of the given value.
      */
    protected def getChildren(x: Any): List[Any] = {
      val l = new ListBuffer[Any]()
      all(queryf {
        case a => l += a
      })(x)
      l.toList
    }

    /**
      * Convenience function to combine the results of two unify calls.
      */
    protected def combineUnifier(u: (Boolean, Substitution), v: (Boolean, Substitution)): (Boolean, Substitution) = (u._1 && v._1, v._2 ++ u._2)
  }
}
