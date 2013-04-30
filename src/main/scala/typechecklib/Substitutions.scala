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

import typechecklib.Types._

import org.kiama.rewriting.Rewriter._


/**
  * The module for substitutions.
  */
object Substitutions {
  
  /**
    * A class for immutable substitutions over types.
    */
  case class Substitution(val sub: Map[TypeVariable, Type]) {
    
    /**
      * Constructor with MapLike Syntax, i.e.
      * {{{
      * val s = new Substitution(x -> y, z -> a)
      * }}}
      */
    def this(elems: (TypeVariable, Type)*) = this(elems.toMap)


    /**
      * Apply this substitution to an element and all its children. 
      */
    def apply[T](e: T): T = {
      val var2Term = rule {
	case v: TypeVariable => sub.getOrElse(v, v)
      }
      
      everywhere(var2Term)(e).getOrElse(e) match {
	case e: T @unchecked => e
      }
    }

    
    /**
      * Composition of two Substitutions.
      */
    def ++(that: Substitution): Substitution = {
      val newThat = that.sub.mapValues(this.apply(_))
      // Note: Map.++ is right-biased
      Substitution(newThat ++ this.sub)
    }


    /**
      * Add a key and the corresponding value to this Substitution. 
      * If the key already exists, its replaced.
      */
    def +(mapping: (TypeVariable, Type)) = Substitution(this.sub + mapping)


    /**
      * Collects all keys of this substitution in an iterable collection.
      */
    def keys: Iterable[TypeVariable] = sub.keys


    /**
      * Collects all keys of this substitution in a set.
      */
    def keySet: Set[TypeVariable] = sub.keySet


    override def toString = sub.toString
  }


  implicit def map2Substitution(m: Map[TypeVariable, Type]): Substitution = {
    Substitution(m)
  }

  implicit def tuples2Substitution(elems: (TypeVariable,Type)*): Substitution = {
    Substitution(elems.toMap)
  }

  implicit def seq2Substitution(elems: Seq[(TypeVariable, Type)]): Substitution = {
    Substitution(elems.toMap)
  }

}
