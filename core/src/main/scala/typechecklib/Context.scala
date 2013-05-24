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

import typechecklib.Types.{Type, TypeVariable, Bottom}


/**
  * A class for immutable contexts.
  */
case class Context(ctx: Map[Any, Type]) {

  /**
    * Constructor with MapLike Syntax, e.g.
    * {{{
    * val ctx = new Context(x -> s, y -> t)
    * }}}
    */
  def this(elems: (Any, Type)*) = this(elems.toMap)

  /**
    * Lookup the type of an expression in this context.
    *
    * @return The type of the given expressions, if it is contained
    *         as a key in the context. The bottom type otherwise.
    */
  def apply(exp: Any): Type = ctx.getOrElse(exp, Bottom)

  /**
    * Tests whether this context contains a binding for an expression.
    */
  def contains(exp: Any) = ctx contains exp

  /**
    * Insert a new mapping into this context.
    */
  def +(mapping: (Any, Type)): Context = Context(ctx + mapping)

  /**
    * Collect all type variables occuring free in the type terms stored in this context.
    */
  val freeVars: List[TypeVariable] = ctx.values.map(_.freeVars).fold(Nil)(_ union _)
}
