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

import typechecklib.Rules._
import typechecklib.Errors._


/**
  * A trait using runtime reflection to find a matching rule during constraint generation.
  */
trait ReflectionBasedConstraintGeneration extends ConstraintGeneration {

  /**
    * The type rules.
    */
  val rules: List[reflect.runtime.universe.Type]


  /**
    * Instantiate the corresponding type rule for the given judgement.
    */
  def instantiateRule(judgement: Judgement): Option[Rule] = {

    var ruleInstance: Option[Rule] = None

    rules.find(rule => { ruleInstance = instantiateRule(rule, judgement.ctx, judgement.expr, judgement.ty) ; ruleInstance.isDefined })

    ruleInstance
  }


  /**
    * Creates an instance of the given [[typechecklib.Rules.Rule Rule]] type at runtime.
    *
    * @return None, if instantiating the rule fails. Otherwise the instantiated rule
    *         wrapped up in a Some.
    */
  def instantiateRule(t: reflect.runtime.universe.Type, args: Any*): Option[Rule] = {
    import scala.reflect.runtime.{ currentMirror => m, universe => uni }
    import uni.{Type => ReflectionType, _}
    import java.lang.Throwable

    val ttag = t
    val ctor = ttag.member(nme.CONSTRUCTOR).asTerm
    if (ctor.isOverloaded) throw new Exception("don't know how to disambiguate")

    val c = ttag.typeSymbol.asClass
    val mm = m.reflectClass(c).reflectConstructor(ctor.asMethod)
    //	  val f = mm.symbol.asMethod.paramss
    //	  println(f)
    try {
      val v = mm.apply(args: _*) match {
        case x: Rule => Some(x)
        case _ => None
      }
      return v
    } catch {
      case e: Throwable => None
    } 
  }
}
