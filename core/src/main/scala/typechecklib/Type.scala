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

import scala.language.postfixOps
import scala.collection.immutable.List.fill

import typechecklib._
import typechecklib.Syntax._
import typechecklib.Substitutions.Substitution

import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting.Strategy


/**
  * The module for types.
  */
object Types {
  
  /**
    * An abstract class representing type terms.
    */
  abstract class Type {

    /**
      * All type variables occuring in this type term.
      */
    def vars: List[TypeVariable] = {
      collectl{ case tv: TypeVariable => tv }(this).distinct
    }

    /**
      * All type variables occuring free, i.e., not bound
      * by the quantifier of a type scheme, in this type.
      */
    def freeVars: List[TypeVariable] = freeVars(Nil).distinct

    /**
      * All type variables occuring free in this type with
      * respect to a given list of bound type variables.
      */
    def freeVars(boundVars: List[TypeVariable]): List[TypeVariable]

    /**
      * All type variables occuring not free, i.e., bound
      * by a quantifier of a type scheme, in this type.
      */
    def boundVars: List[TypeVariable] = (vars diff freeVars).distinct

    /**
      * Generalize this type with respect to an outer context,
      * i.e., bind all free type variables in a type scheme.
      */
    def generalize(ctx: Context): TypeScheme = {
      val freeVars = (this.freeVars diff ctx.freeVars).distinct

      this match {
	case TypeScheme(vars, ty) => TypeScheme(vars union freeVars, ty)
	case _                    => TypeScheme(freeVars, this)
      }
    }

    /**
      * Apply the given substitution to this type.
      *
      * This default implementation of this method applies
      * the substiution to all type variables occuring free
      * in this type.
      */
    def substitute(σ: Substitution): Type = {
      // Remove the bound variables of this type from the substitution
      val φ = σ -- this.boundVars

      def substituteTypeVar: Strategy = rule {
        case tv: TypeVariable => φ(tv)
        case x => all(substituteTypeVar)(x).get
      }

      substituteTypeVar(this).get match {
	case t: Type @unchecked => t
      }
    }

    /**
      * Constructs a function type.
      */
    def -->(ty: Type) = FunctionType(this, ty)

    /**
      * Constructs a two-ary tuple type.
      */
    def **(ty: Type) = TupleType(this :: ty :: Nil)
  }


  /**
    * An object for bottom types.
    */
  case object Bottom extends Type {
    def freeVars(boundVars: List[TypeVariable]) = Nil
  }


  /**
    * A class for type variables.
    */
  case class TypeVariable(ide: Ide) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = if (boundVars contains this) Nil
						  else List(this)

    override def toString(): String = ide.toString
  }

  /**
    * A type variable with a fresh name.
    */
  object TypeVariable {
    def apply(): TypeVariable = TypeVariable(NameSupply.freshName)
  }


  /**
    * Generates ''n'' fresh type variables.
    */
  def freshTVars(n: Int): List[TypeVariable] = fill(n)(TypeVariable())


  /**
    * A class for base types.
    */
  case class BaseType(ide: Ide) extends Type {
    
    def freeVars(boundVars: List[TypeVariable]) = Nil

    override def toString(): String = ide.toString
  }


  /**
    * A class for function types.
    */
  case class FunctionType(dom: Type, ran: Type) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = dom.freeVars(boundVars) union ran.freeVars(boundVars)

    override def toString(): String = dom.toString + " ⇒ " + ran.toString
  }


  /**
    * A class for tuple types.
    */
  case class TupleType(types: List[Type]) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = types.flatMap(_.freeVars(boundVars)).distinct

    override def toString(): String = types.mkString("(", ", ", ")")
  }


  /**
    * A class for type constructors.
    */
  case class TypeConstructor(ide: Ide, types: List[Type]) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = types.flatMap(_.freeVars(boundVars)).distinct

    override def toString(): String = ide + types.mkString("(", ", ", ")")
  }

  
  /**
    * A class for type schemes.
    */
  case class TypeScheme(tvars: List[TypeVariable], ty: Type) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = ty.freeVars(tvars)

    override def toString = tvars.mkString("forall ", ", ", " . ") + ty
  }


  /**
    * Convenience function to construct a [[typechecklib.Types.TypeScheme TypeScheme]].
    */
  def forall(tvars: TypeVariable*)(ty: Type): TypeScheme = TypeScheme(tvars.toList, ty)

  
  /**
    * A class for unary auxiliary functions in deduction rules which evaluate to a type.
    */
  case class TypeFunction1[T](f: MetaFun1[T, Type]) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = Nil

    override def substitute(σ: Substitution): Type = TypeFunction1[T](σ(f))

    def apply() = f.apply
  }


  /**
    * A class for binary auxiliary functions in deduction rules which evaluate to a type.
    */
  case class TypeFunction2[S, T](f: MetaFun2[S, T, Type]) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = Nil

    override def substitute(σ: Substitution): Type = TypeFunction2[S,T](σ(f))

    def apply() = f.apply
  }


  /**
    * A class for auxiliary functions in deduction rules which evaluate to a type.
    */
  case class TypeFunction[T <: Product](f: MetaFun[T, Type]) extends Type {

    def freeVars(boundVars: List[TypeVariable]) = Nil

    override def substitute(σ: Substitution): Type = TypeFunction[T](σ(f))

    def apply() = f.apply
  }
}
