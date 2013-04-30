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

import typechecklib.Constraints.Constraint
import typechecklib.ConstraintGeneration.ConstraintTree

import org.kiama.rewriting.Rewriter._


/**
  * Traverse a deduction tree labeled with constraints.
  */
trait TreeTraversal {
  
  /**
    * Collect all constraints when traversing a deduction tree.
    */
  def flatten(tree: ConstraintTree): List[Constraint]
}


/**
  * Pre-order depth-first traversal of a deduction tree.
  */
object DepthFirstPreOrder extends TreeTraversal {

  def flatten(tree: ConstraintTree): List[Constraint] = {
    val constraints = new ListBuffer[Constraint]()
    tree.children.map(t => constraints ++= flatten(t))
    constraints ++= tree.rule.constraints
    constraints.toList
  }
}


/**
  * Post-order depth-first traversal of a deduction tree.
  */
object DepthFirstPostOrder extends TreeTraversal {

  def flatten(tree: ConstraintTree): List[Constraint] = {
    val constraints = new ListBuffer[Constraint]()
    constraints ++= tree.rule.constraints
    tree.children.map(t => constraints ++= flatten(t))
    constraints.toList
  }
}


/**
  * Breadth-first traversal of a deduction tree.
  */
object BreadthFirst extends TreeTraversal {

  def flatten(tree: ConstraintTree): List[Constraint] = {
    val constraints = new ListBuffer[Constraint]()
    breadthfirst(query {
      case t: ConstraintTree => constraints ++= t.rule.constraints
    })(tree)
    constraints.toList
  }
}


/**
  * Bottom-up traversal of a deduction tree.
  */
object BottomUp extends TreeTraversal {

  def flatten(tree: ConstraintTree): List[Constraint] = {
    val constraints = new ListBuffer[Constraint]()
    bottomup(query {
      case t: ConstraintTree => constraints ++= t.rule.constraints
    })(tree)
    constraints.toList
  }
}


/**
  * Top-down traversal of a deduction tree.
  */
object TopDown extends TreeTraversal {

  def flatten(tree: ConstraintTree): List[Constraint] = {
    val constraints = new ListBuffer[Constraint]()
    topdown(query {
      case t: ConstraintTree => constraints ++= t.rule.constraints
    })(tree)
    constraints.toList
  }
}
