A Scala Type Checker Library
============================

A Scala library providing means to derive type checkers from constraint-based inference rules:
- A small embedded DSL to define constraint-based inference rules.
- A framework to define strategies to collect and solve constraints from a deduction tree.
- A set of basic, predefined strategies for rule instantiation (e.g. reflection-based), deduction tree traversal
  (e.g. depth-first, breadth-first, bottom-up, top-down), and constraint solving (e.g. linear order).

This project builds on and evolves the concepts presented in
[Deriving Type Checkers](https://github.com/mzuber/deriving-type-checkers)
(Technical Report 2012–09, TU Berlin, 2012).

Installation
------------

This project can be built with [sbt](https://www.scala-sbt.org/index.html) version `0.13.18`.

To compile the sources and execute all tests, run:

```
sbt compile test
```

To Package the library, run:

```
sbt package
```

This step will create `jar` files for the library in the `core` directory as well as for all examples in the `examples`
directory.

Example
-------

Define abstract syntax for a simply-typed lambda calculus.

```scala
import typechecklib.Syntax.Ide

abstract class Term
case class Var(ide: Ide) extends Term
case class Abs(x: Var, e: Term) extends Term
case class App(f: Term, e:Term) extends Term
```

Define constraint-based inference rules for a simply-typed lambda calculus.

```scala
import typechecklib.Rules._
import typechecklib.Types._
import typechecklib.Constraints._

/* Parent type for all type rules */
sealed trait SimpleLambdaRules


/*
 * Typing rule for variable lookup:
 * 
 *         T = Γ(x)
 *       ----------- (Var)
 *        Γ ⊢ x : T
 */
case class VarRule(ctx: Context, x: Var, t: Type) extends Axiom with SimpleLambdaRules {
    ctx ⊢ x <:> t | t =:= ctx(x)

    override val name = "Var"
}


/*
 * Typing rule for lambda abstraction:
 * 
 *      Γ,x:T1 ⊢ e : T2       T = T1 -> T2
 *     ------------------------------------ (Abs)
 *                Γ ⊢ λ x.e : T
 */
case class AbsRule(ctx: Context, abs: Abs, t: Type) extends Rule with SimpleLambdaRules {
    val t1 = TypeVariable()
    val t2 = TypeVariable()
    val newCtx = ctx + (abs.x -> t1)

    newCtx ⊢ abs.e <:> t2 ==> 
    ctx ⊢ abs <:> t | (t =:= t1 --> t2)

    override  val name = "Abs"
}


/*
 * Typing rule for application.
 * 
 *    Γ ⊢ f : T1     Γ ⊢ e : T2     T1 = T2 -> T
 *   -------------------------------------------- (App)
 *                  Γ ⊢ (f) e : T
 */
case class AppRule(ctx: Context, app: App, t: Type) extends Rule with SimpleLambdaRules {
    val t1 = TypeVariable()
    val t2 = TypeVariable()

    List(ctx ⊢ app.f <:> t1, ctx ⊢ app.e <:> t2 ) ==> 
    ctx ⊢ app <:> t | (t1 =:= t2 --> t)

    override val name = "App"
}
```

Derive a type checker from the given inference rules with the help of the library.

```scala
import typechecklib._

object LambdaTypeChecker extends TypeChecker with ReflectionBasedConstraintGeneration with DepthFirstPreOrder with LinearConstraintSolver {
    import scala.reflect.runtime.universe.typeOf

    val rules = List(typeOf[VarRule], typeOf[AbsRule], typeOf[AppRule])
}
```

As part of this library a type checker is composed of three key traits:
- The trait `ConstraintGeneration` defining how the given typing rules are instantiated to build deduction tree.
- The trait `TreeTraversal` defining how a deduction tree labeled with constraints should be traversed to collect all
  constraints.
- The trait `ConstraintSolver` defining the strategy how the collected constraints should be solved (e.g. in what 
  order).

In the simply-typed lambda calculus example the type checker uses a reflection-based rule instantiation mechanism,
traverses the deduction tree using a pre-order depth-first strategy, and solves the collected constraints in linear
order.
