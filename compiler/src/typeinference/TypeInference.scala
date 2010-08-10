/*
 * TODO:
 * - Either/Exceptions?
 */

package parser.typeinf

/**
 * Based on Odersky's chapter on Hindley-Milner in 'Scala by Example'.
 */
object TypeInference {

  import parser.ast.types._
  import parser.ast.expressions._
  import parser.ast.patterns._

  // counter for type variables
  private var cnt: Int = 0

  /**
   * Returns a fresh type variable.
   */
  def newTyVar(): TypeExpression = { cnt += 1;  TypeVariable(cnt) }

  /**
   *  Represents a substitution.  A substitution is an idempotent function
   *  from type variables to types.
   */
  abstract case class Subst extends Function1[TypeExpression, TypeExpression] {

    def lookup(x: TypeVariable): Option[TypeExpression]

    /**
     * Applies the substitution.
     */
    def apply(t: TypeExpression) = t match {
      case tv@TypeVariable(a) => lookup(tv) match {
          case Some(u) if t == u => t
          case Some(u) => apply(u)
        }
      case TypeConstructor(tname, ts @ _*) =>
        TypeConstructor(tname, ts map(apply) :_*)
    }

    def extend(tvar: TypeVariable, t: TypeExpression) = new Subst {
      def lookup(y: TypeVariable): Option[TypeExpression] =
        if (tvar == y) Some(t) else Subst.this.lookup(y)
    }
  }

  val emptySubst = new Subst { 
    def lookup(tvar: TypeVariable): Option[TypeExpression] = Some(tvar)
  }

  case class TypeScheme(typeVars: List[TypeVariable],
                        texpression: TypeExpression) {
    def newInstance: TypeExpression = {
      (emptySubst /: typeVars) ((s, tv) => s.extend(tv, newTyVar())) (texpression)
    }
  }

  /**
   * Data type for the type environment.
   * Assign an identifier a type scheme.
   */
  type Env = List[(String, TypeScheme)]

  /**
   * Returns the type scheme that is associated with the given name.
   */
  def lookup(env: Env, x: String): Option[TypeScheme] = env match {
    case List() => None
    case (y, t) :: rest => if (x == y) Some(t) else lookup(rest, x)
  }

  /**
   * Turns a given type expression into a scheme quantifying over all free type
   * variables in the expression, but not in the environment.
   */
  def generalise(env: Env, t: TypeExpression): TypeScheme =
    TypeScheme(freeVars(t) -- freeVars(env), t)

  /**
   * Returns the free type variables of a type expression.
   */
  def freeVars(t: TypeExpression): List[TypeVariable] = t match {
    case tv@TypeVariable(a) => List(tv)
    case TypeConstructor(_, texprs@ _*) =>
      (List[TypeVariable]() /: texprs) ((vars, t) => vars union freeVars(t))
  }


  def freeVars(scheme: TypeScheme): List[TypeVariable] =
    freeVars(scheme.texpression) -- scheme.typeVars

  def freeVars(env: Env): List[TypeVariable] =
    (List[TypeVariable]() /: env) ((tvs,nt) => tvs union freeVars(nt._2))

  /**
   *
   */
  def unify(t: TypeExpression, u: TypeExpression, s: Subst): Subst = (s(t), s(u)) match {
    case (TypeVariable(a), TypeVariable(b)) if (a == b) => s
    case (TypeVariable(a), _) if !(freeVars(u) contains a) =>
        s.extend(TypeVariable(a), u)
    case (TypeVariable(a), _) if (freeVars(u) contains a) =>
        throw new TypeError("Occurs check failed!")
    case (_, TypeVariable(a)) => unify(u, t, s)
    case (TypeConstructor(k1, ts@_*), TypeConstructor(k2, us@_*)) if (k1 == k2) =>
      (s /: (ts zip us)) ((s, tu) => unify(tu._1, tu._2, s))
    case _ => throw new TypeError("cannot unify " + s(t) + " with " + s(u))
  }

  def tp(env: Env, e: Expression, t: TypeExpression, s: Subst): Subst = {
    
    current = e
    e match {
      case Id(x) => lookup(env, x) match {
          case None => throw new TypeError("undefined: " + x)
          case Some(u) => unify(u.newInstance, t, s)
      }
      case BinOp(op, e1, e2) =>
        val te1 = typeOf(env, e1)
        val te2 = typeOf(env, e2)
        unify(te1, te2, s)
      case Lambda(body, PatId(x)) =>
        val a, b = newTyVar()
        // TODO: replace string by constant
        val s1 = unify(t, TypeConstructor("Function", a, b), s)
        val env1 = (x, TypeScheme(List(), a)) :: env
        tp(env1, body, b, s1)
      case App(e1, e2) => val a = newTyVar()
        val s1 = tp(env, e1, TypeConstructor("Function", a, t), s)
        tp(env, e2, a, s1)
      // TODO: additional cases for pattern
      case Let(PatId(x), e1, e2) =>
        val a = newTyVar()
        val s1 = tp(env, e1, a, s)
        tp((x, generalise(env,s1(a))) :: env, e2, t, s1)
    }
  }

  var current: Expression = null

  def typeOf(env: Env, e: Expression): TypeExpression = {
    // TODO: check base types here
    // TODO: provide toString methods
    val a = newTyVar()
    tp(env, e, a, emptySubst)(a)
  }

  case class TypeError(err: String) extends Exception(err) { }

}
