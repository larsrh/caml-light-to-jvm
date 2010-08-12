
package typeinference

object TypeInference {
  
  import parser.ast._
  import parser.ast.types._

  implicit def enrichList(xs: List[(TypeExpression,TypeExpression)]) = new SubstList(xs)
  implicit def enrichtTuple(t: (TypeExpression,TypeExpression)) = new SubstTuple(t)

  class SubstList(xs: List[(TypeExpression,TypeExpression)]) extends Subst[List[(TypeExpression,TypeExpression)]]{
    def subst_(s: (TypeExpression,TypeExpression)) = {
      xs.map { x => x.subst_(s) }
    }
  }

  class SubstTuple(t: (TypeExpression,TypeExpression)) extends Subst[(TypeExpression,TypeExpression)]{
    def subst_(s: (TypeExpression,TypeExpression)) = {
      (t._1.subst_(s), t._2.subst_(s))
    }
  }

  /**
   * Data type for the type environment.
   * Assign an identifier a type scheme.
   */
  type Env = List[(String, TypeScheme)]

  /**
   * Type scheme which is quantified over the free type variables in the
   * given type expression.
   */
  type TypeScheme = (List[TypeVariable], TypeExpression)

  /**
   * Represents a typing error.
   */
  // TODO: use this instead of Either
  case class TypeError(err: String) extends Exception(err) { }


  /**
   * Top-level typecheck method. Given a type environment and an expression
   * returns the type of the given expression.
   */
  def typeCheck(gamma: Env, expr: expressions.Expression): (List[TypeVariable], TypeExpression) = {
    constraintGen(gamma, expr, 1) match {
      case (typeExpr,_,constraints) => generalise(gamma, typeExpr, constraints)
    }
  }

  /**
   * Returns the free type variables of a type expression.
   */
  def freeVars(t: TypeExpression): List[TypeVariable] = t match {
    case tv@TypeVariable(a) => List(tv)
    case TypeConstructor(_, texprs@ _*) =>
      (List[TypeVariable]() /: texprs) ((vars, t) => vars union freeVars(t))
    case TypeRecord(name, fields@_*) =>
      (List[TypeVariable]() /: fields) ((vars, t) => vars union freeVars(t._2))
  }

  /**
   * Returns the free type variables of a type scheme.
   */
  def freeVars(scheme: TypeScheme): List[TypeVariable] =
    freeVars(scheme._2) filterNot (scheme._1 contains)

  /**
   * Returns the free type variables of a type environment.
   */
  def freeVars(env: Env): List[TypeVariable] =
    (List[TypeVariable]() /: env) ((tvs,nt) => tvs union freeVars(nt._2))

  /**
   * Performs unification. Given a list of constraints returns a list of
   * substitutions.
   */
  def unify(constraints: List[(TypeExpression,TypeExpression)]): List[(TypeExpression,TypeExpression)] = {

    // local function
    def unify_(constraints: List[(TypeExpression,TypeExpression)]):
    List[(TypeExpression,TypeExpression)] = {
      constraints match {
        case List() => List()
        case (t1,t2)::u =>
          if (t1 == t2) { unify_(u) }
          else {
            (t1,t2) match {
              case (al@TypeVariable(a), t) =>
                if (freeVars(t).contains(a)) { throw new TypeError("Occurs check failed.") }
                else {
                  val rest = unify_(u.subst_((t,al)))
                  (t,al)::rest
                }
              case (t, al@TypeVariable(a)) =>
                if ((freeVars(t)).contains(a)) { throw new TypeError("Occurs check failed.") }
                else {
                  val rest = unify_(u.subst_((t,al)))
                  (t,al)::rest
                }
              case (TypeConstructor(n1,params1@_*),TypeConstructor(n2,params2@_*)) =>
                if (params1.length != params2.length) {
                  throw new TypeError("TODO: unify - reasonable error message")
                } else if (params1.length == 0) {
		  if (n1 == n2) {
		    unify_(u)
		  } else {
		    throw new TypeError("Couldn't unify TODO")
		  }
		} else {
                  unify_(params1.zip(params2).toList ++ u)
                }
              case _ => System.err.println("blub"); throw new TypeError("TODO: unify - reasonable error message")
            }
          }
      }
    }

    unify_(constraints)
  }

  def generalise[A](gamma: Env, typeExpr: TypeExpression,
		    constraints: List[(TypeExpression,TypeExpression)]): TypeScheme = {
    val subst = unify(constraints)
    val typeExprNew = typeExpr.subst(subst)
    val alphas = freeVars(typeExprNew) -- freeVars(gamma)
    (alphas,typeExprNew)
  }

  def update(gamma: Env, x: String, scheme: TypeScheme): Env = {
    (x,scheme)::gamma
  }

  /**
   * Given a type environment, a type expression and a counter, that's used
   * for creating new type variables, generates a type expression together
   * with the necessary constraints.
   */
  def constraintGen(gamma: Env, expr: expressions.Expression, fresh: Int):
  (TypeExpression,Int,List[(TypeExpression,TypeExpression)]) =
    
    expr match {

      // base types
      case expressions.Integer(_) => (TypeInt(), fresh, List())
      case expressions.Bool(_) => (TypeBool(), fresh, List())
      case expressions.Character(_) => (TypeChar(), fresh, List())

      case expressions.Id(x) =>
        lookup(gamma, x) match {
          case Left(err) => throw new TypeError(err)
          case Right(sigma) =>
	    spec(sigma, fresh) match {
	      case (typeExpr,fresh1) => (typeExpr,fresh,List())
	    }
        }

      case expressions.IfThenElse(e1, e2, e3) =>
	val (t1, fresh1, c1) = constraintGen(gamma, e1, fresh)
	val (t2, fresh2, c2) = constraintGen(gamma, e2, fresh1)
	val (t3, fresh3, c3) = constraintGen(gamma, e3, fresh2)
	(t2, fresh3, (t2,t3) :: (TypeBool(),t1) :: c1 ++ c2 ++ c3)

      case expressions.Lambda(body,patterns.Id(id)) =>
	val gamma1 = update(gamma,id,(List(),TypeVariable(fresh)))
        constraintGen(gamma1,body,fresh+1) match {
          case (typeBody,fresh1,constraints) =>
            (TypeFn(TypeVariable(fresh), typeBody), fresh1, constraints)
        }

      case expressions.Lambda(typeBody, p@patterns.Cons(h,r)) =>
        updateEnv(gamma, p, TypeVariable(fresh), fresh) match {
          case (gamma1, fresh1, constraints1) =>
            constraintGen(gamma1, typeBody, fresh1) match {
	      case (typeBody,fresh2,constraints) =>
                (TypeFn(TypeList(TypeVariable(fresh)), typeBody), fresh2, constraints)
            }
        }

	// TODO
//      case expressions.Lambda(body, p@patterns.Tuple(pat)) =>
//        // TODO: this is essentially the same as above
//        // clarify if as patterns are available, if yes, this needs to be changed
//        updateEnv(gamma, pat, TypeVariable(fresh), fresh) match {
//          case (gamma1, fresh1, constraints1) =>
//            constraintGen(gamma1, body, fresh1) match {
//              case Right((tbody,fresh1,constraints)) =>
//                Right((TFn(TList(TypeVariable(fresh)), tbody), fresh1, constraints))
//            }
//        }

      case expressions.Lambda(body,p@patterns.Underscore) =>
        constraintGen(gamma,body,fresh) match {
	  case (typeBody,fresh1,constraints) =>
            (TypeFn(TypeVariable(fresh), typeBody),fresh1,constraints)
        }

      case expressions.Lambda(body,p@patterns.Nil) =>
        constraintGen(gamma, body, fresh) match {
	  case (typeBody,fresh1,constraints) =>
            ((TypeFn(TypeList(TypeVariable(fresh)), typeBody),fresh1,constraints))
        }

      case l@expressions.Lambda(_, patterns@_*) =>
        constraintGen(gamma, curry(l), fresh)

        // anonymous record definition like in e.g.
        // let x = { one=Integer(1), id=Lamda(Id("x"),patterns.Id("x")) }
      case expressions.Record(defs@_*) =>
        determineRecordFieldTypes(List(), List(), gamma, fresh, defs:_*) match {
          case (fields,fresh1,constraints) =>
            (TypeRecord("anonymous", fields:_*),fresh1,constraints)
        }
        // field access
      case expressions.Field(recExpr, fieldId) =>
        // generate type for recExpr
        constraintGen(gamma, recExpr, fresh) match {
          case (typeRecExpr,fresh1,constraints) =>
            // generated type expression should match a record type
            typeRecExpr match {
              case TypeRecord(recName, fields@_*) =>
                // lookup type of desired field
                val matchedFields = fields filter {f => f._1 == fieldId.name}
                if (matchedFields.length == 0) {
		  throw new TypeError("No such field " + fieldId.name + " in record " + recName + ".")
                } else if (matchedFields.length > 1) {
                  // sanity check; we should never get here since records aren't
		  // allowed to conatain fields with a duplicate name
                  throw new TypeError("Duplicate field " + fieldId.name + " found in record " + recName + ".")
                } else {
                  // return type
                  (matchedFields.head._2, fresh1, constraints)
                }
              case _ => throw new TypeError("No record " + recExpr + " found.")
            }
        }

      case expressions.Cons(hd,rst) =>
        constraintGen(gamma, hd, fresh) match {
          case (te1,fresh1,constraints1) =>
            constraintGen(gamma, rst, fresh1) match {
              case (te2,fresh2,constraints2) =>
                (TypeList(te1), fresh2,(te2,TypeList(te1))::constraints1++constraints2)
            }
        }

        // empty list
      case expressions.Nil =>
        (TypeList(TypeVariable(fresh)), fresh+1, List())

      case expressions.App(e1,e2) =>
        constraintGen(gamma, e1, fresh+1) match {
          case (typeE1, fresh1, constraints1) =>
            constraintGen(gamma, e2, fresh1) match {
              case (typeE2, fresh2, constraints2) =>
		val alpha = TypeVariable(fresh)
                (alpha,fresh2,(typeE1, TypeFn(typeE2,alpha))::(constraints1++constraints2))
            }
        }

      case expressions.Let(patterns.Id(x),expr,body) =>
        constraintGen(gamma,expr,fresh) match {
          case (typeExpr,fresh1,constraints) =>
            val scheme = generalise(gamma,typeExpr,constraints)
	    val gamma1 = update(gamma,x,scheme)
	    constraintGen(gamma1, body, fresh1)
        }

        // ignore matched pattern
      case expressions.Let(patterns.Underscore,expr,body) =>
        constraintGen(gamma,body,fresh)

      case expressions.Let(p@patterns.Cons(head,rest), expr, body) =>
        constraintGen(gamma,expr,fresh) match {
          case (texpr,fresh1,_) => // ignore constraints here, see additional constraint below
            updateEnv(gamma, p, texpr, fresh1) match {
              case (gamma1,fresh2,constraints1) =>
                constraintGen(gamma1, body, fresh2) match {
                  case (tbody,fresh2,constraints2) =>
                    (tbody,fresh2, constraints1 ++ constraints2)
                }
            }
        }

        // ignore constraints for type of expr
        // it only has to match a list
      case expressions.Let(patterns.Nil, expr, body) =>
        constraintGen(gamma,expr,fresh) match {
          case (typeExpr,fresh1,_) =>
	    constraintGen(gamma, body, fresh1) match {
	      case (typeBody, fresh1, constraints) =>
		(typeBody, fresh1+1,
		       (typeExpr,TypeList(TypeVariable(fresh1)))::constraints)
	    }
        }

	// TODO
      case expressions.Let(patterns.Record(patterns@_*), expr, body) =>
        val (typeExpr,fresh1,_) = constraintGen(gamma, expr, fresh)
	val (typeBody, fresh2, constraints) = constraintGen(gamma, body, fresh1)
	typeExpr match {
	  case TypeRecord(_,fields@_*) =>
	    // extract field names
	    val fieldNames = fields map (f => f._1)
	    val newVars = fresh2 to fresh2 + fields.length map (i => TypeVariable(i))
	    (typeBody,fresh2 + fields.length + 1,
	     (typeExpr,TypeRecord("anonymous",fieldNames zip newVars:_*))::constraints)
	  case _ => throw new TypeError("Couldn't match type.")
	}
	    
      case expressions.Match(scrut, clauses@_*) =>
        // TODO: check if clauses are non-empty, check type of scrut againtst patterns
        // all clauses must match the same type
        checkClauses(clauses.toList, gamma, List(), fresh, TypeVariable(fresh)) match {
          // TODO: as pattern?
          case (fresh1,_,clausesExpr) =>
            constraintGen(gamma, scrut, fresh1) match {
              case (typeScrut,fresh2,constraints) =>
                (clausesExpr, fresh1, (typeScrut,clausesExpr)::constraints)
            }
        }
    }

  /**
   * Determines the types of the given records field and returns them
   * together with the generated constraints.
   */
  def determineRecordFieldTypes(
    constraints: List[(TypeExpression,TypeExpression)],
    fields: List[(String,TypeExpression)],
    gamma: Env, fresh: Int, recordFields: (expressions.Id,expressions.Expression)*):
  (List[(String,TypeExpression)],Int,List[(TypeExpression,TypeExpression)]) = {
    if (recordFields.isEmpty) {
      (fields, fresh, constraints)
    } else {
      val recField = recordFields.head
      constraintGen(gamma, recField._2, fresh) match {
        case (typeexpr, fresh1, constraints1) =>
          val constraintsNew = constraints union constraints1
          determineRecordFieldTypes(constraintsNew, (recField._1.name, typeexpr)::fields,
				    gamma, fresh1, recordFields.tail:_*)
      }
    }
  }

  /**
   * Returns the constraints that need to be fulfilled in order to correclty
   * type check a pattern match. This means, each result expression must be of
   * the same type and each pattern must be the same type as the matched expression (TODO)
   */
  def checkClauses(clauses: List[(patterns.Pattern, expressions.Expression)],
		   gamma: Env,
		   constraints: List[(TypeExpression,TypeExpression)],
		   fresh: Int,
		   typeExpr: TypeExpression):
  (Int, List[(TypeExpression,TypeExpression)], TypeExpression) = {
    clauses match {
      case List() =>
	val subst = unify(constraints)
	// TODO: constraints here are actually ignored, fix this by
	// introducing inner method
	(fresh,constraints,typeExpr)
      case h::r =>
	updateEnv(gamma, h._1, TypeVariable(fresh), fresh) match {
	  case (gamma1,fresh1,_) =>
	    constraintGen(gamma1, h._2, fresh1) match {
	      case (thead, fresh2, _) =>
		checkClauses(r, gamma, (thead,typeExpr)::constraints, fresh2, typeExpr)
	    }
	}
    }
  }

  /**
   * Updates the type enviroment based on the given pattern and the given type.
   */
  def updateEnv(env: Env, pattern: patterns.Pattern, typeExpr: TypeExpression, fresh: Int):
  (Env,Int,List[(TypeExpression,TypeExpression)]) = {

    pattern match {
      case patterns.Id(x) =>
        ((x, (List(), typeExpr))::env,fresh+1,List())
      case patterns.Underscore => (env,fresh,List())
      case patterns.Nil  => (env,fresh,List())
	// case tup@patterns.Tuple(pats) =>
	//  var i = 0
	// ((env,fresh,List()) /: pats) (((e,f,c),p) => i += 1; updateEnv(e, p, fresh + i, match {
	//
	//  }))
      case cons@patterns.Cons(head, tail) =>
        val alpha = TypeVariable(fresh)
        updateEnv(env, head, alpha, fresh+1) match {
          case (env1, fresh1, constraints1) =>
            updateEnv(env1, tail, TypeList(alpha), fresh1) match {
              case (env2,fresh2,constraints2) =>
                (env2,fresh2,(typeExpr,TypeList(alpha))::constraints1 ++ constraints2)
            }
        }
    }
  }

  def spec(sigma: TypeScheme, fresh: Int): (TypeExpression, Int) = {
    val alphas = sigma._1
    val t = sigma._2
    val s = (fresh to (fresh + alphas.length) toList).map(x => TypeVariable(x)).zip(alphas)
    (t.subst(s), fresh + alphas.length)
  }

  /**
   * Curries the given function, e.g. when a lambda expression like
   *   Lambda(Id("x"), patterns.Id("x"), patterns.Id("y"))
   * is given, it is brought into this shape:
   *   Lambda(Lambda(patterns.Id("y"), Id("x"), patterns.Id("x"))
   */
  def curry(fun: expressions.Lambda) = {
    ((expressions.Lambda(fun.body, fun.arguments.last)) 
     /: (fun.arguments.init.reverse)) ((l,p) => expressions.Lambda(l, p))
  }

  /**
   * Looks up the given id in the given type environment and returns
   * it type scheme
   */
  def lookup(gamma: Env, id: String): Either[String, TypeScheme] = {
    gamma find {t => t._1 == id} match {
      case Some((id1,scheme)) => Right(scheme)
      case None => Left("variable " + id + " not bound")
    }
  }
}
