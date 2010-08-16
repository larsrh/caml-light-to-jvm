
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
  type Env = Map[String, TypeScheme]

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


  def emptyEnv() = {
    Map[String,TypeScheme]()
  }

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
                if (freeVars(t).contains(al)) { throw new TypeError("Occurs check failed.") }
                else {
                  val rest = unify_(u.subst_((t,al)))
                  (t,al)::rest
                }
              case (t, al@TypeVariable(a)) =>
		unify_((al,t)::u)
              case (TypeConstructor(n1,params1@_*),TypeConstructor(n2,params2@_*)) =>
                if (params1.length != params2.length) {
                  throw new TypeError("TODO: unify - reasonable error message")
                } else if (params1.length == 0) {
		  // base types without any type parameters
		  if (n1 == n2) {
		    unify_(u)
		  } else {
		    throw new TypeError("Couldn't unify TODO")
		  }
		} else {
                  unify_(params1.zip(params2).toList ++ u)
                }
              case _ => throw new TypeError("TODO: unify - reasonable error message")
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
    val alphas = freeVars(typeExprNew) filterNot (freeVars(gamma) contains)
    (alphas.distinct,typeExprNew)
  }

  def update(gamma: Env, x: String, scheme: TypeScheme): Env = {
    gamma + (x -> scheme)
  }

  /**
   * Given a type environment, a type expression and a counter, that's used
   * for creating new type variables, generates a type expression together
   * with the necessary constraints.
   */
  def constraintGen(gamma: Env, expr1: expressions.Expression, fresh: Int):
  (TypeExpression,Int,List[(TypeExpression,TypeExpression)]) = {

    val expr = curry(expr1)
    
    expr match {

      // base types
      case expressions.Integer(_) => (TypeInt(), fresh, List())
      case expressions.Bool(_) => (TypeBool(), fresh, List())
      case expressions.Character(_) => (TypeChar(), fresh, List())

      case expressions.Sequence(_,expr2) =>
	// TODO: we might test here whether expr1 is of type Unit and if so
	// print a warning
	val (typeExpr2, freshNew, constraints) = constraintGen(gamma,expr2, fresh)
	(typeExpr2,freshNew,List())

      case expressions.TupleElem(tup, nr) =>
	val (typeTup, freshNew, constraints) = constraintGen(gamma, tup, fresh)
	typeTup match {
	  case TypeTuple(tupleTypes@_*) =>
	    // tuple indexing start at 1
	    (tupleTypes(nr-1),freshNew,constraints)
	  case _ => throw new TypeError("Couldn't match expected tuple tuple type against type " + typeTup + ".")
	}

	// TODO: ?
//      case expressions.TupleElem(tuple, nr) =>
//	putExpressionIntoEnv(gamma,tuple,tpe,fresh)

      case expressions.Id(x) =>
        lookup(gamma, x) match {
          case Left(err) => throw new TypeError(err)
          case Right(sigma) =>
	    spec(sigma, fresh) match {
	      case (typeExpr,fresh1) => (typeExpr,fresh1,List())
	    }
        }

      case expressions.UnOp(op, e) =>
	val (t, f, c) = constraintGen(gamma, e, fresh)
	(t, f, (TypeBool(), t) :: c)

      case expressions.IfThenElse(e1, e2, e3) =>
	val (t1, fresh1, c1) = constraintGen(gamma, e1, fresh)
	val (t2, fresh2, c2) = constraintGen(gamma, e2, fresh1)
	val (t3, fresh3, c3) = constraintGen(gamma, e3, fresh2)
	println("t2: " + t2)
	println("t3: " + t3)
	(t2, fresh3, (t2,t3) :: (TypeBool(),t1) :: c1 ++ c2 ++ c3)

      case expressions.BinOp(op, e1, e2) =>
	val (t1, fresh1, c1) = constraintGen(gamma, e1, fresh)
	val (t2, fresh2, c2) = constraintGen(gamma, e2, fresh)
	op match {
	  case expressions.BinaryOperator.and | expressions.BinaryOperator.or =>
	    (TypeBool(),fresh2,(t1,TypeBool())::(t2,TypeBool())::(t1,t2)::c1 ++ c2)
	  case expressions.BinaryOperator.eq | expressions.BinaryOperator.neq
	    | expressions.BinaryOperator.geq | expressions.BinaryOperator.leq
	    | expressions.BinaryOperator.gr | expressions.BinaryOperator.le =>
	    (TypeBool(), fresh2, (t1,t2)::c1 ++ c2)
	  case _ => (t1,fresh2,(t1,t2)::c1 ++ c2)
	}

      case expressions.Lambda(body,patterns.Id(id)) =>
	println("1")
	val gamma1 = update(gamma,id,(List(),TypeVariable(fresh)))
        val (typeBody,fresh1,constraints) = constraintGen(gamma1,body,fresh+1)
	(TypeFn(TypeVariable(fresh), typeBody), fresh1, constraints)

      case expressions.Lambda(body, p@patterns.Cons(h,r)) =>
	println("2")
        val (gamma1, fresh1) = putPatternIntoEnv(gamma, p, TypeList(TypeVariable(fresh)), fresh+1)
	val (typeBody,fresh2,constraints) = constraintGen(gamma1, body, fresh1)
	(TypeFn(TypeList(TypeVariable(fresh)), typeBody), fresh2, constraints)

	// TODO
      case expressions.Lambda(body, p@patterns.Tuple(pat)) =>
        // TODO: this is essentially the same as above
        // clarify if as patterns are available, if yes, this needs to be changed
	val (patternType,fresh1) = getPatternType(p,fresh)
        val (gamma1,fresh2) = putPatternIntoEnv(gamma, p, patternType, fresh1)
	val (typeBody,fresh3,constraints) = constraintGen(gamma1, body, fresh2)
	((TypeFn(TypeList(TypeVariable(fresh)), typeBody), fresh3, constraints))

      case expressions.Lambda(body,p@patterns.Underscore) =>
        val (typeBody,fresh1,constraints) = constraintGen(gamma,body,fresh)
	(TypeFn(TypeVariable(fresh), typeBody),fresh1,constraints)

      case expressions.Lambda(body,p@patterns.Nil) =>
        val (typeBody,fresh1,constraints) = constraintGen(gamma, body, fresh)
	((TypeFn(TypeList(TypeVariable(fresh)), typeBody),fresh1,constraints))

        // anonymous record definition like in e.g.
        // let x = { one=Integer(1), id=Lamda(Id("x"),patterns.Id("x")) }
      case expressions.Record(defs@_*) =>
        determineRecordFieldTypes(List(), List(), gamma, fresh, defs:_*) match {
          case (fields,fresh1,constraints) =>
            (TypeRecord("anonymous", fields:_*),fresh1,constraints)
        }

      case expressions.Tuple(expr@_*) =>
	// TODO: 2nd and 5th parameter may be omitted from caller
	val (tupleTypes,freshNew,constraints) = determineTupleTypes(expr.toList, List(), gamma, fresh, List())
	(TypeTuple(tupleTypes:_*),freshNew,constraints)

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
        val (typeE1, fresh1, constraints) = constraintGen(gamma, e1, fresh+1)
	val (typeE2, fresh2, constraints1) = constraintGen(gamma, e2, fresh1)
	val alpha = TypeVariable(fresh)
	(alpha,fresh2,(typeE1, TypeFn(typeE2,alpha))::(constraints++constraints1))

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
        val (typeExpr,fresh1,_) = constraintGen(gamma,expr,fresh)
	val (gamma1,fresh2) = putPatternIntoEnv(gamma, p, typeExpr, fresh1)
	val (tbody,fresh3,constraints) = constraintGen(gamma1, body, fresh2)
	(tbody,fresh3, constraints)

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

      case expressions.Let(p@patterns.Tuple(_), expr, body) =>
	val (typeExpr, fresh1, _) = constraintGen(gamma,expr,fresh)
	val (gamma1,fresh2) = putPatternIntoEnv(gamma, p, typeExpr, fresh1)
	val (typeBody, fresh3, constraints) = constraintGen(gamma1, body, fresh2)
	typeExpr match {
	  case TypeTuple(fields@_*) =>
//	    val newVars = fresh3 to fresh3 + (fields.length - 1) map (i => TypeVariable(i))
	    (typeBody,fresh3 + fields.length,constraints)
//	     (typeExpr,TypeTuple(newVars:_*))::constraints)
	  case _ => throw new TypeError("Couldn't match tuple type.")
	}

      case expressions.Let(patterns.Record(pats@_*), expr, body) =>
        val (typeExpr,fresh1,_) = constraintGen(gamma, expr, fresh)
	val (typeBody, fresh2, constraints) = constraintGen(gamma, body, fresh1)
	typeExpr match {
	  case TypeRecord(_,fields@_*) =>
	    // extract field names
	    val fieldNames = fields map (f => f._1)
	    val newVars = fresh2 to fresh2 + fields.length map (i => TypeVariable(i))
	    (typeBody,fresh2 + fields.length + 1,
	     (typeExpr,TypeRecord("anonymous",fieldNames zip newVars:_*))::constraints)
	  case _ => throw new TypeError("Couldn't match record type.")
	}
	    
      case expressions.Match(scrut, clauses@_*) =>
        // TODO: check if clauses are non-empty
        // all clauses must match the same type
	val (scrutType, fresh1, constraints) = constraintGen(gamma, scrut, fresh)
	val (fresh2, scrutType1, typeExpr) = checkClauses(clauses.toList, gamma, scrut, fresh1)
	(typeExpr, fresh2, List((scrutType,scrutType1)))

      case expressions.LetRec(body, funs@_*) =>
	// put function names into type environment
	val (gamma1, fresh1) = putRecFunctionsIntoScope(gamma, fresh, funs.toList)
	println("gamma1: " + gamma1)
	constraintGen(gamma1, body, fresh1)
    }
  }

  def putRecFunctionsIntoScope(gamma: Env, fresh: Int, 
			       funs: List[(patterns.Id, expressions.Expression)]): (Env,Int)= {
    var currFresh = fresh
    var currGamma = gamma
    println("funs: " + funs)
    for (f <- funs) {
      val funName = f._1.name
      currGamma = currGamma + (funName -> (List(TypeVariable(currFresh)), TypeVariable(currFresh)))
      currFresh = currFresh + 1
    }
        println("currGamma: " + currGamma)


    for (f <- funs) {
      val funName = f._1.name
      val funBody = f._2
      val (funType, freshNew,constraints)  = constraintGen(currGamma, funBody, currFresh)
      // now update type
      val scheme = generalise(currGamma, funType, (currGamma(funName)._2,funType)::constraints)
      currGamma  = currGamma + (funName -> scheme)
      currFresh = freshNew
    }
    println("currGamma 2: " + currGamma)
    (currGamma, currFresh)
  }

  /**
   * Curries the given function, e.g. when a lambda expression like
   *   Lambda(Id("x"), patterns.Id("x"), patterns.Id("y"))
   * is given, it is brought into this shape:
   *   Lambda(Lambda(patterns.Id("y"), Id("x"), patterns.Id("x"))
   */
  def curry(expr: expressions.Expression): expressions.Expression = {
    // curry variable param expressions
    expr match {
      case fun@expressions.Lambda(_,arguments@_*) =>
	if (arguments.length > 1) {
	  ((expressions.Lambda(fun.body, fun.arguments.last))
	   /: (fun.arguments.init.reverse)) ((l,p) => expressions.Lambda(l, p))
	} else {
	  expr
	}
      case a@expressions.App(_,p@_*) =>
	if (p.length > 1) {
	  curryApp(a)
	} else {
	  expr
	}
      case _ => expr
    }
  }

  def curryApp(app: expressions.App) = {
    ((expressions.App(app.func, app.param.last))
     /: (app.param.init.reverse)) ((a,p) => expressions.App(a, p))
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

  def determineTupleTypes(exprs: List[expressions.Expression],
			  types: List[TypeExpression], gamma: Env, fresh: Int,
			  constraints: List[(TypeExpression,TypeExpression)]):
  (List[TypeExpression], Int, List[(TypeExpression,TypeExpression)]) = {
    exprs match {
      case List() => (types,fresh,constraints)
      case e::exprRest =>
	val (typeExpr, fresh1, constraints1) = constraintGen(gamma, e, fresh)
	determineTupleTypes(exprRest, types :+ typeExpr, gamma, fresh1, constraints ++ constraints1)
    }
  }

  /**
   * Returns the constraints that need to be fulfilled in order to correclty
   * type check a pattern match. This means, each result expression must be of
   * the same type and each pattern must be the same type as the matched expression (TODO)
   */
  def checkClauses(clauses: List[(patterns.Pattern, expressions.Expression)],
		   gamma: Env,
		   scrut: expressions.Expression,
		   fresh: Int):
  (Int, TypeExpression, TypeExpression) = {

//    def checkClauses1(clauses: List[(patterns.Pattern, expressions.Expression)],
//		      gamma: Env,
//		      types: List[TypeExpression],
//		      fresh: Int,
//		      scrutTypeO: Option[TypeExpression],
//		      scrutExpr: expressions.Expression,
//		      typeExpr: Option[TypeExpression]):
//    (Int, TypeExpression, TypeExpression) = {
//      clauses match {
//
//	case List() =>
//	  typeExpr match {
//	    case None => throw new TypeError("TODO")
//	    case Some(t1) =>
//	      println("types: " + types)
//
//	      scrutTypeO match {
//		case None => throw new TypeError("No pattern matched TODO")
//		case Some(t) => (fresh,t,t1)
//	      }
//	  }
//
//	case h::r =>
//	  val (patType,fresh1) = getPatternType(h._1, fresh)
//	  val (scrutType,fresh2,constraints) = constraintGen(gamma, scrut, fresh1)
//	  val needToInferScrutType: Boolean = typeExpr match {
//	    case None => true
//	    case Some(_) => false
//	  }
//	  try {
//	    // compare current pattern and scrutinee in order to update
//	    // the type environment
//	    val s = unify((patType,scrutType)::constraints)
//	    val inferredScrutType = scrutType.subst(s)
//	    val (gamma1,f,_) = putPatternIntoEnv(gamma, h._1, inferredScrutType, fresh2)
//	   // val (gamma2,fresh3,cs) = putPatternIntoEnv(gamma1, h._1, patType, f)
//	    val (clauseType,fresh4,cs1) = constraintGen(gamma1, h._2, f)
//	    val subst = unify((clauseType,scrutType)::cs1)
//	    val typeExprSubst = scrutType.subst(subst)
//	    // pattern match succeeded, we can infer the type of scrut, if we need to
//	    if (needToInferScrutType) {
//	      checkClauses1(r,gamma1,typeExprSubst::types, fresh4, Some(inferredScrutType), scrutExpr, Some(clauseType))
//	    } else {
//	      checkClauses1(r,gamma1,typeExprSubst::types, fresh4, scrutTypeO, scrutExpr, typeExpr)
//	    }
//	  } catch {
//	    case err: TypeError =>
//	      if (needToInferScrutType) {
//		checkClauses1(r, gamma, types, fresh2, None, scrutExpr, None)
//	      } else {
//		// already found a valid match
//		checkClauses1(r, gamma, types, fresh2, scrutTypeO, scrutExpr, typeExpr)
//	      }
//	  }
//      }
//    }

    if (clauses.isEmpty) {
      throw new TypeError("No clause found.")
    }

    var types:Set[TypeExpression] = Set()
    var scruttTypeInferred = false
    var firstScrutType: Option[TypeExpression] = None

    for (clause <- clauses) {
      // find out the type the pattern may match against
      val (patType,fresh1) = getPatternType(clause._1, fresh)
//      val (gamma1, fresh2, constraints1) = putPatternIntoEnv(gamma,clause._1,patType,fresh1)
      // determine the type of the scrutinee
      val (scrutType,fresh3,constraints2) = constraintGen(gamma, scrut, fresh1)
      // TODO: if pattern and scrut aren't unifiable -> pattern match doesn't succeed
      // but we should continue to test the remaining patterns: needs more testing
      try {
	// try to unify the types of the pattern and the scrutinee
	println("patType: " + scrutType)
	val s = unify((patType,scrutType)::constraints2) //constraints1 ++ constraints2)
	// pattern match succeeded, we can infer the type of scrut
	val inferredScrutType = scrutType.subst(s)
	println("scrutType: " + inferredScrutType)
	if (!scruttTypeInferred) {
	  scruttTypeInferred = true
	  firstScrutType = Some(inferredScrutType)
	}
	println("gamma: " + gamma)
	val (gamma1,fresh2) = putExpressionIntoEnv(gamma,scrut,inferredScrutType,fresh1)
	println("gamma1: " + gamma1)
	val (gamma2,fresh3) = putPatternIntoEnv(gamma1, clause._1, inferredScrutType, fresh2)
//	val (gamma1,f,_) = putPatternIntoEnv(gamma, , inferredScrutType, fresh2)
//	val (gamma,f,_) = putPatternIntoEnv(gamma1, clause._1, patType, fresh2)

	println("gamma2: " + gamma2)
	//println("inferredScrutType: " +  inferredScrutType)
	// val (gamma2,fresh3,cs) = putPatternIntoEnv(gamma1,c._1,patType,fresh2)
//	  println("gamma2: " + gamma2)
	val (clauseType,fresh4,cs1) = constraintGen(gamma2,clause._2,fresh3)

	types += clauseType
      } catch {
	// pattern match failure, we can't check the right hand sidef of the clause,
	// but we need to check the other clauses
	case err: TypeError => // checkClauses1(r, gamma, List(), fresh2, None, scrut, None)
      }
    }

    println(types)
    var constraints = List[(TypeExpression,TypeExpression)]()
    val tHead = types.head
    for (t <- types.tail) {
      constraints =  (tHead,t)::constraints
    }
    println(constraints)
    try {
      val s = unify(constraints)
      println("s " + s)
      val tpes = types map (t => t.subst(s))

      println("tpes " + tpes)

      for (t <- tpes) {
	if (t != tpes.head) {
	  throw new TypeError("Pattern clauses return values differ. TODO")
	}
      }

      if (scruttTypeInferred) {
	val Some(s) = firstScrutType
	(fresh,s,types.head)
      } else {
	throw new TypeError("TODO")
      }
    } catch {
      // TODO: cleanup
      case _ => throw new TypeError("Pattern clauses return values differ. TODO")
    }
  }

  /**
   * Updates the type enviroment based on the given pattern and the given type.
   * It is assumed that the given type matches the pattern.
   *
   * The new type environment together with the fresh variable are returned.
   */
  def putPatternIntoEnv(env: Env, pattern: patterns.Pattern,
			typeExpr: TypeExpression, fresh: Int): (Env,Int) = {
    // we don't generate any constraints here; if the pattern doesn't
    // match the given type, we found a pattern match error 
    pattern match {
      case patterns.Id(x) =>
	val scheme = (List(),typeExpr)
        (update(env,x,scheme),fresh)
      case patterns.Underscore => (env,fresh)
      case patterns.Nil  => (env,fresh)
      case patterns.Integer(_) =>
	if (typeExpr != TypeInt()) {
	  throw new TypeError("Int Match failed TODO")
	} else {
	  (env,fresh)
	}
      case patterns.Bool(_) =>
	if (typeExpr != TypeBool()) {
	  throw new TypeError("Boolean Match failed TODO")
	} else {
	  (env,fresh)
	}
      case patterns.Character(_) =>
	if (typeExpr != TypeChar()) {
	  throw new TypeError("Character Match failed TODO")
	} else {
	  (env,fresh)
	}
      case tup@patterns.Tuple(pats@_*) =>
	typeExpr match {
	  case TypeTuple(types@_*) =>
	    if (types.length != pats.length) {
	      throw new TypeError("Match failed TODO")
	    } else {
	      var gamma = env
	      var currFresh = fresh
	      var i = 0
	      
	      for (pat <- pats) {
		val (gamma1,fresh1) = putPatternIntoEnv(gamma,pat,types(i),currFresh+1)
		gamma = gamma1
		currFresh = fresh1
		i += 1
	      }
	      
	      (gamma,currFresh)
	    }
	  case _ => throw new TypeError("Match failed TODO")
	}
      case cons@patterns.Cons(head, tail) =>
	typeExpr match {
	  case TypeList(a) => 
	    val (env1, fresh1) = putPatternIntoEnv(env, head, a, fresh+1)
	    val (env2,fresh2) = putPatternIntoEnv(env1, tail, TypeList(a), fresh1)
	    (env2,fresh2)
	  case _ => throw new TypeError("List Pattern match failed TODO")
	}
      case _ => throw new TypeError("Unknown pattern TODO")
    }
  }

  /**
   * Updates the type enviroment based on the given pattern and the expression.
   * It is assumed that the given type matches the expression.
   *
   * The new type environment together with the fresh variable are returned.
   */
  def putExpressionIntoEnv(gamma: Env, expr: expressions.Expression,
			   tpe: TypeExpression, fresh: Int): (Env,Int) = {
    
    expr match {
      case expressions.Id(n) =>
	(update(gamma, n, (List[TypeVariable](), tpe)), fresh)
      case expressions.Nil =>
	(gamma,fresh)
      case expressions.Integer(_) =>
	(gamma,fresh)
      case expressions.Bool(_) =>
	(gamma,fresh)
      case expressions.Character(_) =>
	(gamma,fresh)
      case expressions.IfThenElse(cond, ifTrue, ifFalse) =>
	val(gamma1,fresh1) = putExpressionIntoEnv(gamma,ifTrue,tpe,fresh)
	putExpressionIntoEnv(gamma1,ifFalse,tpe,fresh1)
      case expressions.Let(pattern, definition, body) =>
	putExpressionIntoEnv(gamma,body,tpe,fresh)
      case expressions.LetRec(body, patDef) =>
	putExpressionIntoEnv(gamma,body,tpe,fresh)
      case expressions.BinOp(op, expr1, expr2) =>
	val(gamma1,fresh1) = putExpressionIntoEnv(gamma,expr1,tpe,fresh)
	putExpressionIntoEnv(gamma1,expr2,tpe,fresh1)
      case expressions.UnOp(op, e) =>
	putExpressionIntoEnv(gamma,e,tpe,fresh)
      case expressions.App(func, param) =>
	(gamma,fresh)
      case expressions.Cons(head, tail) =>
	tpe match {
	  case TypeList(a) =>
	    val (gamma1,fresh1) = putExpressionIntoEnv(gamma,head,a,fresh)
	    putExpressionIntoEnv(gamma1,tail,tpe,fresh1)
	}
      case t1@expressions.Tuple(exprs@_*) =>
	println("t1 : " + t1)
	tpe match {
	  case TypeTuple(types@_*) =>
	    var currGamma = gamma
	    var currFresh = fresh
	    for ((e,t) <- exprs zip types) {
	      println("e: " + e)
	      println("t: " + t)
	      val (gamma1, fresh1) = putExpressionIntoEnv(currGamma,e,t,currFresh)
	      currGamma = gamma1
	      currFresh = fresh1
	    }
	    (currGamma,currFresh)
	}

      case expressions.Record(defs) =>
	// can't infer any type information
	(gamma,fresh)
      case expressions.Field(record, name) =>
	// can't infer any type information
	(gamma,fresh)
      case expressions.Match(scrutinee, clauses) =>
	putExpressionIntoEnv(gamma, scrutinee, tpe, fresh)
      case expressions.Lambda(body, arguments) =>
	// can functions be pattern matched? guess not..
	throw new TypeError("TODO")
    }
  }

  /**
   * Returns the type of the given pattern. For constructing the types
   * the current fresh variable must be passed.
   */
  def getPatternType(pattern: patterns.Pattern, fresh: Int): (TypeExpression,Int) = {
    pattern match {
      case patterns.Id(x) =>
	(TypeVariable(fresh),fresh+1)
      case patterns.Underscore =>
	(TypeVariable(fresh),fresh+1)
      case patterns.Nil  =>
	(TypeList(TypeVariable(fresh)), fresh+1)
      case patterns.Integer(_) =>
	(TypeInt(), fresh)
      case patterns.Bool(_) =>
	(TypeBool(), fresh)
      case patterns.Character(_) =>
	(TypeChar(), fresh)
      case patterns.Tuple(pats@_*) =>
	var currFresh = fresh
	var types: List[TypeExpression] = List()
	for (pat <- pats) {
	  val (patType,fresh1) = getPatternType(pat, currFresh)
	  currFresh = fresh1
	  types = types :+ patType
	}
	(TypeTuple(types:_*),currFresh)
      case patterns.Cons(head, tail) =>
	(TypeList(TypeVariable(fresh)),fresh+1)
    }
  }

  def spec(sigma: TypeScheme, fresh: Int): (TypeExpression, Int) = {
    val alphas = sigma._1
    val t = sigma._2
    val s = (fresh to (fresh + alphas.length) toList).map(x => TypeVariable(x)).zip(alphas)
    (t.subst(s), fresh + alphas.length)
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
