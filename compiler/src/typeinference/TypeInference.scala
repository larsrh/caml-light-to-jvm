
package typeinference

object TypeInference {
  
  import parser.ast._
  import parser.ast.types._

  // make subst method available for lists and tuples 
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
   * Represents a typing error.  Will be thrown whenever an expression
   * isn't typed correctly.
   */
  case class TypeError(err: String) extends Exception(err)

  /**
   * Represents a unification error.  Will be thrown whenever constraints
   * aren't unifiable.
   */
  case class UnificationError(err: String) extends Exception(err)

  /**
   * Returns an empty type environment.
   */
  def emptyEnv() = Map[String,TypeScheme]()

  /**
   * Top-level typecheck method. Given a type environment and an expression
   * returns the type of the given expression.
   */
  def typeCheck(gamma: Env, expr: expressions.Expression): (TypeScheme,Env) = {
    val (typeExpr, _, constraints) = constraintGen(gamma, expr, 1)
    val scheme = generalise(gamma, typeExpr, constraints)
    (scheme,gamma)
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
                if (freeVars(t).contains(al)) { throw UnificationError("Occurs check failed.") }
                else {
                  val rest = unify_(u.subst_((t,al)))
                  (t,al)::rest
                }
              case (t, al@TypeVariable(a)) =>
		unify_((al,t)::u)
              case (t1@TypeConstructor(n1,params1@_*),t2@TypeConstructor(n2,params2@_*)) =>
                if (params1.length != params2.length) {
                  throw UnificationError("ERROR: Couldn' unify types: \n" + "\t\t" + t1 + "\n" + "\t\t" + t2)
                } else if (params1.length == 0) {
		  // base types without any type parameters
		  if (n1 == n2) {
		    unify_(u)
		  } else {
		    throw UnificationError("Couldn't unify TODO")
		  }
		} else {
                  unify_(params1.zip(params2).toList ++ u)
                }
              case _ => throw UnificationError("TODO: unify - reasonable error message")
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

    // make sure the expression is in curried form
    val expr = curry(expr1)
    
    expr match {

      // base types
      case expressions.Integer(_) => (TypeInt(), fresh, List())
      case expressions.Bool(_) => (TypeBool(), fresh, List())
      case expressions.Character(_) => (TypeChar(), fresh, List())

      case expressions.Sequence(_,expr2) =>
	// TODO: we might test here whether expr1 is of type Unit and if so
	// TODO: print a warning
	val (typeExpr2, freshNew, constraints) = constraintGen(gamma,expr2, fresh)
	(typeExpr2,freshNew,List())

	// tuple element access, indexing starts at 1
      case expressions.TupleElem(expr, nr) =>
	val (typeExpr, freshNew, constraints) = constraintGen(gamma, expr, fresh)
	// unify here to find out the type of the expression
	val s = unify(constraints)
	val typeTup = typeExpr.subst(s)
	typeTup match {
	  case TypeTuple(tupleTypes@_*) =>
	    try {
	      (tupleTypes(nr-1),freshNew,constraints)
	    } catch {
	      case err: IndexOutOfBoundsException =>
		throw TypeError("ERROR: Tuple " + typeTup + " only constists of " + tupleTypes.length + " elements, "
				+ "tried to access element nr " + nr + ".")
	    }
	  case _ => throw TypeError("ERROR: Couldn't match expected tuple type against type " + typeTup + ".")
	}

      case expressions.Id(x) =>
        val scheme = lookup(gamma, x)
	val (typeExpr,fresh1) = instantiate(scheme, fresh)
	(typeExpr,fresh1,List())

      case expressions.UnOp(op, e) =>
	val (t, f, c) = constraintGen(gamma, e, fresh)
	op match {
	  case expressions.UnaryOperator.neg => (t, f, (TypeInt(), t) :: c)
	  case expressions.UnaryOperator.not => (t, f, (TypeBool(), t) :: c)
	  case _ => throw TypeError("ERROR: Unknown unary operator " + op + ".")
	}
	
      case expressions.IfThenElse(e1, e2, e3) =>
	val (t1, fresh1, c1) = constraintGen(gamma, e1, fresh)
	val (t2, fresh2, c2) = constraintGen(gamma, e2, fresh1)
	val (t3, fresh3, c3) = constraintGen(gamma, e3, fresh2)
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

      case expressions.Lambda(body, p) =>
	val (patternType,fresh1) = getPatternType(p,fresh)
        val (gamma1,fresh2,cs) = putPatternIntoEnv(gamma, p, patternType, fresh1)
	val (typeBody,fresh3,constraints) = constraintGen(gamma1, body, fresh2)
	((TypeFn(patternType,typeBody), fresh3, constraints ++ cs))

        // anonymous record definition like in e.g.
        // let x = { one=Integer(1), id=Lamda(Id("x"),patterns.Id("x")) }
      case expressions.Record(defs@_*) =>
        val (fields,fresh1,constraints) = getRecordFieldTypes(List(), List(), gamma, fresh, defs:_*)
	(TypeRecord("anonymous", fields:_*),fresh1,constraints)

      case expressions.Tuple(expr@_*) =>5
	val (tupleTypes,freshNew,constraints) = getTupleTypes(expr.toList, gamma, fresh)
	(TypeTuple(tupleTypes:_*),freshNew,constraints)

        // field access
      case expressions.Field(expr, fieldId) =>
        // generate type for recExpr
        val (typeExpr,fresh1,constraints) = constraintGen(gamma, expr, fresh)
	val s = unify(constraints)
	val typeRecExpr = typeExpr.subst(s)
	// generated type expression should match a record type
	typeRecExpr match {
	  case TypeRecord(recName, fields@_*) =>
	    // lookup type of desired field
	    val matchedFields = fields filter {f => f._1 == fieldId.name}
	    if (matchedFields.length == 0) {
	      throw TypeError("ERROR: No such field " + fieldId.name + " in record " + recName + ".")
	    } else if (matchedFields.length > 1) {
	      // sanity check; we should never get here since records aren't
	      // allowed to conatain fields with a duplicate name
	      throw TypeError("ERROR: Duplicate field " + fieldId.name + " found in record " + recName + ".")
	    } else {
	      // return type
	      (matchedFields.head._2, fresh1, constraints)
	    }
	  case _ => throw TypeError("ERROR: No record " + expr + " found.")
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
	val (typeExpr,fresh1,constraints1) = constraintGen(gamma,expr,fresh)
	val (gamma1,fresh2,constraints2) = putPatternIntoEnv(gamma, p, typeExpr, fresh1)
	val (tbody,fresh3,constraints3) = constraintGen(gamma1, body, fresh2)
	(tbody,fresh3, constraints1 ++ constraints2 ++ constraints3)

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

      case expressions.Let(p@patterns.Tuple(_@_*), expr, body) =>
	val (typeExpr, fresh1, constraints1) = constraintGen(gamma,expr,fresh)
	val (patType, freshNew) = getPatternType(p, fresh1)
	val (gamma1,fresh2,cs) = putPatternIntoEnv(gamma, p, patType, freshNew)
	val (typeBody, fresh3, constraints2) = constraintGen(gamma1, body, fresh2)
	patType match {
	  case TypeTuple(fields@_*) =>
	    (typeBody,fresh3 + fields.length,(patType,typeExpr)::constraints1++constraints2 ++ cs)
	  case _ => throw TypeError("Couldn't match tuple type.")
	}

      case expressions.Let(p@patterns.Record(pats@_*), expr, body) =>
	val (typeExpr,fresh1,constraints1) = constraintGen(gamma, expr, fresh)
	val (gamma1,fresh2,constraints2) = putPatternIntoEnv(gamma, p, typeExpr, fresh1)
	val (typeBody,fresh3,constraints3) = constraintGen(gamma1, body, fresh2)
	typeExpr match {
	  case TypeRecord(_,fields@_*) =>
	    // extract field names
	    val fieldNames = fields map (f => f._1)
	    val newVars = fresh3 to fresh3 + fields.length map (i => TypeVariable(i))
	    (typeBody,fresh3 + fields.length + 1,
	     (typeExpr,TypeRecord("anonymous",fieldNames zip newVars:_*))::constraints1 ++ constraints2 ++ constraints3)
	  case _ => throw TypeError("Couldn't match record type.")
	}

      case expressions.Let(patterns.Integer(_), expr, body) =>
	val (typeExpr,fresh1,constraints) = constraintGen(gamma,expr,fresh)
	(TypeInt(), fresh1, (typeExpr,TypeInt())::constraints)

      case expressions.Let(patterns.Bool(_), expr, body) =>
	val (typeExpr,fresh1,constraints) = constraintGen(gamma,expr,fresh)
	(TypeBool(), fresh1, (typeExpr,TypeBool())::constraints)

      case expressions.Let(patterns.Character(_), expr, body) =>
	val (typeExpr,fresh1,constraints) = constraintGen(gamma,expr,fresh)
	(TypeChar(), fresh1, (typeExpr,TypeChar())::constraints)
	    
      case expressions.Match(scrut, clauses@_*) =>
	// TODO: check if clauses are non-empty
	// all clauses must match the same type
	val (scrutType, fresh1, constraints) = constraintGen(gamma, scrut, fresh)
	val (fresh2, scrutType1,typeExpr,cs) = checkClauses(clauses.toList, gamma, scrutType, constraints, scrut, fresh1)
	(typeExpr, fresh2, List((scrutType,scrutType1)) ++ cs ++ constraints)

      case expressions.LetRec(body, funs@_*) =>
	// put function names into type environment
	val (gamma1, fresh1) = putRecFunctionsIntoScope(gamma, fresh, funs.toList)
	constraintGen(gamma1, body, fresh1)
    }
  }

  // TODO: rename funs
  def putRecFunctionsIntoScope(gamma: Env, fresh: Int,
			       funs: List[(patterns.Id, expressions.Expression)]): (Env,Int)= {
    var currFresh = fresh
    var currGamma = gamma
    // generate new type variables for each expression on the right hand side
    for (f <- funs) {
      val funName = f._1.name
      currGamma = currGamma + (funName -> (List(), TypeVariable(currFresh))) //update(gamma,funName,(List(), TypeVariable(currFresh)))
      currFresh = currFresh + 1
    }

    var allConstraints = List[(TypeExpression,TypeExpression)]()
    for (f <- funs) {
      val funName = f._1.name
      val funBody = f._2
      val (funType,freshNew,constraints)  = constraintGen(currGamma, funBody, currFresh)
      // collect generated constraints
      allConstraints = constraints ++ allConstraints
      // now update type
      val scheme = generalise(currGamma, funType, (currGamma(funName)._2,funType)::allConstraints)
      currGamma  = currGamma + (funName -> scheme)
      currFresh = freshNew
    }

    (currGamma, currFresh)
  }

  /**
   * Curries the given function, e.g. when a lambda expression like
   *     Lambda(Id("x"), patterns.Id("x"), patterns.Id("y"))
   * is given, it is brought into shape
   *     Lambda(Lambda(patterns.Id("y"), Id("x"), patterns.Id("x"))
   */
  def curry(expr: expressions.Expression): expressions.Expression = {
    expr match {
      case fun@expressions.Lambda(_,arguments@_*) if arguments.length > 1=>
	((expressions.Lambda(fun.body, fun.arguments.last))
	 /: (fun.arguments.init.reverse)) ((l,p) => expressions.Lambda(l, p))
      case app@expressions.App(_,p@_*) if p.length > 1=>
	((expressions.App(app.func, app.param.head))
	 /: (app.param.tail)) ((a,p) => expressions.App(a, p))
      case _ => expr
    }
  }

  /**
   * Determines the types of the given records field and returns them
   * together with the generated constraints.
   */
  def getRecordFieldTypes(
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
	  getRecordFieldTypes(constraintsNew, fields :+ (recField._1.name, typeexpr),
			      gamma, fresh1, recordFields.tail:_*)
      }
    }
  }

  /**
   * Returns the types that appear within a tuple
   */
  def getTupleTypes(exprs: List[expressions.Expression],
		    gamma: Env, fresh: Int):
  (List[TypeExpression], Int, List[(TypeExpression,TypeExpression)]) = {

    // local function
    def getTupleTypes1(express: List[expressions.Expression],
		       types: List[TypeExpression], env: Env, freshVar: Int,
		       constraints: List[(TypeExpression,TypeExpression)]):
    (List[TypeExpression], Int, List[(TypeExpression,TypeExpression)]) = {
      express match {
	case List() => (types,freshVar,constraints)
	case expr::exprRest =>
	  val (typeExpr, fresh1, constraints1) = constraintGen(env, expr, freshVar)
	  getTupleTypes1(exprRest, types :+ typeExpr, env, fresh1, constraints ++ constraints1)
      }
    }

    getTupleTypes1(exprs, List(), gamma, fresh, List())
  }

  /**
   * Returns the constraints that need to be fulfilled in order to correclty
   * type check a pattern match. This means, each result expression must be of
   * the same type and each pattern must be the same type as the matched expression (TODO)
   */
  def checkClauses(clauses: List[(patterns.Pattern, expressions.Expression)],
		   gamma: Env, scrutType: TypeExpression,
		   constr: List[(TypeExpression,TypeExpression)],
		   scrut: expressions.Expression, fresh: Int):
  (Int, TypeExpression, TypeExpression, List[(TypeExpression,TypeExpression)]) = {

    if (clauses.isEmpty) {
      throw TypeError("No clause found.")
    }

    var types:Set[TypeExpression] = Set()
    var scruttTypeInferred = false // flag that signalizes whether a pattern match has succeeded
    var firstScrutType: Option[TypeExpression] = None
    var constraints = List[(TypeExpression,TypeExpression)]()
    var freshNew = fresh
    
    for (clause <- clauses) {
      // find out the type the pattern may match against
      val (patType,fresh1) = getPatternType(clause._1, freshNew)
     
      // TODO: if pattern and scrut aren't unifiable -> pattern match doesn't succeed
      // but we should continue to test the remaining patterns: needs more testing
      try {
	// try to unify the types of the pattern and the scrutinee
	val s = unify((patType,scrutType)::constr)
	// pattern match succeeded, we can infer the type of scrut
	val inferredScrutType = scrutType.subst(s)
	if (!scruttTypeInferred) {
	  scruttTypeInferred = true
	  firstScrutType = Some(inferredScrutType)
	}
	val (gamma2,fresh2,cs) = putPatternIntoEnv(gamma, clause._1, inferredScrutType, fresh1)
	val (clauseType,fresh3,cs1) = constraintGen(gamma2,clause._2,fresh2)
	constraints = (scrutType,inferredScrutType)::(patType,inferredScrutType)::cs1 ++ constraints ++ cs
	types += clauseType
	freshNew = fresh3
      } catch {
	// pattern match failure, we can't check the right hand sidef of the clause,
	// but we need to check the other clauses
	case unificationErr: UnificationError => throw unificationErr
	case typeErr: TypeError => throw typeErr
      }
    }

    // all types must be equal
    val tHead = types.head
    for (t <- types.tail) {
      constraints =  (tHead,t)::constraints
    }
    
    try {
      val s = unify(constraints)
      val tpes = types map (t => t.subst(s))

      for (t <- tpes) {
	if (t != tpes.head) {
	  throw TypeError("ERROR: Pattern clauses return values differ.")
	}
      }

      if (scruttTypeInferred) {
	val Some(st) = firstScrutType
	(freshNew,st,types.head,constraints ++ constr)
      } else {
	throw TypeError("ERROR: Pattern match failure.")
      }
    } catch {
      case _ => throw UnificationError("Pattern clauses return values differ. TODO")
    }
  }

  /**
   * Updates the type enviroment based on the given pattern and the given type.
   * It is assumed that the given type matches the pattern.
   *
   * The new type environment together with the fresh variable and possibly
   * generated constraints are returned.
   */
  def putPatternIntoEnv(env: Env, pattern: patterns.Pattern,
			typeExpr: TypeExpression, fresh: Int):
  (Env,Int,List[(TypeExpression,TypeExpression)]) = {
    pattern match {
      case patterns.Id(x) =>
	val scheme = (List(),typeExpr)
	(update(env,x,scheme),fresh,List())
      case patterns.Underscore =>
	(env,fresh,List())
      case patterns.Nil  =>
	(env,fresh,List())
      case patterns.Integer(_) =>
	(env,fresh,List((typeExpr,TypeInt())))
      case patterns.Bool(_) =>
	(env,fresh,List((typeExpr,TypeBool())))
      case patterns.Character(_) =>
	(env,fresh,List((typeExpr,TypeChar())))
      case tup@patterns.Tuple(pats@_*) =>
	typeExpr match {
	  case TypeTuple(types@_*) =>
	    if (types.length != pats.length) {
	      throw TypeError("ERROR: Tupel pattern match failed.\nType: " + typeExpr + ", pattern: " + tup + " .")
	    } else {
	      var gamma = env
	      var currFresh = fresh
	      var i = 0
	      var constraints: List[(TypeExpression,TypeExpression)] = List()
	      for (pat <- pats) {
		val (gamma1,fresh1,cs) = putPatternIntoEnv(gamma,pat,types(i),currFresh+1)
		constraints = constraints ++ cs
		gamma = gamma1
		currFresh = fresh1
		i += 1
	      }
	      (gamma,currFresh,constraints)
	    }
	  case _ => throw TypeError("Match failed TODO")
	}
      case rec@patterns.Record(defs@_*) =>
	var constraints: List[(TypeExpression,TypeExpression)] = List()
	typeExpr match {
	  case TypeRecord(name, fields@_*) =>
	    var currGamma = env
	    var currFresh = fresh
	    var i = 0
	    val types = fields.map(_._2)
	    var constraints: List[(TypeExpression,TypeExpression)] = List()
	    for (pat <- defs) {
	      val (gamma1,fresh1,cs) = putPatternIntoEnv(currGamma,pat._2,types(i),currFresh+1)
	      constraints = constraints ++ cs
	      currGamma = gamma1
	      currFresh = fresh1
	      i += 1
	    }
	    (currGamma,currFresh,constraints)
	  case _ => throw TypeError("Match failed TODO")
	}
      case cons@patterns.Cons(head, tail) =>
	typeExpr match {
	  case TypeList(a) =>
	    val (env1, fresh1,constraints1) = putPatternIntoEnv(env, head, a, fresh)
	    val (env2,fresh2,constraints2) = putPatternIntoEnv(env1, tail, TypeList(a), fresh1)
	    (env2,fresh2,constraints1 ++ constraints2)
	  case _ => throw TypeError("List Pattern match failed TODO")
	}
      case _ => throw TypeError("Unknown pattern TODO")
    }
  }

  /**
   * Returns the type of the given pattern. For constructing the types
   * the current fresh variable must be passed.
   */
  def getPatternType(pattern: patterns.Pattern, fresh: Int): (TypeExpression,Int) = {
    pattern match {
      case patterns.Id(x) =>
	// TODO: if x is already in type environment: overwrite (we may not forget to restore it afterwards!)
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
      case patterns.Alternative(pat1, pat2) =>
	val (t1,fresh1) = getPatternType(pat1,fresh)
	val (t2,fresh2) = getPatternType(pat2,fresh1)
	// check whether types are compatible
	if (t1 == t2) {
	  (t1, fresh2)
	} else {
	  throw TypeError("ERROR: Types in alternative pattern match differ.")
	}
      case patterns.Record(defs@_*) =>
	var currFresh = fresh
	var types: List[TypeExpression] = List()
	for (pat <- defs) {
	  val (patTypes,fresh1) = getPatternType(pat._2, currFresh)
	  currFresh = fresh1
	  types = types :+ patTypes
	}
	(TypeRecord("anonymous",defs.map(_._1.name).zip(types):_*),currFresh)
    }
  }

  /**
   * Instantiates the given type scheme.
   */
  def instantiate(sigma: TypeScheme, fresh: Int): (TypeExpression, Int) = {
    val alphas = sigma._1
    val t = sigma._2
    val s = (fresh to (fresh + alphas.length) toList).map(x => TypeVariable(x)).zip(alphas)
    (t.subst(s), fresh + alphas.length)
  }

  /**
   * Looks up the given id in the given type environment and returns
   * it type scheme
   */
  def lookup(gamma: Env, id: String): TypeScheme = {
    gamma find {t => t._1 == id} match {
      case Some((id1,scheme)) => scheme
      case None => throw TypeError("ERROR: Variable " + id + " not bound.")
    }
  }

  /**
   * Returns the position of the given label within the given record.
   * Indexing starts at 0. This function is meant for use in the code
   * generation phase.
   */
  def getRecordLabelPos(gamma: Env, recExpr: expressions.Expression, label: expressions.Id): Int = {
    // local method
    def findLabel(fields: List[(String,TypeExpression)], labelName: String, cnt: Int): Int = {
      fields match {
	case List() => throw TypeError("ERROR: No field named " + label.name + " in record " + recExpr + ".")
	case (fieldDef::rest) =>
	  val fieldName = fieldDef._1
	  val fieldlType = fieldDef._2
	  if (labelName == fieldName) {
	    cnt
	  } else {
	    findLabel(rest,labelName,cnt+1)
	  }
      }
    }

    // we don't care about the type environment nor
    // the constraints or the fresh variable here
    val (recType,_,_) = constraintGen(gamma, recExpr, 1)
    recType match {
      case TypeRecord(n,fields@_*) => findLabel(fields.toList, label.name, 0)
      case _ => throw TypeError("ERROR: Expression " + recExpr + " is not a record.")
    }
  }
}
