
package typeinference

object TypeInference {
  
  import parser.ast._
  import parser.ast.types._

  // make subst method available for lists and tuples 
  implicit def enrichList(xs: List[(TypeExpression,TypeExpression)]) =
    new SubstList(xs)
  implicit def enrichTuple(t: (TypeExpression,TypeExpression)) =
    new SubstTuple(t)

  class SubstList(xs: List[(TypeExpression,TypeExpression)]) 
  extends Subst[List[(TypeExpression,TypeExpression)]] {
    def subst_(s: (TypeExpression,TypeExpression)) = 
      xs.map { x => x.subst_(s) }
  }

  class SubstTuple(t: (TypeExpression,TypeExpression))
  extends Subst[(TypeExpression,TypeExpression)] {
    def subst_(s: (TypeExpression,TypeExpression)) = 
      (t._1.subst_(s), t._2.subst_(s))
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
   * A constraint is a relation between two types.
   * We also carry the expression along, which caused a constraint to be
   * generated.
   */
  type Constraint = (expressions.Expression, (TypeExpression,TypeExpression))

  /**
   * Represents a substituion where one type can be replaced by another.
   */
  type Substitution = (TypeExpression,TypeExpression)

  /**
   * Represents a typing error.  Will be thrown whenever an expression
   * isn't typed correctly.
   */
  case class TypeError(err: String) extends Exception(err)

  /**
   * Represents a unification error.  Will be thrown whenever constraints
   * aren't unifiable. An error message as well as the expression that
   * caused the the error are passed with the exception.
   */
  case class UnificationError(err: String, expr: expressions.Expression)
  extends Exception(err)

  /**
   * Returns an empty type environment.
   */
  def emptyEnv() = Map[String,TypeScheme]()

  /**
   * Top-level typecheck method, specifically for tests.
   * Given a type environment and an expression returns the type of the given
   * expression.
   */
  def typeCheckTest(gamma: Env, expr: expressions.Expression): TypeScheme = {
    val (typeExpr, _, constraints, _) = constraintGen(gamma, expr, 1)
    generalise(gamma, typeExpr, constraints)
  }

  /**
   * Top-level typecheck method. Given a type environment and an expression
   * returns the type of the given expression and the type environment.
   */
  def typeCheck(gamma: Env, expr: expressions.Expression): (TypeScheme,Env) = {
    val (typeExpr, _, constraints, gamma1) = constraintGen(gamma, expr, 1)
    val scheme = generalise(gamma, typeExpr, constraints)
    (scheme,gamma1)
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
    (List[TypeVariable]() /: env) ((tvs, nt) => tvs union freeVars(nt._2))

  /**
   * Performs unification. Given a list of constraints returns a list of
   * substitutions.
   */
  def unify(constraints: List[Constraint]): List[Substitution] = {
    // local function
    constraints match {
      case List() => List()
      case (e, (t1, t2))::u =>
	if (t1 == t2) { unify(u) }
	else {
	  (t1,t2) match {
	    case (al@TypeVariable(a), t) =>
	      if (freeVars(t).contains(al)) {
		throw UnificationError("ERROR: Occurs check failed in expression: ", e) }
	      else {
		val rest = unify(u.map(_._1).zip(u.map(_._2).subst_((t,al))))
		(t,al)::rest
	      }
	    case (t, al@TypeVariable(a)) =>
	      unify(((e,(al,t))::u))
	    case (t1@TypeConstructor(n1,params1@_*),t2@TypeConstructor(n2,params2@_*)) =>
	      if (params1.length != params2.length) {
		throw UnificationError("ERROR: Couldn't unify types: \n" + "\t\t" + t1 + "\n" + "\t\t" + t2 + " in expression: ", e)
	      } else if (params1.length == 0) {
		// base types without any type parameters
		if (n1 == n2) {
		  unify(u)
		} else {
		  throw UnificationError("ERROR: Couldn't unify types: \n" + "\t\t" + t1 + "\n" + "\t\t" + t2 + " in expression: ", e)
		}
	      } else {
		unify((params1 zip params2).map((e,_)).toList ++ u)
	      }
	    case _ =>
	      throw UnificationError("ERROR: Couldn't unify types: \n" + "\t\t" + t1 + "\n" + "\t\t" + t2 + " in expression: ", e)
	  }
	}
    }
  }

  /**
   * Generalises the given type expression.
   */
  def generalise[A](gamma: Env, typeExpr: TypeExpression, 
		    constraints: List[Constraint]): TypeScheme = {
    val subst = unify(constraints)
    val typeExprNew = typeExpr.subst(subst)
    val alphas = freeVars(typeExprNew) filterNot (freeVars(gamma) contains)
    (alphas.distinct,typeExprNew)
  }

  /**
   * Updates the given environment, such that given identifier will be
   * associated with the given type scheme.
   */
  def update(gamma: Env, id: String, scheme: TypeScheme): Env =
    gamma + (id -> scheme)

  /**
   * Given a type environment, a type expression and a counter, that's used
   * for creating new type variables, generates a type expression together
   * with the necessary constraints.
   */
  def constraintGen(gamma: Env, expr1: expressions.Expression, fresh: Int):
  (TypeExpression,Int,List[Constraint],Env) = {

    // make sure the expression is in curried form
    val expr = curry(expr1)
    
    expr match {

      // base types
      case expressions.Integer(_) => (TypeInt(), fresh, List(),gamma)
      case expressions.Bool(_) => (TypeBool(), fresh, List(),gamma)
      case expressions.Character(_) => (TypeChar(), fresh, List(),gamma)

      case expressions.Id(x) =>
        val scheme = lookup(gamma, x)
	val (typeExpr,fresh1) = instantiate(scheme, fresh)
	(typeExpr,fresh1,List(),gamma)

      case expressions.Sequence(_,expr2) =>
	// we might test here whether expr1 is of type Unit and if so
	// print a warning
	val (typeExpr2, freshNew, constraints,gamma1) = constraintGen(gamma,expr2, fresh)
	(typeExpr2,freshNew,List(),gamma1)

      case e@expressions.Tuple(expr@_*) =>5
	val (tupleTypes,freshNew,constraints) = getTupleTypes(expr.toList, gamma, fresh)
	(TypeTuple(tupleTypes:_*),freshNew,constraints map ((e,_)),gamma)

	// tuple element access, indexing starts at 0
      case expressions.TupleElem(expr, nr) =>
	val (typeExpr, freshNew,constraints,gamma1) = constraintGen(gamma, expr, fresh)
	// unify here to find out the type of the expression
	val s = unify(constraints)
	val typeTup = typeExpr.subst(s)
	typeTup match {
	  case TypeTuple(tupleTypes@_*) =>
	    try {
	      (tupleTypes(nr),freshNew,constraints,gamma1)
	    } catch {
	      case err: IndexOutOfBoundsException =>
		throw TypeError("ERROR: Tuple " + typeTup + " only constists of " + tupleTypes.length + " elements, "
				+ "tried to access element nr " + nr + ".")
	    }
	  case _ => throw TypeError("ERROR: Couldn't match expected tuple type against type " + typeTup + ".")
	}

      case e@expressions.UnOp(op, ex) =>
	val (t, f, c,_) = constraintGen(gamma, ex, fresh)
	op match {
	  case expressions.UnaryOperator.neg => (t, f, (e,(TypeInt(), t)) :: c,gamma)
	  case expressions.UnaryOperator.not => (t, f, (e,(TypeBool(), t)) :: c,gamma)
	  case _ => throw TypeError("ERROR: Unknown unary operator " + op + ".")
	}
	
      case e@expressions.IfThenElse(e1, e2, e3) =>
	val (t1, fresh1, c1,_) = constraintGen(gamma, e1, fresh)
	val (t2, fresh2, c2,_) = constraintGen(gamma, e2, fresh1)
	val (t3, fresh3, c3,_) = constraintGen(gamma, e3, fresh2)
	(t2, fresh3, (e,(t2,t3)) :: (e, (TypeBool(),t1)) :: c1 ++ c2 ++ c3,gamma)

      case e@expressions.BinOp(op, e1, e2) =>
	val (t1, fresh1, c1,_) = constraintGen(gamma, e1, fresh)
	val (t2, fresh2, c2,_) = constraintGen(gamma, e2, fresh1)
	op match {
	  case expressions.BinaryOperator.and | expressions.BinaryOperator.or =>
	    (TypeBool(),fresh2,
	     (e,(t1,TypeBool()))::(e,(t2,TypeBool()))::(e,(t1,t2))::c1 ++ c2,gamma)
	  case expressions.BinaryOperator.eq | expressions.BinaryOperator.neq
	    | expressions.BinaryOperator.geq | expressions.BinaryOperator.leq
	    | expressions.BinaryOperator.gr | expressions.BinaryOperator.le =>
	    (TypeBool(), fresh2, (e,(t1,t2))::c1 ++ c2,gamma)
	  case _ => 
	    (t1,fresh2,(e,(t1,t2))::c1 ++ c2,gamma)
	}

      case e@expressions.Lambda(body, p) =>
	val (patternType,fresh1) = getPatternType(p,fresh)
        val (gamma1,fresh2,cs) = putPatternIntoEnv(gamma, p, patternType, fresh1)
	val (typeBody,fresh3,constraints,_) = constraintGen(gamma1, body, fresh2)
	(TypeFn(patternType,typeBody), fresh3, constraints ++ (cs map ((e,_))),gamma)

        // anonymous record definition like in e.g.
        // let x = { one=Integer(1), id=Lamda(Id("x"),patterns.Id("x")) }
      case e@expressions.Record(defs@_*) =>
        val (fields,fresh1,constraints) = getRecordFieldTypes(List(), gamma, fresh, defs:_*)
	(TypeRecord("anonymous", fields:_*),fresh1,constraints map ((e,_)),gamma)

        // field access
      case expressions.Field(expr, fieldId) =>
        // generate type for recExpr
        val (typeExpr,fresh1,constraints,gamma1) = constraintGen(gamma, expr, fresh)
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
	      (matchedFields.head._2, fresh1, constraints,gamma1)
	    }
	  case _ => throw TypeError("ERROR: No record " + expr + " found.")
	}

      case e@expressions.Cons(hd,rst) =>
	constraintGen(gamma, hd, fresh) match {
	  case (te1,fresh1,constraints1,_) =>
	    constraintGen(gamma, rst, fresh1) match {
	      case (te2,fresh2,constraints2,_) =>
		(TypeList(te1), fresh2,(e,(te2,TypeList(te1)))::constraints1++constraints2,gamma)
	    }
	}

	// empty list
      case expressions.Nil =>
	(TypeList(TypeVariable(fresh)), fresh+1, List(),gamma)

      case e@expressions.App(e1,e2) =>
	val (typeE1, fresh1, constraints,_) = constraintGen(gamma, e1, fresh+1)
	val (typeE2, fresh2, constraints1,_) = constraintGen(gamma, e2, fresh1)
	val alpha = TypeVariable(fresh)
	(alpha,fresh2,(e,(typeE1, TypeFn(typeE2,alpha)))::(constraints++constraints1),gamma)

      case expressions.Let(patterns.Id(x),expr,body) =>
	val (typeExpr,fresh1,constraints,_)  = constraintGen(gamma,expr,fresh)
	val scheme = generalise(gamma,typeExpr,constraints)
	val gamma1 = update(gamma,x,scheme)
	constraintGen(gamma1, body, fresh1)

      case e@expressions.Let(p@patterns.Alternative(p1, p2), expr, body) =>
	val (typeExpr,fresh1,constraints1,_) = constraintGen(gamma,expr,fresh)
	val (gamma1,fresh2,constraints2) = putPatternIntoEnv(gamma, p, typeExpr, fresh1)
	val (tbody,fresh3,constraints3,_) = constraintGen(gamma1, body, fresh2)
	(tbody,fresh3, constraints1 ++ (constraints2 map ((e,_))) ++ constraints3,gamma)

	// ignore matched pattern
      case expressions.Let(patterns.Underscore,expr,body) =>
	constraintGen(gamma,body,fresh)

      case e@expressions.Let(p@patterns.Cons(head,rest), expr, body) =>
	val (typeExpr,fresh1,constraints1,_) = constraintGen(gamma,expr,fresh)
	val (gamma1,fresh2,constraints2) = putPatternIntoEnv(gamma, p, typeExpr, fresh1)
	val (tbody,fresh3,constraints3,_) = constraintGen(gamma1, body, fresh2)
	(tbody,fresh3, constraints1 ++ (constraints2 map ((e,_))) ++ constraints3,gamma)

	// ignore constraints for type of expr
	// it only has to match a list
      case e@expressions.Let(patterns.Nil, expr, body) =>
	val (typeExpr,fresh1,constraints1,_) = constraintGen(gamma,expr,fresh)
	val (typeBody, fresh2, constraints2,_) = constraintGen(gamma, body, fresh1)
	(typeBody, fresh2+1,
	 (e,(typeExpr,TypeList(TypeVariable(fresh2))))::constraints1 ++ constraints2,gamma)


      case e@expressions.Let(p@patterns.Tuple(_@_*), expr, body) =>
	val (typeExpr, fresh1, constraints1,_) = constraintGen(gamma,expr,fresh)
	val (patType, freshNew) = getPatternType(p, fresh1)
	val (gamma1,fresh2,cs) = putPatternIntoEnv(gamma, p, patType, freshNew)
	val (typeBody, fresh3, constraints2,_) = constraintGen(gamma1, body, fresh2)
	patType match {
	  case TypeTuple(fields@_*) =>
	    (typeBody,fresh3 + fields.length,
	     (e,(patType,typeExpr))::constraints1++constraints2 ++ (cs map ((e,_))),gamma1)
	  case _ => throw TypeError("ERROR: Couldn't match expected tuple type in expression " + e + ".")
	}

      case e@expressions.Let(p@patterns.Record(pats@_*), expr, body) =>
	val (typeExpr,fresh1,constraints1,_) = constraintGen(gamma, expr, fresh)
	val (gamma1,fresh2,constraints2) = putPatternIntoEnv(gamma, p, typeExpr, fresh1)
	val (typeBody,fresh3,constraints3,_) = constraintGen(gamma1, body, fresh2)
	typeExpr match {
	  case TypeRecord(_,fields@_*) =>
	    // extract field names
	    val fieldNames = fields map (f => f._1)
	    val newVars = fresh3 to fresh3 + fields.length map (i => TypeVariable(i))
	    (typeBody,fresh3 + fields.length + 1,
	     (e,(typeExpr,TypeRecord("anonymous",fieldNames zip newVars:_*)))::constraints1 
	     ++ (constraints2 map ((e,_))) ++ constraints3,gamma1)
	  case _ => throw TypeError("Couldn't match record type.")
	}

      case e@expressions.Let(patterns.Integer(_), expr, body) =>
	val (typeExpr,fresh1,constraints,_) = constraintGen(gamma,expr,fresh)
	(TypeInt(), fresh1, (e,(typeExpr,TypeInt()))::constraints,gamma)

      case e@expressions.Let(patterns.Bool(_), expr, body) =>
	val (typeExpr,fresh1,constraints,_) = constraintGen(gamma,expr,fresh)
	(TypeBool(), fresh1, (e, (typeExpr,TypeBool()))::constraints,gamma)

      case e@expressions.Let(patterns.Character(_), expr, body) =>
	val (typeExpr,fresh1,constraints,_) = constraintGen(gamma,expr,fresh)
	(TypeChar(), fresh1, (e,(typeExpr,TypeChar()))::constraints,gamma)
	    
      case e@expressions.Match(scrut, clauses@_*) =>
	// TODO: check if clauses are non-empty
	// all clauses must match the same type
	val (scrutType, fresh1, constraints,_) = constraintGen(gamma, scrut, fresh)
	val (fresh2, scrutType1,typeExpr,cs) = checkClauses(e, gamma, scrutType,
							    constraints map ((_._2)), fresh1)
	(typeExpr, fresh2, (e, (scrutType,scrutType1))::cs++constraints,gamma)

      case expressions.LetRec(body, funs@_*) =>
	// put function names into type environment
	val (gamma1, fresh1) = handleLetRecExpressions(gamma, fresh, funs.toList)
	constraintGen(gamma1, body, fresh1)
    }
  }

  /**
   *
   */
  def handleLetRecExpressions(gamma: Env, fresh: Int,
			      bodies: List[(patterns.Id, expressions.Expression)]): (Env,Int)= {
    var currFresh = fresh
    var currGamma = gamma
    // generate new type variables for each expression on the right hand side
    for (f <- bodies) {
      val funName = f._1.name
      currGamma = update(currGamma,funName,(List(), TypeVariable(currFresh)))
      currFresh += 1
    }

    var allConstraints = List[Constraint]()
    for (f <- bodies) {
      val funName = f._1.name
      val funBody = f._2
      val (funType,freshNew,constraints,_)  = constraintGen(currGamma, funBody, currFresh)
      // collect generated constraints
      allConstraints = constraints ++ allConstraints
      // now update type
      val scheme = generalise(currGamma, funType,
			      (funBody,(currGamma(funName)._2,funType))::allConstraints)
      currGamma  = update(currGamma, funName, scheme)
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
  def getRecordFieldTypes(fields: List[(String,TypeExpression)],
			  gamma: Env, fresh: Int, recordFields: (expressions.Id,expressions.Expression)*):
  (List[(String,TypeExpression)],Int,List[(TypeExpression,TypeExpression)]) = {

    def getRecordFieldTypesLoc(constraints: List[(TypeExpression,TypeExpression)],
			       fields: List[(String,TypeExpression)], gamma: Env, fresh: Int,
			       recordFields: (expressions.Id,expressions.Expression)*):
    (List[(String,TypeExpression)],Int,List[(TypeExpression,TypeExpression)]) = {
      if (recordFields.isEmpty) {
	(fields, fresh, constraints)
      } else {
	val recField = recordFields.head
	val  (typeexpr, fresh1, constraints1,_) = constraintGen(gamma, recField._2, fresh)
	val constraintsNew: List[(TypeExpression,TypeExpression)] = constraints union (constraints1 map (_._2))
	getRecordFieldTypesLoc(constraintsNew, fields :+ (recField._1.name, typeexpr),
			       gamma, fresh1, recordFields.tail:_*)
      }
    }

    getRecordFieldTypesLoc(List(), fields, gamma, fresh, recordFields:_*)
  }

  /**
   * Returns the types that appear within a tuple
   */
  def getTupleTypes(exprs: List[expressions.Expression],
		    gamma: Env, fresh: Int):
  (List[TypeExpression], Int, List[(TypeExpression,TypeExpression)]) = {

    // local function
    def getTupleTypesLoc(express: List[expressions.Expression],
		       types: List[TypeExpression], env: Env, freshVar: Int,
		       constraints: List[(TypeExpression,TypeExpression)]):
    (List[TypeExpression], Int, List[(TypeExpression,TypeExpression)]) = {
      express match {
	case List() => (types,freshVar,constraints)
	case expr::exprRest =>
	  val (typeExpr, fresh1, constraints1,_) = constraintGen(env, expr, freshVar)
	  getTupleTypesLoc(exprRest, types :+ typeExpr, env, fresh1, constraints ++ (constraints1 map ((_._2))))
      }
    }

    getTupleTypesLoc(exprs, List(), gamma, fresh, List())
  }

  /**
   * Returns the constraints that need to be fulfilled in order to correclty
   * type check a pattern match. This means, each result expression must be of
   * the same type and each pattern must be the same type as the matched expression.
   * 
   */
  def checkClauses(matchExpr: expressions.Match,
		   gamma: Env, scrutType: TypeExpression,
		   constr: List[(TypeExpression,TypeExpression)],
		   fresh: Int):
  (Int, TypeExpression, TypeExpression, List[(expressions.Expression, (TypeExpression,TypeExpression))]) = {

    val clauses = matchExpr.clauses.toList
    val scrut = matchExpr.scrutinee

    if (clauses.isEmpty) {
      throw TypeError("No clause found.")
    }

    var types:Set[TypeExpression] = Set()
    var scruttTypeInferred = false // flag that signalizes whether a pattern match has succeeded
    var firstScrutType: Option[TypeExpression] = None
    var constraints = List[Constraint]()
    var freshNew = fresh
    
    for (clause <- clauses) {
      // find out the type the pattern may match against
      val (patType,fresh1) = getPatternType(clause._1, freshNew)
    
      try {
	val e = clause._2
	// try to unify the types of the pattern and the scrutinee
	val s = unify((e, (patType,scrutType))::(constr map ((scrut,_))))
	// pattern match succeeded, we can infer the type of scrut
	val inferredScrutType = scrutType.subst(s)
	if (!scruttTypeInferred) {
	  scruttTypeInferred = true
	  firstScrutType = Some(inferredScrutType)
	}
	val (gamma2,fresh2,cs) = putPatternIntoEnv(gamma, clause._1, inferredScrutType, fresh1)
	val (clauseType,fresh3,cs1,_) = constraintGen(gamma2,clause._2,fresh2)
	constraints = (e,(scrutType,inferredScrutType))::(e,(patType,inferredScrutType))::cs1++ constraints ++ (cs map ((e,_)))
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
      constraints =  (scrut,(tHead,t))::constraints
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
	(freshNew,st,types.head,constraints ++ (constr map ((scrut,_))))
      } else {
	throw TypeError("ERROR: Pattern match failure.")
      }
    } catch {
      case _ => throw UnificationError(
	  "ERROR: Pattern clauses return values differ in expression: ", matchExpr)
    }
  }

  /**
   * Updates the type enviroment based on the given pattern and the given type.
   * It is assumed that the given type matches the pattern.
   *
   * The new type environment together with the fresh variable and possibly
   * generated constraints are returned. Note that the constraints
   * do not carry an expression along.
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
	      throw TypeError("ERROR: Tuple pattern match failed.\nType: " + typeExpr + ", pattern: " + tup + " .")
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
	      val gamma2 = update(gamma1,pat._1.name,(List(),types(i)))
	      constraints = constraints ++ cs
	      currGamma = gamma2
	      currFresh = fresh1
	      i += 1
	    }
	    println(currGamma)
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
   * Look up the given id in the given type environment and return
   * the type scheme
   */
  def lookup(gamma: Env, id: String): TypeScheme = {
    gamma find {t => t._1 == id} match {
      case Some((id1,scheme)) => scheme
      case None => throw TypeError("ERROR: Variable " + id + " not bound.")
    }
  }

  /**
   * Returns the position of the given label within the given record.
   * Indexing of fields starts at 0. This function is meant for use within the
   * code generation phase.
   */
  def getRecordLabelPos(gamma: Env, recExpr: expressions.Expression,
			label: expressions.Id): Int = {
    // local method
    def findLabel(fields: List[(String,TypeExpression)],
		  labelName: String, cnt: Int): Int = {
      fields match {
	case List() =>
	  throw TypeError("ERROR: No field named " + label.name + " in record " + recExpr + ".")
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
    // the fresh variable here
    val (typeExpr,_,constraints,_) = constraintGen(gamma, recExpr, 1)
    val s = unify(constraints)
    val recType = typeExpr.subst(s)
    recType match {
      case TypeRecord(n,fields@_*) =>
	findLabel(fields.toList, label.name, 0)
      case _ =>
	throw TypeError("ERROR: Expression " + recExpr + " is not a record.")
    }
  }
}
