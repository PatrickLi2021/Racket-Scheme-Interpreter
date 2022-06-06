/* Data Definition:
       A raw program is a string/text representing a Rackette program. It is
       represented as a string

   Example Data:
       "(+ 3 5)"", "(lambda (x) (+ x 1) 4)", "5", "(define y 17) (let ((y 3)) (+ y 7))",
       "(let ((x 0)) (let ((f (lambda (a) (* x a)))) (let ((x 1)) (f 5))))""
   */

type rawProgram = string;

/* Data Definition:
       concreteProgramPieces are the elements that comprise a concreteProgram in
       Rackette. They can be one of the following:
         NumberC(int)
         SymbolC(string)
         ListC(list(concreteProgramPiece))
       Nothing else is a concreteProgramPiece

   Example Data:
       NumberC(5), SymbolC("+"), ListC([NumberC(5), SymbolC("+"), NumberC(4)])
   */

type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/* Data Definition:
    A concreteProgram is a list of concreteProgramPieces that is produced as a
    result of reading in the text/raw program.

   Example Data:
    [SymbolC(+), NumberC(4), NumberC(3)],
    [SymbolC("+"), NumberC(4), ListC([NumberC(5), SymbolC("+"), NumberC(4)])]
   */

type concreteProgram = list(concreteProgramPiece);

/* a Rackette name */
/* Data Definition:
    A name is of type Name(string) and it can represent things such as variable
    names and symbols (i.e. built-ins) in Rackette

   Example Data:
    Name("x"), Name("+"), Name("*"), Name("/")
   */

type name =
  | Name(string);

/* Data Definition:
      An expression is any one of the several things defined below. Among other
      things, it can be a number-expression, a name-expression, and 
      if-expression, or a cond-expression.

   Example Data:
      NumE(5), BoolE(true), NameE(Name("x")), AndE(BoolE(true), BoolE(false))
   */

type expression =
  | NumE(int)
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData))
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))

/*
   Data Definition:
    ifData contains 3 items: a boolean expression
    (boolExpr) that is the initial condition in an if statement,
    an expression that's evaluated if the boolean expression is true (trueExpr),
    and an expression that's evaluated if the bool expression is false
    (falseExpr)

   Example Data:
    {boolExpr: ApplicationE([NameE(Name("number?")), NumE(78)]),
     trueExpr: NumE(124), falseExpr: BoolE(true)},
    {boolExpr: ApplicationE([NameE(Name("zero?")), BoolE(false)]),
     trueExpr: NumE(234), falseExpr: BoolE(true)}
 */

and ifData = {
  boolExpr: expression,
  trueExpr: expression,
  falseExpr: expression,
}

/*
   Data Definition:
    condData contains 2 elements for every cond branch:
    a conditional, conditionExpr, which is an expression that
    is either true or false, and an expression that is the
    result of the conditional evaluating to true, resultExpr.

   Example Data:
    {conditionExpr: ApplicationE([NameE(Name("zero?")), NumE(5)]),
     resultxpr: NumE(3)},
    {conditionExpr: ApplicationE([NameE(Name("number?")), BoolE(true)]),
     resultExpr: ApplicationE([NameE(Name("+")), NumE(5), NumE(6)])}
 */

and condData = {
  conditionExpr: expression,
  resultExpr: expression,
}

/*
   Data Definition:
    lambdaData contains 2 elements: a list of names, nameList,
    which is the formal arguments in the lambda expression, and an
    expression, lambdaBody, that is the body of the expression.

   Example Data:
    {nameList: [Name("y"), Name("y"), Name("z")],
     lambdaBody: ApplicationE([NameE(Name("-")), NameE(Name("x")),
    NameE(Name("g")), NameE(Name("z"))])}
    {nameList: [Name("ssag"), Name("h")],
    lambdaBody: ApplicationE([NameE(Name("/")), NameE(Name("x")),
     NameE(Name("yodel")])}
 */

and lambdaData = {
  nameList: list(name),
  lambdaBody: expression,
}

/*
   Data Definition:
    a letPair is contains a name, pairName, that is a
    name in a let binding, followed by an expression, pairExpr, that
    is bound to the name preceding it in the record

   Example Data:
    {pairName: Name("t"), pairExpr: NumE(6)},
    {pairName: Name("q"), pairExpr: BoolE(false)},
 */

and letPair = {
  pairName: name,
  pairExpr: expression,
}

/*
   Data Definition:
    letData is has 2 elements: a list of letPairs,
    letPairs, which is a list of let bindings,
    and the body of the let expression, letBody, which is an expression

   Example Data:
    {letPairs: [{pairName: Name("g"), pairExpr: BoolE(false)}],
     letBody: NumE(623),
    {letPairs: [{pairName: Name("d"), pairExpr: NumE(323)}],
     letBody: ApplictionE([NameE(Name("-")), NumE(53), NameE(Name("x"))])}
 */

and letData = {
  letPairs: list(letPair),
  letBody: expression,
};

/* a Rackette definition */
/* Data Definition:
      A definition contains the keyword define, followed by a name and an
      expression, all enclosed in parentheses.

   Example Data:
      (Name("x"), NumE(5)), (Name("y"), BoolE(true))
   */
type definition = (name, expression);

/* Data Definition:
      An abstractProgramPiece is a piece of Rackette that can be processed.
      It corresponds to either a definition to be added to the top-level
      environment or a Rackette expression

   Example Data:
      Definition(Name("x"), NumE(5)), Expression(BoolE(false))
   */

type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* Data Definition:
      An abstractProgram is an internal representation of a Rackette program as 
      a list of abstractProgramPieces, one per concreteProgramPiece

   Example Data:
      [Definition(Name("x"), NumE(5)), Expression(BoolE(false))]
   */
type abstractProgram = list(abstractProgramPiece);

/* a Rackette value: the result of evaluating a Rackette expression */

/* Data Definition:
      A value is the result of evaluating a Rackette expression. It can be a
      name value, boolean value, list value, among others.

   Example Data:
      NumV(5), BoolV(true), ListV([NumV(4), BoolV(false)]),
      BuiltinV((Name("+"), BuiltinV({bName: "<builtin-proc-+", bProc: plus})))
   */

type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
and builtinData = {
  bName: string,
  bProc: list(value) => value,
}

/*
   Data Definition:
    closureData has 3 items: a list of names in the
    closure expression, cNameList, an expression that's the body of
    the closure, cExpr, and the environment when the closure
    is evaluated.

   Example Data:
    {cNameList: [Name("e"), Name("d")],
     cExpr: NumE(123), cEnv: [(Name("hello"), NumV(6)), (Name("f"), NumV(6))],
    {cNameList: [Name("cig")]
     cExpr: BoolE(false), cEnv: [(Name("y"), NumV(123))]}
 */

and closureData = {
  cNameList: list(name),
  cExpr: expression,
  cEnv: environment,
}
/* Environments and bindings aren't values
   But we use "and" here so bindings have access to values
   and closures have access to environments */

/*
   Data Definition:
    An environment is a list of bindings, each of which contains a name and
    value pair. An environment in Rackette can either be the top-level
    environment or the local environment.

    Example Data: [(Name("x"), NumV(6)), (Name("y"), BoolV(true))],
                  [(Name("y"), ListV([NumV(6), NumV(7)]))]
 */

and environment = list(binding)

/*
   Data Definition:
    A binding is a name and value pair that is added to an environment in
    Rackette.

   Example Data: (Name("y"), NumV(6)), (Name("u"), BoolV(true))
 */

and binding = (name, value);