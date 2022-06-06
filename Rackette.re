open Types;
open CS17SetupRackette;
open Read.Reader;

/*
  Input:  A list of individual strings, alos
  Output: A single string that contains each of the individual strings in the
          input list separated by a space

  Recursion Diagrams:
    Original Input: ["a", "b"]
    Recursive Input: ["b"]

    Ideation Space: Add first elem in list as a string to recursive output

    Recursive Output: "b"
    Original Output: "a b"

    Original Input: ["x"]
    Recursive Input: []

    Ideation Space: If list only contains 1 string, return that string

    Recursive Output: ""
    Original Output: "x"
 */

let rec listToString: list(string) => string =
  alos =>
    switch (alos) {
    | [] => ""
    | [hd] => hd
    | [hd, ...tl] => hd ++ " " ++ listToString(tl)
    };

/*
  Input:  Two environments, env1 and env2, that each contain a list of name and
          value bindings
  Output: A single environment that contains a list of all the bindings in env1
          followed by the bindings in env2
 */

let extendEnv: (environment, environment) => environment =
  (env1, env2) => List.append(env1, env2);

/* Input:  A value, vl, that is produced as a result of evaluating a given
              abstractProgram
      Output: A string that represents the final output of the raw program
              provided as input to Rackette

      Recursion Diagrams:
        Original Input: NumV(5)
        Recursive Input: N/A

        Ideation Space: If the value that is passed in is a number value, then
        simply return its argument

        Recursive Output: N/A
        Original Output: "5"

        Original Input: ListV([NumV(5), NumV(6), NumV(7)])
        Recursive Input: ListV(NumV(6), NumV(7))

        Ideation Space: Call stringOfValue helper procedure on the first elem in
        the original list and add that on to the recursive output

        Recursive Output: "6 7"
        Original Output: "5 6 7"
   */

let rec stringOfValue: value => string =
  vl =>
    switch (vl) {
    | NumV(num) => string_of_int(num)
    | BoolV(arg) => string_of_bool(arg)
    | ListV([]) => "(list)"
    | ListV([hd, ...tl]) =>
      "(list " ++ listToString(List.map(stringOfValue, [hd, ...tl])) ++ ")"
    | ClosureV(_) => "<User-defined procedure>"
    | BuiltinV(builtinData) => "builtin: " ++ builtinData.bName
    };

/*
   Input:  A list of values, alon, that consists of 2 number values (NumV)
           to be added together
   Output: A number value representing the sum of the two number values
           passed in as input. If input is not a list containing 2 number
           values, return error
 */

let plus: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: + expects 2 arguments, but found none")
    | [NumV(_)] => failwith("Error: + expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => NumV(arg1 + arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | [NumV(_), NumV(_), ..._] =>
      failwith("Error: + expects only 2 arguments, but found more than 2")
    | _ => failwith("Error: + expects 2 number arguments, did not find them")
    };

/*
   Input:  A list of values, alon, that consists of 2 number values (NumV) to
           be subtracted
   Output: A number value representing the difference of the two number values
           passed in as input. If input is not a list containing 2 number
           values, return error
 */

let minus: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: - expects 2 arguments, but found none")
    | [NumV(_)] => failwith("Error: - expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => NumV(arg1 - arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | [NumV(_), NumV(_), ..._] =>
      failwith("Error: - expects only 2 arguments, but found more than 2")
    | _ => failwith("Error: - expects 2 number arguments, did not find them")
    };

/*
   Input:  A list of values, alon, that consists of 2 number values (NumV) to
           be multiplied together
   Output: A number value representing the product of the two number values
           passed in as input. If input is not a list containing 2 number
           values, return error
 */

let mult: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: * expects 2 arguments, but found none")
    | [NumV(_)] => failwith("Error: * expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => NumV(arg1 * arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | [NumV(_), NumV(_), ..._] =>
      failwith("Error: * expects only 2 arguments, but found more than 2")
    | _ => failwith("Error: * expects 2 number arguments, did not find them")
    };

/*
   Input:  A list of values, alon, that consists of 2 number values (NumV) to
           be divided
   Output: A number value representing the quotient of the two number values
           passed in as input. If input is not a list containing 2 number
           values, return error
 */

let divide: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: / expects 2 arguments, but found none")
    | [NumV(_)] => failwith("Error: / expects 2 arguments, but found only 1")
    | [NumV(_), NumV(0)] => failwith("Error: division by 0")
    | [NumV(arg1), NumV(arg2)] => NumV(arg1 / arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | [NumV(_), NumV(_), ..._] =>
      failwith("Error: / expects only 2 arguments, but found more than 2")
    | _ => failwith("Error: / expects 2 number arguments, did not find them")
    };

/* Input:  A list of values, alon, that consists of 2 number values (NumV)
      Output: A number value representing the remainder of dividing the first
              number value by the second number value
   */

let remainder: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: remainder expects 2 arguments, but found none")
    | [NumV(_)] =>
      failwith("Error: remainder expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => NumV(arg1 mod arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | [NumV(_), NumV(_), ..._] =>
      failwith(
        "Error: remainder expects only 2 arguments, but found more than 2",
      )
    | _ =>
      failwith(
        "Error: remainder expects 2 number arguments, did not find them",
      )
    };

/*
  Input:  A list of values, alon, that consists of 2 number values (NumV)
  Output: A boolean value, BoolV(false), if the arguments of the 2 initial
          number values are not the same, and BoolV(true) if they are not the
          same.
 */

let equal: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: = expects 2 arguments, but found none")
    | [NumV(_)] => failwith("Error: = expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => BoolV(arg1 == arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | [_, _] => failwith("Error: = expects 2 numbers")
    | _ =>
      failwith("Error: = expects only 2 arguments, but found more than 2")
    };

/*
   Input:  A list of values, alon, that consists of 2 number values (NumV)
   Output: A boolean value, BoolV(true), if the first argument is less than
           the second argument and BoolV(false) if the first argument is not
           less than the second argument.
 */

let lessThan: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: < expects 2 arguments, but found none")
    | [NumV(_)] => failwith("Error: < expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => BoolV(arg1 < arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | _ =>
      failwith("Error: < expects only 2 arguments, but found more than 2")
    };

/*
   Input:  A list of values, alon, that consists of 2 number values (NumV)
   Output: A boolean value, BoolV(true), if the first argument is greater than
           or equal to the second argument and BoolV(false) if the first
           argument is not greater than the second argument.
 */

let greaterThan: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: > expects 2 arguments, but found none")
    | [NumV(_)] => failwith("Error: > expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => BoolV(arg1 > arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | _ =>
      failwith("Error: > expects only 2 arguments, but found more than 2")
    };

/*
   Input:  A list of values, alon, that consists of 2 number values (NumV)
   Output: A boolean value, BoolV(true), if the first argument is less than
           or equal to the second argument and BoolV(false) if the first
           argument is not less than or equal to the second argument.
 */

let lessThanOrEqualTo: list(value) => value =
  alon =>
    switch (alon) {
    | [] => failwith("Error: <= expects 2 arguments, but found none")
    | [NumV(_)] =>
      failwith("Error: <= expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => BoolV(arg1 <= arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | _ =>
      failwith("Error: <= expects only 2 arguments, but found more than 2")
    };

/*
  Input:  A list of values, alon, that consists of 2 number values (NumV)
  Output: A boolean value, BoolV(true), if the first argument is greater than
          or equal to the second argument and BoolV(false) if the first
          argument is not greater than or equal to the second argument.
 */

let greaterThanOrEqualTo: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: >= expects 2 arguments, but found none")
    | [NumV(_)] =>
      failwith("Error: >= expects 2 arguments, but found only 1")
    | [NumV(arg1), NumV(arg2)] => BoolV(arg1 >= arg2)
    | [NumV(_), _] => failwith("Error: Second argument was not a number")
    | [_, NumV(_)] => failwith("Error: First argument was not a number")
    | _ =>
      failwith("Error: >= expects only 2 arguments, but found more than 2")
    };

/*
   Input:  A list of values, alov, that consists of 2 elements of type value
   Output: A boolean value, BoolV(true), if the first element of the list is
           equal to the second element and BoolV(false) if they are not equal.
 */

let isEqual: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: equal? expects 2 arguments, but found none")
    | [_] => failwith("Error: equal? expects 2 arguments, but found only 1")
    | [arg1, arg2] => BoolV(arg1 == arg2)
    | _ =>
      failwith(
        "Error: equal? expects only 2 arguments, but found more than 2",
      )
    };

/*
   Input:  A list of values, alov, that consists of a single element of type
           value.
   Output: A boolean value, BoolV(true), if the element is a NumV(arg) where
           arg is any integer and BoolV(false) otherwise
 */

let isNumber: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: number? expects 1 argument, but found none")
    | [NumV(_)] => BoolV(true)
    | [_] => BoolV(false)
    | _ =>
      failwith(
        "Error: number? expects only 1 argument, but found more than 1",
      )
    };

/*
   Input:  A list of values, alov, that consists of a single element of type
           value.
   Output: A boolean value, BoolV(true), if the element is NumV(0) where and
           BoolV(false) otherwise
 */

let isZero: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: zero? expects 1 argument, but found none")
    | [NumV(0)] => BoolV(true)
    | [NumV(_)] => BoolV(false)
    | [_] => failwith("Error: zero? expects a number and didn't find one")
    | [_, ..._] =>
      failwith("Error: zero? expects only 1 argument, but found more than 1")
    };

/*
   Input:  A list of values, alov, that consists of an element of type value
           followed by an element of type ListV
   Output: A value of type ListV(arg) with the first element added on to it on
           the left end.
 */

let cons: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: cons expects 2 arguments, but found none")
    | [_] => failwith("Error: cons expects 2 arguments, but found only 1")
    | [arg1, ListV([])] => ListV([arg1])
    | [arg1, ListV([hd, ...tl])] => ListV([arg1, hd, ...tl])
    | _ =>
      failwith("Error: cons expects only 2 arguments, but found more than 2")
    };

/*
   Input:  A list of values, alov, that consists of a value of type ListV(arg),
           where arg is a list of arbitrary elems of the same type
   Output: The first element of the input list of values
 */

let first: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: first expects a non-empty list")
    | [ListV([hd, ..._])] => hd
    | _ => failwith("Error: first expects a non-empty list")
    };

/*
   Input:  A list of values, alov, that consists of an element of type
           ListV(arg) where arg is another element of type ListV
   Output: A value of type ListV(arg) containing all but the first element
 */

let rest: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: rest expects a non-empty list")
    | [ListV([_])] => ListV([])
    | [ListV([_, ...tl])] => ListV(tl)
    | _ => failwith("Error: rest expects a non-empty list")
    };

/*
   Input:  A list of values, alov, consisting of an element of type ListV that
           contains another ListV as an argument.
   Output: A boolean value, BoolV(true), if the arguments to the input ListV
           are empty and a boolean value, BoolV(false), if the arguments to
           the input ListV are not empty
 */

let isEmpty: list(value) => value =
  alov =>
    switch (alov) {
    | [ListV([])] => BoolV(true)
    | [NumV(_), ..._] =>
      failwith("Error: empty? expects a list of values as input")
    | [BoolV(_), ..._] =>
      failwith("Error: empty? expects a list of values as input")
    | [] => failwith("Error: empty? expects a list of values as input")
    | _ => BoolV(false)
    };

/*
   Input:  A list of values, alov, that consists of an element of type ListV
   Output: A boolean value, BoolV(true), if the input list is non-empty, and a
           boolean value, BoolV(false), if the input list is empty.
 */

let isCons: list(value) => value =
  alov =>
    switch (alov) {
    | [ListV([_, ..._])] => BoolV(true)
    | [_] => BoolV(false)
    | _ =>
      failwith(
        "Error: cons? expects only 1 argument, but found more than 1 argument",
      )
    };

/*
   Input:  A list of values, alov, that consists of a boolean value
   Output: A boolean value, BoolV(true), if the argument of the input boolean
           value evaluted to false and a boolean value, BoolV(false), if the
           argument of the input boolean evaluated to true.
 */

let opposite: list(value) => value =
  alov =>
    switch (alov) {
    | [] => failwith("Error: not expects 1 argument, but found none")
    | [NumV(_), ..._] => failwith("Error: not expects either true or false")
    | [ListV(_), ..._] => failwith("Error: not expects either true or false")
    | [BoolV(arg)] =>
      if (arg == true) {
        BoolV(false);
      } else {
        BoolV(true);
      }
    | _ => failwith("Error: not expects either true or false")
    };

let initialTle: environment = [
  (Name("+"), BuiltinV({bName: "<builtin-proc-+", bProc: plus})),
  (Name("-"), BuiltinV({bName: "<builtin-proc--", bProc: minus})),
  (Name("*"), BuiltinV({bName: "<builtin-proc-*", bProc: mult})),
  (Name("/"), BuiltinV({bName: "<builtin-proc-/", bProc: divide})),
  (
    Name("remainder"),
    BuiltinV({bName: "<builtin-proc-remainder", bProc: remainder}),
  ),
  (Name("="), BuiltinV({bName: "<builtin-proc-=", bProc: equal})),
  (Name("<"), BuiltinV({bName: "<builtin-proc-<", bProc: lessThan})),
  (Name(">"), BuiltinV({bName: "<builtin-proc->", bProc: greaterThan})),
  (
    Name("<="),
    BuiltinV({bName: "<builtin-proc-<=", bProc: lessThanOrEqualTo}),
  ),
  (
    Name(">="),
    BuiltinV({bName: "<builtin-proc->=", bProc: greaterThanOrEqualTo}),
  ),
  (
    Name("equal?"),
    BuiltinV({bName: "<builtin-proc-equal?", bProc: isEqual}),
  ),
  (
    Name("number?"),
    BuiltinV({bName: "<builtin-proc-number?", bProc: isNumber}),
  ),
  (Name("zero?"), BuiltinV({bName: "<builtin-proc-zero?", bProc: isZero})),
  (Name("cons"), BuiltinV({bName: "<builtin-proc-cons", bProc: cons})),
  (Name("first"), BuiltinV({bName: "<builtin-proc-first", bProc: first})),
  (Name("rest"), BuiltinV({bName: "<builtin-proc-rest", bProc: rest})),
  (
    Name("empty?"),
    BuiltinV({bName: "<builtin-proc-empty?", bProc: isEmpty}),
  ),
  (Name("cons?"), BuiltinV({bName: "<builtin-proc-cons?", bProc: isCons})),
  (Name("not"), BuiltinV({bName: "<builtin-proc-not", bProc: opposite})),
];

/*
   Input:  A concreteProgramPiece, input, that represents the raw text
           inputted into Rackette. The concreteProgramPiece represents a
           Rackette expression.
   Output: An expression that represents the argument to an
           abstractProgramPiece

   Recursion Diagrams:

   Original Input: NumberC(5)
   Recursive Input: N/A

   Ideation Space: Wrap the argument of the input NumberC as a NumE

   Recursive Output: N/A
   Overall Output: NumE(5)


   Original Input: SymbolC("+")
   Recursive Input: N/A

   Ideation Space: Take the argument of the input NumberC and wrap it as a
   NameE

   Recursive Output: N/A
   Overall Output: NameE(Name("+"))
 */

let rec parseExpression: concreteProgramPiece => expression =
  input => {
    /*
       Input:  A list of concrete program pieces, listcPP, that represents the
               contents of a let expression in Rackette
       Output: A list of letPairs that is produced as a result of parsing the name
               and expression associated with each binding in the let
               expression
     */

    let rec parseLet: list(concreteProgramPiece) => list(letPair) =
      listcPP =>
        switch (listcPP) {
        | [ListC([])] => []
        | [ListC([SymbolC(sym), arg])] => [
            {pairName: Name(sym), pairExpr: parseExpression(arg)},
          ]
        | [ListC([SymbolC(sym), arg]), ...tl] => [
            {pairName: Name(sym), pairExpr: parseExpression(arg)},
            ...parseLet(tl),
          ]
        | _ =>
          failwith(
            "Error: let expression expects a series of bindings followed by"
            ++ "expression, but did not find them",
          ) //CHANGED BEFORE FINAL REGRADE
        };

    /*
       Input:  A list of concrete program pieces, listcPP, that represents the
               contents of a cond expression in Rackette
       Output: A list of condData that is produced as a result of parsing the
               the conditional and the result in the cond expression
     */

    let rec parseCond: list(concreteProgramPiece) => list(condData) =
      listcPP =>
        switch (listcPP) {
        | [] => []
        | [ListC([pred, result])] => [
            {
              conditionExpr: parseExpression(pred),
              resultExpr: parseExpression(result),
            },
          ]
        | [ListC([pred, result]), ...tl] => [
            {
              conditionExpr: parseExpression(pred),
              resultExpr: parseExpression(result),
            },
            ...parseCond(tl),
          ]
        | _ =>
          failwith(
            "Error: Expected a clause with a question and"
            ++ "answer, but did not find both",
          ) //CHANGED BEFORE FINAL REGRADE
        };

    /*
       Input:  A list of concrete program pieces, listcPP, that represents the
               contents of a lambda expression in Rackette
       Output: A list of names that represents the formal arguments in the lambda
               expression
     */

    let rec parseLambda: list(concreteProgramPiece) => list(name) =
      listcPP =>
        switch (listcPP) {
        | [] => []
        | [SymbolC(sym)] => [Name(sym)]
        | [SymbolC(sym), ...tl] => [Name(sym), ...parseLambda(tl)]
        | _ =>
          failwith(
            "Error: Lambda expression expected a list of"
            ++ "formal arguments, but did not find them",
          ) //CHANGED BEFORE FINAL REGRADE
        };

    switch (input) {
    | NumberC(num) => NumE(num)
    | SymbolC("true") => BoolE(true)
    | SymbolC("false") => BoolE(false)
    | SymbolC("empty") => EmptyE
    | SymbolC(sym) =>
      switch (sym) {
      | "define" 
      | "and"  
      | "if" 
      | "or"
      | "cond"
      | "lambda"
      | "let" => failwith("Error: reserved keyword")
      | _ => NameE(Name(sym))
      }
    | ListC([SymbolC("and"), arg1, arg2]) =>
      AndE(parseExpression(arg1), parseExpression(arg2))
    | ListC([SymbolC("and"), ..._]) =>
      failwith("Error: and did not have correct number of arguments")
    | ListC([SymbolC("or"), arg1, arg2]) =>
      OrE(parseExpression(arg1), parseExpression(arg2))
    | ListC([SymbolC("or"), ..._]) =>
      failwith("Error: or did not have correct number of arguments")
    | ListC([SymbolC("if"), pred, exp1, exp2]) =>
      IfE({
        boolExpr: parseExpression(pred),
        trueExpr: parseExpression(exp1),
        falseExpr: parseExpression(exp2),
      })
    | ListC([SymbolC("if"), ..._]) =>
      failwith("Error: if did not have correct number of arguments")
    | ListC([SymbolC("cond")]) =>
      failwith("Error: cond expected arguments after, but did not find any")
    | ListC([SymbolC("cond"), ListC([pred, result]), ...tl]) =>
      CondE([
        {
          conditionExpr: parseExpression(pred),
          resultExpr: parseExpression(result),
        },
        ...parseCond(tl),
      ])
    | ListC([
        ListC([SymbolC("lambda"), ListC([hd1, ...tl1]), arg1]),
        ...tl,
      ]) =>
      ApplicationE([
        LambdaE({
          nameList: parseLambda([hd1, ...tl1]),
          lambdaBody: parseExpression(arg1),
        }),
        ...List.map(parseExpression, tl),
      ])
    | ListC([SymbolC("let")]) =>
      failwith(
        "Error: let expects a list of pre-bindings and expressions, but did not find them",
      )
    | ListC([SymbolC("let"), ListC([]), arg]) =>
      LetE({letPairs: [], letBody: parseExpression(arg)})
    | ListC([SymbolC("let"), ListC(arg1), arg2]) =>
      LetE({letPairs: parseLet(arg1), letBody: parseExpression(arg2)})
    | ListC([SymbolC("lambda"), ListC(alod), arg]) =>
      LambdaE({
        nameList: parseLambda(alod),
        lambdaBody: parseExpression(arg),
      })
    | ListC([SymbolC("lambda"), ..._]) =>
      failwith(
        "Error: lambda expression did not have correct number of arguments",
      )
    | ListC([]) =>
      failwith("Error: empty set of parentheses is not a valid expression")
    | ListC(alod) => ApplicationE(List.map(parseExpression, alod))
    };
  };

/*
   Input:  A concreteProgramPiece, input, that represents a definition in
           Rackette
   Output: A definition that is a name and expression binding. The name in the
           binding comes from the second argument in the input ListC and the
           expression that is bound to it is the third argument
 */

let parseDefinition: concreteProgramPiece => definition =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), SymbolC(sym), arg2]) => 
      switch(sym) {
        | "true"
        | "false"
        | "if"
        | "and"
        | "or"
        | "empty" 
        | "lambda" 
        | "let" 
        | "cond"
        | "define" => failwith("Error: Cannot redefine a keyword")
        | _ => (Name(sym), parseExpression(arg2))
      };
    | ListC([_, NumberC(_), ..._]) =>
      failwith("Error: Expected a variable name, found a number")
    | ListC([_, ListC(_), ..._]) =>
      failwith("Error: Expected a variable name, found an expression")
    | _ => failwith("Error: Expected a variable name")
    };

/*
   Input:  A concreteProgramPiece, input, that represents a piece of the
           input program to be parsed
   Output: An abstractProgramPiece that is the result of parsing the input
           concreteProgramPiece inputted into the procedure
 */

let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

/*
   Input:  An element of type concreteProgram, input, that is a representation
           of the text the user types into Rackette as a concrete language
   Output: An abstractProgram that is either an element of type expression or
           type definition
 */

let parse: concreteProgram => abstractProgram =
  input => List.map(parsePiece, input);

/*
   Input:  An environment, env, and a name, n
   Output: An option('a), where the argument will always be of type value

    Recursion Diagrams:

    Original Input: ([(Name("x"), NumV(5)), (Name("y"), NumV(6))], Name("x"))
    Recursive Input: ([(Name("y"), NumV(6))], Name("x"))

    Ideation Space: If the name is not found in the environment in the
    recursive input, then return None. Otherwise, return Some(v)
    with the associated value

    Recursive Output: None
    Overall Output: Some(NumV(5))

    Original Input: ([(Name("y"), NumV(6))], Name("x"))
    Recursive Input: ([], Name("x"))

    Ideation Space: Use recursive output as the overall output

    Recursive Output: None
    Overall Output: None
 */

let rec lookup: (environment, name) => option(value) =
  (env, n) =>
    switch (env) {
    | [] => None
    | [(name, v), ...tail] =>
      if (n == name) {
        Some(v);
      } else {
        lookup(tail, n);
      }
    };

/*
   Input:  A list of names, alon
   Output: A boolean, true, if the input list of names contained all unique
           values, and false otherwise
 */

let uniqueListP: list(name) => bool =
  alon =>
    switch (alon) {
    | [] => true
    | [hd, ...tl] => !List.mem(hd, tl)
    };

/*
   Input:  A list of names, alon, and a list of values, alov, that represent
           the formal and actual arguments of a closure expression
   Output: An environment that contains name and value bindings produced as a
           result of combining each name and value from the input lists
 */

let rec addBindingsToClosure: (list(name), list(value)) => environment =
  (alon, alov) =>
    switch (alon, alov) {
    | ([], []) => []
    | ([], [_, ..._]) =>
      failwith("Error: Number of of formal arguments doesn't match")
    | ([_, ..._], []) =>
      failwith("Error: Number of of formal arguments doesn't match")
    | ([hd1, ...tl1], [hd2, ...tl2]) =>
      switch (alon) {
      | [] => []
      | [_, ..._] =>
        if (uniqueListP(alon) == true) {
          [(hd1, hd2), ...addBindingsToClosure(tl1, tl2)];
        } else {
          failwith("Error: Found a variable that is used more than once");
        }
      }
    };

/*
   Input:  An environment, env, and a binding, bnd
   Output: A new environment that adds the input binding to the input
           environment
 */

let addBinding: (environment, binding) => environment =
  (env, bnd) => [bnd, ...env];

/*
   Input:  Two environments, tle and env, that represent the top-level
           environment and the local environment respectively, and an
           expression to be evaluated
   Output: A value that represents the output of the input expression
           evaluated according to Rackette's rules of evaluation

   Recursion Diagrams:

    Original Input: NumE(5)
    Recursive Input: N/A

    Ideation Space: Take the argument of the original input and put it in a
    NumV wrapper

    Recursive Output: N/A
    Overall Output: NumV(5)

    Original Input: BoolE(true)
    Recursive Input: N/A

    Ideation Space: Take the argument of the original input and put it in a
    BoolV wrapper

    Recursive Output: N/A
    Overall Output: BoolV(true)
 */

let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) => {
    /*
       Input:  A list of letPairs that represents the bindings of a let
               expression and an empty environment for the let bindings to be
               stored in
       Output: An environment that contains name and value bindings produced
               as aresult of combining each name and value from the input lists
     */

    let rec addBindingsForLet: (environment, list(letPair)) => environment =
      (letEnv, alop) =>
        switch (letEnv, alop) {
        | (_, []) => []
        | (letEnv, [{pairName: n, pairExpr: expr}]) =>
          if (lookup(letEnv, n) == None) {
            addBinding(letEnv, (n, eval(tle, env, expr)));
          } else {
            failwith(
              "Error: A variable name was defined locally more than once",
            );
          }
        | (letEnv, [{pairName: n, pairExpr: expr}, ...tl]) =>
          if (lookup(letEnv, n) == None) {
            addBindingsForLet(
              addBinding(letEnv, (n, eval(tle, env, expr))),
              tl,
            );
          } else {
            failwith(
              "Error: A variable name was defined locally more than once",
            );
          }
        };

    /*
       Input:  2 environments, tle and env, that represent the current top-level
               and local environments, along with a list of expressions
       Output: A list of values that is produced as a result of evaluating each
               expression in the input list in the context of the 2 input
               environments
     */

    let rec evalArgs:
      (environment, environment, list(expression)) => list(value) =
      (tle, env, aloe) =>
        switch (aloe) {
        | [] => []
        | [hd, ...tl] => [eval(tle, env, hd), ...evalArgs(tle, env, tl)]
        };

    switch (expr) {
    | NumE(num) => NumV(num)
    | BoolE(bool) => BoolV(bool)
    | EmptyE => ListV([])
    | NameE(Name(arg)) =>
      switch (lookup(extendEnv(env, tle), Name(arg))) {
      | Some(v) => v
      | _ => failwith("Error: This variable/function is not defined")
      }
    | ApplicationE([NameE(Name(n)), ...tl]) =>
      switch (eval(tle, env, NameE(Name(n)))) {
      | BuiltinV({bName: _, bProc: proc}) => proc(evalArgs(tle, env, tl))
      | ClosureV({cNameList: alon, cExpr: expr, cEnv: closEnv}) =>
        eval(
          tle,
          extendEnv(
            addBindingsToClosure(alon, evalArgs(tle, env, tl)),
            closEnv,
          ),
          expr,
        )
      | _ => failwith("Error: Procedure was neither a built-in nor a closure")
      }

    | ApplicationE([LambdaE(arg), ...tl]) =>
      switch (eval(tle, env, LambdaE(arg))) {
      | ClosureV({cNameList: alon, cExpr: expr, cEnv: closEnv}) =>
        eval(
          tle,
          extendEnv(
            addBindingsToClosure(alon, evalArgs(tle, env, tl)),
            closEnv,
          ),
          expr,
        )
      | _ => failwith("Error: Procedure was not a builtin nor a closure")
      }
    | LambdaE({nameList: alon, lambdaBody: exp}) =>
      ClosureV({cNameList: alon, cExpr: exp, cEnv: env})
    | AndE(arg1, arg2) =>
      switch (eval(tle, env, arg1)) {
      | BoolV(false) => BoolV(false)
      | BoolV(true) =>
        switch (eval(tle, env, arg2)) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("Error: and expression has incorrect arguments")
        }
      | _ => failwith("Error: and expression has incorrect arguments")
      }
    | IfE({boolExpr: pred, trueExpr: yesExpr, falseExpr: noExpr}) =>
      switch (eval(tle, env, pred)) {
      | BoolV(_) =>
        if (eval(tle, env, pred) == BoolV(true)) {
          eval(tle, env, yesExpr);
        } else {
          eval(tle, env, noExpr);
        }
      | _ => failwith("Error: if question result was not true or false")
      }
    | OrE(arg1, arg2) =>
      switch (eval(tle, env, arg1)) {
      | BoolV(true) => BoolV(true)
      | BoolV(false) =>
        switch (eval(tle, env, arg2)) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("Error: or expression has incorrect arguments")
        }
      | _ => failwith("Error: or expression has incorrect arguments")
      }
    | CondE(listOfCondData) =>
      switch (listOfCondData) {
      | [{conditionExpr: arg1, resultExpr: arg2}] =>
        if (eval(tle, env, arg1) == BoolV(true)) {
          eval(tle, env, arg2);
        } else if (eval(tle, env, arg1) == BoolV(false)) {
          failwith("Error: All cond expressions resulted in false");
        } else {
          failwith("Error: predicate did not evaluate to a bool");
        }
      | [{conditionExpr: arg1, resultExpr: arg2}, ...tl] =>
        if (eval(tle, env, arg1) == BoolV(true)) {
          eval(tle, env, arg2);
        } else if (eval(tle, env, arg1) == BoolV(false)) {
          eval(tle, env, CondE(tl));
        } else {
          failwith("Error: cond expression did not evaluate properly");
        }
      | _ => failwith("Error: cond expression not found/formatted correctly")
      }
    | LetE({letPairs: alop, letBody: exp}) =>
      switch (alop) {
      | [] => eval(tle, env, exp)
      | [{pairName: _, pairExpr: _}] =>
        eval(tle, extendEnv(addBindingsForLet([], alop), env), exp)
      | [{pairName: _, pairExpr: _}, ..._] =>
        eval(tle, extendEnv(addBindingsForLet([], alop), env), exp)
      }
    | _ => failwith("Error: This expression is not defined in Rackette")
    };
  };

/*
   Input:  An environment, env, and a name and expression binding
   Output: An new environment that is produced as a result of attaching the
           input binding to the input environment.
 */

let addDefinition: (environment, (name, expression)) => environment =
  (env, (name, exp)) =>
    switch (lookup(env, name)) {
    | None => [(name, eval(initialTle, [], exp)), ...env]
    | Some(_) =>
      failwith(
        "Error: This name was defined previously and cannot be re-defined",
      )
    };

/*
   Input:  An abstractProgram, pieces, that represents a list of
           abstractProgramPieces to be evaluated
   Output: A list of values where each value in the list represents the result
           of evaluating each abstractProgramPiece in the input list
 */

let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

/*
   Input:  A rawProgram, program, which is a string that represents the text
           of a Rackette program
   Output: A list of strings that represents the output of the input string
           analagous to the output of the program in DrRacket
 */

let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

// Test Cases for listToString:
checkExpect(
  listToString(["hi", "my", "name"]),
  "hi my name",
  "3 elems in list",
);
checkExpect(listToString(["Brown"]), "Brown", "1 elem in list");

checkExpect(
  listToString(["a", "b", "c", "d"]),
  "a b c d",
  "4 elems in list",
);

// Test Cases for extendEnv
checkExpect(
  extendEnv([(Name("x"), NumV(5))], [(Name("y"), NumV(7))]),
  [(Name("x"), NumV(5)), (Name("y"), NumV(7))],
  "testing extendEnv",
);
checkExpect(
  extendEnv([(Name("x"), NumV(5))], []),
  [(Name("x"), NumV(5))],
  "testing extendEnv",
);
checkExpect(extendEnv([], []), [], "testing extendEnv");

// Test Cases for stringOfValue:
checkExpect(
  stringOfValue(ListV([NumV(45), NumV(5)])),
  "(list 45 5)",
  "list of numbers",
);
checkExpect(
  stringOfValue(ListV([BoolV(true), BoolV(false), BoolV(true)])),
  "(list true false true)",
  "list of booleans",
);
checkExpect(
  stringOfValue(
    ListV([ListV([BoolV(true), BoolV(false)]), ListV([BoolV(true)])]),
  ),
  "(list (list true false) (list true))",
  "list of lists",
);
checkExpect(stringOfValue(NumV(5)), "5", "number value");
checkExpect(stringOfValue(BoolV(true)), "true", "boolean value");
checkExpect(
  stringOfValue(BuiltinV({bName: "+", bProc: plus})),
  "builtin: +",
  "BuiltinV",
);
checkExpect(
  stringOfValue(
    ClosureV({cNameList: [Name("x")], cExpr: NumE(5), cEnv: []}),
  ),
  "<User-defined procedure>",
  "ClosureV",
);

// Check Expects and Error Testing for All Built-In Procedures:

// Generic Test Cases for plus:
checkExpect(
  stringOfValue(plus([NumV(2), NumV(3)])),
  "5",
  "addition of 2 ints",
);
checkExpect(
  stringOfValue(plus([NumV(9), NumV(-1)])),
  "8",
  "addition of ints (one is neg.)",
);
checkExpect(
  stringOfValue(plus([NumV(5), NumV(0)])),
  "5",
  "addition of 2 ints",
);
checkExpect(
  stringOfValue(plus([NumV(-2), NumV(-124)])),
  "-126",
  "addition of 2 ints (both are neg.)",
);

// CheckErrors for plus:
checkError(
  () => plus([NumV(3), NumV(2), NumV(1)]),
  "Error: + expects only 2 arguments, but found more than 2",
);
checkError(
  () => plus([NumV(6)]),
  "Error: + expects 2 arguments, but found only 1",
);
checkError(
  () => plus([NumV(3), NumV(2), NumV(1), NumV(6), NumV(56)]),
  "Error: + expects only 2 arguments, but found more than 2",
);
checkError(() => plus([]), "Error: + expects 2 arguments, but found none");

// Generic Test Cases for minus:
checkExpect(
  stringOfValue(minus([NumV(4), NumV(3)])),
  "1",
  "subtraction of 2 ints",
);
checkExpect(
  stringOfValue(minus([NumV(1), NumV(-12)])),
  "13",
  "subtraction of 2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(minus([NumV(5), NumV(0)])),
  "5",
  "subtraction of 2 ints",
);
checkExpect(
  stringOfValue(minus([NumV(-2), NumV(-124)])),
  "122",
  "subtraction of 2 ints (both are neg.)",
);

// CheckErrors for minus:
checkError(
  () => minus([NumV(34), NumV(22), NumV(14)]),
  "Error: - expects only 2 arguments, but found more than 2",
);
checkError(
  () => minus([NumV(0)]),
  "Error: - expects 2 arguments, but found only 1",
);
checkError(
  () => minus([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: - expects only 2 arguments, but found more than 2",
);
checkError(() => minus([]), "Error: - expects 2 arguments, but found none");

// Generic Test Cases for mult:
checkExpect(
  stringOfValue(mult([NumV(4), NumV(3)])),
  "12",
  "multiplication of 2 ints",
);
checkExpect(
  stringOfValue(mult([NumV(1), NumV(-12)])),
  "-12",
  "multiplication of 2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(mult([NumV(5), NumV(0)])),
  "0",
  "multiplication of 2 ints",
);
checkExpect(
  stringOfValue(mult([NumV(-2), NumV(-124)])),
  "248",
  "multiplication of 2 ints (both are neg.)",
);

// CheckErrors for mult:
checkError(
  () => mult([NumV(34), NumV(22), NumV(14)]),
  "Error: * expects only 2 arguments, but found more than 2",
);
checkError(
  () => mult([NumV(0)]),
  "Error: * expects 2 arguments, but found only 1",
);
checkError(
  () => mult([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: * expects only 2 arguments, but found more than 2",
);
checkError(() => mult([]), "Error: * expects 2 arguments, but found none");

// Generic Test Cases for divide:
checkExpect(
  stringOfValue(divide([NumV(4), NumV(3)])),
  "1",
  "division of 2 ints",
);
checkExpect(
  stringOfValue(divide([NumV(1), NumV(-12)])),
  "0",
  "division of 2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(divide([NumV(5), NumV(1)])),
  "5",
  "division of 2 ints",
);
checkExpect(
  stringOfValue(divide([NumV(-54), NumV(-5)])),
  "10",
  "division of 2 ints (both are neg.)",
);

// CheckErrors for divide:
checkError(
  () => divide([NumV(34), NumV(22), NumV(14)]),
  "Error: / expects only 2 arguments, but found more than 2",
);
checkError(
  () => divide([NumV(0)]),
  "Error: / expects 2 arguments, but found only 1",
);
checkError(
  () => divide([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: / expects only 2 arguments, but found more than 2",
);
checkError(() => divide([]), "Error: / expects 2 arguments, but found none");
checkError(() => divide([NumV(4), NumV(0)]), "Error: division by 0");

// Generic Test Cases for remainder:
checkExpect(
  stringOfValue(remainder([NumV(8), NumV(3)])),
  "2",
  "remainder of division of 2 ints",
);
checkExpect(
  stringOfValue(remainder([NumV(1), NumV(-2)])),
  "1",
  "remainder of division of 2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(remainder([NumV(5), NumV(1)])),
  "0",
  "remainder of division of 2 ints",
);
checkExpect(
  stringOfValue(remainder([NumV(-54), NumV(-5)])),
  "-4",
  "remainder of divion of 2 ints (both are neg.)",
);

// CheckErrors for remainder:
checkError(
  () => remainder([NumV(34), NumV(22), NumV(14)]),
  "Error: remainder expects only 2 arguments, but found more than 2",
);
checkError(
  () => remainder([NumV(0)]),
  "Error: remainder expects 2 arguments, but found only 1",
);
checkError(
  () => remainder([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: remainder expects only 2 arguments, but found more than 2",
);
checkError(
  () => remainder([]),
  "Error: remainder expects 2 arguments, but found none",
);

// Generic Test Cases for equal:
checkExpect(stringOfValue(equal([NumV(8), NumV(3)])), "false", "2 ints");
checkExpect(stringOfValue(equal([NumV(1), NumV(1)])), "true", "2 ints");

// CheckErrors for equal:
checkError(
  () => equal([NumV(34), NumV(22), NumV(14)]),
  "Error: = expects only 2 arguments, but found more than 2",
);
checkError(
  () => equal([NumV(0)]),
  "Error: = expects 2 arguments, but found only 1",
);
checkError(
  () => equal([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: = expects only 2 arguments, but found more than 2",
);
checkError(() => equal([]), "Error: = expects 2 arguments, but found none");

// Generic Test Cases for lessThan:
checkExpect(
  stringOfValue(lessThan([NumV(4), NumV(3)])),
  "false",
  "2 ints",
);
checkExpect(
  stringOfValue(lessThan([NumV(-800), NumV(800)])),
  "true",
  "2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(lessThan([NumV(0), NumV(0)])),
  "false",
  "2 ints",
);
checkExpect(
  stringOfValue(lessThan([NumV(-3), NumV(-124)])),
  "false",
  "2 ints (both are neg.)",
);

// CheckErrors for lessThan:
checkError(
  () => lessThan([NumV(34), NumV(22), NumV(14)]),
  "Error: < expects only 2 arguments, but found more than 2",
);
checkError(
  () => lessThan([NumV(0)]),
  "Error: < expects 2 arguments, but found only 1",
);
checkError(
  () => lessThan([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: < expects only 2 arguments, but found more than 2",
);
checkError(
  () => lessThan([]),
  "Error: < expects 2 arguments, but found none",
);

// Generic Test Cases for greaterThan:
checkExpect(
  stringOfValue(greaterThan([NumV(4), NumV(3)])),
  "true",
  "2 ints",
);
checkExpect(
  stringOfValue(greaterThan([NumV(-800), NumV(800)])),
  "false",
  "2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(greaterThan([NumV(0), NumV(0)])),
  "false",
  "2 ints",
);
checkExpect(
  stringOfValue(greaterThan([NumV(-3), NumV(-124)])),
  "true",
  "2 ints (both are neg.)",
);

// CheckErrors for greaterThan:
checkError(
  () => greaterThan([NumV(34), NumV(22), NumV(14)]),
  "Error: > expects only 2 arguments, but found more than 2",
);
checkError(
  () => greaterThan([NumV(0)]),
  "Error: > expects 2 arguments, but found only 1",
);
checkError(
  () => greaterThan([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: > expects only 2 arguments, but found more than 2",
);
checkError(
  () => greaterThan([]),
  "Error: > expects 2 arguments, but found none",
);

// Generic Test Cases for lessThanOrEqualTo:
checkExpect(
  stringOfValue(lessThanOrEqualTo([NumV(4), NumV(3)])),
  "false",
  "2 ints",
);
checkExpect(
  stringOfValue(lessThanOrEqualTo([NumV(23), NumV(3)])),
  "false",
  "2 ints",
);
checkExpect(
  stringOfValue(lessThanOrEqualTo([NumV(-800), NumV(800)])),
  "true",
  "2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(lessThanOrEqualTo([NumV(0), NumV(0)])),
  "true",
  "2 ints",
);
checkExpect(
  stringOfValue(lessThanOrEqualTo([NumV(-3), NumV(-124)])),
  "false",
  "2 ints (both are neg.)",
);

// CheckErrors for lessThanOrEqualTo:
checkError(
  () => lessThanOrEqualTo([NumV(34), NumV(22), NumV(14)]),
  "Error: <= expects only 2 arguments, but found more than 2",
);
checkError(
  () => lessThanOrEqualTo([NumV(0)]),
  "Error: <= expects 2 arguments, but found only 1",
);
checkError(
  () =>
    lessThanOrEqualTo([NumV(5), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: <= expects only 2 arguments, but found more than 2",
);
checkError(
  () => lessThanOrEqualTo([]),
  "Error: <= expects 2 arguments, but found none",
);

// Generic Test Cases for greaterThanOrEqualTo:
checkExpect(
  stringOfValue(greaterThanOrEqualTo([NumV(4), NumV(3)])),
  "true",
  "2 ints",
);
checkExpect(
  stringOfValue(greaterThanOrEqualTo([NumV(-800), NumV(800)])),
  "false",
  "2 ints (one is neg.)",
);
checkExpect(
  stringOfValue(greaterThanOrEqualTo([NumV(0), NumV(0)])),
  "true",
  "2 ints",
);
checkExpect(
  stringOfValue(greaterThanOrEqualTo([NumV(-3), NumV(-124)])),
  "true",
  "2 ints (both are neg.)",
);

// CheckErrors for greaterThanOrEqualTo:
checkError(
  () => greaterThanOrEqualTo([NumV(34), NumV(22), NumV(14)]),
  "Error: >= expects only 2 arguments, but found more than 2",
);
checkError(
  () => greaterThanOrEqualTo([NumV(0)]),
  "Error: >= expects 2 arguments, but found only 1",
);
checkError(
  () =>
    greaterThanOrEqualTo([
      NumV(5),
      NumV(1),
      NumV(234),
      NumV(2),
      NumV(543),
    ]),
  "Error: >= expects only 2 arguments, but found more than 2",
);
checkError(
  () => greaterThanOrEqualTo([]),
  "Error: >= expects 2 arguments, but found none",
);

// Generic Test Cases for isEqual:
checkExpect(
  stringOfValue(isEqual([NumV(8), NumV(3)])),
  "false",
  "2 ints",
);
checkExpect(stringOfValue(isEqual([NumV(1), NumV(1)])), "true", "2 ints");
checkExpect(
  stringOfValue(isEqual([BoolV(true), BoolV(true)])),
  "true",
  "2 bools",
);
checkExpect(
  stringOfValue(isEqual([BoolV(false), BoolV(true)])),
  "false",
  "2 bools",
);
checkExpect(
  stringOfValue(
    isEqual([ListV([NumV(4), NumV(-3)]), ListV([NumV(4), NumV(-3)])]),
  ),
  "true",
  "2 lists",
);
checkExpect(
  stringOfValue(
    isEqual([ListV([NumV(4), NumV(-3)]), ListV([NumV(4), NumV(-1)])]),
  ),
  "false",
  "2 lists",
);

// CheckErrors for isEqual:
checkError(
  () => isEqual([NumV(34), NumV(22), NumV(14)]),
  "Error: equal? expects only 2 arguments, but found more than 2",
);
checkError(
  () => isEqual([BoolV(false)]),
  "Error: equal? expects 2 arguments, but found only 1",
);
checkError(
  () => isEqual([BoolV(true), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: equal? expects only 2 arguments, but found more than 2",
);
checkError(
  () => isEqual([]),
  "Error: equal? expects 2 arguments, but found none",
);

// Generic Test Cases for isNumber:
checkExpect(stringOfValue(isNumber([NumV(8)])), "true", "Number");
checkExpect(stringOfValue(isNumber([BoolV(false)])), "false", "bool");
checkExpect(
  stringOfValue(isNumber([ListV([NumV(4), NumV(-3)])])),
  "false",
  "list",
);

// CheckErrors for isNumber:
checkError(
  () => isNumber([NumV(34), NumV(22), NumV(14)]),
  "Error: number? expects only 1 argument, but found more than 1",
);
checkError(
  () => isNumber([BoolV(false), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: number? expects only 1 argument, but found more than 1",
);
checkError(
  () => isNumber([]),
  "Error: number? expects 1 argument, but found none",
);

// Generic Test Cases for isZero:
checkExpect(stringOfValue(isZero([NumV(8)])), "false", "Number");
checkExpect(stringOfValue(isZero([NumV(0)])), "true", "Number is 0");

// CheckErrors for isZero:
checkError(
  () => isZero([NumV(34), NumV(22), NumV(14)]),
  "Error: zero? expects only 1 argument, but found more than 1",
);
checkError(
  () => isZero([BoolV(false), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: zero? expects only 1 argument, but found more than 1",
);
checkError(
  () => isZero([]),
  "Error: zero? expects 1 argument, but found none",
);

// Generic Test Cases for cons:
checkExpect(
  stringOfValue(cons([NumV(8), ListV([NumV(5), NumV(4)])])),
  "(list 8 5 4)",
  "list of numbers",
);
checkExpect(
  stringOfValue(cons([BoolV(false), ListV([])])),
  "(list false)",
  "list containing 1 elem",
);
checkExpect(
  stringOfValue(cons([NumV(-1), ListV([NumV(5), NumV(3), NumV(6)])])),
  "(list -1 5 3 6)",
  "list of numbers",
);

// CheckErrors for cons:
checkError(
  () => cons([NumV(34), NumV(22), NumV(14)]),
  "Error: cons expects only 2 arguments, but found more than 2",
);
checkError(
  () => cons([BoolV(false)]),
  "Error: cons expects 2 arguments, but found only 1",
);
checkError(
  () => cons([]),
  "Error: cons expects 2 arguments, but found none",
);

// Generic Test Cases for first:
checkExpect(
  first([ListV([NumV(6), NumV(-3)])]),
  NumV(6),
  "list of nums",
);
checkExpect(first([ListV([NumV(4)])]), NumV(4), "list of nums");
checkExpect(
  first([ListV([BoolV(true), BoolV(false), BoolV(true)])]),
  BoolV(true),
  "list of bools",
);

// CheckErrors for first:
checkError(
  () => first([NumV(34), NumV(22), NumV(14)]),
  "Error: first expects a non-empty list",
);
checkError(
  () => first([NumV(34)]),
  "Error: first expects a non-empty list",
);
checkError(
  () => first([BoolV(false)]),
  "Error: first expects a non-empty list",
);
checkError(
  () => first([BoolV(false), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: first expects a non-empty list",
);
checkError(() => first([]), "Error: first expects a non-empty list");
checkError(
  () => first([ListV([])]),
  "Error: first expects a non-empty list",
);

// Generic Test Cases for rest:
checkExpect(
  rest([ListV([NumV(6), NumV(-3)])]),
  ListV([NumV(-3)]),
  "list of 2 nums",
);
checkExpect(
  rest([ListV([NumV(6), NumV(-3), NumV(5), NumV(4)])]),
  ListV([NumV(-3), NumV(5), NumV(4)]),
  "list of 4 nums",
);
checkExpect(rest([ListV([NumV(4)])]), ListV([]), "list of nums");
checkExpect(
  rest([ListV([BoolV(true), BoolV(false), BoolV(true)])]),
  ListV([BoolV(false), BoolV(true)]),
  "list of bools",
);

// CheckErrors for rest:
checkError(
  () => rest([NumV(34), NumV(22), NumV(14)]),
  "Error: rest expects a non-empty list",
);
checkError(() => rest([NumV(34)]), "Error: rest expects a non-empty list");
checkError(
  () => rest([BoolV(false)]),
  "Error: rest expects a non-empty list",
);
checkError(
  () => rest([BoolV(false), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: rest expects a non-empty list",
);
checkError(() => rest([]), "Error: rest expects a non-empty list");
checkError(() => rest([ListV([])]), "Error: rest expects a non-empty list");

// Generic Test Cases for isEmpty:
checkExpect(stringOfValue(isEmpty([ListV([])])), "true", "empty list");
checkExpect(
  stringOfValue(isEmpty([ListV([NumV(6), NumV(-3)])])),
  "false",
  "non-empty list",
);

// CheckErrors for isEmpty:
checkError(
  () => isEmpty([NumV(34), NumV(22), NumV(14)]),
  "Error: empty? expects a list of values as input",
);
checkError(
  () => isEmpty([NumV(34)]),
  "Error: empty? expects a list of values as input",
);
checkError(
  () => isEmpty([BoolV(false), NumV(1), NumV(2), NumV(543)]),
  "Error: empty? expects a list of values as input",
);
checkError(
  () => isEmpty([]),
  "Error: empty? expects a list of values as input",
);

// Generic Test Cases for isCons:
checkExpect(stringOfValue(isCons([ListV([])])), "false", "empty list");
checkExpect(
  stringOfValue(isCons([ListV([NumV(6), NumV(-3)])])),
  "true",
  "cons list",
);

// CheckErrors for isCons:
checkError(
  () => isCons([NumV(34), NumV(22), NumV(14)]),
  "Error: cons? expects only 1 argument, but found more than 1 argument",
);
checkError(
  () => isCons([BoolV(false), NumV(1), NumV(234), NumV(2), NumV(543)]),
  "Error: cons? expects only 1 argument, but found more than 1 argument",
);

// Generic Test Cases for opposite:
checkExpect(
  stringOfValue(opposite([BoolV(true)])),
  "false",
  "bool is true",
);
checkExpect(
  stringOfValue(opposite([BoolV(false)])),
  "true",
  "bool is false",
);

// CheckErrors for opposite:
checkError(
  () => opposite([NumV(34), NumV(22), NumV(14)]),
  "Error: not expects either true or false",
);
checkError(
  () => opposite([NumV(34)]),
  "Error: not expects either true or false",
);
checkError(
  () => opposite([BoolV(false), NumV(1), NumV(234), NumV(2)]),
  "Error: not expects either true or false",
);
checkError(
  () => opposite([]),
  "Error: not expects 1 argument, but found none",
);
checkError(
  () => opposite([ListV([NumV(6), NumV(-3)])]),
  "Error: not expects either true or false",
);

// Test Cases for parseDefinition:
checkExpectDefinition(
  parseDefinition(ListC([SymbolC("define"), SymbolC("x"), NumberC(5)])),
  (Name("x"), NumE(5)),
  "parsing a definition containing a number expression",
);
checkExpectDefinition(
  parseDefinition(
    ListC([SymbolC("define"), SymbolC("plus"), SymbolC("+")]),
  ),
  (Name("plus"), NameE(Name("+"))),
  "parsing a definition containing a symbol",
);
checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("fun"),
      ListC([
        SymbolC("lambda"),
        ListC([SymbolC("x")]),
        ListC([SymbolC("+"), SymbolC("x"), NumberC(1)]),
      ]),
    ]),
  ),
  (
    Name("fun"),
    LambdaE({
      nameList: [Name("x")],
      lambdaBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    }),
  ),
  "parsing a definition containing a lambda expression",
);
checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("fun"),
      ListC([
        SymbolC("let"),
        ListC([ListC([SymbolC("x"), NumberC(0)])]),
        ListC([SymbolC("+"), SymbolC("x"), NumberC(1)]),
      ]),
    ]),
  ),
  (
    Name("fun"),
    LetE({
      letPairs: [{pairName: Name("x"), pairExpr: NumE(0)}],
      letBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    }),
  ),
  "parsing a definition containing a let expression",
);

checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("hello"),
      ListC([
        SymbolC("or"),
        ListC([SymbolC("<="), NumberC(6), NumberC(7)]),
        ListC([SymbolC("number?"), NumberC(5)]),
      ]),
    ]),
  ),
  (
    Name("hello"),
    OrE(
      ApplicationE([NameE(Name("<=")), NumE(6), NumE(7)]),
      ApplicationE([NameE(Name("number?")), NumE(5)]),
    ),
  ),
  "parsing a definition containing an or expression",
);
checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("h"),
      ListC([
        SymbolC("x"),
        ListC([SymbolC("+"), NumberC(4), NumberC(5)]),
      ]),
    ]),
  ),
  (
    Name("h"),
    ApplicationE([
      NameE(Name("x")),
      ApplicationE([NameE(Name("+")), NumE(4), NumE(5)]),
    ]),
  ),
  "parsing a definition to an application expression",
);

// Test Cases for parseExpression:
checkExpectExpression(
  parseExpression(NumberC(5)),
  NumE(5),
  "parsing a number",
);
checkExpectExpression(parseExpression(NumberC(0)), NumE(0), "parsing 0");
checkExpectExpression(
  parseExpression(SymbolC("+")),
  NameE(Name("+")),
  "parsing a symbol",
);
checkExpectExpression(
  parseExpression(SymbolC("cons?")),
  NameE(Name("cons?")),
  "parsing a builtin",
);
checkExpectExpression(
  parseExpression(
    ListC([SymbolC("or"), SymbolC("false"), SymbolC("false")]),
  ),
  OrE(BoolE(false), BoolE(false)),
  "parsing an or expr",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("and"),
      ListC([SymbolC(">"), NumberC(4), NumberC(1)]),
      SymbolC("true"),
    ]),
  ),
  AndE(ApplicationE([NameE(Name(">")), NumE(4), NumE(1)]), BoolE(true)),
  "parsing an and expr w/ an proc app expr inside",
);
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("or"),
      SymbolC("false"),
      ListC([SymbolC("<="), NumberC(6), NumberC(1)]),
    ]),
  ),
  OrE(
    BoolE(false),
    ApplicationE([NameE(Name("<=")), NumE(6), NumE(1)]),
  ),
  "parsing an or expr w/ an proc app expr inside",
);
checkExpectExpression(
  parseExpression(read("(and (= 4 1) (> 6 2))")),
  AndE(
    ApplicationE([NameE(Name("=")), NumE(4), NumE(1)]),
    ApplicationE([NameE(Name(">")), NumE(6), NumE(2)]),
  ),
  "parsing and exp w/ 2 proc app expr",
);
checkExpectExpression(
  parseExpression(read("(if (zero? x) 1 (+ x 5))")),
  IfE({
    boolExpr: ApplicationE([NameE(Name("zero?")), NameE(Name("x"))]),
    trueExpr: NumE(1),
    falseExpr:
      ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(5)]),
  }),
  "parsing if expr",
);

checkExpectExpression(
  parseExpression(read("(if (> y x) (+ x y) (+ x (- y 6)))")),
  IfE({
    boolExpr:
      ApplicationE([
        NameE(Name(">")),
        NameE(Name("y")),
        NameE(Name("x")),
      ]),
    trueExpr:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
    falseExpr:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        ApplicationE([NameE(Name("-")), NameE(Name("y")), NumE(6)]),
      ]),
  }),
  "parsing if expr with nested expr",
);

checkExpectExpression(
  parseExpression(read("(lambda (x) (+ x 1))")),
  LambdaE({
    nameList: [Name("x")],
    lambdaBody:
      ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
  }),
  "parsing lambda expr",
);

checkExpectExpression(
  parseExpression(read("(lambda (x y) (+ x 1))")),
  LambdaE({
    nameList: [Name("x"), Name("y")],
    lambdaBody:
      ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
  }),
  "parsing another lambda expr",
);

checkExpectExpression(
  parseExpression(read("(lambda (x y) (+ x y))")),
  LambdaE({
    nameList: [Name("x"), Name("y")],
    lambdaBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "parsing a lambda expr where both args are in the body",
);

checkExpectExpression(
  parseExpression(read("(lambda (x y z) (+ x (* z y)))")),
  LambdaE({
    nameList: [Name("x"), Name("y"), Name("z")],
    lambdaBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        ApplicationE([
          NameE(Name("*")),
          NameE(Name("z")),
          NameE(Name("y")),
        ]),
      ]),
  }),
  "parsing a lambda expr w/ nested proc app expr in the body",
);

checkExpectExpression(
  parseExpression(read("(let ((x 5)) (+ x 1))")),
  LetE({
    letPairs: [{pairName: Name("x"), pairExpr: NumE(5)}],
    letBody: ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
  }),
  "parsing a let expr",
);

checkExpectExpression(
  parseExpression(read("(let ((x 5) (y 3)) (+ y 1))")),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(5)},
      {pairName: Name("y"), pairExpr: NumE(3)},
    ],
    letBody: ApplicationE([NameE(Name("+")), NameE(Name("y")), NumE(1)]),
  }),
  "parsing a let expr with 2 bindings",
);

checkExpectExpression(
  parseExpression(read("(let ((x 5) (y 3) (z 1)) (+ (* x z) (- y 4)))")),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(5)},
      {pairName: Name("y"), pairExpr: NumE(3)},
      {pairName: Name("z"), pairExpr: NumE(1)},
    ],
    letBody:
      ApplicationE([
        NameE(Name("+")),
        ApplicationE([
          NameE(Name("*")),
          NameE(Name("x")),
          NameE(Name("z")),
        ]),
        ApplicationE([NameE(Name("-")), NameE(Name("y")), NumE(4)]),
      ]),
  }),
  "parsing a let expr with more than 2 bindings",
);
checkExpectExpression(
  parseExpression(read("(cond ((zero? x) 1) ((not (zero? x)) 2))")),
  CondE([
    {
      conditionExpr:
        ApplicationE([NameE(Name("zero?")), NameE(Name("x"))]),
      resultExpr: NumE(1),
    },
    {
      conditionExpr:
        ApplicationE([
          NameE(Name("not")),
          ApplicationE([NameE(Name("zero?")), NameE(Name("x"))]),
        ]),
      resultExpr: NumE(2),
    },
  ]),
  "parsing a cond expression",
);

// Test Cases for lookup:
checkExpect(
  lookup([(Name("x"), NumV(5))], Name("x")),
  Some(NumV(5)),
  "testing lookup for a number value",
);
checkExpect(
  lookup([(Name("a"), NumV(5)), (Name("b"), BoolV(true))], Name("b")),
  Some(BoolV(true)),
  "testing lookup for a boolean value",
);
checkExpect(
  lookup([(Name("y"), ListV([NumV(5), NumV(6)]))], Name("y")),
  Some(ListV([NumV(5), NumV(6)])),
  "testing lookup for a listV value",
);

// Test Cases for uniqueList:
checkExpect(
  uniqueListP([Name("x"), Name("y"), Name("z")]),
  true,
  "testing a unique list",
);
checkExpect(
  uniqueListP([Name("x"), Name("x")]),
  false,
  "testing a non-unique list",
);
checkExpect(uniqueListP([]), true, "testing a unique list");

// Test Cases for addBindingsToClosure
checkExpect(
  addBindingsToClosure([Name("x"), Name("y")], [NumV(5), NumV(6)]),
  [(Name("x"), NumV(5)), (Name("y"), NumV(6))],
  "testing addBindingsToClosure",
);
checkExpect(
  addBindingsToClosure([Name("brown")], [BoolV(true)]),
  [(Name("brown"), BoolV(true))],
  "testing addBindingsToClosure",
);

// Test Cases for addBinding:
checkExpect(
  addBinding([(Name("x"), NumV(5))], (Name("y"), BoolV(true))),
  [(Name("y"), BoolV(true)), (Name("x"), NumV(5))],
  "add bindings to an env",
);
checkExpect(
  addBinding([], (Name("y"), BoolV(true))),
  [(Name("y"), BoolV(true))],
  "add bindings to an empty env",
);

// Test Cases for eval:
checkExpect(
  stringOfValue(eval(initialTle, [], AndE(BoolE(true), BoolE(true)))),
  "true",
  "eval another simple and-expr",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      AndE(
        ApplicationE([NameE(Name("<")), NumE(5), NumE(3)]),
        BoolE(true),
      ),
    ),
  ),
  "false",
  "eval and-expr w/ proc-app expr",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      OrE(
        BoolE(false),
        ApplicationE([NameE(Name(">=")), NumE(6), NumE(1)]),
      ),
    ),
  ),
  "true",
  "eval or-expr w/ proc-app expr",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      IfE({
        boolExpr: BoolE(false),
        trueExpr: ApplicationE([NameE(Name("*")), NumE(143), NumE(-5)]),
        falseExpr: ApplicationE([NameE(Name("number?")), NumE(5)]),
      }),
    ),
  ),
  "true",
  "eval if-expr w/ mutliple proc-app expressions",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name("+")), NumE(3), NumE(5)]),
    ),
  ),
  "8",
  "eval proc-app expr w/ addition",
);

checkExpect(
  stringOfValue(
    eval(initialTle, [], ApplicationE([NameE(Name("number?")), NumE(4)])),
  ),
  "true",
  "eval proc-app expr w/ number?",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name("*")), NumE(5), NumE(4)]),
    ),
  ),
  "20",
  "eval proc-app expr w/ mult",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name("/")), NumE(123), NumE(5)]),
    ),
  ),
  "24",
  "eval proc-app expr w/ divide",
);

checkExpect(
  stringOfValue(
    eval(initialTle, [], ApplicationE([NameE(Name("zero?")), NumE(0)])),
  ),
  "true",
  "eval proc-app expr w/ zero?",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name("=")), NumE(0), NumE(0)]),
    ),
  ),
  "true",
  "eval proc-app expr w/ =",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name("<=")), NumE(0), NumE(0)]),
    ),
  ),
  "true",
  "eval proc-app expr w/ <=",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name(">=")), NumE(234), NumE(0)]),
    ),
  ),
  "true",
  "eval proc-app expr w/ >=",
);
checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      ApplicationE([NameE(Name("equal?")), BoolE(false), BoolE(true)]),
    ),
  ),
  "false",
  "eval proc-app expr w/ equal?",
);

checkExpect(
  stringOfValue(
    eval(initialTle, [(Name("x"), NumV(4))], NameE(Name("x"))),
  ),
  "4",
  "eval a name expression",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      CondE([
        {
          conditionExpr: ApplicationE([NameE(Name("zero?")), NumE(2)]),
          resultExpr: NumE(1),
        },
        {
          conditionExpr: ApplicationE([NameE(Name("zero?")), NumE(0)]),
          resultExpr: NumE(2),
        },
      ]),
    ),
  ),
  "2",
  "eval cond expression w/ 2 branches",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [],
      CondE([
        {
          conditionExpr: ApplicationE([NameE(Name("zero?")), NumE(2)]),
          resultExpr: NumE(1),
        },
        {
          conditionExpr:
            ApplicationE([NameE(Name("number?")), BoolE(false)]),
          resultExpr: NumE(2),
        },
        {
          conditionExpr:
            ApplicationE([
              NameE(Name("equal?")),
              BoolE(true),
              BoolE(true),
            ]),
          resultExpr: NumE(9),
        },
      ]),
    ),
  ),
  "9",
  "eval cond expression w/ 3 branches",
);

checkExpect(
  addDefinition([], (Name("x"), NumE(5))),
  [(Name("x"), NumV(5))],
  "adding a single binding to empty environment",
);

// Test Cases for addDefinition

checkExpect(
  addDefinition(
    [(Name("y"), BoolV(true)), (Name("z"), NumV(4))],
    (Name("f"), NumE(5)),
  ),
  [
    (Name("f"), NumV(5)),
    (Name("y"), BoolV(true)),
    (Name("z"), NumV(4)),
  ],
  "adding a single binding to env that is not empty",
);

checkError(
  () =>
    addDefinition([(Name("y"), BoolV(true))], (Name("y"), NumE(1))),
  "Error: This name was defined previously and cannot be re-defined",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [(Name("x"), NumV(5)), (Name("y"), NumV(6))],
      NameE(Name("x")),
    ),
  ),
  "5",
  "evaluating a name in the local env",
);

checkExpect(
  stringOfValue(
    eval(
      initialTle,
      [
        (Name("x"), BoolV(true)),
        (Name("y"), NumV(6)),
        (Name("z"), BoolV(false)),
      ],
      NameE(Name("z")),
    ),
  ),
  "false",
  "evaluating another name in the local env",
);

checkExpect(
  stringOfValue(
    eval(
      [
        (Name("a"), BoolV(true)),
        (Name("b"), NumV(7)),
        (Name("c"), BoolV(false)),
      ],
      [(Name("a"), NumV(10)), (Name("y"), NumV(6))],
      NameE(Name("a")),
    ),
  ),
  "10",
  "evaluating a name in the TLE",
);

checkExpect(
  stringOfValue(
    eval(
      [
        (Name("y"), NumV(4)),
        (Name("+"), BuiltinV({bName: "<builtin-proc-+", bProc: plus})),
      ],
      [(Name("x"), NumV(4))],
      ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    ),
  ),
  "5",
  "evaluating app proc expression",
);

checkExpect(
  stringOfValue(
    eval(
      [
        (Name("x"), NumV(4)),
        (Name("*"), BuiltinV({bName: "<builtin-proc-*", bProc: mult})),
      ],
      [(Name("x"), NumV(1)), (Name("y"), NumV(6))],
      ApplicationE([NameE(Name("*")), NameE(Name("x")), NumE(1)]),
    ),
  ),
  "1",
  "evaluating app proc expression",
);
checkExpect(
  stringOfValue(
    eval(
      [
        (Name("a"), BoolV(true)),
        (Name("b"), NumV(7)),
        (Name("c"), BoolV(false)),
        (Name("+"), BuiltinV({bName: "<builtin-proc-+", bProc: plus})),
      ],
      [],
      LetE({
        letPairs: [{pairName: Name("x"), pairExpr: NumE(5)}],
        letBody:
          ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
      }),
    ),
  ),
  "6",
  "evaluating a let expression",
);

checkExpect(
  stringOfValue(
    eval(
      [
        (Name("a"), BoolV(true)),
        (Name("b"), NumV(7)),
        (Name("c"), NumV(6)),
        (Name("+"), BuiltinV({bName: "<builtin-proc-+", bProc: plus})),
      ],
      [],
      LetE({
        letPairs: [
          {pairName: Name("b"), pairExpr: NumE(5)},
          {pairName: Name("c"), pairExpr: NumE(9)},
        ],
        letBody:
          ApplicationE([
            NameE(Name("+")),
            NameE(Name("b")),
            NameE(Name("c")),
          ]),
      }),
    ),
  ),
  "14",
  "evaluating a let expression",
);

checkExpect(
  eval(
    [
      (Name("a"), BoolV(true)),
      (Name("b"), NumV(7)),
      (Name("c"), NumV(6)),
    ],
    [(Name("x"), NumV(6))],
    LambdaE({
      nameList: [Name("x")],
      lambdaBody:
        ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    }),
  ),
  ClosureV({
    cNameList: [Name("x")],
    cExpr: ApplicationE([NameE(Name("+")), NameE(Name("x")), NumE(1)]),
    cEnv: [(Name("x"), NumV(6))],
  }),
  "evaluating a lambda expression to a closure",
);

checkExpect(
  eval(
    [
      (Name("a"), BoolV(true)),
      (Name("b"), NumV(7)),
      (Name("c"), NumV(6)),
      (Name("*"), BuiltinV({bName: "<builtin-proc-*", bProc: mult})),
    ],
    [],
    ApplicationE([
      LambdaE({
        nameList: [Name("x")],
        lambdaBody:
          ApplicationE([NameE(Name("*")), NameE(Name("x")), NumE(1)]),
      }),
      NumE(1),
    ]),
  ),
  NumV(1),
  "evaluating an app proc expr w/ closure",
);

// Test Cases for Rackette
checkExpect(rackette("(+ 3 1)"), ["4"], "testing a simple proc app expr");
checkExpect(rackette("(zero? 8)"), ["false"], "testing zero?");
checkExpect(
  rackette("(+ 3 (* 5 1))"),
  ["8"],
  "testing a simple nested proc app expr",
);
checkExpect(
  rackette("(- (/ 7 3) (* 5 1))"),
  ["-3"],
  "testing a proc app expr w/ 2 nested expressions",
);
checkExpect(rackette("(number? false)"), ["false"], "testing a number?");
checkExpect(
  rackette("(and (< 5 6) (>= 1 4))"),
  ["false"],
  "testing an and expr",
);
checkExpect(
  rackette("(or (zero? 5) (= 1 1))"),
  ["true"],
  "testing an or expr",
);
checkExpect(
  rackette("(if (> 4 1) (+ 6 2) (- 2 8))"),
  ["8"],
  "testing a basic if expr",
);
checkExpect(
  rackette(
    "(if (and (= 7 7) (> 7 1)) (or (number? 6) (equal? true false)) 7)",
  ),
  ["true"],
  "testing an if statement w/ multiple proc apps inside",
);

checkExpect(
  rackette("(let ((x 5)) (* x 7))"),
  ["35"],
  "testing a simple let expr",
);

checkExpect(
  rackette("(let ((x 5) (y 6)) (* x y))"),
  ["30"],
  "testing a let expr w/ 2 bindings",
);
checkExpect(
  rackette("((lambda (x) (+ x 1)) 7)"),
  ["8"],
  "testing a simple lambda expression",
);
checkExpect(
  rackette("((lambda (x y) (+ x y)) 1 2)"),
  ["3"],
  "testing a lambda expression w/ 2 arguments",
);

checkExpect(
  rackette("((lambda (x y z) (+ x (- y z))) 7 8 9)"),
  ["6"],
  "testing a lambda expression w/ 3 arguments",
);
checkExpect(
  rackette("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"),
  ["34"],
  "testing nested lambda expressions",
);

checkExpect(
  rackette("((lambda (x y) ((lambda (x) (+ x y)) x)) 17 18)"),
  ["35"],
  "testing nested lambda expressions",
);
checkExpect(
  rackette("((lambda (x y) ((lambda (x) (+ x y)) y)) 17 18)"),
  ["36"],
  "testing nested lambda expressions",
);
checkExpect(
  rackette("(define y 17) (let ((y 3)) (+ y 7))"),
  ["10"],
  "testing define and let expressions",
);

checkExpect(
  rackette("*"),
  ["builtin: <builtin-proc-*"],
  "testing a single symbol",
);
checkExpect(
  rackette("cons"),
  ["builtin: <builtin-proc-cons"],
  "testing a single symbol without parentheses",
);
checkExpect(
  rackette("(cons 1 empty)"),
  ["(list 1)"],
  "testing cons on an empty list",
);
checkExpect(
  rackette("> 5 1 2 5 2"),
  ["builtin: <builtin-proc->", "5", "1", "2", "5", "2"],
  "testing > without parentheses",
);

checkExpect(
  rackette(
    "(let ((x 0)) (let ((f (lambda (a) (* x a)))) (let ((x 1)) (f 5))))",
  ),
  ["0"],
  "testing let and lambda expressions",
);

checkExpect(
  rackette(
    "(let ((x 0) (y 18)) (let ((f (lambda (a b) (+ x b))) (x 17)) (f y x)))",
  ),
  ["17"],
  "testing let and lambda expressions w/ 2 bindings for let",
);

checkExpect(
  rackette("(let () (+ 3 5))"),
  ["8"],
  "testing a let expr with 0 bindings",
);
checkExpect(
  rackette("(cond ((zero? 1) 6) ((not (zero? 5)) 5))"),
  ["5"],
  "testing a basic cond expression",
);

checkExpect(
  rackette("(define fact 5) fact"),
  ["5"],
  "testing a simple define expression",
);
checkExpect(
  rackette("(define fact (lambda (x) (+ x 14))) (fact 3)"),
  ["17"],
  "testing a define expression w/ lambda",
);
checkExpect(
  rackette(
    "(define fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1)))))) (fact 3)",
  ),
  ["6"],
  "testing define and lambda expressions",
);

checkExpect(
  rackette(
    "(define max2 (lambda (a b) (if (> a b) a b)))
(define right-max (lambda (aloi) (cond
    ((empty? aloi) empty)
    ((empty? (rest aloi)) aloi)
    ((cons? aloi) (cons (max2 (first aloi) (first (right-max (rest aloi))))
                        (right-max (rest aloi))))))) (right-max"
    ++ "(cons 4 (cons 6 (cons 5 empty))))",
  ),
  ["(list 6 6 5)"],
  "testing right-max",
);
checkExpect(
  rackette("(lambda (x) (+ x 1))"),
  ["<User-defined procedure>"],
  "testing a user-defined procedure",
);
checkExpect(
  rackette("(lambda (x y) (+ x (- y x)))"),
  ["<User-defined procedure>"],
  "testing a user-defined procedure w/ two formal arguments",
);
checkExpect(
  rackette(
    "(define digit-add (lambda (a b)
  (if (and (number? a)
           (and (number? b)
           (and (<= 0 a)
           (and (<= a 99)
           (and (<= 0 b)
           (and (<= b 99)
           (<= (+ a b) 99)))))))
      (+ a b) 0)))

(define digit-sub (lambda (a b)
  (if (and (number? a)
           (and (number? b)
           (and (<= 0 a)
           (and (<= a 99)
           (and (<= 0 b)
           (and (<= b 99)
           (>= (- a b) 0)))))))
      (- a b)
      0)))

(define bignum+ (lambda (bignum1 bignum2)
 (if (or (empty? bignum1) (empty? bignum2))
  (if (empty? bignum1) bignum2 bignum1)
  (if (> (digit-add (first bignum1) (first bignum2)) 9)
      (cons (digit-sub (digit-add (first bignum1) (first bignum2)) 10)
            (bignum+ (cons 1 empty) (bignum+ (rest bignum1) (rest bignum2))))
      (cons (digit-add (first bignum1) (first bignum2))
            (bignum+ (rest bignum1) (rest bignum2)))))))
            (bignum+ (cons 9 empty) (cons 1 empty))",
  ),
  ["(list 0 1)"],
  "testing bignum+",
);

checkExpect(
  rackette(
    "
(define subset-sum (lambda (weights target)
  (cond
    ((empty? weights) (zero? target))
    ((cons? weights) (or (subset-sum (rest weights) (- target (first weights)))
                         (subset-sum (rest weights) target))))))"
    ++ "(subset-sum (cons 1 (cons 2 empty)) 2)",
  ),
  ["true"],
  "testing subset-sum",
);

checkExpect(
  rackette(
    "
(define lengthen (lambda (alon)
  (cond
    ((empty? alon) (cons 0 empty))
    ((cons? alon) (cons (first alon) (lengthen (rest alon)))))))"
    ++ "(lengthen (cons 1 (cons 2 empty)))",
  ),
  ["(list 1 2 0)"],
  "testing the lengthen procedure",
);
checkExpect(
  rackette(
    "(define double-all (lambda (alon)
  (cond
    ((empty? alon) empty)
    ((cons? alon) (cons (* 2 (first alon)) (double-all (rest alon)))))))"
    ++ "(double-all (cons 54 (cons 64 (cons 755 empty))))",
  ),
  ["(list 108 128 1510)"],
  "testing the double-all procedure",
);
checkExpect(
  rackette(
    "(define my-member? (lambda (item alon)
  (cond
    ((empty? alon) false)
    ((cons? alon) (if (equal? (first alon) item)
                      true
                      (my-member? item (rest alon))))))) (my-member? 5"
    ++ "(cons 1 (cons 2 (cons 5 empty))))",
  ),
  ["true"],
  "testing the my-member? procedure",
);
checkExpect(
  rackette(
    "(define my-append (lambda (alon1 alon2)
  (cond
    ((and (empty? alon1) (empty? alon2)) empty)
    ((and (empty? alon1) (cons? alon2)) alon2)
    ((and (cons? alon1) (empty? alon2)) alon1)
    ((and (cons? alon1) (cons? alon2)) (cons (first alon1)
    (my-append (rest alon1)"
    ++ "alon2)))))) (my-append (cons 1 (cons 2 empty)) (cons 3 (cons 7 empty)))",
  ),
  ["(list 1 2 3 7)"],
  "testing the my-append procedure",
);
checkExpect(
  rackette(
    "(define flipper (lambda (alos)
  (cond
    ((empty? alos) empty)
    ((empty? (rest alos)) alos)
    ((cons? alos) (cons (first (rest alos))
                        (cons (first alos)
                              (flipper (rest (rest alos)))))))))"
    ++ "(flipper (cons 1 (cons 2 (cons 3 empty))))",
  ),
  ["(list 2 1 3)"],
  "testing the flipper procedure",
);
checkExpect(
  rackette(
    "(define pair-count (lambda (alon1 alon2)
  (cond
    ((or (empty? alon1) (empty? alon2)) 0)
    ((and (cons? alon1) (cons? alon2))
     (if (equal? (first alon1) (first alon2))
         (+ 1 (pair-count (rest alon1) (rest alon2)))
         (pair-count (rest alon1) (rest alon2)))))))"
    ++ "(pair-count (cons 1 (cons 5 (cons 56 empty))) (cons 1 (cons 5"
    ++ "(cons 2 empty))))",
  ),
  ["2"],
  "testing the pair-count procedure",
);
checkExpect(
  rackette("(equal? + +)"),
  ["true"],
  "testing the equal of two + symbols",
);
checkExpect(
  rackette("(define add2 (lambda (x y) (+ x y)))"),
  [],
  "testing a definition",
);
checkExpect(
  rackette(
    " (define map (lambda (proc list) (if (empty? list)
    empty
    (cons (proc (first list)) (map proc (rest list))))))"
    ++ "(map (lambda (x) (+ x 1)) (cons 1 (cons 2 empty)))",
  ),
  ["(list 2 3)"],
  "testing the map procedure",
);

// Check Error Cases for Rackette
checkError(
  () => rackette("(+ 3 false )"),
  "Error: Second argument was not a number",
);
checkError(
  () => rackette("(/ true 9)"),
  "Error: First argument was not a number",
);
checkError(
  () => rackette("(- 3 9 5)"),
  "Error: - expects only 2 arguments, but found more than 2",
);
checkError(
  () => rackette("(* 3)"),
  "Error: * expects 2 arguments, but found only 1",
);
checkError(
  () => rackette("(zero? 5 3)"),
  "Error: zero? expects only 1 argument, but found more than 1",
);
checkError(
  () => rackette("(number? 5 3)"),
  "Error: number? expects only 1 argument, but found more than 1",
);
checkError(
  () => rackette("(let ((x 1) (x 4)) (+ x 1))"),
  "Error: A variable name was defined locally more than once",
);
checkError(
  () => rackette("(define x 5) (define x 6) x"),
  "Error: This name was defined previously and cannot be re-defined",
);
checkError(
  () => rackette("((lambda (x x) (+ x 1)) 1 2)"),
  "Error: Found a variable that is used more than once",
);
checkError(
  () => rackette("(lambda (x))"),
  "Error: lambda expression did not have correct number of arguments",
);
checkError(
  () => rackette("brown"),
  "Error: This variable/function is not defined",
);
checkError(
  () => rackette("(I like CS)"),
  "Error: This variable/function is not defined",
);
checkError(
  () => rackette("(cond
  ((zero? 3) 1)
  ((number? true) 5))"),
  "Error: All cond expressions resulted in false",
);
checkError(
  () => rackette("if (zero? 3) 3 5"),
  "Error: reserved keyword",
);
checkError(
  () => rackette("(if (+ 4 5) 3 5)"),
  "Error: if question result was not true or false",
);
checkError(
  () => rackette("(+ true false)"),
  "Error: + expects 2 number arguments, did not find them",
);
checkError(
  () => rackette("(zero? false)"),
  "Error: zero? expects a number and didn't find one",
);
checkError(
  () => rackette("(or 1 2)"),
  "Error: or expression has incorrect arguments",
);
checkError(
  () => rackette("(and 1 2)"),
  "Error: and expression has incorrect arguments",
);
checkError(
  () => rackette("cond"),
  "Error: reserved keyword",
);
checkError(
  () => rackette("define"),
  "Error: reserved keyword",
);
checkError(
  () => rackette("and"),
  "Error: reserved keyword",
);
checkError(
  () => rackette("or"),
  "Error: reserved keyword",
);
checkError(
  () => rackette("if"),
  "Error: reserved keyword",
);
checkError(
  () => rackette("lambda"),
  "Error: reserved keyword",
);
checkError(
  () => rackette("let"),
  "Error: reserved keyword",
);