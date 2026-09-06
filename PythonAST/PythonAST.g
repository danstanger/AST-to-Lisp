grammar PythonAST;

options {
    backtrack=true;
    memoize=true;
    language=Java;
    output=AST;
    ASTLabelType=CommonTree;
}

tokens {
ALIAS = 'alias' ;
ANNASSIGN = 'AnnAssign' ;
ANNOTATION = 'annotation' ;
ARG = 'arg' ;
ARGS = 'args' ;
ARGUMENTS = 'arguments' ;
Assert = 'Assert' ;
ASNAME = 'asname' ;
ASSIGN = 'Assign' ;
AsyncFunctionDef = 'AsyncFunctionDef' ;
ATTR = 'attr' ;
ATTRIBUTE = 'Attribute' ;
AugAssign = 'AugAssign' ;
Await = 'Await' ;
BASES = 'bases' ;
BINOP = 'BinOp' ;
BoolOp = 'BoolOp' ;
BODY = 'body' ;
BREAK = 'Break' ;
CALL = 'Call' ;
CLASS = 'ClassDef' ;
CAUSE = 'cause' ;
COMPARE = 'Compare' ;
COMPARATORS = 'comparators' ;
COMPREHENSION = 'comprehension' ;
CONSTANT = 'Constant' ;
CONTINUE = 'Continue' ;
CONVERSION = 'conversion' ;
CTX = 'ctx' ;
DECORATOR_LIST = 'decorator_list' ;
DEFAULTS = 'defaults' ;
DEL = 'Del' ;
Delete = 'Delete' ;
DICT = 'Dict' ;
Ellipsis = 'Ellipsis' ;
ELLIPSIS = '...' ;
ELT = 'elt' ;
ELTS = 'elts' ;
EXC = 'exc' ;
ExceptHandler = 'ExceptHandler' ;
EXPR= 'Expr' ;
FINALBODY = 'finalbody' ;
For = 'For' ;
FormattedValue = 'FormattedValue' ;
FUNC = 'func' ;
FunctionDef = 'FunctionDef' ;
GeneratorExp = 'GeneratorExp' ;
GENERATORS = 'generators' ;
HANDLERS = 'handlers' ;
IDENTIFIER = 'name' ;
IF = 'If' ;
IFS = 'ifs' ;
IfExp = 'IfExp';
IMPORT = 'Import' ;
IMPORTFROM = 'ImportFrom' ;
IS_ASYNC = 'is_async' ;
ITER = 'iter' ;
JoinedStr = 'JoinedStr' ;
KEYS = 'keys' ;
KEYWORD = 'keyword' ;
KEYWORDS = 'keywords' ;
KWONLYARGS = 'kwonlyargs' ;
KW_DEFAULTS = 'kw_defaults' ;
Lambda = 'Lambda' ;
LEFT = 'left' ;
LEVEL = 'level' ;
List = 'List' ;
ListComp = 'ListComp' ;
LOAD = 'Load' ;
LOWER = 'lower' ;
MODULE = 'Module' ;
MODULES ;
MSG = 'msg' ;
LC_MODULE = 'module' ;
NAME = 'Name' ;
NAMES = 'names' ;
NIL ;
None = 'None' ;
NUM = 'Num' ;
OP = 'op' ;
OPS = 'ops' ;
OPERAND = 'operand' ;
ORELSE = 'orelse' ;
POSONLYARGS = 'posonlyargs' ;
PASS = 'Pass' ;
RAISE = 'Raise' ;
RETURN = 'Return' ;
RETURNS = 'returns' ;
RIGHT = 'right' ;
SetComp = 'SetComp' ;
SIMPLE = 'simple' ;
SLICE = 'Slice' ;
LC_SLICE = 'slice' ;
Starred = 'Starred' ;
STEP = 'step' ;
STORE = 'Store' ;
SUBSCRIPT = 'Subscript' ;
TARGET = 'target' ;
TARGETS = 'targets' ;
TEST = 'test' ;
Try = 'Try' ;
TUPLE = 'Tuple' ;
TYPE = 'type' ;
TYPE_IGNORES = 'type_ignores' ;
UnaryOp = 'UnaryOp' ;
UPPER = 'upper' ;
VALUE = 'value' ;
VALUES = 'values' ;
VARARG = 'vararg' ;
Yield = 'Yield' ;
YieldFrom = 'YieldFrom' ;
LC_ID = 'id' ;
}

// Operators
fragment BOOLOP : ('And' | 'Or') ;
fragment Operators: ('Add'|'Sub'|'Mult'|'MatMult'|'Div'|'Mod'|'Pow'|'LShift'|
'RShift'|'BitOr'|'BitXor'|'BitAnd'|'FloorDiv') ;
fragment Cmpop:('Eq'|'NotEq'|'Lt'|'LtE'|'Gt'|'GtE'|'Is'|'IsNot'|'In'|'NotIn') ;
fragment UnaryOps : ('Invert'|'Not'|'UAdd'|'USub') ;
// Misc
ID  : ('a'..'z'|'A'..'Z'|'_')+ ;      // match identifiers
INT : ('+'|'-')? ('0'..'9')+ ;         // match integers
STRING : '\'' ('\'\'' | ~('\''))* '\'' ;
LINE_COMMENT : '#' ~('\r'|'\n')* '\r'? '\n'
    {
        $channel = HIDDEN ;
    }
    ;
NEWLINE: '\r'? '\n' 
    {
        $channel = HIDDEN ;  // dont return newlines to parser for file input
    }
    ;
WS  :  (' '|'\r'|'\t'|'\n')
    {
            $channel = HIDDEN;
    }
    ;

ast: module+ -> ^(MODULES module+);

module: MODULE '(' BODY '=' stmtList (',' TYPE_IGNORES '=' list)? ')' ->
	^(MODULE["COMPILATION-UNIT"]
	  ^(BODY stmtList)
	  ^(TYPE_IGNORES list))
    ;

stmt:
    functionDef
    | AsyncFunctionDef
    | classStmt
    | assignStmt
    | annAssignStmt
    | augAssignStmt
    | returnStmt
    | forStmt
    | ifStmt
    | importFrom
    | assertStmt
    | BODY '=' list
    | BASES '=' list
    | DECORATOR_LIST '=' list ','
    | Delete '(' TARGETS '=' targetsList ')'
    | TARGETS '=' list
    | tryStmt
    | Await '(' VALUE '=' expr ')'
    | EXPR '(' VALUE '=' expr ')'
    | RAISE '(' (EXC '=' expr) (',' CAUSE '=' expr)? ')'
    | IMPORT '(' NAMES '=' aliasList ')'
    | TYPE_IGNORES '=' list
//    | ID '=' expr_context
    | ID '=' STRING
    | ID '=' (expr | list)
    | expr
    | NEWLINE
    ;

forStmt : For^ '(' TARGET '='! expr ',' ITER '=' expr (',' BODY '=' stmtList)?
   (',' ORELSE '=' elseStmtList)? (',' type_comment)? ')' ;

ifStmt : IF '(' TEST '=' expr ',' BODY '=' stmtList ',' ORELSE '=' elseStmtList ')' ;
importFrom : IMPORTFROM '(' LC_MODULE '=' STRING ',' NAMES '=' aliasList ',' LEVEL '=' INT ')' ;
assertStmt : Assert '(' TEST '=' expr (',' MSG '=' expr)? ')' ;
tryStmt : Try '(' BODY '=' stmtList ',' HANDLERS '=' handlerList
    (',' ORELSE '=' elseStmtList)? ',' FINALBODY '=' stmtList ')' ;

handlerList : '[' exceptHandler (',' exceptHandler)? ']' ;
exceptHandler : ExceptHandler '('
    TYPE '=' expr (',' identifier)? ',' BODY '=' stmtList ')' ;

functionDef: FunctionDef '('
    identifier ','
    ARGS '=' ARGUMENTS '(' arguments ')' ','
    BODY '=' stmtList ','
    DECORATOR_LIST '=' decoratorList
    (',' RETURNS '=' expr)?
    (',' type_comment)?
    (',' type_params)? ')'
    ;

asyncFunctionDef: AsyncFunctionDef '('
    identifier ','
    ARGS '=' ARGUMENTS '(' arguments ')' ','
    BODY '=' stmtList ','
    DECORATOR_LIST '=' decoratorList
    (',' RETURNS '=' expr)?
    (',' type_comment)?
    (',' type_params)? ')'
    ;

type_comment: STRING ;
type_params: list ;
classStmt : CLASS '('
    identifier ','
    BASES '=' basesList ','
    keywords ','
    BODY '=' stmtList ','
    (DECORATOR_LIST '=' decoratorList)? ')'
    ;

alias : ALIAS '(' identifier (',' ASNAME '=' STRING)? ')' ;
assignStmt : ASSIGN '(' TARGETS '=' targetsList ',' VALUE '=' expr
 (',' stmt)? ')' ;
annAssignStmt : ANNASSIGN '(' TARGET '=' (nameExpr|attributeExpr|subscriptExpr)
 ',' annotation ',' (VALUE '=' expr ',')? SIMPLE '=' INT ')' ;
augAssignStmt : AugAssign '(' TARGET '=' nameExpr ',' operator ',' VALUE '=' expr ')' ;
annotation: ANNOTATION '=' (nameExpr | constantExpr | subscriptExpr) ;
returnStmt : RETURN '(' (VALUE '=' expr)? ')' ;

list : '[' (stmt (',' stmt)*)? ']' ;
aliasList : '[' alias (',' alias)* ']' ;
stmtList : '[' stmt (',' stmt)* ']' -> stmt+ ;
elseStmtList : '[' (stmt (',' stmt)*)? ']' ;

basesList : '[' basesExpr (',' basesExpr)* ']' | '[' ']' ;
decoratorList : '[' nameExpr (',' nameExpr)* ']' | '[' ']' ;
targetsList : '[' (targets (',' targets)*)? ']' ;
targets : nameExpr|attributeExpr|subscriptExpr|tupleExpr ;
identifier : (IDENTIFIER|LC_ID)^ '='! STRING ;
expr_context: CTX^ '='! (LOAD|STORE|DEL) '('! ')'!;

expr:
    constantExpr
    | (PASS | BREAK | CONTINUE) '(' ')'
    | BINOP '(' leftExpr ',' operator ',' rightExpr ')'
    | BoolOp '(' OP '=' BOOLOP '(' ')' ',' VALUES '=' exprList ')'
    | compareExpr
    | CALL^ '('! FUNC '='! expr (',' callArgsList)? (',' keywords)? ')'!
    | DICT '(' (KEYS '=' kexprList ',')? VALUES '=' vexprList ')'
    | GeneratorExp '(' ELT '=' expr ',' GENERATORS '=' comprehensions ')'
    | IfExp '(' TEST '=' expr ',' BODY '=' expr ',' ORELSE '=' expr ')'
    | Lambda '(' ARGS '=' ARGUMENTS '(' arguments ')' ',' BODY '=' expr ')'
    | leftExpr
    | List '(' ELTS '=' exprList ',' expr_context ')'
    | ListComp '(' ELT '=' expr ',' GENERATORS '=' comprehensions ')'
    | SetComp '(' ELT '=' expr ',' GENERATORS '=' comprehensions ')'
    | rightExpr
    | tupleExpr
    | nameExpr
    | attributeExpr
    | subscriptExpr
    | starredExpr
    | FormattedValue '(' VALUE '=' expr ',' CONVERSION '=' INT ')'
    | JoinedStr '(' VALUES '=' exprList ')'
    | UnaryOp '(' unaryOp ',' OPERAND '=' expr ')'
    | INT
    | ID
    | '(' expr ')'
    ;

compareExpr: COMPARE '(' expr ',' OPS '=' cmpops ',' COMPARATORS '=' list ')' ;
constantExpr :  CONSTANT '(' VALUE '=' constants (',' STRING)? ')' ;
constants: None|Ellipsis|ELLIPSIS|INT|ID|STRING ;
comprehensions : '[' (comprehension (',' comprehension)*)? ']' ;
comprehension : COMPREHENSION '(' TARGET '=' expr ',' ITER '=' expr
    (',' IFS '=' exprList)? ',' IS_ASYNC '=' INT ')' ;
basesExpr : nameExpr
    | attributeExpr
    ;
attributeExpr : ATTRIBUTE '(' VALUE '=' expr ',' ATTR '=' STRING ',' expr_context ')' ;
leftExpr : LEFT '=' expr ;
rightExpr : RIGHT '=' expr ;
nameExpr : NAME^ '('! identifier ','! expr_context ')'! ;
starredExpr : Starred '(' VALUE '=' expr ',' expr_context ')' ;
slice :
      SLICE '(' (LOWER '=' expr)? (',')? (UPPER '=' expr)? (',' STEP '=' expr)? ')'
    | expr
    ;
tupleExpr : TUPLE '(' ELTS '=' exprList ',' expr_context ')' ;
kexprList : exprList ;
vexprList : exprList ;
exprList : '[' (expr (',' expr)*)? ']' ;
subscriptExpr : SUBSCRIPT '(' VALUE '=' expr ',' LC_SLICE '=' slice ',' expr_context ')' ;
callArgsList : ARGS '=' '[' (expr (',' expr)*)? ']' ;
arguments :
      POSONLYARGS '=' posArgs ','
      ARGS '=' argsList ','
      (VARARG '=' arg ',')?
      KWONLYARGS '=' kwArgs ','
      KW_DEFAULTS '=' kwDefaults ','
      DEFAULTS '=' defaultsList
    ;

posArgs : argsList ;
kwArgs : argsList ;
kwDefaults : argsList ;
defaultsList : '[' constantExpr (',' constantExpr)* ']' | '[' ']' ;
argsList : '[' arg (',' arg)* ']' | '[' ']' ;
arg: ARG '(' ARG '=' STRING (',' ANNOTATION '=' expr)? ')' ;
keywords : KEYWORDS '=' '[' (keyword (',' keyword)*)? ']' ;
keyword : KEYWORD '(' ARG '=' STRING ',' VALUE '=' expr ')' ;
cmpops : '[' cmpop (',' cmpop)* ']' ;
cmpop : Cmpop '(' ')' ;
operator : OP '=' Operators '(' ')' ;
unaryOp : OP '=' UnaryOps '(' ')' ;
