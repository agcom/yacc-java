%{
	#include <stdio.h>
	
	int yylex();
	void yyerror(const char*);
%}

%token AbstractKeyword "abstract"
%token ContinueKeyword "continue"
%token ForKeyword "for"
%token NewKeyword "new"
%token SwitchKeyword "switch"
%token AssertKeyword "assert"
%token DefaultKeyword "default"
%token IfKeyword "if"
%token PackageKeyword "package"
%token SynchronizedKeyword "synchronized"
%token BooleanKeyword "boolean"
%token DoKeyword "do"
%token GotoKeyword "goto"
%token PrivateKeyword "private"
%token ThisKeyword "this"
%token BreakKeyword "break"
%token DoubleKeyword "double"
%token ImplementsKeyword "implements"
%token ProtectedKeyword "protected"
%token ThrowKeyword "throw"
%token ByteKeyword "byte"
%token ElseKeyword "else"
%token ImportKeyword "import"
%token PublicKeyword "public"
%token ThrowsKeyword "throws"
%token CaseKeyword "case"
%token EnumKeyword "enum"
%token InstanceofKeyword "instanceof"
%token ReturnKeyword "return"
%token TransientKeyword "transient"
%token CatchKeyword "catch"
%token ExtendsKeyword "extends"
%token IntKeyword "int"
%token ShortKeyword "short"
%token TryKeyword "try"
%token CharKeyword "char"
%token FinalKeyword "final"
%token InterfaceKeyword "interface"
%token StaticKeyword "static"
%token VoidKeyword "void"
%token ClassKeyword "class"
%token FinallyKeyword "finally"
%token LongKeyword "long"
%token StrictfpKeyword "strictfp"
%token VolatileKeyword "volatile"
%token ConstKeyword "const"
%token FloatKeyword "float"
%token NativeKeyword "native"
%token SuperKeyword "super"
%token WhileKeyword "while"
%token UnderlineKeyword '_'
%token VarKeyword "var"
%token YieldKeyword "yield"
%token PermitsKeyword "permits"
%token SealedKeyword "sealed"
%token NonSealedKeyword "non-sealed"
%token RecordKeyword "record"

%token Identifier
%token Literal

%token OpenParenthesisSeparator '('
%token CloseParenthesisSeparator ')'
%token OpenBraceSeparator '{'
%token CloseBraceSeparator '}'
%token OpenBracketSeparator '['
%token CloseBracketSeparator ']'
%token SemicolonSeparator ';'
%token CommaSeparator ','
%token DotSeparator '.'
%token DoubleColonSeparator "::"

%token AssignmentOperator '='
%token GreaterThanOperator '>'
%token LessThanOperator '<'
%token NotOperator '!'
%token ComplementOperator '~'
%token QuestionMarkOperator '?'
%token ColonOperator ':'
%token AddOperator '+'
%token MinusOperator '-'
%token MultiplyOperator '*'
%token XorOperator '^'
%token ModOperator '%'
%token OrOperator '|'
%token AndOperator '&'
%token DivideOperator '/'
%token ArrowOperator "->"
%token EqualOperator "=="
%token GreaterThanOrEqualOperator ">="
%token LessThanOrEqualOperator "<="
%token NotEqualOperator "!="
%token BooleanAndOperator "&&"
%token BooleanOrOperator "||"
%token IncrementOperator "++"
%token DecrementOperator "--"
%token ShiftLeftOperator "<<"
%token ShiftRightOperator ">>"
%token ShiftRightArithmeticOperator ">>>"
%token AddAssignmentOperator "+="
%token MinusAssignmentOperator "-="
%token MultiplyAssignmentOperator "*="
%token DivideAssignmentOperator "/="
%token AndAssignmentOperator "&="
%token OrAssignmentOperator "|="
%token XorAssignmentOperator "^="
%token ModAssignmentOperator "%="
%token ShiftLeftAssignmnetOperator "<<="
%token ShiftRightAssignmentOperator ">>="
%token ShiftRightArithmeticAssignmentOperator ">>>="

%%
Compilation:
	PkgOpt Imports Classes
	;

PkgOpt:
	Pkg
	| %empty
	;

Pkg:
	"package" Name ';'
	;

Name:
	Name '.' Identifier
	| Identifier
	;

Imports:
	Imports Import
	| %empty
	;

Import:
	"import" StaticOpt Name ';'
	;

StaticOpt:
	"static"
	| %empty
	;

Classes:
	Classes Class
	| %empty
	;

Class:
	ClassMods "class" Identifier ClassExtendsOpt ClassImplOpt ClassBody
	;

ClassMods:
	ClassMods ClassMod
	| %empty
	;

ClassMod:
	"public"
	| "protected"
	| "private"
	| "abstract"
	| "static"
	| "final"
	| "strictfp"
	;

ClassExtendsOpt:
	ClassExtends
	| %empty
	;

ClassExtends:
	"extends" Name
	;

ClassImplOpt:
	ClassImpl
	| %empty
	;

ClassImpl:
	"implements" NameListNonEmpty
	;

NameListNonEmpty:
	Name
	| NameListNonEmpty ',' Name
	;

ClassBody:
	'{' ClassBodyMembers '}'
	;

ClassBodyMembers:
	ClassBodyMembers ClassBodyMember
	| %empty
	;

ClassBodyMember:
	Init
	| Method
	;

Init:
	Block
	;

Method:
	MethodMods MethodHead MethodBody
	;

MethodMods:
	MethodMods MethodMod
	| %empty
	;

MethodMod:
	"public"
	| "protected"
	| "private"
	| "abstract"
	| "static"
	| "final"
	| "synchronized"
	| "native"
	| "strictfp"
	;

MethodHead:
	MethodReturnType MethodSign ThrowsOpt
	;

MethodReturnType:
	Type
	| "void"
	;

Type:
	Name
	| PrimType
	| ArrType
	;

PrimType:
	"byte"
	| "short"
	| "int"
	| "long"
	| "char"
	| "float"
	| "double"
	| "boolean"
	;

ArrType:
	PrimType Dims
	| Name Dims
	;

Dims:
	'[' ']'
	| Dims '[' ']'
	;

MethodSign:
	Identifier '(' ParamList ')'
	;

ParamList:
	%empty
	| ParamListNonEmpty
	;

ParamListNonEmpty:
	Param
	| ParamList ',' Param
	;

Param:
	VarMods Type Identifier
	;

VarMods:
	VarMods VarMod
	| %empty
	;

VarMod:
	"final"
	;

ThrowsOpt:
	Throws
	| %empty
	;

Throws:
	"throws" TypeListNonEmpty
	;

TypeListNonEmpty:
	Type
	| TypeListNonEmpty ',' Type
	;

MethodBody:
	Block
	| ';'
	;

Block:
	'{' BlockStmts '}'
	;

BlockStmts:
	BlockStmts BlockStmt
	| %empty
	;

BlockStmt:
	Var
	| Stmt
	;

Var:
	VarMods VarType VarDeclListNonEmpty
	;

VarType:
	Type
	| "var"
	;

VarDeclListNonEmpty:
	VarDecl
	| VarDeclListNonEmpty ',' VarDecl
	;

VarDecl:
	Identifier VarAssignOpt
	;

VarAssignOpt:
	VarAssign
	| %empty
	;

VarAssign:
	'=' VarInit
	;

VarInit:
	Expr
	| ArrInit
	;

Stmt:
	';'
	| Block
	| StmtExpr ';'
	| AssertStmt
	| SwitchStmt
	| DoStmt
	| BreakStmt
	| ContStmt
	| ReturnStmt
	| SyncStmt
	| ThrowStmt
	| TryStmt
	| LblStmt
	| IfStmt
	| IfElseStmt
	| WhileStmt
	| ForStmt
	;

StmtNoShortIf:
	';'
	| Block
	| StmtExpr ';'
	| AssertStmt
	| SwitchStmt
	| DoStmt
	| BreakStmt
	| ContStmt
	| ReturnStmt
	| SyncStmt
	| ThrowStmt
	| TryStmt
	| LblStmtNoShortIf
	| IfElseStmtNoShortIf
	| WhileStmtNoShortIf
	| ForStmtNoShortIf
	;

StmtExpr:
	Assign
	| PreIncExpr
	| PreDecExpr
	| PostIncExpr
	| PostDecExpr
	| MethodInvoke
	| NewClassExpr
	;

LblStmt:
	Identifier ':' Stmt
	;

LblStmtNoShortIf:
	Identifier ':' StmtNoShortIf
	;

IfStmt:
	"if" '(' Expr ')' Stmt
	;

IfElseStmt:
	"if" '(' Expr ')' StmtNoShortIf "else" Stmt
	;

IfElseStmtNoShortIf:
	"if" '(' Expr ')' StmtNoShortIf "else" StmtNoShortIf
	;

AssertStmt:
	"assert" Expr ';'
	| "assert" Expr ':' Expr ';'
	;

SwitchStmt:
	"switch" '(' Expr ')' SwitchBlock
	;

SwitchBlock:
	'{' SwitchRulesNonEmpty '}'
	| '{' SwitchBlockStmtGroups SwitchLblColons '}'
	;

SwitchRulesNonEmpty:
	SwitchRule
	| SwitchRulesNonEmpty SwitchRule
	;

SwitchRule:
	SwitchLbl "->" Expr ';'
	| SwitchLbl "->" Block
	| SwitchLbl "->" ThrowStmt
	;

SwitchLbl:
	"case" CondExprListNonEmpty
	| "default"
	;

CondExprListNonEmpty:
	CondExpr
	| CondExprListNonEmpty ',' CondExpr
	;

SwitchBlockStmtGroups:
	SwitchBlockStmtGroups SwitchBlockStmtGroup
	| %empty
	;

SwitchLblColons:
	SwitchLblColons SwitchLbl ':'
	| %empty
	;

SwitchBlockStmtGroup:
	SwitchLbl ':' SwitchLblColons BlockStmts
	;

WhileStmt:
	"while" '(' Expr ')' Stmt
	;

WhileStmtNoShortIf:
	"while" '(' Expr ')' StmtNoShortIf
	;

DoStmt:
	"do" Stmt "while" '(' Expr ')'
	;

ForStmt:
	ForIStmt
	| ForEachStmt
	;

ForStmtNoShortIf:
	ForIStmtNoShortIf
	| ForEachStmtNoShortIf
	;

ForIStmt:
	"for" '(' ForInitOpt ';' Expr ';' ForUpOpt ')' Stmt
	;

ForIStmtNoShortIf:
	"for" '(' ForInitOpt ';' Expr ';' ForUpOpt ')' StmtNoShortIf
	;

ForInitOpt:
	ForInit
	| %empty
	;

ForInit:
	StmtExprListNonEmpty
	| VarDecl
	;

StmtExprListNonEmpty:
	StmtExpr
	| StmtExprListNonEmpty ',' StmtExpr
	;

ForUpOpt:
	ForUp
	| %empty
	;

ForUp:
	StmtExprListNonEmpty
	;

ForEachStmt:
	"for" '(' VarDecl ':' Expr ')' Stmt
	;

ForEachStmtNoShortIf:
	"for" '(' VarDecl ':' Expr ')' StmtNoShortIf
	;

BreakStmt:
	"break" IdentifierOpt ';'
	;

IdentifierOpt:
	Identifier
	| %empty
	;

ContStmt:
	"continue" IdentifierOpt ';'
	;

ReturnStmt:
	"return" ExprOpt ';'
	;

ExprOpt:
	Expr
	| %empty
	;

ThrowStmt:
	"throw" Expr ';'
	;

SyncStmt:
	"synchronized" '(' Expr ')' Block
	;

TryStmt:
	"try" Block CatchesNonEmpty
	| "try" Block Catches Finally
	;

CatchesNonEmpty:
	Catch
	| CatchesNonEmpty Catch
	;

Catch:
	"catch" '(' VarMods CatchNameListNonEmpty ')' Block
	;

CatchNameListNonEmpty:
	Name
	| CatchNameListNonEmpty '|' Name
	;

Catches:
	CatchesNonEmpty
	| %empty
	;

Finally:
	"finally" Block
	;

Primary:
	PrimaryNoNewArr
	| NewArrExpr
	;

PrimaryNoNewArr:
	Literal
	| ClassLiteral
	| "this"
	| Name '.' "this"
	| '(' Expr ')'
	| NewClassExpr
	| FieldAccess
	| ArrAccess
	| MethodInvoke
	| MethodRef
	;

ClassLiteral:
	Type '.' "class"
	| "void" '.' "class"
	;

NewClassExpr:
	"new" Name '(' ArgList ')'
	;

ArgList:
	ArgListNonEmpty
	| %empty
	;

ArgListNonEmpty:
	ArgListNonEmpty ',' Arg
	| Arg
	;

Arg:
	Expr
	;

FieldAccess:
	Primary '.' Identifier
	| "super" '.' Identifier
	| Name '.' "super" '.' Identifier
	;

ArrAccess:
	Name '[' Expr ']'
	| PrimaryNoNewArr '[' Expr ']'
	;

MethodInvoke:
	Name '(' ArgList ')'
	| Primary '.' Identifier '(' ArgList ')'
	| "super" '.' Identifier '(' ArgList ')'
	| Name '.' "super" '.' Identifier '(' ArgList ')'
	;

MethodRef:
	Name "::" Identifier
	| Primary "::" Identifier
	| ArrType "::" Identifier
	| "super" "::" Identifier
	| Name '.' "super" "::" Identifier
	| Name "::" "new"
	| ArrType "::" "new"
	;

NewArrExpr:
	"new" PrimType DimExprs DimsOpt
	| "new" Name DimExprs DimsOpt
	| "new" PrimType Dims ArrInit
	| "new" Name Dims ArrInit
	;

DimExprs:
	DimExpr
	| DimExprs DimExpr
	;

DimExpr:
	'[' Expr ']'
	;

DimsOpt:
	Dims
	| %empty
	;

ArrInit:
	'{' VarInitList CommaOpt '}'
	;

VarInitList:
	VarInitListNonEmpty
	| %empty
	;

VarInitListNonEmpty:
	VarInit
	| VarInitListNonEmpty ',' VarInit
	;

CommaOpt:
	','
	| %empty
	;

Expr:
	AssignExpr
	;

AssignExpr:
	CondExpr
	| Assign
	;

CondExpr:
	CondOrExpr
	| CondOrExpr '?' Expr ':' CondExpr
	;

CondOrExpr:
	CondAndExpr
	| CondOrExpr "||" CondAndExpr
	;

CondAndExpr:
	InclOrExpr
	| CondOrExpr "||" CondAndExpr
	;

CondAndExpr:
	InclOrExpr
	| CondAndExpr "&&" InclOrExpr
	;

InclOrExpr:
	ExclOrExpr
	| InclOrExpr '|' ExclOrExpr
	;

ExclOrExpr:
	AndExpr
	| ExclOrExpr '^' AndExpr
	;

AndExpr:
	EqExpr
	AndExpr '&' EqExpr
	;

EqExpr:
	RelExpr
	| EqExpr "==" RelExpr
	| EqExpr "!=" RelExpr
	;

RelExpr:
	ShiftExpr
	| RelExpr '<' ShiftExpr
	| RelExpr '>' ShiftExpr
	| RelExpr "<=" ShiftExpr
	| RelExpr ">=" ShiftExpr
	| InstOfExpr
	;

InstOfExpr:
	RelExpr "instanceof" Name
	;

ShiftExpr:
	AddExpr
	| ShiftExpr "<<" AddExpr
	| ShiftExpr ">>" AddExpr
	| ShiftExpr ">>>" AddExpr
	;

AddExpr:
	MultExpr
	| AddExpr '+' MultExpr
	| AddExpr '-' MultExpr
	;

MultExpr:
	UnExpr
	| MultExpr '*' UnExpr
	| MultExpr '/' UnExpr
	| MultExpr '%' UnExpr
	;

UnExpr:
	PreIncExpr
	| PreDecExpr
	| '+' UnExpr
	| '-' UnExpr
	| UnExprNotPlusMinus
	;

PreIncExpr:
	"++" UnExpr
	;

PreDecExpr:
	"--" UnExpr
	;

UnExprNotPlusMinus:
	PostfixExpr
	| '~' UnExpr
	| '!' UnExpr
	| CastExpr
	;

PostfixExpr:
	Primary
	| Name
	| PostIncExpr
	| PostDecExpr
	;

PostIncExpr:
	PostfixExpr "++"
	;

PostDecExpr:
	PostfixExpr "--"
	;

CastExpr:
	'(' Type ')' UnExpr
	;

Assign:
	LHS AssignOp Expr
	;

LHS:
	Name
	| FieldAccess
	| ArrAccess
	;

AssignOp:
	'='
	| "*="
	| "/="
	| "%="
	| "+="
	| "-="
	| "<<="
	| ">>="
	| ">>>="
	| "&="
	| "^="
	| "|="
	;


%%

void yyerror(const char* s) {
	fprintf(stderr, "%s\n", s);
}

int main() {
	return yyparse();
}
