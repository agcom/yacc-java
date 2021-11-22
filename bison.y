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

CompilationUnit:
	PackageDeclarationOptional ImportDeclarationKleeneStar ClassDeclarationKleeneStar
	;

PackageDeclarationOptional:
	PackageDeclaration
	| %empty
	;

PackageDeclaration:
	"package" Name ';'
	;

Name:
	Identifier DotIdentifierKleeneStar
	;

DotIdentifierKleeneStar:
	DotIdentifierKleeneStar '.' Identifier
	| %empty
	;

ImportDeclarationKleeneStar:
	ImportDeclarationKleeneStar ImportDeclaration
	| %empty
	;

ImportDeclaration:
	"import" StaticKeywordOptional Name ';'
	;

StaticKeywordOptional:
	"static"
	| %empty
	;

ClassDeclarationKleeneStar:
	ClassDeclarationKleeneStar ClassDeclaration
	| %empty
	;

ClassDeclaration:
	ClassModifierKleeneStar "class" Identifier ClassExtendsOptional ClassImplementsOptional ClassBody
	;

ClassModifierKleeneStar:
	ClassModifierKleeneStar ClassModifier
	| %empty
	;

ClassModifier:
	"public"
	| "protected"
	| "private"
	| "abstract"
	| "static"
	| "final"
	| "strictfp"
	;

ClassExtendsOptional:
	ClassExtends
	| %empty
	;

ClassExtends:
	"extends" Name
	;

ClassImplementsOptional:
	ClassImplements
	| %empty
	;

ClassImplements:
	"implements" NameListNonEmpty
	;

NameListNonEmpty:
	Name CommaNameKleeneStar
	;

CommaNameKleeneStar:
	CommaNameKleeneStar ',' Name
	| %empty
	;

ClassBody:
	'{' ClassBodyDeclarationKleeneStar '}'
	;

ClassBodyDeclarationKleeneStar:
	ClassBodyDeclarationKleeneStar ClassBodyDeclaration
	| %empty
	;

ClassBodyDeclaration:
	Initializer
	| MethodDeclaration
	;

Initializer:
	Block
	;

MethodDeclaration:
	MethodModifierKleeneStar MethodHeader MethodBody
	;

MethodModifierKleeneStar:
	MethodModifierKleeneStar MethodModifier
	| %empty;

MethodModifier:
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

MethodHeader:
	Result MethodDeclarator ThrowsOptional
	;

Result:
	Type
	| "void"
	;

Type:
	Name
	| PrimitiveType
	| ArrayType
	;

PrimitiveType:
	"byte"
	| "short"
	| "int"
	| "long"
	| "char"
	| "float"
	| "double"
	| "boolean"
	;

ArrayType:
	PrimitiveTypeOrName Dims
	;

PrimitiveTypeOrName:
	PrimitiveType
	| Name
	;

Dims:
	OpenBracketCloseBracketKleenePlus
	;

OpenBracketCloseBracketKleenePlus:
	'[' ']' OpenBracketCloseBracketKleeneStar
	;

OpenBracketCloseBracketKleeneStar:
	OpenBracketCloseBracketKleeneStar '[' ']'
	| %empty
	;

MethodDeclarator:
	Identifier '(' ParameterList ')' DimsOptional
	;

ParameterList:
	ParameterListNonEmpty
	| %empty
	;

ParameterListNonEmpty:
	Parameter CommaParameterKleeneStar
	;

Parameter:
	VariableModifierKleeneStar Type VariableDeclaratorId
	;

VariableModifierKleeneStar:
	VariableModifierKleeneStar VariableModifier
	| %empty
	;

VariableModifier:
	"final"
	;

VariableDeclaratorId:
	Identifier DimsOptional
	;

CommaParameterKleeneStar:
	CommaParameterKleeneStar ',' Parameter
	| %empty
	;

DimsOptional:
	Dims
	| %empty
	;

ThrowsOptional:
	Throws
	| %empty
	;

Throws:
	"throws" TypeListNonEmpty
	;

TypeListNonEmpty:
	Type CommaTypeKleeneStar
	;

CommaTypeKleeneStar:
	CommaTypeKleeneStar ',' Type
	| %empty
	;

MethodBody:
	Block
	| ';'
	;

Block:
	'{' BlockStatementKleeneStar '}'
	;

BlockStatementKleeneStar:
	BlockStatementKleeneStar BlockStatement
	| %empty
	;

BlockStatement:
	VariableDeclaration
	| Statement
	;

VariableDeclaration:
	VariableModifierKleeneStar VariableType VariableDeclaratorListNonEmpty
	;

VariableModifierKleeneStar:
	VariableModifierKleeneStar VariableModifier
	| %empty
	;

VariableModifier:
	"final"
	;

VariableType:
	Type
	| "var"
	;

VariableDeclaratorListNonEmpty:
	VariableDeclarator CommaVariableDeclaratorKleeneStar
	;

CommaVariableDeclaratorKleeneStar:
	CommaVariableDeclaratorKleeneStar ',' VariableDeclarator
	;

VariableDeclarator:
	VariableDeclaratorId EqualVariableInitializerOptional
	;

EqualVariableInitializerOptional:
	'=' VariableInitializer
	| %empty
	;

VariableInitializer:
	Expression
	| ArrayInitializer
	;

Statement:
	Block
	| EmptyStatement
	| ExpressionStatement
	| AssertStatement
	| SwitchStatement
	| DoStatement
	| BreakStatement
	| ContinueStatement
	| ReturnStatement
	| SynchronizedStatement
	| ThrowStatement
	| TryStatement
	| LabeledStatement
	| IfThenStatement
	| IfThenElseStatement
	| WhileStatement
	| ForStatement
	;

EmptyStatement:
	';'
	;

ExpressionStatement:
	StatementExpression ';'
	;

StatementExpression:
	Assignment
	| PreIncrementExpression
	| PreDecrementExpression
	| PostIncrementExpression
	| PostDecrementExpression
	| MethodInvocation
	| ClassInstanceCreationExpression
	;

// Stubs

Expression: Literal '+' Literal ';';
ArrayInitializer: "new" "int" '[' ']' '{' '}';

%%

void yyerror(const char* s) {
	fprintf(stderr, "%s\n", s);
}

int main() {
	return yyparse();
}
