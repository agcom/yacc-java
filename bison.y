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
%token UnderlineKeyword "_"
%token VarKeyword "var"
%token YieldKeyword "yield"
%token PermitsKeyword "permits"
%token SealedKeyword "sealed"
%token NonSealedKeyword "non-sealed"
%token RecordKeyword "record"

%token Identifier
%token Literal

%%

CompilationUnit:
	PackageDeclarationOptional ImportDeclarationKleeneStar ClassDeclarationKleeneStar
	;

ImportDeclarationKleeneStar:
	ImportDeclarationKleeneStar ImportDeclaration
	| %empty
	;
	
ClassDeclarationKleeneStar:
	ClassDeclarationKleeneStar ClassDeclaration
	| %empty
	;

PackageDeclarationOptional:
	PackageDeclaration
	| %empty
	;

PackageDeclaration:
	"package" Name ';'
	;

ImportDeclaration:
	"import" StaticKeywordOptional Name DotStarOptional ';'
	;

StaticKeywordOptional:
	"static"
	| %empty
	;

DotStarOptional:
	".*"
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

ClassImplementsOptional:
	ClassImplements
	| %empty
	;

ClassExtends:
	"extends" Name
	;

ClassImplements:
	"implements" NameListLeastOne
	;

NameListLeastOne:
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
	| FieldDeclaration
	| MethodDeclaration
	| ClassDeclaration
	;

Type:
	PrimitiveType
	| ReferenceType
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
	

ReferenceType:
	Name
	| ArrayType
	;

ArrayType:
	PrimitiveType Dims
	| Name Dims
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

DotIdentifierKleeneStar:
	DotIdentifierKleeneStar '.' Identifier
	| %empty
	;

Name:
	Identifier DotIdentifierKleeneStar
	;

Initializer:
	StaticKeywordOptional Block
	;

StaticKeywordOptional:
	"static"
	| %empty
	;

FieldDeclaration:
	FieldModifierKleeneStar Type VariableDeclaratorListLeastOne ';'
	;

VariableDeclaratorListLeastOne:
	VariableDeclarator CommaVariableDeclaratorKleeneStar
	;

CommaVariableDeclaratorKleeneStar:
	CommaVariableDeclaratorKleeneStar ',' VariableDeclarator
	;

FieldModifier:
	"public"
	| "protected"
	| "private"
	| "static"
	| "final"
	| "transient"
	| "volatile"
	;

FieldModifierKleeneStar:
	FieldModifierKleeneStar FieldModifier
	| %empty
	;

VariableDeclarator:
	VariableDeclaratorId EqualVariableInitializerOptional
	;

EqualVariableInitializerOptional:
	'=' VariableInitializer
	| %empty
	;
	

VariableDeclaratorId:
	Identifier DimsOptional
	;

DimsOptional:
	Dims
	| %empty
	;

VariableInitializer:
	Expression
	| ArrayInitializer
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

ThrowsOptional:
	Throws
	| %empty
	;

Result:
	Type
	| "void"
	;

MethodDeclarator:
	Identifier '(' ParameterList ')' DimsOptional
	;

ParameterList:
	ParameterListLeastOneOptional
	;

ParameterListLeastOneOptional:
	ParameterListLeastOne
	| %empty
	;

ParameterListLeastOne:
	Parameter CommaParameterKleeneStar
	;

CommaParameterKleeneStar:
	CommaParameterKleeneStar ',' Parameter
	| %empty
	;

Parameter:
	VariableModifierKleeneStar Type TripleDotIdentifierOrVariableDeclaratorId
	;

TripleDotIdentifierOrVariableDeclaratorId:
	"..." Identifier
	| VariableDeclaratorId
	;

VariableModifierKleeneStar:
	VariableModifierKleeneStar VariableModifier
	| %empty
	;

VariableModifier:
	"final"
	;

Throws:
	"throws" TypeListLeastOne
	;

TypeListLeastOne:
	Type CommaTypeKleeneStar
	;

CommaTypeKleeneStar:
	CommaTypeKleeneStar ',' Type
	| %empty
	;

MethodBody:
	Block
	';'
	;

Block:
	'{' '}'
	;

Expression:
	%empty
	;

ArrayInitializer:
	'{' '}'
	;

%%

void yyerror(const char* s) {
	fprintf(stderr, "%s\n", s);
}

int main() {
	return yyparse();
}
