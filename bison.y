%{
	#include <stdio.h>
	#include "lex.yy.c"
	#include <stdbool.h>
	
	int yylex();
	void yyerror(const char*);
%}

%code requires {
	#include "ast.c"
}

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
%token VarKeyword "var"
%token YieldKeyword "yield"
%token PermitsKeyword "permits"
%token SealedKeyword "sealed"
%token NonSealedKeyword "non-sealed"
%token RecordKeyword "record"

%token Identifier
%token Literal

%token TripleDotSeparator "..."
%token DoubleColonSeparator "::"

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

// Uncomment both following lines to output more detailed error messages.
// %define parse.error detailed
// %define parse.lac full

%define api.value.type {ast_node*}

%%
Compilation:
	PkgOpt Imports ClassOrInterOrSemicolons {
		ast_pkg* pkg = $1->val.pkg;
		free($1);
		ast_imports* imports = $2->val.imports;
		free($2);
		
		ast_compil* compil = ast_mk_compil(pkg, imports);
		
		$$ = ast_mk_node(AST_NODE_TYPE_COMPIL);
		$$->val.compil = compil;
		
		ast_print_node($$, 0);
		
		printf("\nclasses and interfaces ...\n");
	}
	;

PkgOpt:
	Pkg { $$ = $1; }
	| %empty {
		ast_pkg* pkg = ast_mk_pkg(NULL);
		
		$$ = ast_mk_node(AST_NODE_TYPE_PKG);
		$$->val.pkg = pkg;
	}
	;

Pkg:
	"package" Name ';' {
		ast_name* name = $2->val.name;
		free($2);
		
		ast_pkg* pkg = ast_mk_pkg(name);
		
		$$ = ast_mk_node(AST_NODE_TYPE_PKG);
		$$->val.pkg = pkg;
	}
	;

Name:
	Name '.' Identifier {
		ast_name* name = $1->val.name;
		name->ids = realloc(name->ids, (name->len+1)*sizeof(ast_lex*));
		name->ids[name->len++] = $3->val.lex;
		free($3);
		
		$$ = $1;
	}
	| Identifier {
		ast_name* name = ast_mk_name();
		name->ids = calloc(1, sizeof(ast_lex*));
		name->ids[0] = $1->val.lex; name->len = 1;
		free($1);
		
		$$ = ast_mk_node(AST_NODE_TYPE_NAME);
		$$->val.name = name;
	}
	;

Imports:
	Imports Import {
		ast_imports* imports = $1->val.imports;
		
		if (imports->len == 0) {
			imports->imports = calloc(1, sizeof(ast_import*));
		} else {
			imports->imports = realloc(imports->imports, (imports->len+1)*sizeof(ast_import*));
		}
		
		ast_import* import = $2->val.import;
		free($2);
		
		imports->imports[imports->len++] = import;
		
		$$ = $1;
	}
	| %empty {
		ast_imports* imports = ast_mk_imports();
		
		$$ = ast_mk_node(AST_NODE_TYPE_IMPORTS);
		$$->val.imports = imports;
	}
	;

Import:
	"import" StaticOpt Name ImportDotStarOpt ';' {
		bool is_static = $2->val.bol;
		bool is_star = $4->val.bol;
		ast_name* name = $3->val.name;
		free($3);
		
		ast_import* import = ast_mk_import(is_static, is_star, name);
		
		$$ = ast_mk_node(AST_NODE_TYPE_IMPORT);
		$$->val.import = import;
	}
	;

ImportDotStarOpt:
	'.' '*' {
		bool bol = true;
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = bol;
	}
	| %empty {
		bool bol = false;
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = bol;
	}
	;

StaticOpt:
	"static" {
		bool bol = true;
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = bol;
	}
	| %empty {
		bool bol = false;
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = bol;
	}
	;

ClassOrInterOrSemicolons:
	ClassOrInterOrSemicolons ClassOrInterOrSemicolon
	| %empty
	;

ClassOrInterOrSemicolon:
	Class
	| Inter
	| ';'
	;

Inter:
	NormInter
	| AnnInter
	;

AnnInter:
	ClassOrInterMods '@' "interface" Identifier AnnInterBody
	;

AnnInterBody:
	'{' AnnInterMembers '}'
	;

AnnInterMembers:
	AnnInterMembers AnnInterMember
	| %empty
	;

AnnInterMember:
	AnnInterElem
	| InterField
	| ';'
	;

AnnInterElem:
	InterFieldOrInterMethodMods TypeParamsOpt TypePlusVoid Identifier '(' ')' Dims DefValOpt ';'
	;

DefValOpt:
	DefVal
	| %empty
	;

DefVal:
	"default" ElemVal
	;

ElemVal:
	CondExpr
	| ElemValArrInit
	;

ElemValArrInit:
	'{' ElemValListNonEmpty '}'
	;

ElemValListNonEmpty:
	ElemVal
	| ElemValListNonEmpty ',' ElemVal
	;

NormInter:
	ClassOrInterMods "interface" Identifier TypeParamsOpt InterExtendsOpt ClassOrInterPermitsOpt InterBody
	;

InterExtendsOpt:
	"extends" NameListNonEmpty
	;

InterBody:
	'{' InterMembers '}'
	;

InterMembers:
	InterMembers InterMember
	| %empty
	;

InterMember:
	InterMethod
	| InterField
	| ';'
	;

InterField:
	InterFieldOrInterMethodMods TypeParamsOpt TypePlusVoid VarDeclrListNonEmpty ';'
	;

InterFieldOrInterMethodMods:
	InterFieldOrInterMethodMods InterFieldOrInterMethodMod
	| %empty
	;

InterFieldOrInterMethodMod:
	"public"
	| "private"
	| "abstract"
	| "default"
	| "static"
	| "strictfp"
	| "final"
	;

InterMethod:
	InterFieldOrInterMethodMods TypeParamsOpt TypePlusVoid MethodDeclr ThrowsOpt MethodBody
	;

NormClass:
	ClassOrInterMods "class" Identifier TypeParamsOpt ClassExtendsOpt ClassImplOpt ClassOrInterPermitsOpt ClassBody
	;

ClassOrInterPermitsOpt:
	ClassOrInterPermits
	| %empty
	;

ClassOrInterPermits:
	"permits" NameListNonEmpty
	;

NameListNonEmpty:
	Name
	| NameListNonEmpty ',' Name
	;

Class:
	NormClass
	| Enum
	| Record
	;

Record:
	ClassOrInterMods "record" Identifier TypeParamsOpt RecordHead ClassImplOpt RecordBody
	;

RecordHead:
	'(' RecordCompList ')'
	;

RecordCompList:
	RecordCompListNonEmpty
	| %empty
	;

RecordCompListNonEmpty:
	RecordComp
	| RecordCompListNonEmpty ',' RecordComp
	;

RecordComp:
	Type Identifier
	| VarityRecordComp
	;

VarityRecordComp:
	Type "..." Identifier
	;

RecordBody:
	'{' RecordMembers '}'
	;

RecordMembers:
	RecordMembers RecordMember
	| %empty
	;

RecordMember:
	ClassMember
	| CompactConstrDecl
	;

CompactConstrDecl:
	MethodOrFieldMods TypeParamsOpt Identifier ConstrBody
	;

ConstrBody:
	'{' BlockStmts '}'
	;

Enum:
	ClassOrInterMods "enum" Identifier ClassImplOpt EnumBody
	;

EnumBody:
	'{' EnumConstList EnumBodyMembers '}'
	;

EnumBodyMembers:
	';' ClassMembers
	;

EnumConstList:
	EnumConstListNonEmpty
	| %empty
	;

EnumConstListNonEmpty:
	EnumConst
	| EnumConstListNonEmpty ',' EnumConst
	;

EnumConst:
	Identifier OpenParArgListCloseParOpt ClassBodyOpt
	;

OpenParArgListCloseParOpt:
	'(' ArgList ')'
	| %empty
	;

ClassOrInterMods:
	ClassOrInterMods ClassOrInterMod {
		ast_mods* mods = $1->val.mods;
		ast_mod* mod = $2->val.mod;
		free($2);
		
		if (mods->len == 0) {
			mods->mods = calloc(1, sizeof(ast_import*));
		} else {
			mods->mods = realloc(mods->mods, (mods->len+1)*sizeof(ast_mod*));
		}
		mods->mods[mods->len++] = mod;
		
		$$ = $1;
	}
	| %empty {
		ast_mods* mods = ast_mk_mods();
		
		$$ = ast_mk_node(AST_NODE_TYPE_MODS);
		$$->val.mods = mods;
	}
	;

ClassOrInterMod:
	Ann {
		ast_ann* ann = $1->val.ann;
		free($1);
		
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_ANN);
		$$->val.mod->mod.ann = ann;
	}
	| "public" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_PUBLIC;
	}
	| "protected" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_PROTECTED;
	}
	| "private" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_PRIVATE;
	}
	| "abstract" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_ABSTRACT;
	}
	| "static" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_STATIC;
	}
	| "final" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_FINAL;
	}
	| "strictfp" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_STRICTFP;
	}
	| "sealed" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_SEALED;
	}
	| "non-sealed" {
		$$ = ast_mk_node(AST_NODE_TYPE_MOD);
		$$->val.mod = ast_mk_mod(AST_MOD_TYPE_CONST);
		$$->val.mod->mod.cnst = AST_CONST_MOD_NON_SEALED;
	}
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

ClassBody:
	'{' ClassMembers '}'
	;

ClassMembers:
	ClassMembers ClassMember
	| %empty
	;

ClassMember:
	Init
	| Method
	| Field
	| Constr
	| ';'
	;

Constr:
	MethodOrFieldMods ConstrDeclr ThrowsOpt ConstrBody
	;

ConstrDeclr:
	TypeParamsOpt Identifier '(' ParamList ')'
	;

Field:
	MethodOrFieldMods TypeParamsOpt TypePlusVoid VarDeclrListNonEmpty ';'
	;

MethodOrFieldMod:
	"public"
	| "protected"
	| "private"
	| "abstract"
	| "static"
	| "final"
	| "synchronized"
	| "native"
	| "strictfp"
	| "transient"
	| "volatile"
	;

MethodOrFieldMods:
	MethodOrFieldMods MethodOrFieldMod
	| %empty
	;

Init:
	MethodOrFieldMods Block
	;

Method:
	MethodOrFieldMods TypeParamsOpt TypePlusVoid MethodDeclr ThrowsOpt MethodBody
	;

TypePlusVoid:
	Type
	| "void"
	;

Type:
	PrimType
	| RefType
	;

RefType:
	Name
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
	PrimType DimsNonEmpty
	| Name DimsNonEmpty
	;

DimsNonEmpty:
	'[' ']'
	| DimsNonEmpty '[' ']'
	;

MethodDeclr:
	Identifier '(' ParamList ')' Dims
	;

Dims:
	DimsNonEmpty
	| %empty
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
	VarMods Type VarDeclrId
	| VarityParam
	;

VarityParam:
	VarMods Type "..." Identifier
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
	Class
	| Inter
	| VarDeclStmt
	| Stmt
	;

VarDeclStmt:
	VarDecl ';'
	;

VarType:
	Type
	| "var"
	;

VarDeclrListNonEmpty:
	VarDeclr
	| VarDeclrListNonEmpty ',' VarDeclr
	;

VarDecl:
	VarType VarDeclrListNonEmpty
	;

VarDeclr:
	VarDeclrId VarAssignOpt
	;

VarDeclrId:
	Identifier Dims
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
	| YieldStmt
	| LblStmt
	| IfStmt
	| IfElseStmt
	| WhileStmt
	| ForStmt
	;

YieldStmt:
	"yield" Expr ';'
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
	'{' SwitchBlockStmtsNonEmpty '}'
	| '{' SwitchRulesNonEmpty '}'
	| '{' '}'
	;

SwitchRulesNonEmpty:
	SwitchRulesNonEmpty SwitchRule
	| SwitchRule
	;

SwitchRule:
	SwitchLbl "->" Expr ';'
	| SwitchLbl "->" Block
	| SwitchLbl "->" ThrowStmt
	;

SwitchBlockStmtsNonEmpty:
	SwitchBlockStmtsNonEmpty SwitchBlockStmt
	| SwitchBlockStmt
	;

SwitchBlockStmt:
	SwitchLbl ':' BlockStmts
	;

SwitchLbl:
	"case" CondExprNoLambdaListNonEmpty
	| "default"
	;

CondExprNoLambdaListNonEmpty:
	CondExprNoLambda
	| CondExprNoLambdaListNonEmpty ',' CondExprNoLambda
	;

WhileStmt:
	"while" '(' Expr ')' Stmt
	;

WhileStmtNoShortIf:
	"while" '(' Expr ')' StmtNoShortIf
	;

DoStmt:
	"do" Stmt "while" '(' Expr ')' ';'
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
	| TryWithResStmt
	;

TryWithResStmt:
	"try" ResSpec Block Catches FinallyOpt
	;

ResSpec:
	'(' ResListNonEmpty ')'
	;

ResListNonEmpty:
	Res
	| ResListNonEmpty ';' Res
	;

Res:
	VarDecl
	| FieldAccess
	| Name
	;

FinallyOpt:
	Finally
	| %empty
	;

CatchesNonEmpty:
	Catch
	| CatchesNonEmpty Catch
	;

Catch:
	"catch" '(' CatchParam ')' Block
	;

CatchParam:
	VarMods CatchNameListNonEmpty VarDeclrId
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
	"void" '.' "class"
	| PrimType Dims '.' "class"
	;

NewClassExpr:
	"new" Name '(' ArgList ')' ClassBodyOpt
	| Name '.' "new" Name '(' ArgList ')' ClassBodyOpt
	| Primary '.' "new" Name '(' ArgList ')' ClassBodyOpt
	;

ClassBodyOpt:
	ClassBody
	| %empty
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
	RefType "::" Identifier
	| Primary "::" Identifier
	| RefType "::" "new"
	| "super" "::" Identifier
	| Name '.' "super" "::" Identifier
	;

NewArrExpr:
	"new" PrimType DimExprs DimsOpt
	| "new" Name DimExprs DimsOpt
	| "new" PrimType DimsNonEmpty ArrInit
	| "new" Name DimsNonEmpty ArrInit
	;

DimExprs:
	DimExpr
	| DimExprs DimExpr
	;

DimExpr:
	'[' Expr ']'
	;

DimsOpt:
	DimsNonEmpty
	| %empty
	;

ArrInit:
	'{' VarInitList '}'
	;

VarInitList:
	VarInitListNonEmpty
	| %empty
	;

VarInitListNonEmpty:
	VarInit
	| VarInitListNonEmpty ',' VarInit
	;

Expr:
	AssignExpr
	| LambdaExpr
	;

AssignExpr:
	CondExpr
	| Assign
	;

CondExprNoLambda:
	CondOrExpr
	| CondOrExpr '?' Expr ':' CondExprNoLambda
	;

CondExpr:
	CondOrExpr
	| CondOrExpr '?' Expr ':' CondExpr
	| CondOrExpr '?' Expr ':' LambdaExpr
	;

CondOrExpr:
	CondAndExpr
	| CondOrExpr "||" CondAndExpr
	;

CondAndExpr:
	OrExpr
	| CondAndExpr "&&" OrExpr
	;

OrExpr:
	ExclOrExpr
	| OrExpr '|' ExclOrExpr
	;

ExclOrExpr:
	AndExpr
	| ExclOrExpr '^' AndExpr
	;

AndExpr:
	EqExpr
	| AndExpr '&' EqExpr
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
	RelExpr "instanceof" RefType
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
	| TypeCastExpr
	| SwitchExpr
	;

TypeCastExpr:
	'(' PrimType ')' UnExpr
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

SwitchExpr:
	"switch" '(' Expr ')' SwitchBlock
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

TypeParams:
	'<' TypeParamListNonEmpty '>'
	;

TypeParamListNonEmpty:
	TypeParam
	| TypeParamListNonEmpty ',' TypeParam
	;

TypeParam:
	Identifier TypeBoundOpt
	;

TypeBoundOpt:
	TypeBound
	| %empty
	;

TypeBound:
	"extends" Name ExtraBounds
	;

ExtraBounds:
	ExtraBounds ExtraBound
	| %empty
	;

ExtraBound:
	'&' Name
	;

TypeParamsOpt:
	TypeParams
	| %empty
	;

LambdaExpr:
	Identifier "->" LambdaBody
	;

LambdaBody:
	Expr
	| Block
	;

Ann:
	'@' Name OpenParAnnElemsCloseParOpt {
		ast_name* name = $2->val.name;
		free($2);
		bool has_elems = $3->val.bol;
		free($3);
	
		ast_ann* ann = ast_mk_ann(name, has_elems);
		
		$$ = ast_mk_node(AST_NODE_TYPE_ANN);
		$$->val.ann = ann;
	}
	;

OpenParAnnElemsCloseParOpt:
	'(' AnnElems ')' {
		$$ = $2;
	}
	| %empty {
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = false;
	}
	;

AnnElems:
	ElemVal {
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = true;
	}
	| ElemValPairListNonEmpty {
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = true;
	}
	| %empty {
		$$ = ast_mk_node(AST_NODE_TYPE_BOOL);
		$$->val.bol = false;
	}
	;

ElemValPairListNonEmpty:
	ElemValPairListNonEmpty ',' ElemValPair
	| ElemValPair
	;

ElemValPair:
	Identifier '=' ElemVal
	;
%%

void yyerror(const char* s) {
	fprintf(stderr, "%s\n", s);
}

int main() {
	return yyparse();
}
