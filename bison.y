%{
	#include <stdio.h>
	#include "tree.c"
	
	int yylex();
	void yyerror(const char*);
%}

%define api.value.type {node*}

%destructor { rmnd($$); } <*>

%token Identifier
%token Literal

%%
CompilationUnit: OrdinaryCompilationUnit { $$ = $1; tree($1); }; 

OrdinaryCompilationUnit: PackageDeclarationOptional ImportDeclarationRecursion TopLevelClassOrInterfaceDeclarationRecursion;
PackageDeclarationOptional: PackageDeclaration | %empty;
ImportDeclarationRecursion: ImportDeclarationRecursion ImportDeclaration | %empty;
TopLevelClassOrInterfaceDeclarationRecursion: TopLevelClassOrInterfaceDeclarationRecursion TopLevelClassOrInterfaceDeclaration | %empty;

PackageDeclaration: PackageModifierRecursion "package" Identifier DotIdentifierRecursion ';';
PackageModifierRecursion: PackageModifierRecursion PackageModifier | %empty;
DotIdentifierRecursion: DotIdentifierRecursion '.' Identifier | %empty;

ImportDeclaration: SingleTypeImportDeclaration | TypeImportOnDemandDeclaration | SingleStaticImportDeclaration | StaticImportOnDemandDeclaration;

TopLevelClassOrInterfaceDeclaration: ClassDeclaration | InterfaceDeclaration | ';';

PackageModifier: Annotation;

SingleTypeImportDeclaration: "import" TypeName ';';

TypeImportOnDemandDeclaration: "import" PackageOrTypeName '.' '*' ';';

SingleStaticImportDeclaration: "import" "static" TypeName '.' Identifier ';';

StaticImportOnDemandDeclaration: "import" "static" TypeName '.' '*' ';';

ClassDeclaration: NormalClassDeclaration | EnumDeclaration | RecordDeclaration;

InterfaceDeclaration: NormalInterfaceDeclaration | AnnotationInterfaceDeclaration;

Annotation: NormalAnnotation | MarkerAnnotation | SingleElementAnnotation;

TypeName: TypeIdentifier | PackageOrTypeName '.' TypeIdentifier;

PackageOrTypeName: Identifier | PackageOrTypeName '.' Identifier;

NormalClassDeclaration: ClassModifierRecursion "class" TypeIdentifier TypeParametersOptional ClassExtendsOptional ClassImplementsOptional ClassPermitsOptional ClassBody
ClassModifierRecursion: ClassModifierRecursion ClassModifier | %empty;
TypeParametersOptional: TypeParameters | %empty;
ClassExtendsOptional: ClassExtends | %empty;
ClassImplementsOptional: ClassImplements | %empty;
ClassPermitsOptional: ClassPermits | %empty;

EnumDeclaration: ClassModifierRecursion "enum" TypeIdentifier ClassImplementsOptional EnumBody;
ClassModifierRecursion: ClassModifierRecursion ClassModifier | %empty;
ClassImplementsOptional: ClassImplements | %empty;

RecordDeclaration: ClassModifierRecursion "record" TypeIdentifier TypeParametersOptional RecordHeader ClassImplementsOptional RecordBody;

NormalInterfaceDeclaration: InterfaceModifierRecursion "interface" TypeIdentifier TypeParametersOptional InterfaceExtendsOptional InterfacePermitsOptional InterfaceBody;
InterfaceModifierRecursion: InterfaceModifierRecursion InterfaceModifier | %empty;
InterfaceExtendsOptional: InterfaceExtends | %empty;
InterfacePermitsOptional: InterfacePermits | %empty;

AnnotationInterfaceDeclaration: InterfaceModifierRecursion '@' "interface" TypeIdentifier AnnotationInterfaceBody;

NormalAnnotation: '@' TypeName '(' ElementValuePairListOptional ')'
ElementValuePairListOptional: ElementValuePairList | %empty;

MarkerAnnotation: '@' TypeName

SingleElementAnnotation: '@' TypeName '(' ElementValue ')'

TypeIdentifier: Identifier;

ClassBody: '{' ClassBodyDeclarationRecursion '}';
ClassBodyDeclarationRecursion: ClassBodyDeclarationRecursion ClassBodyDeclaration | %empty;

ClassModifier: Annotation | "public" | "protected" | "private" | "abstract" | "static" | "final" | "sealed" | "non-sealed" | "strictfp";

TypeParameters: '<' TypeParameterList '>';

ClassExtends: "extends" ClassType;

ClassImplements: "implements" InterfaceTypeList;

ClassPermits: "permits" TypeName CommaTypeNameRecursion;
CommaTypeNameRecursion: CommaTypeNameRecursion ',' TypeName | %empty;

EnumBody: '{' EnumConstantListOptional CommaOptional EnumBodyDeclarationsOptional '}';
EnumConstantListOptional: EnumConstantList | %empty;
CommaOptional: ',' | %empty;
EnumBodyDeclarationsOptional: EnumBodyDeclarations | %empty;

RecordHeader: '(' RecordComponentListOptional ')';
RecordComponentListOptional: RecordComponentList | %empty;

RecordBody: '{' RecordBodyDeclarationRecursion '}';
RecordBodyDeclarationRecursion: RecordBodyDeclarationRecursion RecordBodyDeclaration | %empty;

InterfaceBody: '{' InterfaceMemberDeclarationRecursion '}';
InterfaceMemberDeclarationRecursion: InterfaceMemberDeclarationRecursion InterfaceMemberDeclaration | %empty;

InterfaceModifier: Annotation | "public" | "protected" | "private" | "abstract" | "static" | "sealed" | "non-sealed" | "strictfp";

InterfaceExtends: "extends" InterfaceTypeList;

InterfacePermits: "permits" TypeName CommaTypeNameRecursion
CommaTypeNameRecursion: CommaTypeNameRecursion ',' TypeName | %empty;

AnnotationInterfaceBody: '{' AnnotationInterfaceMemberDeclarationRecursion '}';
AnnotationInterfaceMemberDeclarationRecursion: AnnotationInterfaceMemberDeclarationRecursion AnnotationInterfaceMemberDeclaration | %empty;

ElementValuePairList: ElementValuePair CommaElementValuePairRecursion;
CommaElementValuePairRecursion: CommaElementValuePairRecursion ',' ElementValuePair | %empty;

ElementValue: ConditionalExpression | ElementValueArrayInitializer | Annotation;

ClassBodyDeclaration: ClassMemberDeclaration | InstanceInitializer | StaticInitializer | ConstructorDeclaration;

TypeParameterList: TypeParameter CommaTypeParameterRecursion;
CommaTypeParameterRecursion: CommaTypeParameterRecursion ',' TypeParameter | %empty;

ClassType: AnnotationRecursion TypeIdentifier TypeArgumentsOptional | PackageName '.' AnnotationRecursion TypeIdentifier TypeArgumentsOptional | ClassOrInterfaceType '.' AnnotationRecursion TypeIdentifier TypeArgumentsOptional;
AnnotationRecursion: AnnotationRecursion Annotation | %empty;
TypeArgumentsOptional: TypeArguments | %empty;

InterfaceTypeList: InterfaceType CommaInterfaceTypeRecursion;
CommaInterfaceTypeRecursion: CommaInterfaceTypeRecursion ',' InterfaceType | %empty;

EnumConstantList: EnumConstant CommaEnumConstantRecursion;
CommaEnumConstantRecursion: CommaEnumConstantRecursion ',' EnumConstant | %empty;

EnumBodyDeclarations: ';' ClassBodyDeclarationRecursion;

RecordComponentList: RecordComponent CommaRecordComponentRecursion;
CommaRecordComponentRecursion: CommaRecordComponentRecursion ',' RecordComponent;

RecordBodyDeclaration: ClassBodyDeclaration | CompactConstructorDeclaration;

InterfaceMemberDeclaration: ConstantDeclaration | InterfaceMethodDeclaration | ClassDeclaration | InterfaceDeclaration | ';';

AnnotationInterfaceMemberDeclaration: AnnotationInterfaceElementDeclaration | ConstantDeclaration | ClassDeclaration | InterfaceDeclaration | ';';

ElementValuePair: Identifier '=' ElementValue;

ConditionalExpression: ConditionalOrExpression | ConditionalOrExpression '?' Expression ':' ConditionalExpression | ConditionalOrExpression '?' Expression ':' LambdaExpression;

ElementValueArrayInitializer:'{' ElementValueListOptional CommaOptional '}';
ElementValueListOptional: ElementValueList | %empty;

ClassMemberDeclaration: FieldDeclaration | MethodDeclaration | ClassDeclaration | InterfaceDeclaration | ';';

InstanceInitializer: Block;

StaticInitializer: "static" Block;

ConstructorDeclaration: ConstructorModifierRecursion ConstructorDeclarator ThrowsOptional ConstructorBody;
ConstructorModifierRecursion: ConstructorModifierRecursion ConstructorModifier | %empty;
ThrowsOptional: Throws | %empty;

TypeParameter: TypeParameterModifierRecursion TypeIdentifier TypeBoundOptional;
TypeParameterModifierRecursion: TypeParameterModifierRecursion TypeParameterModifier | %empty;
TypeBoundOptional: TypeBound | %empty;

PackageName: Identifier | PackageName '.' Identifier;

ClassOrInterfaceType: ClassType | InterfaceType;

TypeArguments: '<' TypeArgumentList '>';

InterfaceType: ClassType;

EnumConstant: EnumConstantModifierRecursion Identifier OpenParenthesisArgumentListOptionalCloseParenthesisOptional ClassBodyOptional;
EnumConstantModifierRecursion: EnumConstantModifierRecursion EnumConstantModifier | %empty;
ArgumentListOptional: ArgumentList | %empty;
OpenParenthesisArgumentListOptionalCloseParenthesisOptional: '(' ArgumentListOptional ')' | %empty;
ClassBodyOptional: ClassBody | %empty;

RecordComponent: RecordComponentModifierRecursion UnannType Identifier | VariableArityRecordComponent;
RecordComponentModifierRecursion: RecordComponentModifierRecursion RecordComponentModifier | %empty;

CompactConstructorDeclaration: ConstructorModifierRecursion SimpleTypeName ConstructorBody;
ConstructorModifierRecursion: ConstructorModifierRecursion ConstructorModifier | %empty;

ConstantDeclaration: ConstantModifierRecursion UnannType VariableDeclaratorList ';';
ConstantModifierRecursion: ConstantModifierRecursion ConstantModifier | %empty;

InterfaceMethodDeclaration: InterfaceMethodModifierRecursion MethodHeader MethodBody;
InterfaceMethodModifierRecursion: InterfaceMethodModifierRecursion InterfaceMethodModifier | %empty;

AnnotationInterfaceElementDeclaration: AnnotationInterfaceElementModifierRecursion UnannType Identifier '(' ')' DimsOptional DefaultValueOptional ';';
AnnotationInterfaceElementModifierRecursion: AnnotationInterfaceElementModifierRecursion AnnotationInterfaceElementModifier | %empty;
DimsOptional: Dims | %empty;
DefaultValueOptional: DefaultValue | %empty;

ConditionalOrExpression: ConditionalAndExpression | ConditionalOrExpression "||" ConditionalAndExpression;

Expression: LambdaExpression | AssignmentExpression;

LambdaExpression: LambdaParameters "->" LambdaBody;

ElementValueList: ElementValue CommaElementValueRecursion;
CommaElementValueRecursion: CommaElementValueRecursion ',' ElementValue | %empty;

FieldDeclaration: FieldModifierRecursion UnannType VariableDeclaratorList ';';
FieldModifierRecursion: FieldModifierRecursion FieldModifier | %empty;

MethodDeclaration: MethodModifierRecursion MethodHeader MethodBody;
MethodModifierRecursion: MethodModifierRecursion MethodModifier | %empty;

Block: '{' BlockStatementsOptional '}';
BlockStatementsOptional: BlockStatements | %empty;

ConstructorDeclarator: TypeParametersOptional SimpleTypeName '(' ReceiverParameterCommaOptional FormalParameterListOptional ')';
TypeParametersOptional: TypeParameters | %empty;
ReceiverParameterCommaOptional: ReceiverParameter ',' | %empty;
FormalParameterListOptional: FormalParameterList | %empty;

ConstructorBody:'{' ExplicitConstructorInvocationOptional BlockStatementsOptional '}';
ExplicitConstructorInvocationOptional: ExplicitConstructorInvocation | %empty;
BlockStatementsOptional: BlockStatements | %empty;

ConstructorModifier: Annotation | "public" | "protected" | "private";

UnannType: UnannPrimitiveType | UnannReferenceType;

VariableDeclaratorList: VariableDeclarator CommaVariableDeclaratorRecursion;
CommaVariableDeclaratorRecursion: CommaVariableDeclaratorRecursion ',' VariableDeclarator | %empty;

ConstantModifier: Annotation | "public" | "static" | "final";

MethodHeader: Result MethodDeclarator ThrowsOptional | TypeParameters AnnotationRecursion Result MethodDeclarator ThrowsOptional;

MethodBody: Block | ';';

InterfaceMethodModifier: Annotation | "public" | "private" | "abstract" | "default" | "static" | "strictfp";

AnnotationInterfaceElementModifier: Annotation | "public" | "abstract";

Dims: AnnotationRecursion '[' ']' AnnotationRecursionOpenBracketCloseBracketRecursion;
AnnotationRecursionOpenBracketCloseBracketRecursion: AnnotationRecursionOpenBracketCloseBracketRecursion AnnotationRecursion '[' ']' | %empty;

DefaultValue: "default" ElementValue;

ConditionalAndExpression: InclusiveOrExpression | ConditionalAndExpression "&&" InclusiveOrExpression

AssignmentExpression: ConditionalExpression | Assignment;

LambdaParameters: '(' LambdaParameterListOptional ')' | Identifier;
LambdaParameterListOptional: LambdaParameterList | %empty;

LambdaBody: Expression | Block;

FieldModifier: Annotation | "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile";

MethodModifier: Annotation | "public" | "protected" | "private" | "abstract" | "static" | "final" | "synchronized" | "native" | "strictfp";

BlockStatements: BlockStatement BlockStatementRecursion;
BlockStatementRecursion: BlockStatementRecursion BlockStatement | %empty;

SimpleTypeName: TypeIdentifier;

ReceiverParameter: AnnotationRecursion UnannType IdentifierDotOptional "this";
IdentifierDotOptional: Identifier '.' | %empty;

FormalParameterList: FormalParameter CommaFormalParameterRecursion;
CommaFormalParameterRecursion: CommaFormalParameterRecursion ',' FormalParameter | %empty;

ExplicitConstructorInvocation: TypeArgumentsOptional "this" '(' ArgumentListOptional ')' ';' | TypeArgumentsOptional "super" '(' ArgumentListOptional ')' ';' | ExpressionName '.' TypeArgumentsOptional "super" '(' ArgumentListOptional ')' ';' | Primary '.' TypeArgumentsOptional "super" '(' ArgumentListOptional ')' ';';

UnannPrimitiveType: NumericType | "boolean";

UnannReferenceType: UnannClassOrInterfaceType | UnannTypeVariable | UnannArrayType;

VariableDeclarator: VariableDeclaratorId EqualVariableInitializerOptional;
EqualVariableInitializerOptional: '=' VariableInitializer | %empty;

Result: UnannType | "void";

MethodDeclarator: Identifier '(' ReceiverParameterCommaOptional FormalParameterListOptional ')' DimsOptional;

InclusiveOrExpression: ExclusiveOrExpression | InclusiveOrExpression '|' ExclusiveOrExpression;

Assignment: LeftHandSide AssignmentOperator Expression;

LambdaParameterList: LambdaParameter CommaLambdaParameterRecursion | Identifier CommaIdentifierRecursion;
CommaLambdaParameterRecursion: CommaLambdaParameterRecursion ',' LambdaParameter | %empty;
CommaIdentifierRecursion: CommaIdentifierRecursion ',' Identifier | %empty;

BlockStatement: LocalClassOrInterfaceDeclaration | LocalVariableDeclarationStatement | Statement;

FormalParameter: VariableModifierRecursion UnannType VariableDeclaratorId | VariableArityParameter;
VariableModifierRecursion: VariableModifierRecursion VariableModifier | %empty;	

ExpressionName: Identifier | AmbiguousName '.' Identifier;

Primary: PrimaryNoNewArray | ArrayCreationExpression;

NumericType: IntegralType | FloatingPointType;

UnannClassOrInterfaceType: UnannClassType | UnannInterfaceType;

UnannTypeVariable: TypeIdentifier;

UnannArrayType: UnannPrimitiveType Dims | UnannClassOrInterfaceType Dims | UnannTypeVariable Dims;

VariableDeclaratorId: Identifier DimsOptional;

VariableArityParameter: VariableModifierRecursion UnannType AnnotationRecursion "..." Identifier;

VariableModifier: Annotation | "final";

AmbiguousName: Identifier | AmbiguousName '.' Identifier;

PrimaryNoNewArray: Literal | ClassLiteral | "this" | TypeName '.' "this" | '(' Expression ')' | ClassInstanceCreationExpression | FieldAccess | ArrayAccess | MethodInvocation | MethodReference;

ArrayCreationExpression: "new" PrimitiveType DimExprs DimsOptional | "new" ClassOrInterfaceType DimExprs DimsOptional | "new" PrimitiveType Dims ArrayInitializer | "new" ClassOrInterfaceType Dims ArrayInitializer;

IntegralType: "byte" | "short" | "int" | "long" | "char";

FloatingPointType: "float" | "double";

UnannClassType: TypeIdentifier TypeArgumentsOptional | PackageName '.' AnnotationRecursion TypeIdentifier TypeArgumentsOptional | UnannClassOrInterfaceType '.' AnnotationRecursion TypeIdentifier TypeArgumentsOptional;

UnannInterfaceType: UnannClassType;

ClassLiteral: TypeName OpenBracketCloseBracketRecursion '.' "class" | NumericType OpenBracketCloseBracketRecursion '.' "class" | "boolean" OpenBracketCloseBracketRecursion '.' "class" | "void" '.' "class";
OpenBracketCloseBracketRecursion: OpenBracketCloseBracketRecursion '[' ']' | %empty;

ClassInstanceCreationExpression: UnqualifiedClassInstanceCreationExpression | ExpressionName '.' UnqualifiedClassInstanceCreationExpression | Primary '.' UnqualifiedClassInstanceCreationExpression;

FieldAccess: Primary '.' Identifier | "super" '.' Identifier | TypeName '.' "super" '.' Identifier;

ArrayAccess: ExpressionName '[' Expression ']' | PrimaryNoNewArray '[' Expression ']';

MethodInvocation: MethodName '(' ArgumentListOptional ')' | TypeName '.' TypeArgumentsOptional Identifier '(' ArgumentListOptional ')' | ExpressionName '.' TypeArgumentsOptional Identifier '(' ArgumentListOptional ')' | Primary '.' TypeArgumentsOptional Identifier '(' ArgumentListOptional ')' | "super" '.' TypeArgumentsOptional Identifier '(' ArgumentListOptional ')' | TypeName '.' "super" '.' TypeArgumentsOptional Identifier '(' ArgumentListOptional ')';

MethodReference: ExpressionName "::" TypeArgumentsOptional Identifier | Primary "::" TypeArgumentsOptional Identifier | ReferenceType "::" TypeArgumentsOptional Identifier | "super" "::" TypeArgumentsOptional Identifier | TypeName '.' "super" "::" TypeArgumentsOptional Identifier | ClassType "::" TypeArgumentsOptional "new" | ArrayType "::" "new";

PrimitiveType: AnnotationRecursion NumericType | AnnotationRecursion "boolean";

DimExprs: DimExpr DimExprRecursion;
DimExprRecursion: DimExprRecursion DimExpr | %empty;

ArrayInitializer: '{' VariableInitializerListOptional CommaOptional '}';
VariableInitializerListOptional: VariableInitializerList | %empty;

UnqualifiedClassInstanceCreationExpression: "new" TypeArgumentsOptional ClassOrInterfaceTypeToInstantiate '(' ArgumentListOptional ')' ClassBodyOptional;

MethodName: UnqualifiedMethodIdentifier;

ReferenceType: ClassOrInterfaceType | TypeVariable | ArrayType;

ArrayType: PrimitiveType Dims | ClassOrInterfaceType Dims | TypeVariable Dims;

DimExpr: AnnotationRecursion '[' Expression ']';

VariableInitializerList: VariableInitializer CommaVariableInitializerRecursion;
CommaVariableInitializerRecursion: CommaVariableInitializerRecursion ',' VariableInitializer | %empty;

ClassOrInterfaceTypeToInstantiate: AnnotationRecursion Identifier DotAnnotationRecursionIdentifierRecursion TypeArgumentsOrDiamondOptional;
DotAnnotationRecursionIdentifierRecursion: DotAnnotationRecursionIdentifierRecursion '.' AnnotationRecursion Identifier | %empty;
TypeArgumentsOrDiamondOptional: TypeArgumentsOrDiamond | %empty;

UnqualifiedMethodIdentifier: Identifier;

TypeVariable: AnnotationRecursion TypeIdentifier;

VariableInitializer: Expression | ArrayInitializer;

TypeArgumentsOrDiamond: TypeArguments | "<>";

ArgumentList: Expression CommaExpressionRecursion;
CommaExpressionRecursion: CommaExpressionRecursion ',' Expression | %empty;

TypeBound: "extends" TypeVariable | "extends" ClassOrInterfaceType AdditionalBoundRecursion;
AdditionalBoundRecursion: AdditionalBoundRecursion AdditionalBound | %empty;

AdditionalBound: '&' InterfaceType;

TypeArguments: '<' TypeArgumentList '>';

TypeArgumentList: TypeArgument CommaTypeArgumentRecursion;
CommaTypeArgumentRecursion: CommaTypeArgumentRecursion ',' TypeArgument | %empty;

TypeArgument: ReferenceType | Wildcard;

Wildcard: AnnotationRecursion '?' WildcardBoundsOptional;
WildcardBoundsOptional: WildcardBounds | %empty;

WildcardBounds: "extends" ReferenceType | "super" ReferenceType;

Throws: "throws" ExceptionTypeList;

ExceptionTypeList: ExceptionType CommaExceptionTypeRecursion;
CommaExceptionTypeRecursion: CommaExceptionTypeRecursion ',' ExceptionType | %empty;

ExceptionType: ClassType | TypeVariable;

EnumConstantModifier: Annotation;

RecordComponentModifier: Annotation;

VariableArityRecordComponent: RecordComponentModifierRecursion UnannType AnnotationRecursion "..." Identifier

LocalClassOrInterfaceDeclaration: ClassDeclaration | NormalInterfaceDeclaration;

LocalVariableDeclarationStatement: LocalVariableDeclaration ';';

Statement: StatementWithoutTrailingSubstatement | LabeledStatement | IfThenStatement | IfThenElseStatement | WhileStatement | ForStatement;

LocalVariableDeclaration: VariableModifierRecursion LocalVariableType VariableDeclaratorList;
VariableModifierRecursion: VariableModifierRecursion VariableModifier | %empty;

LocalVariableType: UnannType | "var";

StatementWithoutTrailingSubstatement: Block | EmptyStatement | ExpressionStatement | AssertStatement | SwitchStatement | DoStatement | BreakStatement | ContinueStatement | ReturnStatement | SynchronizedStatement | ThrowStatement | TryStatement | YieldStatement;

EmptyStatement: ';';

ExpressionStatement: StatementExpression ';';

StatementExpression: Assignment | PreIncrementExpression | PreDecrementExpression | PostIncrementExpression | PostDecrementExpression | MethodInvocation | ClassInstanceCreationExpression;

PreIncrementExpression: "++" UnaryExpression;

UnaryExpression: PreIncrementExpression | PreDecrementExpression | '+' UnaryExpression | '-' UnaryExpression | UnaryExpressionNotPlusMinus;

UnaryExpressionNotPlusMinus: PostfixExpression | '~' UnaryExpression | '!' UnaryExpression | CastExpression | SwitchExpression;

PostfixExpression: Primary | ExpressionName | PostIncrementExpression | PostDecrementExpression;

CastExpression: '(' PrimitiveType ')' UnaryExpression | '(' ReferenceType AdditionalBoundRecursion ')' UnaryExpressionNotPlusMinus | '(' ReferenceType AdditionalBoundRecursion ')' LambdaExpression;

SwitchExpression: "switch" '(' Expression ')' SwitchBlock;

SwitchBlock: '{' SwitchRule SwitchRuleRecursion '}' | '{' SwitchBlockStatementGroupRecursion SwitchLabelColonRecursion '}'
SwitchRuleRecursion: SwitchRuleRecursion SwitchRule | %empty;
SwitchBlockStatementGroupRecursion: SwitchBlockStatementGroupRecursion SwitchBlockStatementGroup | %empty;
SwitchLabelColonRecursion: SwitchLabelColonRecursion SwitchLabel ':' | %empty;

SwitchRule: SwitchLabel "->" Expression ';' | SwitchLabel "->" Block | SwitchLabel "->" ThrowStatement;

SwitchBlockStatementGroup: SwitchLabel ':' SwitchLabelColonRecursion BlockStatements;

SwitchLabel: "case" CaseConstant CommaCaseConstantRecursion | "default";
CommaCaseConstantRecursion: CommaCaseConstantRecursion ',' CaseConstant | %empty;

CaseConstant: ConditionalExpression;

PreDecrementExpression: "--" UnaryExpression;

PostIncrementExpression: PostfixExpression "++";

PostDecrementExpression: PostfixExpression "--";

AssertStatement: "assert" Expression ';' | "assert" Expression ':' Expression ';';

SwitchStatement: "switch" '(' Expression ')' SwitchBlock;

DoStatement: "do" Statement "while" '(' Expression ')' ';';

BreakStatement: "break" IdentifierOptional ';';
IdentifierOptional: Identifier | %empty;

ContinueStatement: "continue" IdentifierOptional ';';

ReturnStatement: "return" ExpressionOptional ';';
ExpressionOptional: Expression | %empty;

SynchronizedStatement: "synchronized" '(' Expression ')' Block;

ThrowStatement: "throw" Expression ';';

TryStatement: "try" Block Catches | "try" Block CatchesOptional Finally | TryWithResourcesStatement;
CatchesOptional: Catches | %empty;

Catches: CatchClause CatchClauseRecursion;
CatchClauseRecursion: CatchClauseRecursion CatchClause | %empty;

CatchClause: "catch" '(' CatchFormalParameter ')' Block;

CatchFormalParameter: VariableModifierRecursion CatchType VariableDeclaratorId;

CatchType: UnannClassType PipeClassTypeRecursion;
PipeClassTypeRecursion: PipeClassTypeRecursion '|' ClassType | %empty;

Finally: "finally" Block;

TryWithResourcesStatement: "try" ResourceSpecification Block CatchesOptional FinallyOptional
FinallyOptional: Finally | %empty;

ResourceSpecification: '(' ResourceList SemicolonOptional ')';
SemicolonOptional: ';' | %empty;

ResourceList: Resource SemicolonResourceRecursion;
SemicolonResourceRecursion: SemicolonResourceRecursion ';' Resource | %empty;

Resource: LocalVariableDeclaration | VariableAccess;

VariableAccess: ExpressionName | FieldAccess;

YieldStatement: "yield" Expression ';';

LabeledStatement: Identifier ':' Statement;

IfThenStatement: "if" '(' Expression ')' Statement;

IfThenElseStatement: "if" '(' Expression ')' StatementNoShortIf "else" Statement;

WhileStatement: "while" '(' Expression ')' Statement;

ForStatement: BasicForStatement | EnhancedForStatement;

BasicForStatement: "for" '(' ForInitOptional ';' ExpressionOptional ';' ForUpdateOptional ')' Statement;
ForInitOptional: ForInit | %empty;
ForUpdateOptional: ForUpdate | %empty;

ForInit: StatementExpressionList | LocalVariableDeclaration;

StatementExpressionList: StatementExpression CommaStatementExpressionRecursion;
CommaStatementExpressionRecursion: CommaStatementExpressionRecursion ',' StatementExpression | %empty;

ForUpdate: StatementExpressionList;

EnhancedForStatement: "for" '(' LocalVariableDeclaration ':' Expression ')' Statement;

StatementNoShortIf: StatementWithoutTrailingSubstatement | LabeledStatementNoShortIf | IfThenElseStatementNoShortIf | WhileStatementNoShortIf | ForStatementNoShortIf;

LabeledStatementNoShortIf: Identifier ':' StatementNoShortIf;

IfThenElseStatementNoShortIf: "if" '(' Expression ')' StatementNoShortIf "else" StatementNoShortIf;

WhileStatementNoShortIf: "while" '(' Expression ')' StatementNoShortIf;

ForStatementNoShortIf: BasicForStatementNoShortIf | EnhancedForStatementNoShortIf;

BasicForStatementNoShortIf: "for" '(' ForInitOptional ';' ExpressionOptional ';' ForUpdateOptional ')' StatementNoShortIf;

EnhancedForStatementNoShortIf: "for" '(' LocalVariableDeclaration ':' Expression ')' StatementNoShortIf;

LambdaParameter: VariableModifierRecursion LambdaParameterType VariableDeclaratorId | VariableArityParameter;

LambdaParameterType: UnannType | "var";

AssignmentOperator: '=' |  "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=";

ExclusiveOrExpression: AndExpression | ExclusiveOrExpression '^' AndExpression;

AndExpression: EqualityExpression | AndExpression '&' EqualityExpression;

EqualityExpression: RelationalExpression | EqualityExpression "==" RelationalExpression | EqualityExpression "!=" RelationalExpression;

RelationalExpression: ShiftExpression | RelationalExpression '<' ShiftExpression | RelationalExpression '>' ShiftExpression | RelationalExpression "<=" ShiftExpression | RelationalExpression ">=" ShiftExpression | InstanceofExpression;

ShiftExpression: AdditiveExpression | ShiftExpression "<<" AdditiveExpression | ShiftExpression ">>" AdditiveExpression | ShiftExpression ">>>" AdditiveExpression;

AdditiveExpression: MultiplicativeExpression | AdditiveExpression '+' MultiplicativeExpression | AdditiveExpression '-' MultiplicativeExpression;

MultiplicativeExpression: UnaryExpression | MultiplicativeExpression '*' UnaryExpression | MultiplicativeExpression '/' UnaryExpression | MultiplicativeExpression '%' UnaryExpression;

InstanceofExpression: RelationalExpression "instanceof" ReferenceType | RelationalExpression "instanceof" Pattern;

Pattern: TypePattern;

TypePattern: LocalVariableDeclaration;

TypeParameterModifier: Annotation;

LeftHandSide: ExpressionName | FieldAccess | ArrayAccess;

%%

void yyerror(const char* s) {
	fprintf(stderr, %s\n, s);
}

int main() {
	return yyparse();
}
