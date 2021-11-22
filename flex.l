%top{
// Java SE 17 language Flex description
// Reference: https://docs.oracle.com/javase/specs/jls/se17/html/jls-3.html
}

%option nodefault
%option noyywrap

%{
#include "bison.tab.h"
#include <stdarg.h>
%}

LineTerminator \r\n|[\n\r]

AnyCharacter [^\r\n]

WhiteSpace [ \t\f]|{LineTerminator}
Comment "//"{AnyCharacter}*|"/*"({AnyCharacter}|{LineTerminator})*"*/"

Identifier {IdentifierStart}{IdentifierPart}*
IdentifierStart [A-Za-z]|_|"\$"
IdentifierPart {IdentifierStart}|[0-9]

AbstractKeyword abstract
ContinueKeyword continue
ForKeyword for
NewKeyword new
SwitchKeyword switch
AssertKeyword assert
DefaultKeyword default
IfKeyword if
PackageKeyword package
SynchronizedKeyword synchronized
BooleanKeyword boolean
DoKeyword do
GotoKeyword goto
PrivateKeyword private
ThisKeyword this
BreakKeyword break
DoubleKeyword double
ImplementsKeyword implements
ProtectedKeyword protected
ThrowKeyword throw
ByteKeyword byte
ElseKeyword else
ImportKeyword import
PublicKeyword public
ThrowsKeyword throws
CaseKeyword case
InstanceofKeyword instanceof
ReturnKeyword return
TransientKeyword transient
CatchKeyword catch
ExtendsKeyword extends
IntKeyword int
ShortKeyword short
TryKeyword try
CharKeyword char
FinalKeyword final
InterfaceKeyword interface
StaticKeyword static
VoidKeyword void
ClassKeyword class
FinallyKeyword finally
LongKeyword long
StrictfpKeyword strictfp
VolatileKeyword volatile
ConstKeyword const
FloatKeyword float
NativeKeyword native
SuperKeyword super
WhileKeyword while
UnderlineKeyword _
VarKeyword var

Literal {IntegerLiteral}|{FloatingPointLiteral}|{BooleanLiteral}|{CharacterLiteral}|{StringLiteral}|{NullLiteral}

IntegerLiteral {DecimalIntegerLiteral}|{HexIntegerLiteral}|{OctalIntegerLiteral}|{BinaryIntegerLiteral}
DecimalIntegerLiteral {DecimalNumeral}{IntegerTypeSuffix}?
HexIntegerLiteral {HexNumeral}{IntegerTypeSuffix}?
OctalIntegerLiteral {OctalNumeral}{IntegerTypeSuffix}?
BinaryIntegerLiteral {BinaryNumeral}{IntegerTypeSuffix}?
IntegerTypeSuffix l|L

DecimalNumeral 0|[1-9](({DecimalDigit}|_)*{DecimalDigit})?
DecimalDigit [0-9]

HexNumeral 0[xX]{HexDigits}
HexDigit [0-9a-fA-F]
HexDigits {HexDigit}|{HexDigit}({HexDigit}|_)*{HexDigit}

OctalNumeral 0({OctalDigit}|_)*{OctalDigit}
OctalDigit [0-7]

BinaryNumeral 0[bB]({BinaryDigit}|{BinaryDigit}({BinaryDigit}|_)*{BinaryDigit})
BinaryDigit [01]

FloatingPointLiteral {DecimalFloatingPointLiteral}|{HexadecimalFloatingPointLiteral}

DecimalFloatingPointLiteral {FloatingPointDigits}\.{FloatingPointDigits}?{ExponentPart}?{FloatTypeSuffix}?|\.{FloatingPointDigits}{ExponentPart}?{FloatTypeSuffix}?|{FloatingPointDigits}{ExponentPart}{FloatTypeSuffix}?|{FloatingPointDigits}{ExponentPart}?{FloatTypeSuffix}
FloatingPointDigits {DecimalDigit}|{DecimalDigit}({DecimalDigit}|_)*{DecimalDigit}
ExponentPart [eE][+-]?{FloatingPointDigits}
FloatTypeSuffix [fFdD]

HexadecimalFloatingPointLiteral {HexSignificand}{BinaryExponent}{FloatTypeSuffix}?
HexSignificand {HexNumeral}\.?|0[xX]{HexDigits}?\.{HexDigits}
BinaryExponent [pP][+-]?{FloatingPointDigits}

BooleanLiteral true|false

/* [^\r\n] stands for {AnyCharacter} */
CharacterLiteral \'([^\r\n'\\]|{EscapeSequence})\'
EscapeSequence \\b|\\s|\\t|\\n|\\f|\\r|\\\"|\\\'|\\\\|{OctalEscape}|{UnicodeEscape}
OctalEscape \\{OctalDigit}{1,2}|\\{ZeroToThree}{OctalDigit}{2}
UnicodeEscape \\u+{HexDigit}{4}
ZeroToThree [0-3]

/* [^\r\n] stands for {AnyCharacter} */
StringLiteral \"([^\r\n"\\]|{EscapeSequence})*\"|{TextBlock}
TextBlock \"\"\"[ \t\f]*{LineTerminator}({AnyCharacter}|{EscapeSequence}|{LineTerminator})*\"\"\"

NullLiteral null

OpenParenthesisSeparator \(
CloseParenthesisSeparator \)
OpenBraceSeparator \{
CloseBraceSeparator \}
OpenBracketSeparator \[
CloseBracketSeparator \]
SemicolonSeparator \;
CommaSeparator ,
DotSeparator \.
DoubleColonSeparator ::

AssignmentOperator \=
GreaterThanOperator >
LessThanOperator <
NotOperator !
ComplementOperator ~
QuestionMarkOperator \?
ColonOperator :
AddOperator \+
MinusOperator \-
MultiplyOperator \*
XorOperator \^
ModOperator %
OrOperator \|
AndOperator &
DivideOperator \/
ArrowOperator ->
EqualOperator ==
GreaterThanOrEqualOperator >=
LessThanOrEqualOperator <=
NotEqualOperator !=
BooleanAndOperator &&
BooleanOrOperator \|\|
IncrementOperator \+\+
DecrementOperator --
ShiftLeftOperator <<
ShiftRightOperator >>
ShiftRightArithmeticOperator >>>
AddAssignmentOperator \+=
MinusAssignmentOperator -=
MultiplyAssignmentOperator \*=
DivideAssignmentOperator \/=
AndAssignmentOperator &=
OrAssignmentOperator \|=
XorAssignmentOperator \^=
ModAssignmentOperator %=
ShiftLeftAssignmnetOperator <<=
ShiftRightAssignmentOperator >>=
ShiftRightArithmeticAssignmentOperator >>>=

%%

{WhiteSpace}|{Comment} { /* No op. */ }

{AbstractKeyword} { return AbstractKeyword; }
{ContinueKeyword} { return ContinueKeyword; }
{ForKeyword} { return ForKeyword; }
{NewKeyword} { return NewKeyword; }
{SwitchKeyword} { return SwitchKeyword; }
{AssertKeyword} { return AssertKeyword; }
{DefaultKeyword} { return DefaultKeyword; }
{IfKeyword} { return IfKeyword; }
{PackageKeyword} { return PackageKeyword; }
{SynchronizedKeyword} { return SynchronizedKeyword; }
{BooleanKeyword} { return BooleanKeyword; }
{DoKeyword} { return DoKeyword; }
{GotoKeyword} { return GotoKeyword; }
{PrivateKeyword} { return PrivateKeyword; }
{ThisKeyword} { return ThisKeyword; }
{BreakKeyword} { return BreakKeyword; }
{DoubleKeyword} { return DoubleKeyword; }
{ImplementsKeyword} { return ImplementsKeyword; }
{ProtectedKeyword} { return ProtectedKeyword; }
{ThrowKeyword} { return ThrowKeyword; }
{ByteKeyword} { return ByteKeyword; }
{ElseKeyword} { return ElseKeyword; }
{ImportKeyword} { return ImportKeyword; }
{PublicKeyword} { return PublicKeyword; }
{ThrowsKeyword} { return ThrowsKeyword; }
{CaseKeyword} { return CaseKeyword; }
{InstanceofKeyword} { return InstanceofKeyword; }
{ReturnKeyword} { return ReturnKeyword; }
{TransientKeyword} { return TransientKeyword; }
{CatchKeyword} { return CatchKeyword; }
{ExtendsKeyword} { return ExtendsKeyword; }
{IntKeyword} { return IntKeyword; }
{ShortKeyword} { return ShortKeyword; }
{TryKeyword} { return TryKeyword; }
{CharKeyword} { return CharKeyword; }
{FinalKeyword} { return FinalKeyword; }
{InterfaceKeyword} { return InterfaceKeyword; }
{StaticKeyword} { return StaticKeyword; }
{VoidKeyword} { return VoidKeyword; }
{ClassKeyword} { return ClassKeyword; }
{FinallyKeyword} { return FinallyKeyword; }
{LongKeyword} { return LongKeyword; }
{StrictfpKeyword} { return StrictfpKeyword; }
{VolatileKeyword} { return VolatileKeyword; }
{ConstKeyword} { return ConstKeyword; }
{FloatKeyword} { return FloatKeyword; }
{NativeKeyword} { return NativeKeyword; }
{SuperKeyword} { return SuperKeyword; }
{WhileKeyword} { return WhileKeyword; }
{UnderlineKeyword} { return UnderlineKeyword; }
{VarKeyword} { return VarKeyword; }

{Literal} { return Literal; }
{Identifier} { return Identifier; }

{OpenParenthesisSeparator} { return OpenParenthesisSeparator; }
{CloseParenthesisSeparator} { return CloseParenthesisSeparator; }
{OpenBraceSeparator} { return OpenBraceSeparator; }
{CloseBraceSeparator} { return CloseBraceSeparator; }
{OpenBracketSeparator} { return OpenBracketSeparator; }
{CloseBracketSeparator} { return CloseBracketSeparator; }
{SemicolonSeparator} { return SemicolonSeparator; }
{CommaSeparator} { return CommaSeparator; }
{DotSeparator} { return DotSeparator; }
{DoubleColonSeparator} { return DoubleColonSeparator; }

{AssignmentOperator} { return AssignmentOperator; }
{GreaterThanOperator} { return GreaterThanOperator; }
{LessThanOperator} { return LessThanOperator; }
{NotOperator} { return NotOperator; }
{ComplementOperator} { return ComplementOperator; }

{QuestionMarkOperator} { return QuestionMarkOperator; }
{ColonOperator} { return ColonOperator; }
{AddOperator} { return AddOperator; }
{MinusOperator} { return MinusOperator; }
{MultiplyOperator} { return MultiplyOperator; }
{XorOperator} { return XorOperator; }
{ModOperator} { return ModOperator; }
{OrOperator} { return OrOperator; }
{AndOperator} { return AndOperator; }
{DivideOperator} { return DivideOperator; }
{ArrowOperator} { return ArrowOperator; }
{EqualOperator} { return EqualOperator; }
{GreaterThanOrEqualOperator} { return GreaterThanOrEqualOperator; }
{LessThanOrEqualOperator} { return LessThanOrEqualOperator; }
{NotEqualOperator} { return NotEqualOperator; }
{BooleanAndOperator} { return ;BooleanAndOperator }
{BooleanOrOperator} { return BooleanOrOperator; }
{IncrementOperator} { return IncrementOperator; }
{DecrementOperator} { return DecrementOperator; }
{ShiftLeftOperator} { return ShiftLeftOperator; }
{ShiftRightOperator} { return ShiftRightOperator; }
{ShiftRightArithmeticOperator} { return ShiftRightArithmeticOperator; }
{AddAssignmentOperator} { return AddAssignmentOperator; }
{MinusAssignmentOperator} { return MinusAssignmentOperator; }
{MultiplyAssignmentOperator} { return MultiplyAssignmentOperator; }
{DivideAssignmentOperator} { return DivideAssignmentOperator; }
{AndAssignmentOperator} { return AndAssignmentOperator; }
{OrAssignmentOperator} { return OrAssignmentOperator; }
{XorAssignmentOperator} { return XorAssignmentOperator; }
{ModAssignmentOperator} { return ModAssignmentOperator; }
{ShiftLeftAssignmnetOperator} { return ShiftLeftAssignmnetOperator; }
{ShiftRightAssignmentOperator} { return ShiftRightAssignmentOperator; }
{ShiftRightArithmeticAssignmentOperator} { return ShiftRightArithmeticOperator; }

(?s:.) { return yytext[0]; }

%%
