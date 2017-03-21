grammar Common;

// starting point for parsing codes inside a Java method
modifier
    :   (   'native'
        |   'synchronized'
        |   'transient'
        |   'volatile'
        )
    ;

variableModifier
    :   'final'
    |	'const'
    |   'volatile'
    |   'signed'
    |   'unsigned'
    ;

variableDeclarators
    :   variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    :   variableDeclaratorId ('='? variableInitializer)?
    ;

variableDeclaratorId
    :   Identifier ('[' ']')*
    ;

variableInitializer
    :   arrayInitializer
    |   expression
    ;

variableInitializerList
    :   variableInitializer '...'?
    |   variableInitializerList ',' variableInitializer '...'?
    ;

arrayInitializer
    :   '{' (variableInitializer (',' variableInitializer)* (',')? )? '}'
    |   '{' variableInitializerList ','? '}'
    |   '{' '}'
    ;

enumConstantName
    :   Identifier
    ;

typeType
    :   classOrInterfaceType ('[' ']')*
    |   primitiveType ('[' ']')*
    ;

classOrInterfaceType
    :   Identifier typeArguments? ('.' Identifier typeArguments? )*
    ;

typeList
    :   typeType (',' typeType)*
    ;
    
classBody
    :   '{' classBodyDeclaration* '}'
    ;
    
classBodyDeclaration
    :   ';'
    |   'static'? block
    ;

primitiveType
    :   nestednamespecifier? typename
    |   nestednamespecifier Template simpletemplateid
    |   'boolean'
    |   'bool'
    |   'char'
	|   'char16_t'
	|   'char32_t'
	|   'wchar_t'
    |   'byte'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    |   'void'
	|   Auto
	|   decltypespecifier
    ;

typeArguments
    :   '<' typeArgument (',' typeArgument)* '>'
    ;

typeArgument
    :   typeType
    |   '?' (('extends' | 'super') typeType)?
    ;

// Parse codes with the "methodbody" rule

methodBody
    :   ctorinitializer? block
	|   '=' DEFAULT ';'
	|   '=' DELETE ';'
    ;

ctorinitializer
    :   ':' meminitializerlist
    ;

meminitializerlist
    :   meminitializer '...'?
	|   meminitializer '...'? ',' meminitializerlist
    ;

meminitializer
    :   meminitializerid '(' variableInitializerList? ')'
	|   meminitializerid arrayInitializer
    ;

meminitializerid
    :   classordecltype
	|   Identifier
    ;

classordecltype
    :   nestednamespecifier? classname
	|   decltypespecifier
	;

decltypespecifier
    :   Decltype '(' expression ')'
	|   Decltype '(' Auto ')'
	;

nestednamespecifier
    :   '::'
	|   typename '::'
	|   namespacename '::'
	|   decltypespecifier '::'
	|   nestednamespecifier Identifier '::'
	|   nestednamespecifier Template? simpletemplateid '::'
    ;


namespacename
:
	Identifier
;

originalnamespacename
:
	Identifier
;

typename
    :   Identifier
	|   simpletemplateid
    ;

classname
    :   Identifier
	|   simpletemplateid
    ;

classkey
    :   CLASS
	|   Struct
	|   Union
    ;

simpletemplateid
    :   Identifier '<' templateargumentlist? '>'
    ;

templateargumentlist
:
	templateargument '...'?
	| templateargumentlist ',' templateargument '...'?
;

templateargument
:
	typeid
	| expression
	| idexpression
;

qualifiedName
    :   Identifier ('.' Identifier)*
    ;

// STATEMENTS / BLOCKS

block
    :   '{' blockStatement* '}'
    ;

blockStatement
    :   declaration ';'
    |   statement
    ;

localVariableDeclaration
    :   variableModifier* typeType variableDeclarators
    ;

statement
    :   attributespecifierseq? block
    |   ASSERT expression (':' expression)? ';'
    |   attributespecifierseq? 'if' parExpression statement ('else' statement)?
    |   attributespecifierseq? 'for' '(' forControl ')' statement
    |   attributespecifierseq? 'while' parExpression statement
    |   attributespecifierseq? 'do' statement 'while' parExpression ';'
    |   attributespecifierseq? 'try' ctorinitializer? block (catchClause+ finallyBlock? | finallyBlock)
    |   attributespecifierseq? 'try' ctorinitializer? resourceSpecification block catchClause* finallyBlock?
    |   attributespecifierseq? 'switch' parExpression '{' switchBlockStatementGroup* switchLabel* '}'
    |   'synchronized' parExpression block
    |   attributespecifierseq? 'return' expression? ';'
    |   attributespecifierseq? 'return' arrayInitializer ';'
    |   'throw' expression ';'
    |   attributespecifierseq? 'break' Identifier? ';'
    |   attributespecifierseq? 'continue' Identifier? ';'
    |   attributespecifierseq? 'goto' Identifier? ';'
    |   ';'
    |   attributespecifierseq? expression ';'?
    |   attributespecifierseq? Identifier ':' statement
    |   attributespecifierseq? switchLabel statement
    ;

catchClause
    :   'catch' '(' variableModifier* catchType Identifier ')' block
    |   'catch' '('	attributespecifierseq? typespecifierseq declarator ')' block
    |   'catch' '('	attributespecifierseq? typespecifierseq abstractdeclarator?  ')' block
    |   'catch' '('	'...' ')' block
    ;

catchType
    :   qualifiedName ('|' qualifiedName)*
    ;

finallyBlock
    :   'finally' block
    ;

resourceSpecification
    :   '(' resources ';'? ')'
    ;

resources
    :   resource (';' resource)*
    ;

resource
    :   variableModifier* classOrInterfaceType variableDeclaratorId '=' expression
    ;

switchBlockStatementGroup
    :   switchLabel+ blockStatement+
    ;

switchLabel
    :   'case' expression ':'
    |   'case' enumConstantName ':'
    |   'default' ':'
    ;

forControl
    :   forrangedeclaration ':' forInit
    |   forInit? ';' expression? ';' expressionList?
    ;

forInit
    :   localVariableDeclaration
    |   expressionList
    |   simpledeclaration
    |   arrayInitializer
    ;

forrangedeclaration
    :   attributespecifierseq? declspecifierseq declarator
	|   variableModifier* typeType variableDeclaratorId
    ;

// EXPRESSIONS

parExpression
    :   '(' expression ')'
	|   attributespecifierseq? declspecifierseq declarator '=' variableInitializer
	|   attributespecifierseq? declspecifierseq declarator arrayInitializer
    ;

expressionList
    :   expression (',' expression)*
    ;

expression
    :   primary
    |   expression '.' Identifier
    |   expression '.' 'this'
    |   expression '.' 'new' nonWildcardTypeArguments? innerCreator
    |   expression '.' 'super' superSuffix
    |   expression '.' explicitGenericInvocation
    |   expression '.' Template? idexpression
    |   expression '.' Template? pseudodestructorname
    |   expression '.*' expression
    |   expression '->' Template? idexpression
    |   expression '->' Template? pseudodestructorname
    |   expression '->*' expression
    |   expression '[' expression ']'
    |   expression '[' arrayInitializer ']'
    |   expression '(' expressionList? ')'
    |   expression '(' variableInitializerList? ')'
    |   primitiveType '(' variableInitializerList? ')'
    |   typenamespecifier '(' variableInitializerList? ')'
	|   primitiveType arrayInitializer
	|   typenamespecifier arrayInitializer
    |   '::'? 'new' creator
    |   '(' typeType? ')' expression
    |   expression ('++' | '--')
    |   ('+'|'-'|'++'|'--') expression
    |   ('~'|'!'|'*'|'|'|'&') expression
    |   expression ('*'|'/'|'%') expression
    |   expression ('+'|'-') expression
    |   expression ('<' '<' | '>' '>' '>' | '>' '>') expression
    |   expression ('<=' | '>=' | '>' | '<') expression
    |   expression 'instanceof' typeType
    |   expression ('==' | '!=') expression
    |   expression '&' expression
    |   expression '^' expression
    |   expression '|' expression
    |   expression '&&' expression
    |   expression '||' expression
    |   expression '<<' expression
    |   expression '>>' expression
    |   expression '?' expression ':' expression
	|   Dynamic_cast '<' typeid '>' '(' expression ')'
	|   Static_cast '<' typeid '>' '(' expression ')'
	|   Reinterpret_cast '<' typeid '>' '(' expression ')'
	|   Const_cast '<' typeid '>' '(' expression ')'
	|   Typeid '(' expression ')'
	|   Typeid '(' typeid ')'
	|   '(' typeid ')' expression
	|   Sizeof expression
	|   Sizeof '(' typeid ')'
	|   Sizeof '...' '(' Identifier ')'
	|   Alignof '(' typeid ')'
	|   Noexcept '(' expression ')'
	|   '::'? 'delete' ('[' ']')? expression
    |   <assoc=right> expression
        (   '='
        |   '+='
        |   '-='
        |   '*='
        |   '/='
        |   '&='
        |   '|='
        |   '^='
        |   '>>='
        |   '>>>='
        |   '<<='
        |   '%='
        )
        (   expression
        |   variableInitializer
        )
    ;

pseudodestructorname
:
	nestednamespecifier? typename '::' '~' typename
	| nestednamespecifier Template simpletemplateid '::' '~' typename
	| nestednamespecifier? '~' typename
	| '~' decltypespecifier
;

primary
    :   '(' expression ')'
    |   'this'
    |   'super'
    |   literal
    |   Identifier
    |   idexpression
    |   lambdaexpression
    |   typeType '.' 'class'
    |   'void' '.' 'class'
    |   nonWildcardTypeArguments (explicitGenericInvocationSuffix | 'this' arguments)
    ;

lambdaexpression
:
	lambdaintroducer lambdadeclarator? block
;

lambdaintroducer
:
	'[' lambdacapture? ']'
;

lambdacapture
:
	capturedefault
	| capturelist
	| capturedefault ',' capturelist
;

capturedefault
:
	'&'
	| '='
;

capturelist
:
	capture '...'?
	| capturelist ',' capture '...'?
;

capture
:
	simplecapture
	| initcapture
;

simplecapture
:
	Identifier
	| '&' Identifier
	| THIS
;

initcapture
:
	Identifier initializer
	| '&' Identifier initializer
;

initializer
:
	'=' variableInitializer
	| arrayInitializer
	| '(' variableInitializerList ')'
;

lambdadeclarator
:
	'(' parameterdeclarationclause ')' Mutable? exceptionspecification?
	attributespecifierseq? trailingreturntype?
;

idexpression
:
	unqualifiedid
	| qualifiedid
;

unqualifiedid
:
	Identifier
	| Operator OPERATOR
	| Operator typespecifierseq conversiondeclarator?
	| Operator StringLiteral Identifier
	| Operator UserDefinedStringLiteral
	| '~' classname
	| '~' decltypespecifier
	| templateid
;

qualifiedid
:
	nestednamespecifier Template? unqualifiedid
;

templateid
:
	simpletemplateid
	| Operator OPERATOR '<' templateargumentlist? '>'
	| Operator StringLiteral Identifier '<' templateargumentlist? '>'
	| Operator UserDefinedStringLiteral '<' templateargumentlist? '>'
;

conversiondeclarator
    :   ptroperator conversiondeclarator?
    ;

ptroperator
    :   '*' attributespecifierseq? cvqualifierseq?
	|   '&' attributespecifierseq?
	|   '&&' attributespecifierseq?
	|   nestednamespecifier '*' attributespecifierseq? cvqualifierseq?
	;

typespecifierseq
    :   typespecifier attributespecifierseq?
	|   typespecifier typespecifierseq
    ;

typespecifier
    :   trailingtypespecifier
	|   classspecifier
	|   enumspecifier
    ;

classspecifier
:
	classhead '{' memberspecification? '}'
;

memberspecification
:
	memberdeclaration memberspecification?
	| accessspecifier ':' memberspecification?
;

memberdeclaration
:
	attributespecifierseq? declspecifierseq? memberdeclaratorlist? ';'
	| functiondefinition
	| usingdeclaration
	| static_assertdeclaration
	| templatedeclaration
	| aliasdeclaration
	| emptydeclaration
;

functiondefinition
:
	attributespecifierseq? declspecifierseq? declarator virtspecifierseq?
	methodBody
;

usingdeclaration
:
	Using Typename? nestednamespecifier unqualifiedid ';'
	| Using '::' unqualifiedid ';'
;

static_assertdeclaration
:
	Static_assert '(' expression ',' StringLiteral ')' ';'
;

templatedeclaration
:
	Template '<' templateparameterlist '>' declaration
;

templateparameterlist
:
	templateparameter
	| templateparameterlist ',' templateparameter
;

templateparameter
:
	typeparameter
	| parameterdeclaration
;

typeparameter
:
	CLASS '...'? Identifier?
	| CLASS Identifier? '=' typeid
	| Typename '...'? Identifier?
	| Typename Identifier? '=' typeid
	| Template '<' templateparameterlist '>' CLASS '...'? Identifier?
	| Template '<' templateparameterlist '>' CLASS Identifier? '=' idexpression
;

declarationseq
:
	declaration
	| declarationseq declaration
;

declaration
:
	localVariableDeclaration
	| blockdeclaration
	| functiondefinition
	| templatedeclaration
	| explicitinstantiation
	| explicitspecialization
	| linkagespecification
	| namespacedefinition
	| emptydeclaration
	| attributedeclaration
;

blockdeclaration
:
	simpledeclaration
	| asmdefinition
	| namespacealiasdefinition
	| usingdeclaration
	| usingdirective
	| static_assertdeclaration
	| aliasdeclaration
	| opaqueenumdeclaration
;

initdeclaratorlist
:
	declarator initializer?
	| initdeclaratorlist ',' declarator initializer?
;

explicitinstantiation
:
	Extern? Template declaration
;

explicitspecialization
:
	Template '<' '>' declaration
;

usingdirective
:
	attributespecifierseq? Using Namespace nestednamespecifier? namespacename ';'
;

asmdefinition
:
	Asm '(' StringLiteral ')' ';'
;

linkagespecification
:
	Extern StringLiteral '{' declarationseq? '}'
	| Extern StringLiteral declaration
;

aliasdeclaration
:
	Using Identifier attributespecifierseq? '=' typeid ';'
;

simpledeclaration
:
	declspecifierseq? initdeclaratorlist? ';'
	| attributespecifierseq declspecifierseq? initdeclaratorlist ';'
;

emptydeclaration
:
	';'
;

attributedeclaration
:
	attributespecifierseq ';'
;

opaqueenumdeclaration
:
	enumkey attributespecifierseq? Identifier enumbase? ';'
;

namespacedefinition
:
	Inline? Namespace Identifier '{' declarationseq? '}'
    | Inline? Namespace originalnamespacename '{' declarationseq? '}'
	| Inline? Namespace '{' declarationseq? '}'
;

namespacealiasdefinition
:
	Namespace Identifier '=' nestednamespecifier? namespacename ';'
;

memberdeclaratorlist
:
	memberdeclarator
	| memberdeclaratorlist ',' memberdeclarator
;

memberdeclarator
:
	declarator virtspecifierseq? purespecifier?
	| declarator ('=' variableInitializer | arrayInitializer)?
	| Identifier? attributespecifierseq? ':' expression
;

virtspecifierseq
:
	virtspecifier
	| virtspecifierseq virtspecifier
;

virtspecifier
:
	Override
	| FINAL
;

purespecifier
:
	ASSIGN val = OctalLiteral
	{if($val.text.compareTo("0")!=0) throw new InputMismatchException(this);}
;

classhead
:
	classkey attributespecifierseq? classheadname classvirtspecifier? baseclause?
	| classkey attributespecifierseq? baseclause?
;

baseclause
:
	':' basespecifierlist
;

basespecifierlist
:
	basespecifier '...'?
	| basespecifierlist ',' basespecifier '...'?
;

basespecifier
:
	attributespecifierseq? classordecltype
	| attributespecifierseq? Virtual accessspecifier? classordecltype
	| attributespecifierseq? accessspecifier Virtual? classordecltype
;

accessspecifier
:
	PRIVATE
	| PROTECTED
	| PUBLIC
;

classheadname
:
	nestednamespecifier? classname
;

classvirtspecifier
:
	FINAL
;

trailingtypespecifierseq
:
	trailingtypespecifier attributespecifierseq?
	| trailingtypespecifier trailingtypespecifierseq
;

trailingtypespecifier
    :   primitiveType
	|   elaboratedtypespecifier
	|   typenamespecifier
	|   variableModifier
    ;

elaboratedtypespecifier
    :   classkey attributespecifierseq? nestednamespecifier? Identifier
	|   classkey simpletemplateid
	|   classkey nestednamespecifier Template? simpletemplateid
	|   ENUM nestednamespecifier? Identifier
    ;

typenamespecifier
    :   Typename nestednamespecifier Identifier
	|   Typename nestednamespecifier Template? simpletemplateid
	;

cvqualifierseq
    :	variableModifier cvqualifierseq?
    ;

enumspecifier
    :   enumhead '{' enumeratorlist? '}'
	|   enumhead '{' enumeratorlist ',' '}'
	;

enumeratorlist
:
	enumeratordefinition
	| enumeratorlist ',' enumeratordefinition
;

enumeratordefinition
:
	Identifier
	| Identifier '=' expression
;

enumhead
    :   enumkey attributespecifierseq? Identifier? enumbase?
	|   enumkey attributespecifierseq? nestednamespecifier Identifier enumbase?
	;

enumkey
    :   ENUM
	|   ENUM CLASS
	|   ENUM Struct
	;

enumbase
    :   ':' typespecifierseq
    ;

attributespecifierseq
    :   attributespecifier
	|   attributespecifierseq attributespecifier
	;

attributespecifier
    :   '[' '[' attributelist ']' ']'
	|   alignmentspecifier
	;

alignmentspecifier
    :   Alignas '(' typeid '...'? ')'
	|   Alignas '(' expression '...'? ')'
    ;

typeid
    :   typespecifierseq abstractdeclarator?
    ;

abstractdeclarator
    :   ptrabstractdeclarator
	|   noptrabstractdeclarator? parametersandqualifiers trailingreturntype
	|   abstractpackdeclarator
	;

ptrabstractdeclarator
:
	noptrabstractdeclarator
	| ptroperator ptrabstractdeclarator?
;

noptrabstractdeclarator
:
	noptrabstractdeclarator parametersandqualifiers
	| parametersandqualifiers
	| noptrabstractdeclarator '[' expression? ']' attributespecifierseq?
	| '[' expression? ']' attributespecifierseq?
	| '(' ptrabstractdeclarator ')'
;

abstractpackdeclarator
:
	noptrabstractpackdeclarator
	| ptroperator abstractpackdeclarator
;

noptrabstractpackdeclarator
:
	noptrabstractpackdeclarator parametersandqualifiers
	| noptrabstractpackdeclarator '[' expression? ']'
	attributespecifierseq?
	| '...'
;

parametersandqualifiers
:
	'(' parameterdeclarationclause ')' cvqualifierseq? refqualifier?
	exceptionspecification? attributespecifierseq?
;

exceptionspecification
:
	dynamicexceptionspecification
	| noexceptspecification
;

dynamicexceptionspecification
:
	THROW '(' typeidlist? ')'
;

typeidlist
:
	typeid '...'?
	| typeidlist ',' typeid '...'?
;

noexceptspecification
:
	Noexcept '(' expression ')'
	| Noexcept
;

refqualifier
:
	'&'
	| '&&'
;

parameterdeclarationclause
:
	parameterdeclarationlist? '...'?
	| parameterdeclarationlist ',' '...'
;

parameterdeclarationlist
:
	parameterdeclaration
	| parameterdeclarationlist ',' parameterdeclaration
;

parameterdeclaration
:
	attributespecifierseq? declspecifierseq declarator
	| attributespecifierseq? declspecifierseq declarator '=' variableInitializer
	| attributespecifierseq? declspecifierseq abstractdeclarator?
	| attributespecifierseq? declspecifierseq abstractdeclarator? '='
	variableInitializer
;

declarator
:
	ptrdeclarator
	| noptrdeclarator parametersandqualifiers trailingreturntype
;

ptrdeclarator
:
	noptrdeclarator
	| ptroperator ptrdeclarator
;

noptrdeclarator
:
	'...'? idexpression attributespecifierseq?
	| noptrdeclarator parametersandqualifiers
	| noptrdeclarator '[' expression? ']' attributespecifierseq?
	| '(' ptrdeclarator ')'
;

declspecifier
:
	storageclassspecifier
	| typespecifier
	| functionspecifier
	| Friend
	| Typedef
	| Constexpr
;

storageclassspecifier
:
	Register
	| STATIC
	| Thread_local
	| Extern
	| Mutable
;

functionspecifier
:
	Inline
	| Virtual
	| Explicit
;

declspecifierseq
:
	declspecifier attributespecifierseq?
	| declspecifier declspecifierseq
;

trailingreturntype
:
	'->' trailingtypespecifierseq abstractdeclarator?
;

attributelist
    :   attribute?
	|   attributelist ',' attribute?
	|   attribute '...'
	|   attributelist ',' attribute '...'
    ;

attribute
    :   attributetoken attributeargumentclause?
    ;

attributetoken
    :   Identifier
	|   Identifier '::' Identifier
    ;

attributeargumentclause
    :   '(' balancedtokenseq ')'
    ;

balancedtokenseq
    :   balancedtoken?
	|   balancedtokenseq balancedtoken
	;

balancedtoken
    :   '(' balancedtokenseq ')'
	|   '[' balancedtokenseq ']'
	|   '{' balancedtokenseq '}'
	;

creator
    :   nonWildcardTypeArguments createdName creatorInitializer
    |   '(' variableInitializerList ')'? createdName creatorInitializer
    ;

createdName
    :   Identifier typeArgumentsOrDiamond? ('.' Identifier typeArgumentsOrDiamond?)*
    |   primitiveType
    |   newtypeid
    |   '(' typeid ')'
    ;

newtypeid
:
	typespecifierseq newdeclarator?
;

newdeclarator
:
	ptroperator newdeclarator?
	| noptrnewdeclarator
;

noptrnewdeclarator
:
	'[' expression ']' attributespecifierseq?
	| noptrnewdeclarator '[' expression ']' attributespecifierseq?
;

innerCreator
    :   Identifier nonWildcardTypeArgumentsOrDiamond? creatorInitializer
    ;

creatorInitializer
    :   '['
        (   ']' ('[' ']')* arrayInitializer
        |   expression ']' ('[' expression ']')* ('[' ']')*
        )
    |   arguments classBody?
    |   arrayInitializer
    |   '(' variableInitializerList ')'
    ;

explicitGenericInvocation
    :   nonWildcardTypeArguments explicitGenericInvocationSuffix
    ;

nonWildcardTypeArguments
    :   '<' typeList '>'
    ;

typeArgumentsOrDiamond
    :   '<' '>'
    |   typeArguments
    ;

nonWildcardTypeArgumentsOrDiamond
    :   '<' '>'
    |   nonWildcardTypeArguments
    ;

superSuffix
    :   arguments
    |   '.' Identifier arguments?
    ;

explicitGenericInvocationSuffix
    :   'super' superSuffix
    |   Identifier arguments
    ;

arguments
    :   '(' expressionList? ')'
    ;

// LEXER

// Separators

LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';

// Operators

ASSIGN          : '=';
GT              : '>';
LT              : '<';
BANG            : '!';
TILDE           : '~';
QUESTION        : '?';
COLON           : ':';
EQUAL           : '==';
LE              : '<=';
GE              : '>=';
NOTEQUAL        : '!=';
AND             : '&&';
OR              : '||';
INC             : '++';
DEC             : '--';
ADD             : '+';
SUB             : '-';
MUL             : '*';
DIV             : '/';
BITAND          : '&';
BITOR           : '|';
CARET           : '^';
MOD             : '%';

ADD_ASSIGN      : '+=';
SUB_ASSIGN      : '-=';
MUL_ASSIGN      : '*=';
DIV_ASSIGN      : '/=';
AND_ASSIGN      : '&=';
OR_ASSIGN       : '|=';
XOR_ASSIGN      : '^=';
MOD_ASSIGN      : '%=';
LSHIFT          : '<<';
RSHIFT          : '>>';
LSHIFT_ASSIGN   : '<<=';
RSHIFT_ASSIGN   : '>>=';
URSHIFT_ASSIGN  : '>>>=';
DOT_STAR        : '.*';
ARROW_STAR      : '->*';
ARROW           : '->';
DOUBLE_COLON    : '::';

OPERATOR        : NEW
                | DELETE
                | NEW '[' ']'
                | DELETE '[' ']'
            	| '(' ')'
            	| '[' ']'
                | '='
                | '>'
                | '<'
                | '!'
                | '~'
                | '?'
                | ':'
                | '=='
                | '<='
                | '>='
                | '!='
                | '&&'
                | '||'
                | '++'
                | '--'
                | '+'
                | '-'
                | '*'
                | '/'
                | '&'
                | '|'
                | '^'
                | '%'
                | ','
                | '+='
                | '-='
                | '*='
                | '/='
                | '&='
                | '|='
                | '^='
                | '%='
                | '<<'
                | '>>'
                | '<<='
                | '>>='
                | '>>>='
                | '->*'
                | '->'
                ;

// Keywords

Alignas       : 'alignas';
Alignof       : 'alignof';
Asm           : 'asm';
Auto          : 'auto';
ABSTRACT      : 'abstract';
ASSERT        : 'assert';
Bool          : 'bool';
BOOLEAN       : 'boolean';
BREAK         : 'break';
BYTE          : 'byte';
CASE          : 'case';
CATCH         : 'catch';
CHAR          : 'char';
Char16        : 'char16_t';
Char32        : 'char32_t';
CLASS         : 'class';
CONST         : 'const';
Constexpr     : 'constexpr';
Const_cast    : 'const_cast';
CONTINUE      : 'continue';
Decltype      : 'decltype';
DEFAULT       : 'default';
DELETE        : 'delete';
DO            : 'do';
DOUBLE        : 'double';
Dynamic_cast  : 'dynamic_cast';
ELSE          : 'else';
ENUM          : 'enum';
Explicit      : 'explicit';
Export        : 'export';
Extern        : 'extern';
EXTENDS       : 'extends';
FALSE         : 'false';
FINAL         : 'final';
FINALLY       : 'finally';
FLOAT         : 'float';
FOR           : 'for';
Friend        : 'friend';
IF            : 'if';
GOTO          : 'goto';
IMPLEMENTS    : 'implements';
IMPORT        : 'import';
Inline        : 'inline';
INSTANCEOF    : 'instanceof';
INT           : 'int';
INTERFACE     : 'interface';
LONG          : 'long';
Mutable       : 'mutable';
Namespace     : 'namespace';
NATIVE        : 'native';
NEW           : 'new';
Noexcept      : 'noexcept';
Nullptr       : 'nullptr';
Operator      : 'operator';
Override      : 'override';
PACKAGE       : 'package';
PRIVATE       : 'private';
PROTECTED     : 'protected';
PUBLIC        : 'public';
Register      : 'register';
Reinterpret_cast : 'reinterpret_cast';
RETURN        : 'return';
SHORT         : 'short';
Signed        : 'signed';
Sizeof        : 'sizeof';
Static_assert : 'static_assert';
Static_cast   : 'static_cast';
STATIC        : 'static';
STRICTFP      : 'strictfp';
Struct        : 'struct';
SUPER         : 'super';
SWITCH        : 'switch';
SYNCHRONIZED  : 'synchronized';
Template      : 'template';
THIS          : 'this';
Thread_local  : 'thread_local';
THROW         : 'throw';
THROWS        : 'throws';
TRANSIENT     : 'transient';
TRUE          : 'true';
TRY           : 'try';
Typedef       : 'typedef';
Typeid        : 'typeid';
Typename      : 'typename';
Union         : 'union';
Unsigned      : 'unsigned';
Using         : 'using';
Virtual       : 'virtual';
VOID          : 'void';
VOLATILE      : 'volatile';
Wchar         : 'wchar_t';
WHILE         : 'while';

literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   CharacterLiteral
    |   StringLiteral
    |   BooleanLiteral
    |   PointerLiteral
	|   UserDefinedLiteral
    |   NullLiteral
    ;

// Pointer Literals
PointerLiteral
    :   Nullptr
    ;

// User Defined Literals
UserDefinedLiteral
    :   UserDefinedIntegerLiteral
	|   UserDefinedFloatingLiteral
	|   UserDefinedStringLiteral
	|   UserDefinedCharacterLiteral
	;

UserDefinedIntegerLiteral
    :   DecimalLiteral Udsuffix
	|   OctalLiteral Udsuffix
	|   HexadecimalLiteral Udsuffix
	|   BinaryLiteral Udsuffix
	;

UserDefinedFloatingLiteral
    :   DigitSequence '.' DigitSequence? ExponentPart? Udsuffix?
    |   DigitSequence? '.' DigitSequence ExponentPart? Udsuffix?
    |   DigitSequence '.' ExponentPart? Udsuffix?
    |   '.' DigitSequence ExponentPart? Udsuffix?
    |   DigitSequence ExponentPart Udsuffix?
    |   DigitSequence Udsuffix
    ;

UserDefinedStringLiteral
    :   StringLiteral Udsuffix
    ;

UserDefinedCharacterLiteral
    :   CharacterLiteral Udsuffix
    ;

fragment
Udsuffix
    :   Identifier
    ;

// Integer Literals
IntegerLiteral
    :   DecimalLiteral IntegerTypeSuffix?
    |   HexadecimalLiteral IntegerTypeSuffix?
    |   OctalLiteral IntegerTypeSuffix?
    |   BinaryLiteral IntegerTypeSuffix?
    ;

fragment
IntegerTypeSuffix
    :   Longsuffix?
	|   Unsignedsuffix Longsuffix?
	|   Unsignedsuffix Longlongsuffix?
	|   Longsuffix Unsignedsuffix?
	|   Longlongsuffix Unsignedsuffix?
    ;

fragment
Unsignedsuffix
    :   [uU]
    ;

fragment
Longsuffix
    :   [lL]
    ;

fragment
Longlongsuffix
    :   'll'
	|   'LL'
	;

DecimalLiteral
    :   '0'
    |   NonZeroDigit (Digits? | '_'+ Digits)
    |   NonZeroDigit ('\''? Digit)*
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

HexadecimalLiteral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit (HexDigitOrUnderscore* HexDigit)?
    |   HexDigit ('\''? HexDigit)*
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
HexDigitOrUnderscore
    :   HexDigit
    |   '_'
    ;

OctalLiteral
    :   '0' '_'+? OctalDigits
    |	'0' ('\''? OctalDigit)*
    ;

fragment
OctalDigits
    :   OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
OctalDigitOrUnderscore
    :   OctalDigit
    |   '_'
    ;

BinaryLiteral
    :   '0' [bB] BinaryDigits
    ;

fragment
BinaryDigits
    :   BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
    |   BinaryDigit ('\''? BinaryDigit)*
    ;

fragment
BinaryDigit
    :   [01]
    ;

fragment
BinaryDigitOrUnderscore
    :   BinaryDigit
    |   '_'
    ;

// Floating Point Literals
FloatingPointLiteral
    :   DecimalFloatingPointLiteral
    |   HexadecimalFloatingPointLiteral
    ;

fragment
DecimalFloatingPointLiteral
    :   DigitSequence '.' DigitSequence? ExponentPart? FloatTypeSuffix?
    |   DigitSequence? '.' DigitSequence ExponentPart? FloatTypeSuffix?
    |   DigitSequence '.' ExponentPart? FloatTypeSuffix?
    |   '.' DigitSequence ExponentPart? FloatTypeSuffix?
    |   DigitSequence ExponentPart FloatTypeSuffix?
    |   DigitSequence FloatTypeSuffix
    ;

fragment
DigitSequence
    :   Digits
	|   Digit ('\''? Digit)*
	;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? DigitSequence
    ;

fragment
Sign
    :   [+-]
    ;

fragment
FloatTypeSuffix
    :   [flFLdD]
    ;

fragment
HexadecimalFloatingPointLiteral
    :   HexSignificand BinaryExponent FloatTypeSuffix?
    ;

fragment
HexSignificand
    :   HexadecimalLiteral '.'?
    |   '0' [xX] HexDigits? '.' HexDigits
    ;

fragment
BinaryExponent
    :   BinaryExponentIndicator SignedInteger
    ;

fragment
BinaryExponentIndicator
    :   [pP]
    ;

// Boolean Literals

BooleanLiteral
    :   TRUE
    |   FALSE
    ;

// Character Literals

CharacterLiteral
    :   '\'' Character+ '\''
	|   'u' '\'' Character+ '\''
	|   'U' '\'' Character+ '\''
	|   'L' '\'' Character+ '\''
	;

fragment
Character
    :   ~['\\\r\n]
    |   EscapeSequence
    |   UniversalCharacter
    ;

fragment
EscapeSequence
    :   SimpleEscape
    |   OctalEscape
    |   UnicodeEscape
    ;

fragment
SimpleEscape
    :   '\\\''
	|   '\\"'
	|   '\\?'
	|   '\\\\'
	|   '\\a'
	|   '\\b'
	|   '\\f'
	|   '\\n'
	|   '\\r'
	|   '\\t'
	|   '\\v'
	;

fragment
OctalEscape
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' OctalDigit OctalDigit OctalDigit
    |   '\\' [0-3] OctalDigit OctalDigit
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    |   '\\x' HexDigit+
    ;

fragment
UniversalCharacter
    :   '\\u' Hexquad
	|   '\\U' Hexquad Hexquad
	;

fragment
Hexquad
    :   HexDigit HexDigit HexDigit HexDigit
    ;

// String Literals

StringLiteral
    :   '"' (Character+)? '"'
    | 	EncodingPrefix? '"' Character* '"'
    |   EncodingPrefix? 'R' RawString
    ;

fragment
EncodingPrefix
    :   'u8'
	|   'u'
	|   'U'
	|   'L'
	;

fragment
RawString
    :   '"' .*? '(' .*? ')' .*? '"'
    ;

// Null Literal

NullLiteral
    :   'null'
    ;

// Identifiers

Identifier
    :   Letter LetterOrDigit*
    ;

fragment
Letter
    :   [a-zA-Z$_]
    |   UniversalCharacter
    |   ~[\u0000-\u007F\uD800-\uDBFF]
    |   [\uD800-\uDBFF] [\uDC00-\uDFFF]
    ;

fragment
LetterOrDigit
    :   [a-zA-Z0-9$_]
    |   UniversalCharacter
    |   ~[\u0000-\u007F\uD800-\uDBFF]
    |   [\uD800-\uDBFF] [\uDC00-\uDFFF]
    ;

// Additional symbols not defined in the lexical specification

AT : '@';
ELLIPSIS : '...';

// Whitespace and comments

Whitespace  
    :  [ \t\r\n\u000C]+ -> skip
    ;

BlockComment
    :   '/*' .*? '*/' -> channel(HIDDEN)
    ;

LineComment
    :   '//' ~[\r\n]* -> channel(HIDDEN)
    ;

MultiLineMacro
    :   '#' (~[\n]*? '\\' '\r'? '\n')+ ~[\n]+ -> channel(HIDDEN)
    ;

Directive
    :   '#' ~[\n]* -> channel(HIDDEN)
    ;