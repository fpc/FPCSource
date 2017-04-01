{
    This file is part of the Free Component Library

    Pascal source parser
    Copyright (c) 2000-2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Abstract:
  Resolves references by setting TPasElement.CustomData as TResolvedReference.
  Creates search scopes for elements with sub identifiers by setting
    TPasElement.CustomData as TPasScope: unit, program, library, interface,
    implementation, procs

Works:
- built-in types as TPasUnresolvedSymbolRef: longint, int64, string, pointer, ...
- references in statements, error if not found
- interface and implementation types, vars, const
- params, local types, vars, const
- nested procedures
- nested forward procs, nested must be resolved before proc body
- program/library/implementation forward procs
- search in used units
- unitname.identifier
- alias types, 'type a=b'
- type alias type 'type a=type b'
- choose the most compatible overloaded procedure
- while..do
- repeat..until
- if..then..else
- binary operators
- case..of
- try..finally..except, on, else, raise
- for loop
- spot duplicates
- type cast base types
- char
  - ord(), chr()
- record
  - variants
  - const param makes children const too
- class:
  - forward declaration
  - instance.a
  - find ancestor, search in ancestors
  - virtual, abstract, override
  - method body
  - Self
  - inherited
  - property
    - read var, read function
    - write var, write function
    - stored function
    - defaultexpr
  - is and as operator
  - nil
  - constructor result type, rrfNewInstance
  - destructor call type: rrfFreeInstance
  - type cast
  - class of
  - class method, property, var, const
  - class-of.constructor
  - class-of typecast upwards/downwards
  - class-of option to allow is-operator
  - typecast Self in class method upwards/downwards
  - property with params
  - default property
  - visibility, override: warn and fix if lower
  - sealed
- with..do
- enums - TPasEnumType, TPasEnumValue
  - propagate to parent scopes
  - function ord(): integer
  - function low(ordinal): ordinal
  - function high(ordinal): ordinal
  - function pred(ordinal): ordinal
  - function high(ordinal): ordinal
  - cast integer to enum
- sets - TPasSetType
  - set of char
  - set of integer
  - set of boolean
  - set of enum
  - ranges 'a'..'z'  2..5
  - operators: +, -, *, ><, <=, >=
  - in-operator
  - assign operators: +=, -=, *=
  - include(), exclude()
- typed const: check expr type
- function length(const array or string): integer
- procedure setlength(var array or string; newlength: integer)
- ranges TPasRangeType
- procedure exit, procedure exit(const function result)
- check if types only refer types+const
- check const expression types, e.g. bark on "const c:string=3;"
- procedure inc/dec(var ordinal; decr: ordinal = 1)
- function Assigned(Pointer or Class or Class-Of): boolean
- arrays TPasArrayType
  - TPasEnumType, char, integer, range
  - low, high, length, setlength, assigned
  - function concat(array1,array2,...): array
  - function copy(array): array, copy(a,start), copy(a,start,end)
  - insert(item; var array; index: integer)
  - delete(var array; start, count: integer)
  - element
  - multi dimensional
  - const
  - open array, override, pass array literal, pass var
  - type cast array to arrays with same dimensions and compatible element type
- check if var initexpr fits vartype: var a: type = expr;
- built-in functions high, low for range types
- procedure type
- method type
- function without params: mark if call or address, rrfImplicitCallWithoutParams
- procedure break, procedure continue
- built-in functions pred, succ for range type and enums
- untyped parameters
- built-in procedure str(const boolean|integer|enumvalue|classinstance,var s: string)

ToDo:
- fix slow lookup declaration proc in PParser
- fail to write a loop var inside the loop
- warn: create class with abstract methods
- classes - TPasClassType
   - nested var, const
   - nested types
- check if constant is longint or int64
- for..in..do
- pointer TPasPointerType
- records - TPasRecordType,
   - const  TRecordValues
   - function default(record type): record
   - pointer of record
- proc: check if forward and impl default values match
- call array of proc without ()
- pointer type, ^type, @ operator, [] operator
- type alias type
- object
- interfaces
  - implements, supports
- TPasResString
- generics, nested param lists
- dotted unitnames
- type helpers
- record/class helpers
- generics
- operator overload
- is nested
- TPasFileType
- labels
- many more: search for "ToDo:"

Debug flags: -d<x>
  VerbosePasResolver

Notes:
 Functions and function types without parameters:
   property P read f; // use function f, not its result
   f.  // implicit resolve f once if param less function or function type
   f[]  // implicit resolve f once if a param less function or function type
   @f;  use function f, not its result
   @p.f;  @ operator applies to f, not p
   @f();  @ operator applies to result of f
   f(); use f's result
   FuncVar:=Func; if mode=objfpc: incompatible
                  if mode=delphi: implicit addr of function f
   if f=g then : can implicit resolve each side once
   p(f), f as var parameter: can implicit
}
unit PasResolver;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, contnrs, PasTree, PParser, PScanner;

const
  ParserMaxEmbeddedColumn = 2048;
  ParserMaxEmbeddedRow = $7fffffff div ParserMaxEmbeddedColumn;

// message numbers
const
  nIdentifierNotFound = 3001;
  nNotYetImplemented = 3002;
  nIllegalQualifier = 3003;
  nSyntaxErrorExpectedButFound = 3004;
  nWrongNumberOfParametersForCallTo = 3005;
  nIncompatibleTypeArgNo = 3006;
  nIncompatibleTypeArgNoVarParamMustMatchExactly = 3007;
  nVariableIdentifierExpected = 3008;
  nDuplicateIdentifier = 3009;
  nXExpectedButYFound = 3010;
  nAncestorCycleDetected = 3011;
  nCantUseForwardDeclarationAsAncestor = 3012;
  nCantDetermineWhichOverloadedFunctionToCall = 3013;
  nForwardTypeNotResolved = 3014;
  nForwardProcNotResolved = 3015;
  nInvalidProcModifiers = 3016;
  nAbstractMethodsMustNotHaveImplementation = 3017;
  nCallingConventionMismatch = 3018;
  nResultTypeMismatchExpectedButFound = 3019;
  nFunctionHeaderMismatchForwardVarName = 3020;
  nFunctionHidesIdentifier = 3021;
  nNoMethodInAncestorToOverride = 3022;
  nInheritedOnlyWorksInMethods = 3023;
  nInheritedNeedsAncestor = 3024;
  nNoPropertyFoundToOverride = 3025;
  nExprTypeMustBeClassOrRecordTypeGot = 3026;
  nPropertyNotWritable = 3027;
  nIncompatibleTypesGotExpected = 3028;
  nTypesAreNotRelated = 3029;
  nAbstractMethodsCannotBeCalledDirectly = 3030;
  nMissingParameterX = 3031;
  nOnlyClassMembersCanBeReferredWithClassReferences = 3032;
  nInOperatorExpectsSetElementButGot = 3033;
  nWrongNumberOfParametersForTypeCast = 3034;
  nIllegalTypeConversionTo = 3035;
  nConstantExpressionExpected = 3036;
  nLeftSideOfIsOperatorExpectsAClassButGot = 3037;
  nNotReadable = 3038;
  nClassPropertyAccessorMustBeStatic = 3039;
  nClassPropertyAccessorMustNotBeStatic = 3040;
  nOnlyOneDefaultPropertyIsAllowed = 3041;
  nWrongNumberOfParametersForArray = 3042;
  nCantAssignValuesToAnAddress = 3043;
  nIllegalExpression = 3044;
  nCantAccessPrivateMember = 3045;
  nMustBeInsideALoop = 3046;
  nExpectXArrayElementsButFoundY = 3047;
  nCannotCreateADescendantOfTheSealedClass = 3048;
  nAncestorIsNotExternal = 3049;
  nVirtualMethodXHasLowerVisibility = 3050; // FPC 3250
  nExternalClassInstanceCannotAccessStaticX = 3051;

// resourcestring patterns of messages
resourcestring
  sIdentifierNotFound = 'identifier not found "%s"';
  sNotYetImplemented = 'not yet implemented: %s';
  sIllegalQualifier = 'illegal qualifier "%s"';
  sSyntaxErrorExpectedButFound = 'Syntax error, "%s" expected but "%s" found';
  sWrongNumberOfParametersForCallTo = 'Wrong number of parameters specified for call to "%s"';
  sIncompatibleTypeArgNo = 'Incompatible type arg no. %s: Got "%s", expected "%s"';
  sIncompatibleTypeArgNoVarParamMustMatchExactly = 'Incompatible type arg no. %s: Got "%s", expected "%s". Var param must match exactly.';
  sVariableIdentifierExpected = 'Variable identifier expected';
  sDuplicateIdentifier = 'Duplicate identifier "%s" at %s';
  sXExpectedButYFound = '%s expected, but %s found';
  sAncestorCycleDetected = 'Ancestor cycle detected';
  sCantUseForwardDeclarationAsAncestor = 'Can''t use forward declaration "%s" as ancestor';
  sCantDetermineWhichOverloadedFunctionToCall = 'Can''t determine which overloaded function to call';
  sForwardTypeNotResolved = 'Forward type not resolved "%s"';
  sForwardProcNotResolved = 'Forward %s not resolved "%s"';
  sInvalidProcModifiers = 'Invalid %s modifiers %s';
  sAbstractMethodsMustNotHaveImplementation = 'Abstract method must not have an implementation.';
  sCallingConventionMismatch = 'Calling convention mismatch';
  sResultTypeMismatchExpectedButFound = 'Result type mismatch, expected %s, but found %s';
  sFunctionHeaderMismatchForwardVarName = 'function header "%s" doesn''t match forward : var name changes %s => %s';
  sFunctionHidesIdentifier = 'function hides identifier "%s" at "%s"';
  sNoMethodInAncestorToOverride = 'There is no method in an ancestor class to be overridden "%s"';
  sInheritedOnlyWorksInMethods = 'Inherited works only in methods';
  sInheritedNeedsAncestor = 'inherited needs an ancestor';
  sNoPropertyFoundToOverride = 'No property found to override';
  sExprTypeMustBeClassOrRecordTypeGot = 'Expression type must be class or record type, got %s';
  sPropertyNotWritable = 'No member is provided to access property';
  sIncompatibleTypesGotExpected = 'Incompatible types: got "%s" expected "%s"';
  sTypesAreNotRelated = 'Types are not related';
  sAbstractMethodsCannotBeCalledDirectly = 'Abstract methods cannot be called directly';
  sMissingParameterX = 'Missing parameter %s';
  sOnlyClassMembersCanBeReferredWithClassReferences = 'Only class methods, class properties and class variables can be referred with class references';
  sInOperatorExpectsSetElementButGot = 'the in-operator expects a set element, but got %s';
  sWrongNumberOfParametersForTypeCast = 'wrong number of parameters for type cast to %s';
  sIllegalTypeConversionTo = 'Illegal type conversion: "%s" to "%s"';
  sConstantExpressionExpected = 'Constant expression expected';
  sLeftSideOfIsOperatorExpectsAClassButGot = 'left side of is-operator expects a class, but got %s';
  sNotReadable = 'not readable';
  sClassPropertyAccessorMustBeStatic = 'class property accessor must be static';
  sClassPropertyAccessorMustNotBeStatic = 'class property accessor must not be static';
  sOnlyOneDefaultPropertyIsAllowed = 'Only one default property is allowed';
  sWrongNumberOfParametersForArray = 'Wrong number of parameters for array';
  sCantAssignValuesToAnAddress = 'Can''t assign values to an address';
  sIllegalExpression = 'Illegal expression';
  sCantAccessPrivateMember = 'Can''t access %s member %s';
  sMustBeInsideALoop = '%s must be inside a loop';
  sExpectXArrayElementsButFoundY = 'Expect %s array elements, but found %s';
  sCannotCreateADescendantOfTheSealedClass = 'Cannot create a descendant of the sealed class "%s"';
  sAncestorIsNotExternal = 'Ancestor "%s" is not external';
  sVirtualMethodXHasLowerVisibility = 'Virtual method "%s" has a lower visibility (%s) than parent class %s (%s)';
  sExternalClassInstanceCannotAccessStaticX = 'External class instance cannot access static %s';

type
  TResolverBaseType = (
    btNone,        // undefined
    btCustom,      // provided by descendant resolver
    btContext,     // a class or record
    btModule,
    btUntyped,     // TPasArgument without ArgType
    btChar,        // char
    btWideChar,    // widechar
    btString,      // string
    btAnsiString,  // ansistring
    btShortString, // shortstring
    btWideString,  // widestring
    btUnicodeString,// unicodestring
    btReal,        // real  platform, single or double
    btSingle,      // single  1.5E-45..3.4E38, digits 7-8, bytes 4
    btDouble,      // double  5.0E-324..1.7E308, digits 15-16, bytes 8
    btExtended,    // extended  platform, double or 1.9E-4932..1.1E4932, digits 19-20, bytes 10
    btCExtended,   // cextended
    btComp,        // comp  -2E64+1..2E63-1, digits 19-20, bytes 8
    btCurrency,    // currency  ?, bytes 8
    btBoolean,     // boolean
    btByteBool,    // bytebool  true=not zero
    btWordBool,    // wordbool  true=not zero
    btLongBool,    // longbool  true=not zero
    btQWordBool,   // qwordbool true=not zero
    btByte,        // byte  0..255
    btShortInt,    // shortint -128..127
    btWord,        // word  unsigned 2 bytes
    btSmallInt,    // smallint signed 2 bytes
    btLongWord,    // longword unsigned 4 bytes
    btCardinal,    // cardinal see longword
    btLongint,     // longint  signed 4 bytes
    btQWord,       // qword   0..18446744073709551615, bytes 8
    btInt64,       // int64   -9223372036854775808..9223372036854775807, bytes 8
    btPointer,     // pointer
    btFile,        // file
    btText,        // text
    btVariant,     // variant
    btNil,         // nil = pointer, class, procedure, method, ...
    btProc,        // TPasProcedure
    btBuiltInProc,
    btSet,         // []    see SubType
    btRange,       // a..b  see SubType
    btArray        // (a,b,...)
    );
  TResolveBaseTypes = set of TResolverBaseType;
const
  btAllInteger = [btComp,btCurrency,btByte,btShortInt,btWord,btSmallInt,
    btLongWord,btCardinal,btLongint,btQWord,btInt64];
  btAllStrings = [btString,btAnsiString,btShortString,
    btWideString,btUnicodeString];
  btAllStringAndChars = btAllStrings+[btChar,btWideChar];
  btAllFloats = [btReal,btSingle,btDouble,btExtended,btCExtended];
  btAllBooleans = [btBoolean,btByteBool,btWordBool,btLongBool,btQWordBool];
  btAllStandardTypes = [
    btChar,
    btWideChar,
    btString,
    btAnsiString,
    btShortString,
    btWideString,
    btUnicodeString,
    btReal,
    btSingle,
    btDouble,
    btExtended,
    btCExtended,
    btComp,
    btCurrency,
    btBoolean,
    btByteBool,
    btWordBool,
    btLongBool,
    btQWordBool,
    btByte,
    btShortInt,
    btWord,
    btSmallInt,
    btLongWord,
    btCardinal,
    btLongint,
    btQWord,
    btInt64,
    btPointer,
    btFile,
    btText,
    btVariant
    ];
  btArrayRangeTypes = [btBoolean,btChar,btWideChar,
      btByte,btShortInt,btWord,btSmallInt,btLongWord,btCardinal,btLongint];

  BaseTypeNames: array[TResolverBaseType] of shortstring =(
    'None',
    'Custom',
    'Context',
    'Module',
    'Untyped',
    'Char',
    'WideChar',
    'String',
    'AnsiString',
    'ShortString',
    'WideString',
    'UnicodeString',
    'Real',
    'Single',
    'Double',
    'Extended',
    'CExtended',
    'Comp',
    'Currency',
    'Boolean',
    'ByteBool',
    'WordBool',
    'LongBool',
    'QWordBool',
    'Byte',
    'ShortInt',
    'Word',
    'SmallInt',
    'LongWord',
    'Cardinal',
    'Longint',
    'QWord',
    'Int64',
    'Pointer',
    'File',
    'Text',
    'Variant',
    'Nil',
    'Procedure/Function',
    'BuiltInProc',
    'set literal',
    'range..',
    'array literal'
    );

type
  TResolverBuiltInProc = (
    bfCustom,
    bfLength,
    bfSetLength,
    bfInclude,
    bfExclude,
    bfBreak,
    bfContinue,
    bfExit,
    bfInc,
    bfDec,
    bfAssigned,
    bfChr,
    bfOrd,
    bfLow,
    bfHigh,
    bfPred,
    bfSucc,
    bfStrProc,
    bfStrFunc,
    bfConcatArray,
    bfCopyArray,
    bfInsertArray,
    bfDeleteArray
    );
  TResolverBuiltInProcs = set of TResolverBuiltInProc;
const
  ResolverBuiltInProcNames: array[TResolverBuiltInProc] of shortstring = (
    'Custom',
    'Length',
    'SetLength',
    'Include',
    'Exclude',
    'Break',
    'Continue',
    'Exit',
    'Inc',
    'Dec',
    'Assigned',
    'Chr',
    'Ord',
    'Low',
    'High',
    'Pred',
    'Succ',
    'Str',
    'Str',
    'Concat',
    'Copy',
    'Insert',
    'Delete'
    );
  bfAllStandardProcs = [Succ(bfCustom)..high(TResolverBuiltInProc)];

const
  ResolverResultVar = 'Result';

type

  { EPasResolve }

  EPasResolve = class(Exception)
  private
    FPasElement: TPasElement;
    procedure SetPasElement(AValue: TPasElement);
  public
    Id: int64;
    MsgType: TMessageType;
    MsgNumber: integer;
    MsgPattern: String;
    Args: TMessageArgs;
    destructor Destroy; override;
    property PasElement: TPasElement read FPasElement write SetPasElement;
  end;

  { TResolveData - base class for data stored in TPasElement.CustomData }

  TResolveData = Class(TPasElementBase)
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    Owner: TObject; // e.g. a TPasResolver
    Next: TResolveData; // TPasResolver uses this for its memory chain
    constructor Create; virtual;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;// Element.CustomData=Self
  end;
  TResolveDataClass = class of TResolveData;

  { TUnresolvedPendingRef }

  TUnresolvedPendingRef = class(TPasUnresolvedSymbolRef)
  public
    Element: TPasType; // TPasClassOfType or TPasPointerType
  end;

  TPasScope = class;

  TIterateScopeElement = procedure(El: TPasElement; ElScope, StartScope: TPasScope;
    Data: Pointer; var Abort: boolean) of object;

  { TPasScope -
    Elements like TPasClassType use TPasScope descendants as CustomData for
    their sub identifiers.
    TPasResolver.Scopes has a stack of TPasScope for searching identifiers.
    }

  TPasScope = Class(TResolveData)
  public
    VisibilityContext: TPasElement; // methods sets this to a TPasClassType,
      // used to check if the current context is allowed to access a
      // private/protected element
    class function IsStoredInElement: boolean; virtual;
    class function FreeOnPop: boolean; virtual;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure WriteIdentifiers(Prefix: string); virtual;
  end;
  TPasScopeClass = class of TPasScope;

  { TPasModuleScope }

  TPasModuleScope = class(TPasScope)
  public
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
  end;

  TPasIdentifierKind = (
    pikNone, // not yet initialized
    pikBaseType, // e.g. longint
    pikBuiltInProc,  // e.g. High(), SetLength()
    pikSimple, // simple vars, consts, types, enums
    pikProc // may need parameter list with round brackets
    {
    pikIndexedProperty, // may need parameter list with edged brackets
    pikGeneric, // may need parameter list with angle brackets
    pikDottedUses // namespace, needs dotted identifierss }
    );
  TPasIdentifierKinds = set of TPasIdentifierKind;

  { TPasIdentifier }

  TPasIdentifier = Class(TObject)
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    {$IFDEF VerbosePasResolver}
    Owner: TObject;
    {$ENDIF}
    Identifier: String;
    NextSameIdentifier: TPasIdentifier; // next identifier with same name
    Kind: TPasIdentifierKind;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
  end;

  { TPasIdentifierScope - elements with a list of sub identifiers }

  TPasIdentifierScope = Class(TPasScope)
  private
    FItems: TFPHashList;
    procedure InternalAdd(Item: TPasIdentifier);
    procedure OnClearItem(Item, Dummy: pointer);
    procedure OnWriteItem(Item, Dummy: pointer);
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindLocalIdentifier(const Identifier: String): TPasIdentifier; inline;
    function FindIdentifier(const Identifier: String): TPasIdentifier; virtual;
    function RemoveLocalIdentifier(El: TPasElement): boolean; virtual;
    function AddIdentifier(const Identifier: String; El: TPasElement;
      const Kind: TPasIdentifierKind): TPasIdentifier; virtual;
    function FindElement(const aName: string): TPasElement;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasDefaultScope - root scope }

  TPasDefaultScope = class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasSectionScope - e.g. interface, implementation, program, library }

  TPasSectionScope = Class(TPasIdentifierScope)
  public
    UsesList: TFPList; // list of TPasSectionScope
    constructor Create; override;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasEnumTypeScope }

  TPasEnumTypeScope = Class(TPasIdentifierScope)
  public
    CanonicalSet: TPasSetType;
    destructor Destroy; override;
  end;

  { TPasRecordScope }

  TPasRecordScope = Class(TPasIdentifierScope)
  end;

  TPasClassScopeFlag = (
    pcsfAncestorResolved,
    pcsfSealed
    );
  TPasClassScopeFlags = set of TPasClassScopeFlag;

  { TPasClassScope }

  TPasClassScope = Class(TPasIdentifierScope)
  public
    AncestorScope: TPasClassScope;
    DirectAncestor: TPasType; // TPasClassType or TPasAliasType or TPasTypeAliasType
    DefaultProperty: TPasProperty;
    Flags: TPasClassScopeFlags;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;
  TPasClassScopeClass = class of TPasClassScope;

  { TPasProcedureScope }

  TPasProcedureScope = Class(TPasIdentifierScope)
  public
    DeclarationProc: TPasProcedure; // the corresponding forward declaration
    ImplProc: TPasProcedure; // the corresponding proc with Body
    OverriddenProc: TPasProcedure; // if IsOverride then this is the ancestor proc (virtual or override)
    ClassScope: TPasClassScope;
    SelfArg: TPasArgument;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    destructor Destroy; override;
  end;

  { TPasPropertyScope }

  TPasPropertyScope = Class(TPasIdentifierScope)
  public
    AncestorProp: TPasProperty;
    destructor Destroy; override;
  end;

  { TPasExceptOnScope }

  TPasExceptOnScope = Class(TPasIdentifierScope)
  end;

  TPasWithScope = class;

  TPasWithExprScopeFlag = (
    wesfNeedTmpVar,
    wesfOnlyTypeMembers,
    wesfConstParent
    );
  TPasWithExprScopeFlags = set of TPasWithExprScopeFlag;

  { TPasWithExprScope }

  TPasWithExprScope = Class(TPasScope)
  public
    WithScope: TPasWithScope;
    Index: integer;
    Expr: TPasExpr;
    Scope: TPasScope;
    Flags: TPasWithExprScopeFlags;
    class function IsStoredInElement: boolean; override;
    class function FreeOnPop: boolean; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;
  TPasWithExprScopeClass = class of TPasWithExprScope;

  { TPasWithScope }

  TPasWithScope = Class(TPasScope)
  public
    // Element is the TPasImplWithDo
    ExpressionScopes: TObjectList; // list of TPasWithExprScope
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TPasSubScope - base class for sub scopes aka dotted scopes }

  TPasSubScope = Class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasIterateFilterData }

  TPasIterateFilterData = record
    OnIterate: TIterateScopeElement;
    Data: Pointer;
  end;
  PPasIterateFilterData = ^TPasIterateFilterData;

  { TPasModuleDotScope - scope for searching unitname.<identifier> }

  TPasModuleDotScope = Class(TPasSubScope)
  private
    FModule: TPasModule;
    procedure OnInternalIterate(El: TPasElement; ElScope, StartScope: TPasScope;
      Data: Pointer; var Abort: boolean);
    procedure SetModule(AValue: TPasModule);
  public
    InterfaceScope: TPasSectionScope;
    ImplementationScope: TPasSectionScope;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    property Module: TPasModule read FModule write SetModule;
  end;

  { TPasDotIdentifierScope }

  TPasDotIdentifierScope = Class(TPasSubScope)
  public
    IdentifierScope: TPasIdentifierScope;
    OnlyTypeMembers: boolean; // true=only class var/procs, false=default=all
    ConstParent: boolean;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasDotRecordScope - used for aRecord.subidentifier }

  TPasDotRecordScope = Class(TPasDotIdentifierScope)
  end;

  { TPasDotEnumTypeScope - used for EnumType.EnumValue }

  TPasDotEnumTypeScope = Class(TPasDotIdentifierScope)
  end;

  { TPasDotClassScope - used for aClass.subidentifier }

  TPasDotClassScope = Class(TPasDotIdentifierScope)
  private
    FClassScope: TPasClassScope;
    procedure SetClassScope(AValue: TPasClassScope);
  public
    InheritedExpr: boolean; // this is 'inherited <name>' instead of '.<name'
    property ClassScope: TPasClassScope read FClassScope write SetClassScope;
  end;

  TResolvedReferenceFlag = (
    rrfDotScope, // found reference via a dot scope (TPasDotIdentifierScope)
    rrfImplicitCallWithoutParams, // a TPrimitiveExpr is an implicit call without params
    rrfNewInstance, // constructor call (without it call constructor as normal method)
    rrfFreeInstance, // destructor call (without it call destructor as normal method)
    rrfVMT, // use VMT for call
    rrfConstInherited  // parent is const and children are too
    );
  TResolvedReferenceFlags = set of TResolvedReferenceFlag;

  { TResolvedRefContext }

  TResolvedRefContext = Class
  end;

  TResolvedRefAccess = (
    rraNone,
    rraRead,  // expression is read
    rraAssign, // expression is LHS assign
    rraReadAndAssign, // expression is LHS +=, -=, *=, /=
    rraVarParam, // expression is passed to a var parameter
    rraOutParam, // expression is passed to an out parameter
    rraParamToUnknownProc // used as param, before knowing what overladed proc to call,
      // will later be changed to rraRead, rraVarParam, rraOutParam
    );
  TPRResolveVarAccesses = set of TResolvedRefAccess;

  { TResolvedReference - CustomData for normal references }

  TResolvedReference = Class(TResolveData)
  private
    FDeclaration: TPasElement;
    procedure SetDeclaration(AValue: TPasElement);
  public
    Flags: TResolvedReferenceFlags;
    Access: TResolvedRefAccess;
    Context: TResolvedRefContext;
    WithExprScope: TPasWithExprScope;// if set, this reference used a With-block expression.
    destructor Destroy; override;
    property Declaration: TPasElement read FDeclaration write SetDeclaration;
  end;

  { TResolvedRefCtxConstructor }

  TResolvedRefCtxConstructor = Class(TResolvedRefContext)
  public
    Typ: TPasType; // e.g. TPasClassType
  end;

  TPasResolverResultFlag = (
    rrfReadable,
    rrfWritable,
    rrfAssignable,  // not writable in general, e.g. aString[1]:=
    rrfCanBeStatement
    );
  TPasResolverResultFlags = set of TPasResolverResultFlag;

  { TPasResolverResult }

  TPasResolverResult = record
    BaseType: TResolverBaseType;
    SubType: TResolverBaseType; // for btSet and btRange
    IdentEl: TPasElement; // if set then this specific identifier is the value, can be a type
    TypeEl: TPasType; // can be nil for const expression
    ExprEl: TPasExpr;
    Flags: TPasResolverResultFlags;
  end;
  PPasResolvedElement = ^TPasResolverResult;

  TPasResolverComputeFlag = (
    rcSkipTypeAlias,
    rcNoImplicitProc,    // do not call a function without params, includes rcNoImplicitProcType
    rcNoImplicitProcType, // do not call a proc type without params
    rcConstant,  // resolve a constant expresson
    rcType       // resolve a type expression
    );
  TPasResolverComputeFlags = set of TPasResolverComputeFlag;

  TResElDataBuiltInSymbol = Class(TResolveData)
  public
  end;

  { TResElDataBaseType - CustomData for compiler built-in types (TPasUnresolvedSymbolRef), e.g. longint }

  TResElDataBaseType = Class(TResElDataBuiltInSymbol)
  public
    BaseType: TResolverBaseType;
  end;
  TResElDataBaseTypeClass = class of TResElDataBaseType;

  TResElDataBuiltInProc = Class;

  TOnGetCallCompatibility = function(Proc: TResElDataBuiltInProc;
    Exp: TPasExpr; RaiseOnError: boolean): integer of object;
  TOnGetCallResult = procedure(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
    out ResolvedEl: TPasResolverResult) of object;
  TOnFinishParamsExpr = procedure(Proc: TResElDataBuiltInProc;
    Params: TParamsExpr) of object;

  TBuiltInProcFlag = (
    bipfCanBeStatement // a call is enough for a simple statement
    );
  TBuiltInProcFlags = set of TBuiltInProcFlag;

  { TResElDataBuiltInProc - TPasUnresolvedSymbolRef(aType).CustomData for compiler built-in procs like 'length' }

  TResElDataBuiltInProc = Class(TResElDataBuiltInSymbol)
  public
    Proc: TPasUnresolvedSymbolRef;
    Signature: string;
    BuiltIn: TResolverBuiltInProc;
    GetCallCompatibility: TOnGetCallCompatibility;
    GetCallResult: TOnGetCallResult;
    FinishParamsExpression: TOnFinishParamsExpr;
    Flags: TBuiltInProcFlags;
  end;

  { TPRFindData }

  TPRFindData = record
    ErrorPosEl: TPasElement;
    Found: TPasElement;
    ElScope: TPasScope; // Where Found was found
    StartScope: TPasScope; // where the searched started
  end;
  PPRFindData = ^TPRFindData;

  TPasResolverOption = (
    proFixCaseOfOverrides,  // fix Name of overriding procs to the overriden proc
    proClassPropertyNonStatic,  // class property accessor must be non static
    proPropertyAsVarParam, // allows to pass a property as a var/out argument
    proClassOfIs, // class-of supports is and as operator
    proExtClassInstanceNoTypeMembers, // class members of external class cannot be accessed by instance
    proOpenAsDynArrays // open arrays work like dyn arrays
    );
  TPasResolverOptions = set of TPasResolverOption;

  { TPasResolver }

  TPasResolver = Class(TPasTreeContainer)
  private
    type
      TResolveDataListKind = (lkBuiltIn,lkModule);
    procedure ClearResolveDataList(Kind: TResolveDataListKind);
  private
    FBaseTypes: array[TResolverBaseType] of TPasUnresolvedSymbolRef;
    FBaseTypeStringIndex: TResolverBaseType;
    FDefaultScope: TPasDefaultScope;
    FLastCreatedData: array[TResolveDataListKind] of TResolveData;
    FLastElement: TPasElement;
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgElement: TPasElement;
    FLastMsgId: int64;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FOptions: TPasResolverOptions;
    FPendingForwards: TFPList; // list of TPasElement needed to check for forward procs
    FRootElement: TPasElement;
    FScopeClass_Class: TPasClassScopeClass;
    FScopeClass_WithExpr: TPasWithExprScopeClass;
    FScopeCount: integer;
    FScopes: array of TPasScope; // stack of scopes
    FStoreSrcColumns: boolean;
    FSubScopeCount: integer;
    FSubScopes: array of TPasScope; // stack of scopes
    FTopScope: TPasScope;
    function GetBaseTypes(bt: TResolverBaseType): TPasUnresolvedSymbolRef; inline;
    function GetScopes(Index: integer): TPasScope; inline;
  protected
    const
      cIncompatible = High(integer);
      cExact = 0;
    type
      TFindCallElData = record
        Params: TParamsExpr;
        Found: TPasElement; // TPasProcedure or TPasUnresolvedSymbolRef(built in proc) or TPasType (typecast)
        ElScope, StartScope: TPasScope;
        Distance: integer; // compatibility distance
        Count: integer;
        List: TFPList; // if not nil then collect all found elements here
      end;
      PFindCallElData = ^TFindCallElData;

      TFindOverloadProcData = record
        Proc: TPasProcedure;
        Args: TFPList;        // List of TPasArgument objects
        OnlyScope: TPasScope;
        Found: TPasProcedure;
        ElScope, StartScope: TPasScope;
        FoundNonProc: TPasElement;
      end;
      PFindOverloadProcData = ^TFindOverloadProcData;

    procedure OnFindFirstElement(El: TPasElement; ElScope, StartScope: TPasScope;
      FindFirstElementData: Pointer; var Abort: boolean); virtual;
    procedure OnFindCallElements(El: TPasElement; ElScope, StartScope: TPasScope;
      FindProcsData: Pointer; var Abort: boolean); virtual;
    procedure OnFindOverloadProc(El: TPasElement; ElScope, StartScope: TPasScope;
      FindOverloadData: Pointer; var Abort: boolean); virtual;
  protected
    procedure SetCurrentParser(AValue: TPasParser); override;
    procedure CheckTopScope(ExpectedClass: TPasScopeClass);
    function AddIdentifier(Scope: TPasIdentifierScope;
      const aName: String; El: TPasElement;
      const Kind: TPasIdentifierKind): TPasIdentifier; virtual;
    procedure AddModule(El: TPasModule); virtual;
    procedure AddSection(El: TPasSection); virtual;
    procedure AddType(El: TPasType); virtual;
    procedure AddRecordType(El: TPasRecordType); virtual;
    procedure AddClassType(El: TPasClassType); virtual;
    procedure AddVariable(El: TPasVariable); virtual;
    procedure AddEnumType(El: TPasEnumType); virtual;
    procedure AddEnumValue(El: TPasEnumValue); virtual;
    procedure AddProperty(El: TPasProperty); virtual;
    procedure AddProcedure(El: TPasProcedure); virtual;
    procedure AddProcedureBody(El: TProcedureBody); virtual;
    procedure AddArgument(El: TPasArgument); virtual;
    procedure AddFunctionResult(El: TPasResultElement); virtual;
    procedure AddExceptOn(El: TPasImplExceptOn); virtual;
    procedure ResolveImplBlock(Block: TPasImplBlock); virtual;
    procedure ResolveImplElement(El: TPasImplElement); virtual;
    procedure ResolveImplCaseOf(CaseOf: TPasImplCaseOf); virtual;
    procedure ResolveImplLabelMark(Mark: TPasImplLabelMark); virtual;
    procedure ResolveImplForLoop(Loop: TPasImplForLoop); virtual;
    procedure ResolveImplWithDo(El: TPasImplWithDo); virtual;
    procedure ResolveImplAssign(El: TPasImplAssign); virtual;
    procedure ResolveImplSimple(El: TPasImplSimple); virtual;
    procedure ResolveImplRaise(El: TPasImplRaise); virtual;
    procedure ResolveExpr(El: TPasExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveStatementConditionExpr(El: TPasExpr); virtual;
    procedure ResolveNameExpr(El: TPasExpr; const aName: string; Access: TResolvedRefAccess); virtual;
    procedure ResolveInherited(El: TInheritedExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveInheritedCall(El: TBinaryExpr; Access: TResolvedRefAccess);         virtual;
    procedure ResolveBinaryExpr(El: TBinaryExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveSubIdent(El: TBinaryExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveFuncParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveArrayParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveSetParamsExpr(Params: TParamsExpr); virtual;
    procedure ResolveArrayValues(El: TArrayValues); virtual;
    procedure FinishModule(CurModule: TPasModule); virtual;
    procedure FinishUsesList; virtual;
    procedure FinishTypeSection(El: TPasDeclarations); virtual;
    procedure FinishTypeDef(El: TPasType); virtual;
    procedure FinishEnumType(El: TPasEnumType); virtual;
    procedure FinishSetType(El: TPasSetType); virtual;
    procedure FinishRangeType(El: TPasRangeType); virtual;
    procedure FinishRecordType(El: TPasRecordType); virtual;
    procedure FinishClassType(El: TPasClassType); virtual;
    procedure FinishClassOfType(El: TPasClassOfType); virtual;
    procedure FinishArrayType(El: TPasArrayType); virtual;
    procedure FinishConstDef(El: TPasConst); virtual;
    procedure FinishProcedure(aProc: TPasProcedure); virtual;
    procedure FinishProcedureType(El: TPasProcedureType); virtual;
    procedure FinishMethodDeclHeader(Proc: TPasProcedure); virtual;
    procedure FinishMethodImplHeader(ImplProc: TPasProcedure); virtual;
    procedure FinishExceptOnExpr; virtual;
    procedure FinishExceptOnStatement; virtual;
    procedure FinishDeclaration(El: TPasElement); virtual;
    procedure FinishVariable(El: TPasVariable); virtual;
    procedure FinishPropertyOfClass(PropEl: TPasProperty); virtual;
    procedure FinishArgument(El: TPasArgument); virtual;
    procedure FinishAncestors(aClass: TPasClassType); virtual;
    procedure FinishParamExpressionAccess(Expr: TPasExpr; Access: TResolvedRefAccess);
    procedure ReplaceProcScopeImplArgsWithDeclArgs(ImplProcScope: TPasProcedureScope);
    procedure CheckProcSignatureMatch(DeclProc, ImplProc: TPasProcedure);
    procedure CheckPendingForwards(El: TPasElement);
    procedure ComputeBinaryExpr(Bin: TBinaryExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeArrayParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeFuncParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeSetParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure CheckIsClass(El: TPasElement; const ResolvedEl: TPasResolverResult);
    function CheckTypeCastClassInstanceToClass(
      const FromClassRes, ToClassRes: TPasResolverResult;
      ErrorEl: TPasElement): integer; virtual;
    procedure CheckRangeExpr(Left, Right: TPasExpr;
      out LeftResolved, RightResolved: TPasResolverResult);
    procedure CheckSetElementsCompatible(Left, Right: TPasExpr;
      const LeftResolved, RightResolved: TPasResolverResult);
    function CheckIsOrdinal(const ResolvedEl: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnError: boolean): boolean;
    procedure ConvertRangeToFirstValue(var ResolvedEl: TPasResolverResult);
    function IsCharLiteral(const Value: string): boolean; virtual;
    function CheckBuiltInMinParamCount(Proc: TResElDataBuiltInProc; Expr: TPasExpr;
      MinCount: integer; RaiseOnError: boolean): boolean;
    function CheckBuiltInMaxParamCount(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
      MaxCount: integer; RaiseOnError: boolean): integer;
    function CheckRaiseTypeArgNo(id: int64; ArgNo: integer; Param: TPasExpr;
      const ParamResolved: TPasResolverResult; Expected: string; RaiseOnError: boolean): integer;
    // custom types (added by descendant resolvers)
    function CheckAssignCompatibilityCustom(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean; var Handled: boolean): integer; virtual;
    function CheckEqualCompatibilityCustomType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer; virtual;
  protected
    // built-in functions
    function BI_Length_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Length_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_SetLength_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_SetLength_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_InExclude_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_InExclude_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Break_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function BI_Continue_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function BI_Exit_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function BI_IncDec_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_IncDec_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Assigned_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Assigned_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_Chr_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Chr_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_Ord_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Ord_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_LowHigh_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_LowHigh_OnGetCallResult(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_PredSucc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_PredSucc_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_Str_CheckParam(IsFunc: boolean; Param: TPasExpr;
      const ParamResolved: TPasResolverResult; ArgNo: integer;
      RaiseOnError: boolean): integer;
    function BI_StrProc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_StrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_StrFunc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_StrFunc_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_ConcatArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_ConcatArray_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_CopyArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_CopyArray_OnGetCallResult({%H-}Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_InsertArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_InsertArray_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_DeleteArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_DeleteArray_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual; // does not free built-in identifiers
    // overrides of TPasTreeContainer
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      overload; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASrcPos: TPasSourcePos): TPasElement;
      overload; override;
    function FindElement(const aName: String): TPasElement; override; // used by TPasParser
    function FindElementWithoutParams(const AName: String; ErrorPosEl: TPasElement;
      NoProcsWithArgs: boolean): TPasElement;
    function FindElementWithoutParams(const AName: String; out Data: TPRFindData;
      ErrorPosEl: TPasElement; NoProcsWithArgs: boolean): TPasElement;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure CheckFoundElement(const FindData: TPRFindData;
      Ref: TResolvedReference); virtual;
    function GetVisibilityContext: TPasElement;
    procedure FinishScope(ScopeType: TPasScopeType; El: TPasElement); override;
    // built in types and functions
    procedure ClearBuiltInIdentifiers; virtual;
    procedure AddObjFPCBuiltInIdentifiers(
      const TheBaseTypes: TResolveBaseTypes = btAllStandardTypes;
      const TheBaseProcs: TResolverBuiltInProcs = bfAllStandardProcs); virtual;
    function AddBaseType(const aName: string; Typ: TResolverBaseType): TResElDataBaseType;
    function AddCustomBaseType(const aName: string; aClass: TResElDataBaseTypeClass): TPasUnresolvedSymbolRef;
    function IsBaseType(aType: TPasType; BaseType: TResolverBaseType): boolean;
    function AddBuiltInProc(const aName: string; Signature: string;
      const GetCallCompatibility: TOnGetCallCompatibility;
      const GetCallResult: TOnGetCallResult;
      const FinishParamsExpr: TOnFinishParamsExpr = nil;
      const BuiltIn: TResolverBuiltInProc = bfCustom;
      const Flags: TBuiltInProcFlags = []): TResElDataBuiltInProc;
    // add extra TResolveData (E.CustomData) to free list
    procedure AddResolveData(El: TPasElement; Data: TResolveData;
      Kind: TResolveDataListKind);
    function CreateReference(DeclEl, RefEl: TPasElement;
      Access: TResolvedRefAccess;
      FindData: PPRFindData = nil): TResolvedReference; virtual;
    // scopes
    function CreateScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; virtual;
    procedure PopScope;
    procedure PushScope(Scope: TPasScope); overload;
    function PushScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; overload;
    function PushModuleDotScope(aModule: TPasModule): TPasModuleDotScope;
    function PushClassDotScope(var CurClassType: TPasClassType): TPasDotClassScope;
    function PushRecordDotScope(CurRecordType: TPasRecordType): TPasDotRecordScope;
    function PushEnumDotScope(CurEnumType: TPasEnumType): TPasDotEnumTypeScope;
    procedure ResetSubScopes(out Depth: integer);
    procedure RestoreSubScopes(Depth: integer);
    // log and messages
    class procedure UnmangleSourceLineNumber(LineNumber: integer;
      out Line, Column: integer);
    class function GetElementSourcePosStr(El: TPasElement): string;
    procedure SetLastMsg(const id: int64; MsgType: TMessageType; MsgNumber: integer;
      Const Fmt : String; Args : Array of const; Element: TPasElement);
    procedure LogMsg(const id: int64; MsgType: TMessageType; MsgNumber: integer;
      const Fmt: String; Args: Array of const; PosEl: TPasElement); overload;
    procedure RaiseMsg(const Id: int64; MsgNumber: integer; const Fmt: String;
      Args: Array of const; ErrorPosEl: TPasElement);
    procedure RaiseNotYetImplemented(id: int64; El: TPasElement; Msg: string = ''); virtual;
    procedure RaiseInternalError(id: int64; const Msg: string = '');
    procedure RaiseInvalidScopeForElement(id: int64; El: TPasElement; const Msg: string = '');
    procedure RaiseIdentifierNotFound(id: int64; Identifier: string; El: TPasElement);
    procedure RaiseXExpectedButYFound(id: int64; const X,Y: string; El: TPasElement);
    procedure RaiseConstantExprExp(id: int64; ErrorEl: TPasElement);
    procedure RaiseIncompatibleTypeDesc(id: int64; MsgNumber: integer;
      const Args: array of const; const DescA,DescB: String; ErrorEl: TPasElement);
    procedure RaiseIncompatibleType(id: int64; MsgNumber: integer;
      const Args: array of const; TypeA, TypeB: TPasType; ErrorEl: TPasElement);
    procedure RaiseIncompatibleTypeRes(id: int64; MsgNumber: integer;
      const Args: array of const; const TypeA, TypeB: TPasResolverResult;
      ErrorEl: TPasElement);
    procedure WriteScopes;
    // find value and type of an element
    procedure ComputeElement(El: TPasElement; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil);
    // checking compatibilility
    function IsSameType(TypeA, TypeB: TPasType): boolean; // check if it is exactly the same
    function CheckCallProcCompatibility(ProcType: TPasProcedureType;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckCallPropertyCompatibility(PropEl: TPasProperty;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckCallArrayCompatibility(ArrayEl: TPasArrayType;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckParamCompatibility(Expr: TPasExpr; Param: TPasArgument;
      ParamNo: integer; RaiseOnError: boolean): integer;
    function CheckAssignCompatibilityUserType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckAssignCompatibilityArrayType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckConstArrayCompatibility(Params: TParamsExpr;
      const ArrayResolved: TPasResolverResult; RaiseOnError: boolean;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil): integer;
    function CheckEqualCompatibilityUserType(
      const TypeA, TypeB: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckTypeCast(El: TPasType; Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckTypeCastRes(const FromResolved, ToResolved: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnError: boolean): integer; virtual;
    function CheckTypeCastArray(FromType, ToType: TPasArrayType;
      ErrorEl: TPasElement; RaiseOnError: boolean): integer;
    function CheckSrcIsADstType(
      const ResolvedSrcType, ResolvedDestType: TPasResolverResult;
      ErrorEl: TPasElement): integer;
    function CheckClassIsClass(SrcType, DestType: TPasType;
      ErrorEl: TPasElement): integer; virtual;
    function CheckClassesAreRelated(TypeA, TypeB: TPasType;
      ErrorEl: TPasElement): integer;
    function CheckOverloadProcCompatibility(Proc1, Proc2: TPasProcedure): boolean;
    function CheckProcAssignCompatibility(Proc1, Proc2: TPasProcedureType): boolean;
    function CheckProcArgCompatibility(Arg1, Arg2: TPasArgument): boolean;
    function CheckProcArgTypeCompatibility(Arg1, Arg2: TPasType): boolean;
    function CheckCanBeLHS(const ResolvedEl: TPasResolverResult;
      ErrorOnFalse: boolean; ErrorEl: TPasElement): boolean;
    function CheckAssignCompatibility(const LHS, RHS: TPasElement;
      RaiseOnIncompatible: boolean = true): integer;
    function CheckAssignResCompatibility(const LHS, RHS: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
    function CheckEqualElCompatibility(Left, Right: TPasElement;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
    function CheckEqualResCompatibility(const LHS, RHS: TPasResolverResult;
      LErrorEl: TPasElement; RaiseOnIncompatible: boolean;
      RErrorEl: TPasElement = nil): integer;
    function ResolvedElHasValue(const ResolvedEl: TPasResolverResult): boolean;
    function ResolvedElCanBeVarParam(const ResolvedEl: TPasResolverResult): boolean;
    function ResolvedElIsClassInstance(const ResolvedEl: TPasResolverResult): boolean;
    // uility functions
    function GetPasPropertyType(El: TPasProperty): TPasType;
    function GetPasPropertyAncestor(El: TPasProperty): TPasProperty;
    function GetPasPropertyGetter(El: TPasProperty): TPasElement;
    function GetPasPropertySetter(El: TPasProperty): TPasElement;
    function GetPasClassAncestor(ClassEl: TPasClassType; SkipAlias: boolean): TPasType;
    function GetLoop(El: TPasElement): TPasImplElement;
    function ResolveAliasType(aType: TPasType): TPasType;
    function ExprIsAddrTarget(El: TPasExpr): boolean;
    function GetLastExprIdentifier(El: TPasExpr): TPasExpr;
    function ParentNeedsExprResult(El: TPasExpr): boolean;
    function GetReference_NewInstanceClass(Ref: TResolvedReference): TPasClassType;
    function IsDynArray(TypeEl: TPasType): boolean;
    function IsOpenArray(TypeEl: TPasType): boolean;
    function IsDynOrOpenArray(TypeEl: TPasType): boolean;
    function IsClassMethod(El: TPasElement): boolean;
    function IsProcedureType(const ResolvedEl: TPasResolverResult): boolean;
    function IsArrayType(const ResolvedEl: TPasResolverResult): boolean;
    function IsTypeCast(Params: TParamsExpr): boolean;
    function ProcNeedsParams(El: TPasProcedureType): boolean;
    function GetRangeLength(RangeResolved: TPasResolverResult): integer;
  public
    property BaseTypes[bt: TResolverBaseType]: TPasUnresolvedSymbolRef read GetBaseTypes;
    property BaseTypeStringIndex: TResolverBaseType read FBaseTypeStringIndex write FBaseTypeStringIndex;
    property LastElement: TPasElement read FLastElement;
    property StoreSrcColumns: boolean read FStoreSrcColumns write FStoreSrcColumns; {
       If true Line and Column is mangled together in TPasElement.SourceLineNumber.
       Use method UnmangleSourceLineNumber to extract. }
    property Scopes[Index: integer]: TPasScope read GetScopes;
    property ScopeCount: integer read FScopeCount;
    property TopScope: TPasScope read FTopScope;
    property RootElement: TPasElement read FRootElement;
    property DefaultScope: TPasDefaultScope read FDefaultScope write FDefaultScope;
    property LastMsg: string read FLastMsg write FLastMsg;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
    property LastMsgElement: TPasElement read FLastMsgElement write FLastMsgElement;
    property LastMsgId: int64 read FLastMsgId write FLastMsgId;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property Options: TPasResolverOptions read FOptions write FOptions;
    property ScopeClass_Class: TPasClassScopeClass read FScopeClass_Class write FScopeClass_Class;
    property ScopeClass_WithExpr: TPasWithExprScopeClass read FScopeClass_WithExpr write FScopeClass_WithExpr;
  end;

function GetObjName(o: TObject): string;
function GetProcDesc(ProcType: TPasProcedureType; UseName: boolean = true; AddPaths: boolean = false): string;
function GetTypeDesc(aType: TPasType; AddPath: boolean = false): string;
function GetTreeDesc(El: TPasElement; Indent: integer = 0): string;
function GetResolverResultDesc(const T: TPasResolverResult): string;
function GetResolverResultDescription(const T: TPasResolverResult; OnlyType: boolean = false): string;
function ResolverResultFlagsToStr(const Flags: TPasResolverResultFlags): string;
procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement;
  TypeEl: TPasType; Flags: TPasResolverResultFlags); overload;
procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; TypeEl: TPasType;
  Flags: TPasResolverResultFlags); overload;
procedure SetResolverValueExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; TypeEl: TPasType; ExprEl: TPasExpr;
  Flags: TPasResolverResultFlags); overload;
function ProcNeedsImplProc(Proc: TPasProcedure): boolean;
function dbgs(const Flags: TPasResolverComputeFlags): string; overload;
function dbgs(const a: TResolvedRefAccess): string;
function dbgs(const Flags: TResolvedReferenceFlags): string; overload;

implementation

function GetObjName(o: TObject): string;
begin
  if o=nil then
    Result:='nil'
  else if o is TPasElement then
    Result:=TPasElement(o).Name+':'+o.ClassName
  else
    Result:=o.ClassName;
end;

function GetProcDesc(ProcType: TPasProcedureType; UseName: boolean;
  AddPaths: boolean): string;
var
  Args: TFPList;
  i: Integer;
  Arg: TPasArgument;
begin
  if ProcType=nil then exit('nil');
  Result:=ProcType.TypeName;
  if UseName and (ProcType.Parent is TPasProcedure) then
    begin
    if AddPaths then
      Result:=Result+' '+ProcType.Parent.FullName
    else
      Result:=Result+' '+ProcType.Parent.Name;
    end;
  Args:=ProcType.Args;
  if Args.Count>0 then
    begin
    Result:=Result+'(';
    for i:=0 to Args.Count-1 do
      begin
      if i>0 then Result:=Result+';';
      Arg:=TPasArgument(Args[i]);
      if AccessNames[Arg.Access]<>'' then
        Result:=Result+AccessNames[Arg.Access];
      if Arg.ArgType=nil then
        Result:=Result+'untyped'
      else
        Result:=Result+GetTypeDesc(Arg.ArgType,AddPaths);
      end;
    Result:=Result+')';
    end;
  if ProcType.IsOfObject then
    Result:=Result+' of object';
  if ProcType.IsNested then
    Result:=Result+' is nested';
  if cCallingConventions[ProcType.CallingConvention]<>'' then
    Result:=Result+';'+cCallingConventions[ProcType.CallingConvention];
end;

function GetTypeDesc(aType: TPasType; AddPath: boolean): string;

  function GetName: string;
  var
    s: String;
  begin
    Result:=aType.Name;
    if AddPath then
      begin
      s:=aType.FullPath;
      if (s<>'') and (s<>'.') then
        Result:=s+'.'+Result;
      end;
  end;

var
  C: TClass;
begin
  if aType=nil then exit('nil');
  C:=aType.ClassType;
  if (C=TPasUnresolvedSymbolRef) then
    begin
    Result:=GetName;
    if TPasUnresolvedSymbolRef(aType).CustomData is TResElDataBuiltInProc then
      Result:=Result+'()';
    exit;
    end
  else if (C=TPasUnresolvedTypeRef) then
    Result:=GetName
  else
    Result:=GetName;
end;

function GetTreeDesc(El: TPasElement; Indent: integer): string;

  procedure LineBreak(SubIndent: integer);
  begin
    Inc(Indent,SubIndent);
    Result:=Result+LineEnding+Space(Indent);
  end;

var
  i, l: Integer;
begin
  if El=nil then exit('nil');
  Result:=El.Name+':'+El.ClassName+'=';
  if El is TPasExpr then
    begin
    if El.ClassType<>TBinaryExpr then
      Result:=Result+OpcodeStrings[TPasExpr(El).OpCode];
    if El.ClassType=TUnaryExpr then
      Result:=Result+GetTreeDesc(TUnaryExpr(El).Operand,Indent)
    else if El.ClassType=TBinaryExpr then
      Result:=Result+GetTreeDesc(TBinaryExpr(El).left,Indent)
         +OpcodeStrings[TPasExpr(El).OpCode]
         +GetTreeDesc(TBinaryExpr(El).right,Indent)
    else if El.ClassType=TPrimitiveExpr then
      Result:=Result+TPrimitiveExpr(El).Value
    else if El.ClassType=TBoolConstExpr then
      Result:=Result+BoolToStr(TBoolConstExpr(El).Value,'true','false')
    else if El.ClassType=TNilExpr then
      Result:=Result+'nil'
    else if El.ClassType=TInheritedExpr then
      Result:=Result+'inherited'
    else if El.ClassType=TSelfExpr then
      Result:=Result+'Self'
    else if El.ClassType=TParamsExpr then
      begin
      LineBreak(2);
      Result:=Result+GetTreeDesc(TParamsExpr(El).Value,Indent)+'(';
      l:=length(TParamsExpr(El).Params);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+GetTreeDesc(TParamsExpr(El).Params[i],Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+')';
      end
    else if El.ClassType=TRecordValues then
      begin
      Result:=Result+'(';
      l:=length(TRecordValues(El).Fields);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+TRecordValues(El).Fields[i].Name+':'
            +GetTreeDesc(TRecordValues(El).Fields[i].ValueExp,Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+')';
      end
    else if El.ClassType=TArrayValues then
      begin
      Result:=Result+'[';
      l:=length(TArrayValues(El).Values);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+GetTreeDesc(TArrayValues(El).Values[i],Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+']';
      end;
    end
  else if El is TPasProcedure then
    begin
    Result:=Result+GetTreeDesc(TPasProcedure(El).ProcType,Indent);
    end
  else if El is TPasProcedureType then
    begin
    Result:=Result+'(';
    l:=TPasProcedureType(El).Args.Count;
    if l>0 then
      begin
      inc(Indent,2);
      for i:=0 to l-1 do
        begin
        LineBreak(0);
        Result:=Result+GetTreeDesc(TPasArgument(TPasProcedureType(El).Args[i]),Indent);
        if i<l-1 then
          Result:=Result+';'
        end;
      dec(Indent,2);
      end;
    Result:=Result+')';
    if El is TPasFunction then
      Result:=Result+':'+GetTreeDesc(TPasFunctionType(TPasFunction(El).ProcType).ResultEl,Indent);
    if TPasProcedureType(El).IsOfObject then
      Result:=Result+' of object';
    if TPasProcedureType(El).IsNested then
      Result:=Result+' of nested';
    if cCallingConventions[TPasProcedureType(El).CallingConvention]<>'' then
      Result:=Result+'; '+cCallingConventions[TPasProcedureType(El).CallingConvention];
    end
  else if El.ClassType=TPasResultElement then
    Result:=Result+GetTreeDesc(TPasResultElement(El).ResultType,Indent)
  else if El.ClassType=TPasArgument then
    begin
    if AccessNames[TPasArgument(El).Access]<>'' then
      Result:=Result+AccessNames[TPasArgument(El).Access];
    if TPasArgument(El).ArgType=nil then
      Result:=Result+'untyped'
    else
      Result:=Result+GetTreeDesc(TPasArgument(El).ArgType,Indent);
    end
  else if El.ClassType=TPasUnresolvedSymbolRef then
    begin
    if TPasUnresolvedSymbolRef(El).CustomData is TResElDataBuiltInProc then
      Result:=Result+TResElDataBuiltInProc(TPasUnresolvedSymbolRef(El).CustomData).Signature;
    end;
end;

function GetResolverResultDesc(const T: TPasResolverResult): string;
begin
  if (T.BaseType=btCustom) and (T.TypeEl<>nil) and (T.TypeEl.Name<>'') then
    Result:=T.TypeEl.Name
  else
    Result:=BaseTypeNames[T.BaseType];
  if T.BaseType in [btSet,btRange,btArray] then
    begin
    if (T.SubType=btCustom) and (T.TypeEl<>nil) and (T.TypeEl.Name<>'') then
      Result:=Result+' of '+GetTypeDesc(T.TypeEl,true)
    else
      Result:=Result+' of '+BaseTypeNames[T.SubType];
    end;
  if T.IdentEl<>nil then
    begin
    // named element
    if T.IdentEl=T.TypeEl then
      Result:=Result+',type '+GetTypeDesc(T.TypeEl,true)
    else if T.IdentEl.Name<>'' then
      Result:=Result+','+T.IdentEl.Name+'/'+T.IdentEl.ClassName+':'+GetTypeDesc(T.TypeEl,true)
    else
      Result:=Result+','+T.IdentEl.ElementTypeName+'/'+T.IdentEl.ClassName+':'+GetTypeDesc(T.TypeEl,true);
    if T.ExprEl<>nil then
      Result:=Result+'='+GetTreeDesc(T.ExprEl);
    end
  else if T.TypeEl<>nil then
    begin
    // anonymous constant expression with named type
    Result:=Result+',const '+GetTreeDesc(T.TypeEl);
    if T.ExprEl<>nil then
      Result:=Result+'='+GetTreeDesc(T.ExprEl);
    end
  else
    begin
    // anonymous const expr without explicit type, e.g. 123.4
    Result:=Result+',const '+GetTreeDesc(T.ExprEl);
    end;
end;

function GetResolverResultDescription(const T: TPasResolverResult;
  OnlyType: boolean): string;

  function GetSubTypeName: string;
  begin
    if (T.TypeEl<>nil) and (T.TypeEl.Name<>'') then
      Result:=T.TypeEl.Name
    else
      Result:=BaseTypeNames[T.SubType];
  end;

var
  ArrayEl: TPasArrayType;
begin
  case T.BaseType of
  btModule: exit(T.IdentEl.ElementTypeName+' '+T.IdentEl.Name);
  btNil: exit('nil');
  btRange:
    Result:='range of '+GetSubTypeName;
  btSet:
    Result:='set literal of '+GetSubTypeName;
  btArray:
    Result:='array literal of '+GetSubTypeName;
  btContext:
    begin
    if T.TypeEl.ClassType=TPasClassOfType then
      Result:='class of '+TPasClassOfType(T.TypeEl).DestType.Name
    else if T.TypeEl.ClassType=TPasAliasType then
      Result:=TPasAliasType(T.TypeEl).DestType.Name
    else if T.TypeEl.ClassType=TPasTypeAliasType then
      Result:='type '+TPasAliasType(T.TypeEl).DestType.Name
    else if T.TypeEl.ClassType=TPasArrayType then
      begin
      ArrayEl:=TPasArrayType(T.TypeEl);
      if length(ArrayEl.Ranges)=0 then
        Result:='array of '+ArrayEl.ElType.Name
      else
        Result:='array[] of '+ArrayEl.ElType.Name;
      end
    else if T.TypeEl is TPasProcedureType then
      Result:=GetProcDesc(TPasProcedureType(T.TypeEl),false)
    else if T.TypeEl.Name<>'' then
      Result:=T.TypeEl.Name
    else
      Result:=T.TypeEl.ElementTypeName;
    end;
  btCustom:
    Result:=T.TypeEl.Name;
  else
    Result:=BaseTypeNames[T.BaseType];
  end;
  if (not OnlyType) and (T.TypeEl<>T.IdentEl) and (T.IdentEl<>nil) then
    Result:=T.IdentEl.Name+':'+Result;
end;

function ResolverResultFlagsToStr(const Flags: TPasResolverResultFlags): string;
var
  f: TPasResolverResultFlag;
  s: string;
begin
  Result:='';
  for f in Flags do
    begin
    if Result<>'' then Result:=Result+',';
    str(f,s);
    Result:=Result+s;
    end;
  Result:='['+Result+']';
end;

procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement; TypeEl: TPasType;
  Flags: TPasResolverResultFlags);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=IdentEl;
  ResolvedType.TypeEl:=TypeEl;
  ResolvedType.ExprEl:=nil;
  ResolvedType.Flags:=Flags;
end;

procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; TypeEl: TPasType; Flags: TPasResolverResultFlags
  );
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=nil;
  ResolvedType.TypeEl:=TypeEl;
  ResolvedType.ExprEl:=nil;
  ResolvedType.Flags:=Flags;
end;

procedure SetResolverValueExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; TypeEl: TPasType; ExprEl: TPasExpr;
  Flags: TPasResolverResultFlags);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=nil;
  ResolvedType.TypeEl:=TypeEl;
  ResolvedType.ExprEl:=ExprEl;
  ResolvedType.Flags:=Flags;
end;

function ProcNeedsImplProc(Proc: TPasProcedure): boolean;
begin
  Result:=true;
  if Proc.IsExternal then exit(false);
  if Proc.IsForward then exit;
  if Proc.Parent.ClassType=TInterfaceSection then exit;
  if Proc.Parent.ClassType=TPasClassType then
    begin
    // a method declaration
    if not Proc.IsAbstract then exit;
    end;
  Result:=false;
end;

function dbgs(const Flags: TPasResolverComputeFlags): string;
var
  s: string;
  f: TPasResolverComputeFlag;
begin
  Result:='';
  for f in Flags do
    if f in Flags then
      begin
      if Result<>'' then Result:=Result+',';
      str(f,s);
      Result:=Result+s;
      end;
  Result:='['+Result+']';
end;

function dbgs(const a: TResolvedRefAccess): string;
begin
  str(a,Result);
end;

function dbgs(const Flags: TResolvedReferenceFlags): string;
var
  s: string;
  f: TResolvedReferenceFlag;
begin
  Result:='';
  for f in Flags do
    if f in Flags then
      begin
      if Result<>'' then Result:=Result+',';
      str(f,s);
      Result:=Result+s;
      end;
  Result:='['+Result+']';
end;

{ TPasPropertyScope }

destructor TPasPropertyScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasPropertyScope.Destroy START ',ClassName);
  {$ENDIF}
  ReleaseAndNil(TPasElement(AncestorProp));
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasPropertyScope.Destroy END',ClassName);
  {$ENDIF}
end;

{ TPasEnumTypeScope }

destructor TPasEnumTypeScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasEnumTypeScope.Destroy START ',ClassName);
  {$ENDIF}
  ReleaseAndNil(TPasElement(CanonicalSet));
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasEnumTypeScope.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasDotIdentifierScope }

function TPasDotIdentifierScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=IdentifierScope.FindIdentifier(Identifier);
end;

procedure TPasDotIdentifierScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  IdentifierScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasDotIdentifierScope.WriteIdentifiers(Prefix: string);
begin
  IdentifierScope.WriteIdentifiers(Prefix);
end;

{ TPasWithExprScope }

class function TPasWithExprScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

class function TPasWithExprScope.FreeOnPop: boolean;
begin
  Result:=false;
end;

procedure TPasWithExprScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  Scope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasWithExprScope.WriteIdentifiers(Prefix: string);
begin
  writeln(Prefix+'WithExpr: '+GetTreeDesc(Expr,length(Prefix)));
  Scope.WriteIdentifiers(Prefix);
end;

{ TPasWithScope }

constructor TPasWithScope.Create;
begin
  inherited Create;
  ExpressionScopes:=TObjectList.Create(true);
end;

destructor TPasWithScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasWithScope.Destroy START ',ClassName);
  {$ENDIF}
  FreeAndNil(ExpressionScopes);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasWithScope.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasProcedureScope }

function TPasProcedureScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then exit;
  if ClassScope<>nil then
    Result:=ClassScope.FindIdentifier(Identifier);
end;

procedure TPasProcedureScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  if ClassScope<>nil then
    ClassScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasProcedureScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  if ClassScope<>nil then
    ClassScope.WriteIdentifiers(Prefix+'  ');
end;

destructor TPasProcedureScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcedureScope.Destroy START ',ClassName);
  {$ENDIF}
  inherited Destroy;
  ReleaseAndNil(TPasElement(SelfArg));
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasProcedureScope.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasClassScope }

function TPasClassScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then exit;
  if AncestorScope<>nil then
    Result:=AncestorScope.FindIdentifier(Identifier);
end;

procedure TPasClassScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  if AncestorScope<>nil then
    AncestorScope.IterateElements(aName,StartScope,OnIterateElement,Data,Abort);
end;

procedure TPasClassScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  if AncestorScope<>nil then
    AncestorScope.WriteIdentifiers(Prefix+'  ');
end;

{ TPasDotClassScope }

procedure TPasDotClassScope.SetClassScope(AValue: TPasClassScope);
begin
  if FClassScope=AValue then Exit;
  FClassScope:=AValue;
  IdentifierScope:=AValue;
end;

{ TPasIdentifier }

procedure TPasIdentifier.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if Element<>nil then
    Element.Release;
  FElement:=AValue;
  if Element<>nil then
    Element.AddRef;
end;

destructor TPasIdentifier.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifier.Destroy START ',ClassName,' "',Identifier,'"');
  {$ENDIF}
  Element:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifier.Destroy END ',ClassName);
  {$ENDIF}
end;

{ EPasResolve }

procedure EPasResolve.SetPasElement(AValue: TPasElement);
begin
  if FPasElement=AValue then Exit;
  if PasElement<>nil then
    PasElement.Release;
  FPasElement:=AValue;
  if PasElement<>nil then
    PasElement.AddRef;
end;

destructor EPasResolve.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('EPasResolve.Destroy START ',ClassName);
  {$ENDIF}
  PasElement:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('EPasResolve.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TResolvedReference }

procedure TResolvedReference.SetDeclaration(AValue: TPasElement);
begin
  if FDeclaration=AValue then Exit;
  if Declaration<>nil then
    Declaration.Release;
  FDeclaration:=AValue;
  if Declaration<>nil then
    Declaration.AddRef;
end;

destructor TResolvedReference.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolvedReference.Destroy START ',ClassName);
  {$ENDIF}
  Declaration:=nil;
  FreeAndNil(Context);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolvedReference.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasSubScope }

class function TPasSubScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TPasModuleDotScope }

procedure TPasModuleDotScope.OnInternalIterate(El: TPasElement; ElScope,
  StartScope: TPasScope; Data: Pointer; var Abort: boolean);
var
  FilterData: PPasIterateFilterData absolute Data;
begin
  if El.ClassType=TPasModule then
    exit; // skip used units
  // call the original iterator
  FilterData^.OnIterate(El,ElScope,StartScope,FilterData^.Data,Abort);
end;

procedure TPasModuleDotScope.SetModule(AValue: TPasModule);
begin
  if FModule=AValue then Exit;
  if Module<>nil then
    Module.Release;
  FModule:=AValue;
  if Module<>nil then
    Module.AddRef;
end;

destructor TPasModuleDotScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSubModuleScope.Destroy START ',ClassName);
  {$ENDIF}
  Module:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSubModuleScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasModuleDotScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  if ImplementationScope<>nil then
    begin
    Result:=ImplementationScope.FindLocalIdentifier(Identifier);
    if (Result<>nil) and (Result.Element.ClassType<>TPasModule) then
      exit;
    end;
  if InterfaceScope<>nil then
    Result:=InterfaceScope.FindLocalIdentifier(Identifier)
  else
    Result:=nil;
end;

procedure TPasModuleDotScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  FilterData: TPasIterateFilterData;
begin
  FilterData.OnIterate:=OnIterateElement;
  FilterData.Data:=Data;
  if ImplementationScope<>nil then
    begin
    ImplementationScope.IterateElements(aName,StartScope,@OnInternalIterate,@FilterData,Abort);
    if Abort then exit;
    end;
  if InterfaceScope<>nil then
    InterfaceScope.IterateElements(aName,StartScope,@OnInternalIterate,@FilterData,Abort);
end;

procedure TPasModuleDotScope.WriteIdentifiers(Prefix: string);
begin
  if ImplementationScope<>nil then
    ImplementationScope.WriteIdentifiers(Prefix+'  ');
  if InterfaceScope<>nil then
    InterfaceScope.WriteIdentifiers(Prefix+'  ');
end;

{ TPasSectionScope }

constructor TPasSectionScope.Create;
begin
  inherited Create;
  UsesList:=TFPList.Create;
end;

destructor TPasSectionScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSectionScope.Destroy START ',ClassName);
  {$ENDIF}
  FreeAndNil(UsesList);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasSectionScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasSectionScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then
    exit;
  for i:=0 to UsesList.Count-1 do
    begin
    UsesScope:=TPasIdentifierScope(UsesList[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasSectionScope.FindIdentifier "',Identifier,'" in used unit ',GetObjName(UsesScope.Element));
    {$ENDIF}
    Result:=UsesScope.FindIdentifier(Identifier);
    if Result<>nil then exit;
    end;
end;

procedure TPasSectionScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
begin
  inherited IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
  if Abort then exit;
  for i:=0 to UsesList.Count-1 do
    begin
    UsesScope:=TPasIdentifierScope(UsesList[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasSectionScope.IterateElements "',aName,'" in used unit ',GetObjName(UsesScope.Element));
    {$ENDIF}
    UsesScope.IterateElements(aName,StartScope,OnIterateElement,Data,Abort);
    if Abort then exit;
    end;
end;

procedure TPasSectionScope.WriteIdentifiers(Prefix: string);
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
begin
  inherited WriteIdentifiers(Prefix);
  for i:=0 to UsesList.Count-1 do
    begin
    UsesScope:=TPasIdentifierScope(UsesList[i]);
    writeln(Prefix+'Uses: '+GetObjName(UsesScope.Element));
    end;
end;

{ TPasModuleScope }

procedure TPasModuleScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  if CompareText(aName,Element.Name)<>0 then exit;
  OnIterateElement(Element,Self,StartScope,Data,Abort);
end;

{ TPasDefaultScope }

class function TPasDefaultScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TResolveData }

procedure TResolveData.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if Element<>nil then
    Element.Release;
  FElement:=AValue;
  if Element<>nil then
    Element.AddRef;
end;

constructor TResolveData.Create;
begin

end;

destructor TResolveData.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolveData.Destroy START ',ClassName);
  {$ENDIF}
  Element:=nil;
  Owner:=nil;
  Next:=nil;
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TResolveData.Destroy END ',ClassName);
  {$ENDIF}
end;

{ TPasScope }

class function TPasScope.IsStoredInElement: boolean;
begin
  Result:=true;
end;

class function TPasScope.FreeOnPop: boolean;
begin
  Result:=not IsStoredInElement;
end;

procedure TPasScope.IterateElements(const aName: string; StartScope: TPasScope;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
begin
  if aName='' then ;
  if StartScope=nil then ;
  if Data=nil then ;
  if OnIterateElement=nil then ;
  if Abort then ;
end;

procedure TPasScope.WriteIdentifiers(Prefix: string);
begin
  writeln(Prefix,'Element: ',GetObjName(Element));
end;

{ TPasIdentifierScope }

// inline
function TPasIdentifierScope.FindLocalIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  LoName: String;
begin
  LoName:=lowercase(Identifier);
  Result:=TPasIdentifier(FItems.Find(LoName));
end;

procedure TPasIdentifierScope.OnClearItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Ident: TPasIdentifier;
begin
  if Dummy=nil then ;
  //writeln('TPasIdentifierScope.OnClearItem ',PasIdentifier.Identifier+':'+PasIdentifier.ClassName);
  while PasIdentifier<>nil do
    begin
    Ident:=PasIdentifier;
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    Ident.Free;
    end;
end;

procedure TPasIdentifierScope.OnWriteItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Prefix: String;
begin
  Prefix:=AnsiString(Dummy);
  while PasIdentifier<>nil do
    begin
    writeln(Prefix,'Identifier="',PasIdentifier.Identifier,'" Element=',GetObjName(PasIdentifier.Element));
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    end;
end;

procedure TPasIdentifierScope.InternalAdd(Item: TPasIdentifier);
var
  Index: Integer;
  OldItem: TPasIdentifier;
  LoName: ShortString;
begin
  LoName:=lowercase(Item.Identifier);
  Index:=FItems.FindIndexOf(LoName);
  {$IFDEF VerbosePasResolver}
  if Item.Owner<>nil then
    raise Exception.Create('20160925184110');
  Item.Owner:=Self;
  {$ENDIF}
  //writeln('  Index=',Index);
  if Index>=0 then
    begin
    // insert LIFO - last in, first out
    OldItem:=TPasIdentifier(FItems.List^[Index].Data);
    {$IFDEF VerbosePasResolver}
    if lowercase(OldItem.Identifier)<>LoName then
      raise Exception.Create('20160925183438');
    {$ENDIF}
    Item.NextSameIdentifier:=OldItem;
    FItems.List^[Index].Data:=Item;
    end
  else
    begin
    FItems.Add(LoName, Item);
    {$IFDEF VerbosePasResolver}
    if FindIdentifier(Item.Identifier)<>Item then
      raise Exception.Create('20160925183849');
    {$ENDIF}
    end;
end;

constructor TPasIdentifierScope.Create;
begin
  FItems:=TFPHashList.Create;
end;

destructor TPasIdentifierScope.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifierScope.Destroy START ',ClassName);
  {$ENDIF}
  FItems.ForEachCall(@OnClearItem,nil);
  FItems.Clear;
  FreeAndNil(FItems);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasIdentifierScope.Destroy END ',ClassName);
  {$ENDIF}
end;

function TPasIdentifierScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=FindLocalIdentifier(Identifier);
  {$IFDEF VerbosePasResolver}
  if (Result<>nil) and (Result.Owner<>Self) then
    begin
    writeln('TPasIdentifierScope.FindIdentifier Result.Owner<>Self Owner='+GetObjName(Result.Owner));
    raise Exception.Create('20160925184159');
    end;
  {$ENDIF}
end;

function TPasIdentifierScope.RemoveLocalIdentifier(El: TPasElement): boolean;
var
  Identifier, PrevIdentifier: TPasIdentifier;
  LoName: ShortString;
begin
  LoName:=lowercase(El.Name);
  Identifier:=TPasIdentifier(FItems.Find(LoName));
  FindLocalIdentifier(El.Name);
  PrevIdentifier:=nil;
  Result:=false;
  while Identifier<>nil do
    begin
    {$IFDEF VerbosePasResolver}
    if (Identifier.Owner<>Self) then
      raise Exception.Create('20160925184159');
    {$ENDIF}
    if Identifier.Element=El then
      begin
      if PrevIdentifier<>nil then
        begin
        PrevIdentifier.NextSameIdentifier:=Identifier.NextSameIdentifier;
        Identifier.Free;
        Identifier:=PrevIdentifier.NextSameIdentifier;
        end
      else
        begin
        FItems.Remove(Identifier);
        PrevIdentifier:=Identifier;
        Identifier:=Identifier.NextSameIdentifier;
        PrevIdentifier.Free;
        PrevIdentifier:=nil;
        if Identifier<>nil then
          FItems.Add(Loname,Identifier);
        end;
      Result:=true;
      continue;
      end;
    PrevIdentifier:=Identifier;
    Identifier:=Identifier.NextSameIdentifier;
    end;
end;

function TPasIdentifierScope.AddIdentifier(const Identifier: String;
  El: TPasElement; const Kind: TPasIdentifierKind): TPasIdentifier;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.AddIdentifier Identifier="',Identifier,'" El=',GetObjName(El));
  Item:=TPasIdentifier.Create;
  Item.Identifier:=Identifier;
  Item.Element:=El;
  Item.Kind:=Kind;

  InternalAdd(Item);
  //writeln('TPasIdentifierScope.AddIdentifier END');
  Result:=Item;
end;

function TPasIdentifierScope.FindElement(const aName: string): TPasElement;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.FindElement "',aName,'"');
  Item:=FindIdentifier(aName);
  if Item=nil then
    Result:=nil
  else
    Result:=Item.Element;
  //writeln('TPasIdentifierScope.FindElement Found="',GetObjName(Result),'"');
end;

procedure TPasIdentifierScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
var
  Item: TPasIdentifier;
  {$IFDEF VerbosePasResolver}
  OldElement: TPasElement;
  {$ENDIF}
begin
  Item:=FindLocalIdentifier(aName);
  while Item<>nil do
    begin
    //writeln('TPasIdentifierScope.IterateElements ',ClassName,' ',Item.Identifier,' ',GetObjName(Item.Element));
    {$IFDEF VerbosePasResolver}
    OldElement:=Item.Element;
    {$ENDIF}
    OnIterateElement(Item.Element,Self,StartScope,Data,Abort);
    {$IFDEF VerbosePasResolver}
    if OldElement<>Item.Element then
      raise Exception.Create('20160925183503');
    {$ENDIF}
    if Abort then exit;
    Item:=Item.NextSameIdentifier;
    end;
end;

procedure TPasIdentifierScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  Prefix:=Prefix+'  ';
  FItems.ForEachCall(@OnWriteItem,Pointer(Prefix));
end;

{ TPasResolver }

// inline
function TPasResolver.GetBaseTypes(bt: TResolverBaseType
  ): TPasUnresolvedSymbolRef;
begin
  Result:=FBaseTypes[bt];
end;

// inline
function TPasResolver.GetScopes(Index: integer): TPasScope;
begin
  Result:=FScopes[Index];
end;

procedure TPasResolver.ClearResolveDataList(Kind: TResolveDataListKind);
var
  El: TPasElement;
  RData: TResolveData;
begin
  // clear CustomData
  while FLastCreatedData[Kind]<>nil do
    begin
    RData:=FLastCreatedData[Kind];
    El:=RData.Element;
    El.CustomData:=nil;
    FLastCreatedData[Kind]:=RData.Next;
    RData.Free;
    end;
end;

procedure TPasResolver.OnFindFirstElement(El: TPasElement; ElScope,
  StartScope: TPasScope; FindFirstElementData: Pointer; var Abort: boolean);
var
  Data: PPRFindData absolute FindFirstElementData;
  ok: Boolean;
begin
  ok:=true;
  if (El is TPasProcedure)
      and ProcNeedsParams(TPasProcedure(El).ProcType) then
    // found a proc, but it needs parameters -> remember the first and continue
    ok:=false;
  if ok or (Data^.Found=nil) then
    begin
    Data^.Found:=El;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    end;
  if ok then
    Abort:=true;
end;

procedure TPasResolver.OnFindCallElements(El: TPasElement; ElScope,
  StartScope: TPasScope; FindProcsData: Pointer; var Abort: boolean);
var
  Data: PFindCallElData absolute FindProcsData;
  Proc, PrevProc: TPasProcedure;
  Distance: integer;
  BuiltInProc: TResElDataBuiltInProc;
  CandidateFound: Boolean;
  VarType, TypeEl: TPasType;
  C: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindCallElements START ---------');
  {$ENDIF}
  CandidateFound:=false;

  if (El is TPasProcedure) then
    begin
    // identifier is a proc
    Proc:=TPasProcedure(El);

    if Data^.Found=Proc then
      begin
      // this proc was already found. This happens when this is the forward
      // declaration or a previously found implementation.
      Data^.ElScope:=ElScope;
      Data^.StartScope:=StartScope;
      exit;
      end;

    if (Proc.CustomData is TPasProcedureScope)
        and (TPasProcedureScope(Proc.CustomData).DeclarationProc<>nil)
    then
      begin
      // this proc has a forward declaration -> use that instead
      Proc:=TPasProcedureScope(Proc.CustomData).DeclarationProc;
      El:=Proc;
      end;

    if Data^.Found is TPasProcedure then
      begin
      // there is already a previous proc
      PrevProc:=TPasProcedure(Data^.Found);

      if (Data^.Distance=cExact) and (PrevProc.Parent<>Proc.Parent)
          and (PrevProc.Parent.ClassType=TPasClassType) then
        begin
        // there was already a perfect proc in a descendant
        Abort:=true;
        exit;
        end;

      // check if previous found proc is override of found proc
      if (PrevProc.IsOverride)
          and (TPasProcedureScope(PrevProc.CustomData).OverriddenProc=Proc) then
        begin
        // previous found proc is override of found proc -> skip
        exit;
        end;
      end;

    Distance:=CheckCallProcCompatibility(Proc.ProcType,Data^.Params,false);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Proc Compatible=',Distance,' Data^.Found=',Data^.Found<>nil,' Data^.Compatible=',ord(Data^.Distance));
    {$ENDIF}
    CandidateFound:=true;
    end
  else if El is TPasType then
    begin
    TypeEl:=ResolveAliasType(TPasType(El));
    C:=TypeEl.ClassType;
    if C=TPasUnresolvedSymbolRef then
      begin
      if TypeEl.CustomData.ClassType=TResElDataBuiltInProc then
        begin
        // call of built-in proc
        BuiltInProc:=TResElDataBuiltInProc(TypeEl.CustomData);
        if (BuiltInProc.BuiltIn in [bfStrProc,bfStrFunc])
            and ((BuiltInProc.BuiltIn=bfStrProc) = ParentNeedsExprResult(Data^.Params)) then
          begin
          // str function can only be used within an expression
          // str procedure can only be used outside an expression
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.OnFindCallElements BuiltInProc=',El.Name,' skip');
          {$ENDIF}
          exit;
          end;
        Distance:=BuiltInProc.GetCallCompatibility(BuiltInProc,Data^.Params,false);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements BuiltInProc=',El.Name,' Distance=',Distance);
        {$ENDIF}
        CandidateFound:=true;
        end
      else if TypeEl.CustomData is TResElDataBaseType then
        begin
        // type cast to base type
        Abort:=true; // can't be overloaded
        if Data^.Found<>nil then exit;
        Distance:=CheckTypeCast(TPasType(El),Data^.Params,false);
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements Base type cast=',El.Name,' Distance=',Distance);
        {$ENDIF}
        CandidateFound:=true;
        end;
      end
    else if (C=TPasClassType)
        or (C=TPasClassOfType)
        or (C=TPasEnumType)
        or (C=TPasArrayType) then
      begin
      // type cast to a class, class-of, enum, or array
      Abort:=true; // can't be overloaded
      if Data^.Found<>nil then exit;
      Distance:=CheckTypeCast(TPasType(El),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements type cast to =',GetObjName(El),' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end
  else if El is TPasVariable then
    begin
    Abort:=true; // can't be overloaded
    if Data^.Found<>nil then exit;
    VarType:=ResolveAliasType(TPasVariable(El).VarType);
    if VarType is TPasProcedureType then
      begin
      Distance:=CheckCallProcCompatibility(TPasProcedureType(VarType),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements call var of proctype=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end
  else if El.ClassType=TPasArgument then
    begin
    Abort:=true; // can't be overloaded
    if Data^.Found<>nil then exit;
    VarType:=ResolveAliasType(TPasArgument(El).ArgType);
    if VarType is TPasProcedureType then
      begin
      Distance:=CheckCallProcCompatibility(TPasProcedureType(VarType),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements call arg of proctype=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end;

  if not CandidateFound then
    begin
    // El does not support the () operator
    Abort:=true;
    if Data^.Found=nil then
      begin
      // El is the first element found -> raise error
      // ToDo: use the ( as error position
      RaiseMsg(20170216151525,nIllegalQualifier,sIllegalQualifier,['('],Data^.Params);
      end;
    exit;
    end;

  // El is a candidate
  if (Data^.Found=nil) or (Distance<Data^.Distance) then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Found a better candidate Distance=',Distance,' Data^.Distance=',Data^.Distance);
    {$ENDIF}
    Data^.Found:=El;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Data^.Distance:=Distance;
    Data^.Count:=1;
    if Data^.List<>nil then
      begin
      Data^.List.Clear;
      Data^.List.Add(El);
      end;
    end
  else if Distance=Data^.Distance then
    begin
    inc(Data^.Count);
    if (Data^.List<>nil) then
      begin
      if (Data^.List.IndexOf(El)>=0) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.OnFindCallElements Found El twice: ',GetTreeDesc(El),
          ' ',GetElementSourcePosStr(El),
          ' PrevElScope=',GetObjName(Data^.ElScope),' ',GetTreeDesc(Data^.ElScope.Element),
          ' ElScope=',GetObjName(ElScope),' ',GetTreeDesc(ElScope.Element)
          );
        {$ENDIF}
        RaiseInternalError(20160924230805);
        end;
      Data^.List.Add(El);
      end;
    end;
end;

procedure TPasResolver.OnFindOverloadProc(El: TPasElement; ElScope,
  StartScope: TPasScope; FindOverloadData: Pointer; var Abort: boolean);
var
  Data: PFindOverloadProcData absolute FindOverloadData;
  Proc: TPasProcedure;
begin
  //writeln('TPasResolver.OnFindOverloadProc START ',El.Name,':',El.ElementTypeName,' itself=',El=Data^.Proc);
  if not (El is TPasProcedure) then
    begin
    // identifier is not a proc
    if (El is TPasVariable) then
      begin
      if TPasVariable(El).Visibility=visStrictPrivate then
        exit;
      if (TPasVariable(El).Visibility=visPrivate)
          and (El.GetModule<>StartScope.Element.GetModule) then
        exit;
      end;
    Data^.FoundNonProc:=El;
    Abort:=true;
    exit;
    end;
  // identifier is a proc
  if El=Data^.Proc then
    exit; // found itself -> normal when searching for overloads

  //writeln('TPasResolver.OnFindOverloadProc Data^.OnlyScope=',GetObjName(Data^.OnlyScope),' ElScope=',GetObjName(ElScope),' ',Data^.OnlyScope=ElScope);
  if (Data^.OnlyScope<>nil) and (Data^.OnlyScope<>ElScope) then
    begin
    // do not search any further, only one scope should be searched
    // for example when searching the method declaration of a method body
    Abort:=false;
    exit;
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindOverloadProc ',GetTreeDesc(El,2));
  {$ENDIF}
  Proc:=TPasProcedure(El);
  if CheckOverloadProcCompatibility(Data^.Proc,Proc) then
    begin
    Data^.Found:=Proc;
    Data^.ElScope:=ElScope;
    Data^.StartScope:=StartScope;
    Abort:=true;
    end;
end;

procedure TPasResolver.SetCurrentParser(AValue: TPasParser);
begin
  //writeln('TPasResolver.SetCurrentParser ',AValue<>nil);
  if AValue=CurrentParser then exit;
  Clear;
  inherited SetCurrentParser(AValue);
  if CurrentParser<>nil then
    CurrentParser.Options:=CurrentParser.Options
      +[po_resolvestandardtypes,po_nooverloadedprocs,po_keepclassforward,
        po_arrayrangeexpr];
end;

procedure TPasResolver.CheckTopScope(ExpectedClass: TPasScopeClass);
begin
  if TopScope=nil then
    RaiseInternalError(20160922163319,'Expected TopScope='+ExpectedClass.ClassName+' but found nil');
  if TopScope.ClassType<>ExpectedClass then
    RaiseInternalError(20160922163323,'Expected TopScope='+ExpectedClass.ClassName+' but found '+TopScope.ClassName);
end;

function TPasResolver.AddIdentifier(Scope: TPasIdentifierScope;
  const aName: String; El: TPasElement; const Kind: TPasIdentifierKind
  ): TPasIdentifier;
var
  Identifier, OlderIdentifier: TPasIdentifier;
  ClassScope: TPasClassScope;
  OlderEl: TPasElement;
begin
  if (Kind=pikSimple) and (Scope is TPasClassScope)
      and (El.ClassType<>TPasProperty) then
    begin
    // check duplicate in ancestors
    ClassScope:=TPasClassScope(Scope).AncestorScope;
    while ClassScope<>nil do
      begin
      OlderIdentifier:=ClassScope.FindLocalIdentifier(aName);
      while OlderIdentifier<>nil do
        begin
        OlderEl:=OlderIdentifier.Element;
        OlderIdentifier:=OlderIdentifier.NextSameIdentifier;
        if OlderEl is TPasVariable then
          begin
          if TPasVariable(OlderEl).Visibility=visStrictPrivate then
            continue; // OlderEl is hidden
          if (TPasVariable(OlderEl).Visibility=visPrivate)
              and (OlderEl.GetModule<>El.GetModule) then
            continue; // OlderEl is hidden
          end;
        RaiseMsg(20170221130001,nDuplicateIdentifier,sDuplicateIdentifier,
                 [aName,GetElementSourcePosStr(OlderEl)],El);
        end;
      ClassScope:=ClassScope.AncestorScope;
      end;
    end;

  Identifier:=Scope.AddIdentifier(aName,El,Kind);

  // check duplicate in current scope
  OlderIdentifier:=Identifier.NextSameIdentifier;
  if (OlderIdentifier<>nil) then
    if (Identifier.Kind=pikSimple) or (OlderIdentifier.Kind=pikSimple) then
      begin
      if (OlderIdentifier.Element.ClassType=TPasEnumValue)
          and (OlderIdentifier.Element.Parent.Parent<>Scope.Element) then
        // this enum was propagated from a sub type -> remove enum
        Scope.RemoveLocalIdentifier(OlderIdentifier.Element);
      RaiseMsg(20170216151530,nDuplicateIdentifier,sDuplicateIdentifier,
               [aName,GetElementSourcePosStr(OlderIdentifier.Element)],El);
      end;

  Result:=Identifier;
end;

procedure TPasResolver.FinishModule(CurModule: TPasModule);
var
  CurModuleClass: TClass;
  i: Integer;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule START ',CurModule.Name);
  {$ENDIF}
  CurModuleClass:=CurModule.ClassType;
  if (CurModuleClass=TPasProgram) or (CurModuleClass=TPasLibrary) then
    begin
    // resolve begin..end block
    ResolveImplBlock(CurModule.InitializationSection);
    end
  else if (CurModuleClass=TPasModule) then
    begin
    if CurModule.FinalizationSection<>nil then
      // finalization section finished -> resolve
      ResolveImplBlock(CurModule.FinalizationSection);
    if CurModule.InitializationSection<>nil then
      // initialization section finished -> resolve
      ResolveImplBlock(CurModule.InitializationSection);
    end
  else
    RaiseInternalError(20160922163327); // unknown module

  // check all methods have bodies
  // and all forward classes and pointers are resolved
  for i:=0 to FPendingForwards.Count-1 do
    CheckPendingForwards(TPasElement(FPendingForwards[i]));
  FPendingForwards.Clear;

  // close all sections
  while (TopScope<>nil) and (TopScope.ClassType=TPasSectionScope) do
    PopScope;
  CheckTopScope(TPasModuleScope);
  PopScope;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule END ',CurModule.Name);
  {$ENDIF}
end;

procedure TPasResolver.FinishUsesList;
var
  Section: TPasSection;
  i: Integer;
  El, PublicEl: TPasElement;
  Scope: TPasSectionScope;
  UsesScope: TPasIdentifierScope;
begin
  CheckTopScope(TPasSectionScope);
  Scope:=TPasSectionScope(TopScope);
  Section:=TPasSection(Scope.Element);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishUsesList Section=',Section.ClassName,' Section.UsesList.Count=',Section.UsesList.Count);
  {$ENDIF}
  for i:=0 to Section.UsesList.Count-1 do
    begin
    El:=TPasElement(Section.UsesList[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishUsesList ',GetObjName(El));
    {$ENDIF}
    if (El.ClassType=TProgramSection) then
      RaiseInternalError(20160922163346,'used unit is a program: '+GetObjName(El));

    // add unitname as identifier
    AddIdentifier(Scope,El.Name,El,pikSimple);

    // check used unit
    PublicEl:=nil;
    if (El.ClassType=TLibrarySection) then
      PublicEl:=El
    else if (El.ClassType=TPasModule) then
      PublicEl:=TPasModule(El).InterfaceSection;
    if PublicEl=nil then
      RaiseInternalError(20160922163352,'uses element has no interface section: '+GetObjName(El));
    if PublicEl.CustomData=nil then
      RaiseInternalError(20160922163358,'uses element has no resolver data: '
        +El.Name+'->'+GetObjName(PublicEl));
    if not (PublicEl.CustomData is TPasIdentifierScope) then
      RaiseInternalError(20160922163403,'uses element has invalid resolver data: '
        +El.Name+'->'+GetObjName(PublicEl)+'->'+PublicEl.CustomData.ClassName);

    UsesScope:=TPasIdentifierScope(PublicEl.CustomData);
    Scope.UsesList.Add(UsesScope);
    end;
end;

procedure TPasResolver.FinishTypeSection(El: TPasDeclarations);
var
  i: Integer;
  Decl: TPasElement;
  ClassOfEl: TPasClassOfType;
  Data: TPRFindData;
  UnresolvedEl: TUnresolvedPendingRef;
  Abort: boolean;
  OldClassType: TPasClassType;
  ClassOfName: String;
begin
  // resolve pending forwards
  for i:=0 to El.Declarations.Count-1 do
    begin
    Decl:=TPasElement(El.Declarations[i]);
    if Decl is TPasClassType then
      begin
      if TPasClassType(Decl).IsForward and (TPasClassType(Decl).CustomData=nil) then
        RaiseMsg(20170216151534,nForwardTypeNotResolved,sForwardTypeNotResolved,[Decl.Name],Decl);
      end
    else if (Decl.ClassType=TPasClassOfType) then
      begin
      ClassOfEl:=TPasClassOfType(Decl);
      Data:=Default(TPRFindData);
      if (ClassOfEl.DestType.ClassType=TUnresolvedPendingRef) then
        begin
        // forward class-of -> resolve now
        UnresolvedEl:=TUnresolvedPendingRef(ClassOfEl.DestType);
        ClassOfName:=UnresolvedEl.Name;
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.FinishTypeSection resolving "',ClassOfEl.Name,'" = class of unresolved "',ClassOfName,'"');
        {$ENDIF}
        Data.ErrorPosEl:=UnresolvedEl;
        Abort:=false;
        (TopScope as TPasIdentifierScope).IterateElements(ClassOfName,
          TopScope,@OnFindFirstElement,@Data,Abort);
        if (Data.Found=nil) then
          RaiseIdentifierNotFound(20170216151543,UnresolvedEl.Name,UnresolvedEl);
        if Data.Found.ClassType<>TPasClassType then
          RaiseXExpectedButYFound(20170216151548,'class',Data.Found.ElementTypeName,UnresolvedEl);
        // replace unresolved
        ClassOfEl.DestType:=TPasClassType(Data.Found);
        ClassOfEl.DestType.AddRef;
        UnresolvedEl.Release;
        end
      else
        begin
        // class-of has found a type
        // another later in the same type section has priority -> check
        OldClassType:=ClassOfEl.DestType as TPasClassType;
        if ClassOfEl.DestType.Parent=ClassOfEl.Parent then
          continue; // class in same type section -> ok
        // class not in same type section -> check
        ClassOfName:=OldClassType.Name;
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.FinishTypeSection resolving "',ClassOfEl.Name,'" = class of resolved "',ClassOfName,'"');
        {$ENDIF}
        Data.ErrorPosEl:=ClassOfEl;
        Abort:=false;
        (TopScope as TPasIdentifierScope).IterateElements(ClassOfName,
          TopScope,@OnFindFirstElement,@Data,Abort);
        if (Data.Found=nil) then
          continue;
        if Data.Found.ClassType<>TPasClassType then
          RaiseXExpectedButYFound(20170221171040,'class',Data.Found.ElementTypeName,ClassOfEl);
        ClassOfEl.DestType:=TPasClassType(Data.Found);
        ClassOfEl.DestType.AddRef;
        OldClassType.Release;
        end;
      end;
    end;
end;

procedure TPasResolver.FinishTypeDef(El: TPasType);
var
  C: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishTypeDef El=',GetObjName(El));
  {$ENDIF}
  C:=El.ClassType;
  if C=TPasEnumType then
    FinishEnumType(TPasEnumType(El))
  else if C=TPasSetType then
    FinishSetType(TPasSetType(El))
  else if C=TPasRangeType then
    FinishRangeType(TPasRangeType(El))
  else if C=TPasRecordType then
    FinishRecordType(TPasRecordType(El))
  else if C=TPasClassType then
    FinishClassType(TPasClassType(El))
  else if C=TPasClassOfType then
    FinishClassOfType(TPasClassOfType(El))
  else if C=TPasArrayType then
    FinishArrayType(TPasArrayType(El));
end;

procedure TPasResolver.FinishEnumType(El: TPasEnumType);
begin
  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishSetType(El: TPasSetType);
var
  BaseTypeData: TResElDataBaseType;
  StartResolved, EndResolved: TPasResolverResult;
  RangeExpr: TBinaryExpr;
  C: TClass;
  EnumType: TPasType;
begin
  EnumType:=El.EnumType;
  C:=EnumType.ClassType;
  if C=TPasEnumType then
    exit
  else if C=TPasRangeType then
    begin
    RangeExpr:=TPasRangeType(EnumType).RangeExpr;
    if RangeExpr.Parent=El then
      CheckRangeExpr(RangeExpr.left,RangeExpr.right,StartResolved,EndResolved);
    exit;
    end
  else if C=TPasUnresolvedSymbolRef then
    begin
    if EnumType.CustomData is TResElDataBaseType then
      begin
      BaseTypeData:=TResElDataBaseType(EnumType.CustomData);
      if BaseTypeData.BaseType in [btChar,btBoolean] then
        exit;
      RaiseXExpectedButYFound(20170216151553,'char or boolean',EnumType.ElementTypeName,EnumType);
      end;
    end;
  RaiseXExpectedButYFound(20170216151557,'enum type',EnumType.ElementTypeName,EnumType);
end;

procedure TPasResolver.FinishRangeType(El: TPasRangeType);
var
  StartResolved, EndResolved: TPasResolverResult;
begin
  ResolveExpr(El.RangeExpr.left,rraRead);
  ResolveExpr(El.RangeExpr.right,rraRead);
  CheckRangeExpr(El.RangeExpr.left,El.RangeExpr.right,StartResolved,EndResolved);
end;

procedure TPasResolver.FinishRecordType(El: TPasRecordType);
begin
  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishClassType(El: TPasClassType);
begin
  if TopScope.Element=El then
    PopScope;
end;

procedure TPasResolver.FinishClassOfType(El: TPasClassOfType);
begin
  if El.DestType is TUnresolvedPendingRef then exit;
  if El.DestType is TPasClassType then exit;
  RaiseMsg(20170216151602,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
    [El.DestType.Name,'class'],El);
end;

procedure TPasResolver.FinishArrayType(El: TPasArrayType);
var
  i: Integer;
  Expr: TPasExpr;
  RangeResolved: TPasResolverResult;
begin
  for i:=0 to length(El.Ranges)-1 do
    begin
    Expr:=El.Ranges[i];
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,RangeResolved,[rcConstant]);
    if (RangeResolved.IdentEl<>nil) and not (RangeResolved.IdentEl is TPasType) then
      RaiseXExpectedButYFound(20170216151607,'range',RangeResolved.IdentEl.ElementTypeName,Expr);
    if (RangeResolved.BaseType=btRange) and (RangeResolved.SubType in btArrayRangeTypes) then
      // range, e.g. 1..2
    else if RangeResolved.BaseType in btArrayRangeTypes then
      // full range, e.g. array[char]
    else if (RangeResolved.BaseType=btContext) and (RangeResolved.TypeEl is TPasEnumType) then
      // e.g. array[enumtype]
    else
      RaiseXExpectedButYFound(20170216151609,'range',RangeResolved.IdentEl.ElementTypeName,Expr);
    end;
end;

procedure TPasResolver.FinishConstDef(El: TPasConst);
begin
  ResolveExpr(El.Expr,rraRead);
  if El.VarType<>nil then
    CheckAssignCompatibility(El,El.Expr,true);
end;

procedure TPasResolver.FinishProcedure(aProc: TPasProcedure);
var
  i: Integer;
  Body: TProcedureBody;
  SubEl: TPasElement;
  SubProcScope: TPasProcedureScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishProcedure START');
  {$ENDIF}
  CheckTopScope(TPasProcedureScope);
  if TPasProcedureScope(TopScope).Element<>aProc then
    RaiseInternalError(20170220163043);
  Body:=aProc.Body;
  if Body<>nil then
    begin
    ResolveImplBlock(Body.Body);

    // check if all forward procs are resolved
    for i:=0 to Body.Declarations.Count-1 do
      begin
      SubEl:=TPasElement(Body.Declarations[i]);
      if (SubEl is TPasProcedure) and TPasProcedure(SubEl).IsForward then
        begin
        SubProcScope:=TPasProcedure(SubEl).CustomData as TPasProcedureScope;
        if SubProcScope.ImplProc=nil then
          RaiseMsg(20170216151613,nForwardProcNotResolved,sForwardProcNotResolved,
            [SubEl.ElementTypeName,SubEl.Name],SubEl);
        end;
      end;
    end;
  PopScope;
end;

procedure TPasResolver.FinishProcedureType(El: TPasProcedureType);
var
  ProcName: String;
  FindData: TFindOverloadProcData;
  DeclProc, Proc: TPasProcedure;
  Abort: boolean;
  DeclProcScope, ProcScope: TPasProcedureScope;
  ParentScope: TPasScope;
  pm: TProcedureModifier;
begin
  if El.Parent is TPasProcedure then
    begin
    // finished header of a procedure declaration
    // -> search the best fitting proc
    CheckTopScope(TPasProcedureScope);
    Proc:=TPasProcedure(El.Parent);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishProcedureHeader El=',GetTreeDesc(El),' ',GetElementSourcePosStr(El),' IsForward=',Proc.IsForward,' Parent=',GetObjName(El.Parent));
    {$ENDIF}
    ProcName:=Proc.Name;

    if Proc.IsExternal then
      for pm in TProcedureModifier do
        if (pm in Proc.Modifiers)
            and not (pm in [pmVirtual, pmDynamic, pmOverride,
                        pmOverload, pmMessage, pmReintroduce,
                        pmStatic, pmVarargs,
                        pmExternal, pmDispId,
                        pmfar]) then
          RaiseMsg(20170216151616,nInvalidProcModifiers,
            sInvalidProcModifiers,[Proc.ElementTypeName,'external, '+ModifierNames[pm]],Proc);

    if Proc.Parent is TPasClassType then
      begin
      // method declaration
      if Proc.IsAbstract then
        begin
        if not Proc.IsVirtual then
          RaiseMsg(20170216151623,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'abstract without virtual'],Proc);
        if Proc.IsOverride then
          RaiseMsg(20170216151625,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'abstract, override'],Proc);
        end;
      if Proc.IsVirtual and Proc.IsOverride then
        RaiseMsg(20170216151627,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'virtual, override'],Proc);
      if Proc.IsForward then
        RaiseMsg(20170216151629,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'forward'],Proc);
      if Proc.IsStatic then
        if (Proc.ClassType<>TPasClassProcedure) and (Proc.ClassType<>TPasClassFunction) then
          RaiseMsg(20170216151631,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'static'],Proc);
      end
    else
      begin
      // intf proc, forward proc, proc body, method body
      if Proc.IsAbstract then
        RaiseMsg(20170216151634,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'abstract'],Proc);
      if Proc.IsVirtual then
        RaiseMsg(20170216151635,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'virtual'],Proc);
      if Proc.IsOverride then
        RaiseMsg(20170216151637,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'override'],Proc);
      if Proc.IsMessage then
        RaiseMsg(20170216151638,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'message'],Proc);
      if Proc.IsStatic then
        RaiseMsg(20170216151640,nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'static'],Proc);
      end;

    if Pos('.',ProcName)>1 then
      begin
      FinishMethodImplHeader(Proc);
      exit;
      end;

    // finish interface/implementation/nested procedure/method declaration

    if not IsValidIdent(ProcName) then
      RaiseNotYetImplemented(20160922163407,El);

    if Proc.LibraryExpr<>nil then
      ResolveExpr(Proc.LibraryExpr,rraRead);
    if Proc.LibrarySymbolName<>nil then
      ResolveExpr(Proc.LibrarySymbolName,rraRead);

    if Proc.Parent is TPasClassType then
      begin
      FinishMethodDeclHeader(Proc);
      exit;
      end;

    // finish interface/implementation/nested procedure
    FindData:=Default(TFindOverloadProcData);
    FindData.Proc:=Proc;
    FindData.Args:=Proc.ProcType.Args;
    Abort:=false;
    IterateElements(ProcName,@OnFindOverloadProc,@FindData,Abort);
    if FindData.FoundNonProc<>nil then
      begin
      // proc hides a non proc -> forbidden within module
      if (Proc.GetModule=FindData.FoundNonProc.GetModule) then
        RaiseMsg(20170216151649,nDuplicateIdentifier,sDuplicateIdentifier,
          [FindData.FoundNonProc.Name,GetElementSourcePosStr(FindData.FoundNonProc)],Proc.ProcType);
      end;
    if FindData.Found=nil then
      exit; // no overload -> ok

    // overload found with same signature
    DeclProc:=FindData.Found;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishProcedureHeader overload found: Proc2=',GetTreeDesc(DeclProc),' ',GetElementSourcePosStr(DeclProc),' IsForward=',DeclProc.IsForward,' Parent=',GetObjName(DeclProc.Parent));
    {$ENDIF}
    if (Proc.Parent=DeclProc.Parent)
        or ((Proc.Parent is TImplementationSection)
           and (DeclProc.Parent is TInterfaceSection)
           and (Proc.Parent.Parent=DeclProc.Parent.Parent))
    then
      begin
      // both procs are defined in the same scope
      if ProcNeedsImplProc(Proc) or (not ProcNeedsImplProc(DeclProc)) then
        RaiseMsg(20170216151652,nDuplicateIdentifier,sDuplicateIdentifier,
                 [ProcName,GetElementSourcePosStr(DeclProc)],Proc.ProcType);
      CheckProcSignatureMatch(DeclProc,Proc);
      DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
      DeclProcScope.ImplProc:=Proc;
      ProcScope:=Proc.CustomData as TPasProcedureScope;
      ProcScope.DeclarationProc:=DeclProc;
      // remove ImplProc from scope
      ParentScope:=Scopes[ScopeCount-2];
      (ParentScope as TPasIdentifierScope).RemoveLocalIdentifier(Proc);
      // replace arguments with declaration arguments
      ReplaceProcScopeImplArgsWithDeclArgs(ProcScope);
      end
    else
      begin
      // give a hint, that proc is hiding DeclProc
      LogMsg(20170216151656,mtHint,nFunctionHidesIdentifier,sFunctionHidesIdentifier,
        [DeclProc.Name,GetElementSourcePosStr(DeclProc)],Proc.ProcType);
      end;
    end
  else if El.Name<>'' then
    begin
    // finished proc type, e.g. type TProcedure = procedure;
    end
  else
    RaiseNotYetImplemented(20160922163411,El.Parent);
end;

procedure TPasResolver.FinishMethodDeclHeader(Proc: TPasProcedure);

  procedure VisibilityLowered(Proc, OverloadProc: TPasProcedure);
  begin
    LogMsg(20170325004215,mtNote,nVirtualMethodXHasLowerVisibility,
      sVirtualMethodXHasLowerVisibility,[Proc.Name,
        VisibilityNames[Proc.Visibility],OverloadProc.Parent.Name,
        VisibilityNames[OverloadProc.Visibility]],Proc);
    Proc.Visibility:=OverloadProc.Visibility;
  end;

var
  Abort: boolean;
  ClassScope: TPasClassScope;
  FindData: TFindOverloadProcData;
  OverloadProc: TPasProcedure;
  ProcScope: TPasProcedureScope;
begin
  Proc.ProcType.IsOfObject:=true;
  ProcScope:=TopScope as TPasProcedureScope;
  ClassScope:=Scopes[ScopeCount-2] as TPasClassScope;
  ProcScope.ClassScope:=ClassScope;
  FindData:=Default(TFindOverloadProcData);
  FindData.Proc:=Proc;
  FindData.Args:=Proc.ProcType.Args;
  Abort:=false;
  ClassScope.IterateElements(Proc.Name,ClassScope,@OnFindOverloadProc,@FindData,Abort);
  if FindData.FoundNonProc<>nil then
    // proc hides a non proc -> duplicate
    RaiseMsg(20170216151659,nDuplicateIdentifier,sDuplicateIdentifier,
      [FindData.FoundNonProc.Name,GetElementSourcePosStr(FindData.FoundNonProc)],Proc.ProcType);
  if FindData.Found=nil then
    begin
    // no overload
    if Proc.IsOverride then
      RaiseMsg(20170216151702,nNoMethodInAncestorToOverride,
        sNoMethodInAncestorToOverride,[GetProcDesc(Proc.ProcType)],Proc.ProcType);
    end
  else
    begin
    // overload found
    OverloadProc:=FindData.Found;
    if Proc.Parent=OverloadProc.Parent then
      // overload in same scope -> duplicate
      RaiseMsg(20170216151705,nDuplicateIdentifier,sDuplicateIdentifier,
        [OverloadProc.Name,GetElementSourcePosStr(OverloadProc)],Proc.ProcType);
    ProcScope.OverriddenProc:=OverloadProc;
    if Proc.IsOverride then
      begin
      if (not OverloadProc.IsVirtual) and (not OverloadProc.IsOverride) then
        // the OverloadProc fits the signature, but is not virtual
        RaiseMsg(20170216151708,nNoMethodInAncestorToOverride,
          sNoMethodInAncestorToOverride,[GetProcDesc(Proc.ProcType)],Proc.ProcType);
      // override a virtual method
      CheckProcSignatureMatch(OverloadProc,Proc);
      // check visibility
      if Proc.Visibility<>OverloadProc.Visibility then
        case Proc.Visibility of
          visPrivate,visStrictPrivate:
            if not (OverloadProc.Visibility in [visPrivate,visStrictPrivate]) then
              VisibilityLowered(Proc,OverloadProc);
          visProtected,visStrictProtected:
            if not (OverloadProc.Visibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected]) then
              VisibilityLowered(Proc,OverloadProc);
          visPublic:
            if not (OverloadProc.Visibility in [visPrivate..visPublic,visStrictPrivate,visStrictProtected]) then
              VisibilityLowered(Proc,OverloadProc);
          visPublished: ;
        else
          RaiseNotYetImplemented(20170325003315,Proc,'visibility');
        end;
      // check name case
      if proFixCaseOfOverrides in Options then
        Proc.Name:=OverloadProc.Name;
      end
    else if not Proc.IsReintroduced then
      begin
        // give a hint, that proc is hiding OverloadProc
        LogMsg(20170216151712,mtHint,nFunctionHidesIdentifier,sFunctionHidesIdentifier,
          [OverloadProc.Name,GetElementSourcePosStr(OverloadProc)],Proc.ProcType);
      end;
    end;
end;

procedure TPasResolver.FinishMethodImplHeader(ImplProc: TPasProcedure);
var
  ProcName: String;
  CurClassType: TPasClassType;
  FindData: TFindOverloadProcData;
  Abort: boolean;
  ImplProcScope, DeclProcScope: TPasProcedureScope;
  DeclProc: TPasProcedure;
  CurClassScope: TPasClassScope;
  SelfArg: TPasArgument;
  p: Integer;
begin
  if ImplProc.IsExternal then
    RaiseMsg(20170216151715,nInvalidProcModifiers,sInvalidProcModifiers,[ImplProc.ElementTypeName,'external'],ImplProc);
  if ImplProc.IsExported then
    RaiseMsg(20170216151717,nInvalidProcModifiers,sInvalidProcModifiers,[ImplProc.ElementTypeName,'export'],ImplProc);

  ProcName:=ImplProc.Name;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishMethodBodyHeader searching declaration "',ProcName,'" ...');
  {$ENDIF}
  ImplProc.ProcType.IsOfObject:=true;

  repeat
    p:=Pos('.',ProcName);
    if p<1 then break;
    Delete(ProcName,1,p);
  until false;

  // search ImplProc in class
  if not IsValidIdent(ProcName) then
    RaiseNotYetImplemented(20160922163421,ImplProc.ProcType);

  // search proc in class
  ImplProcScope:=ImplProc.CustomData as TPasProcedureScope;
  CurClassScope:=ImplProcScope.ClassScope;
  if CurClassScope=nil then
    RaiseInternalError(20161013172346);
  CurClassType:=CurClassScope.Element as TPasClassType;
  FindData:=Default(TFindOverloadProcData);
  FindData.Proc:=ImplProc;
  FindData.Args:=ImplProc.ProcType.Args;
  FindData.OnlyScope:=CurClassScope;
  Abort:=false;
  CurClassScope.IterateElements(ProcName,CurClassScope,@OnFindOverloadProc,@FindData,Abort);
  if FindData.Found=nil then
    RaiseIdentifierNotFound(20170216151720,ImplProc.Name,ImplProc.ProcType);

  // connect method declaration and body
  DeclProc:=FindData.Found;
  if DeclProc.IsAbstract then
    RaiseMsg(20170216151722,nAbstractMethodsMustNotHaveImplementation,sAbstractMethodsMustNotHaveImplementation,[],ImplProc);
  if DeclProc.IsExternal then
    RaiseXExpectedButYFound(20170216151725,'method','external method',ImplProc);
  CheckProcSignatureMatch(DeclProc,ImplProc);
  ImplProcScope.DeclarationProc:=DeclProc;
  DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
  DeclProcScope.ImplProc:=ImplProc;

  // replace arguments in scope with declaration arguments
  ReplaceProcScopeImplArgsWithDeclArgs(ImplProcScope);

  if not DeclProc.IsStatic then
    begin
    // add 'Self'
    if (DeclProc.ClassType=TPasClassConstructor)
        or (DeclProc.ClassType=TPasClassDestructor)
        or (DeclProc.ClassType=TPasClassProcedure)
        or (DeclProc.ClassType=TPasClassFunction) then
      begin
      // 'Self' in a class proc is the class VMT
      AddIdentifier(ImplProcScope,'Self',CurClassType,pikSimple);
      end
    else
      begin
      // 'Self' in a proc is the hidden instance argument
      SelfArg:=TPasArgument.Create('Self',DeclProc);
      ImplProcScope.SelfArg:=SelfArg;
      SelfArg.Access:=argConst;
      SelfArg.ArgType:=CurClassType;
      CurClassType.AddRef;
      AddIdentifier(ImplProcScope,'Self',SelfArg,pikSimple);
      end;
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishMethodBodyHeader END of searching proc "',ImplProc.Name,'" ...');
  {$ENDIF}
end;

procedure TPasResolver.FinishExceptOnExpr;
var
  El: TPasImplExceptOn;
  ResolvedType: TPasResolverResult;
begin
  CheckTopScope(TPasExceptOnScope);
  El:=TPasImplExceptOn(FTopScope.Element);
  ComputeElement(El.TypeEl,ResolvedType,[rcSkipTypeAlias,rcType]);
  CheckIsClass(El.TypeEl,ResolvedType);
end;

procedure TPasResolver.FinishExceptOnStatement;
begin
  //writeln('TPasResolver.FinishExceptOnStatement START');
  CheckTopScope(TPasExceptOnScope);
  ResolveImplElement(TPasImplExceptOn(FTopScope.Element).Body);
  PopScope;
end;

procedure TPasResolver.FinishDeclaration(El: TPasElement);
begin
  if El.ClassType=TPasVariable then
    FinishVariable(TPasVariable(El))
  else if El.ClassType=TPasProperty then
    FinishPropertyOfClass(TPasProperty(El))
  else if El.ClassType=TPasArgument then
    FinishArgument(TPasArgument(El));
end;

procedure TPasResolver.FinishVariable(El: TPasVariable);
begin
  if El.Expr<>nil then
    begin
    ResolveExpr(El.Expr,rraRead);
    CheckAssignCompatibility(El,El.Expr,true);
    end;
end;

procedure TPasResolver.FinishPropertyOfClass(PropEl: TPasProperty);
var
  PropType: TPasType;
  ClassScope: TPasClassScope;

  procedure GetPropType;
  var
    AncEl: TPasElement;
    AncProp: TPasProperty;
  begin
    if PropType<>nil then exit;
    if PropEl.VarType<>nil then
      PropType:=PropEl.VarType
      // Note: a property with a type has no ancestor property
    else
      begin
      // search property in ancestor
      AncEl:=nil;
      if ClassScope.AncestorScope<>nil then
        AncEl:=ClassScope.AncestorScope.FindElement(PropEl.Name);
      if (not (AncEl is TPasProperty)) then
        RaiseMsg(20170216151741,nNoPropertyFoundToOverride,sNoPropertyFoundToOverride,[],PropEl);
      // found -> create reference
      AncProp:=TPasProperty(AncEl);
      (PropEl.CustomData as TPasPropertyScope).AncestorProp:=AncProp;
      AncProp.AddRef;
      // check property versus class property
      if PropEl.ClassType<>AncProp.ClassType then
        RaiseXExpectedButYFound(20170216151744,AncProp.ElementTypeName,PropEl.ElementTypeName,PropEl);
      // get inherited type
      PropType:=GetPasPropertyType(AncProp);
      // update DefaultProperty
      if (ClassScope.DefaultProperty=AncProp) then
        ClassScope.DefaultProperty:=PropEl;
      end;
  end;

  function GetAccessor(Expr: TPasExpr): TPasElement;
  var
    Prim: TPrimitiveExpr;
    DeclEl: TPasElement;
    Identifier: TPasIdentifier;
    Scope: TPasIdentifierScope;
  begin
    if Expr.ClassType=TBinaryExpr then
      begin
      if (TBinaryExpr(Expr).left is TPrimitiveExpr) then
        begin
        Prim:=TPrimitiveExpr(TBinaryExpr(Expr).left);
        if Prim.Kind<>pekIdent then
          RaiseXExpectedButYFound(20170216151746,'class',Prim.Value,Prim);
        Scope:=TopScope as TPasIdentifierScope;
        // search in class and ancestors, not in unit interface
        Identifier:=Scope.FindIdentifier(Prim.Value);
        if Identifier=nil then
          RaiseIdentifierNotFound(20170216151749,Prim.Value,Prim);
        DeclEl:=Identifier.Element;
        if DeclEl.ClassType<>TPasClassType then
          RaiseXExpectedButYFound(20170216151752,'class',DeclEl.ElementTypeName,Prim);
        CreateReference(DeclEl,Prim,rraRead);
        end
      else
        RaiseMsg(20170216151754,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
      if TBinaryExpr(Expr).OpCode<>eopSubIdent then
        RaiseMsg(20170216151757,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
      PushClassDotScope(TPasClassType(DeclEl));
      Expr:=TBinaryExpr(Expr).right;
      Result:=GetAccessor(Expr);
      PopScope;
      end
    else if Expr.ClassType=TPrimitiveExpr then
      begin
      Prim:=TPrimitiveExpr(Expr);
      if Prim.Kind<>pekIdent then
        RaiseXExpectedButYFound(20170216151800,'identifier',Prim.Value,Prim);
      Scope:=TopScope as TPasIdentifierScope;
      // search in class and ancestors, not in unit interface
      Identifier:=Scope.FindIdentifier(Prim.Value);
      if Identifier=nil then
        RaiseIdentifierNotFound(20170216151803,Prim.Value,Prim);
      DeclEl:=Identifier.Element;
      CreateReference(DeclEl,Prim,rraRead);
      Result:=DeclEl;
      end
    else
      RaiseNotYetImplemented(20160922163436,Expr);
  end;

  procedure CheckArgs(Proc: TPasProcedure; ErrorEl: TPasElement);
  var
    ArgNo: Integer;
    PropArg, ProcArg: TPasArgument;
    PropArgResolved, ProcArgResolved: TPasResolverResult;
  begin
    ArgNo:=0;
    while ArgNo<PropEl.Args.Count do
      begin
      if ArgNo>=Proc.ProcType.Args.Count then
        RaiseMsg(20170216151805,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],ErrorEl);
      PropArg:=TPasArgument(PropEl.Args[ArgNo]);
      ProcArg:=TPasArgument(Proc.ProcType.Args[ArgNo]);
      inc(ArgNo);

      // check access: var, const, ...
      if PropArg.Access<>ProcArg.Access then
        RaiseMsg(20170216151808,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),AccessNames[ProcArg.Access],AccessNames[PropArg.Access]],ErrorEl);

      // check typed
      if PropArg.ArgType=nil then
        begin
        if ProcArg.ArgType<>nil then
          RaiseMsg(20170216151811,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),ProcArg.ArgType.ElementTypeName,'untyped'],ErrorEl);
        end
      else if ProcArg.ArgType=nil then
        RaiseMsg(20170216151813,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),'untyped',PropArg.ArgType.ElementTypeName],ErrorEl)
      else
        begin
        ComputeElement(PropArg,PropArgResolved,[rcNoImplicitProc]);
        ComputeElement(ProcArg,ProcArgResolved,[rcNoImplicitProc]);

        if (PropArgResolved.BaseType<>ProcArgResolved.BaseType) then
          RaiseMsg(20170216151816,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),BaseTypeNames[ProcArgResolved.BaseType],BaseTypeNames[PropArgResolved.BaseType]],ErrorEl);
        if PropArgResolved.TypeEl=nil then
          RaiseInternalError(20161010125255);
        if ProcArgResolved.TypeEl=nil then
          RaiseInternalError(20161010125304);
        if (PropArgResolved.TypeEl<>ProcArgResolved.TypeEl) then
          RaiseIncompatibleType(20170216151819,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],ProcArgResolved.TypeEl,PropArgResolved.TypeEl,ErrorEl);
        end;
      end;
  end;

var
  ResultType: TPasType;
  CurClassType: TPasClassType;
  AccEl: TPasElement;
  Proc: TPasProcedure;
  Arg: TPasArgument;
  PropArgCount: Integer;
  PropTypeResolved, DefaultResolved: TPasResolverResult;
begin
  CheckTopScope(TPasPropertyScope);
  PopScope;

  PropType:=nil;
  CurClassType:=PropEl.Parent as TPasClassType;
  ClassScope:=CurClassType.CustomData as TPasClassScope;
  GetPropType;
  if PropEl.IndexExpr<>nil then
    begin
    ResolveExpr(PropEl.IndexExpr,rraRead);
    RaiseNotYetImplemented(20160922163439,PropEl.IndexExpr);
    end;
  if PropEl.ReadAccessor<>nil then
    begin
    // check compatibility
    AccEl:=GetAccessor(PropEl.ReadAccessor);
    if AccEl is TPasVariable then
      begin
      if PropEl.Args.Count>0 then
        RaiseXExpectedButYFound(20170216151823,'function',AccEl.ElementTypeName,PropEl.ReadAccessor);
      if TPasVariable(AccEl).VarType<>PropType then
        RaiseIncompatibleType(20170216151826,nIncompatibleTypesGotExpected,
          [],PropType,TPasVariable(AccEl).VarType,PropEl.ReadAccessor);
      if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
        if vmClass in PropEl.VarModifiers then
          RaiseXExpectedButYFound(20170216151828,'class var','var',PropEl.ReadAccessor)
        else
          RaiseXExpectedButYFound(20170216151831,'var','class var',PropEl.ReadAccessor);
      end
    else if AccEl is TPasProcedure then
      begin
      // check function
      Proc:=TPasProcedure(AccEl);
      if (vmClass in PropEl.VarModifiers) then
        begin
        if Proc.ClassType<>TPasClassFunction then
          RaiseXExpectedButYFound(20170216151834,'class function',Proc.ElementTypeName,PropEl.ReadAccessor);
        if Proc.IsStatic=(proClassPropertyNonStatic in Options) then
          if Proc.IsStatic then
            RaiseMsg(20170216151837,nClassPropertyAccessorMustNotBeStatic,sClassPropertyAccessorMustNotBeStatic,[],PropEl.ReadAccessor)
          else
            RaiseMsg(20170216151839,nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],PropEl.ReadAccessor);
        end
      else
        begin
        if Proc.ClassType<>TPasFunction then
          RaiseXExpectedButYFound(20170216151842,'function',Proc.ElementTypeName,PropEl.ReadAccessor);
        end;
      // check function result type
      ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
      if not IsSameType(ResultType,PropType) then
        RaiseXExpectedButYFound(20170216151844,'function result '+GetTypeDesc(PropType,true),
          GetTypeDesc(ResultType,true),PropEl.ReadAccessor);
      // check args
      CheckArgs(Proc,PropEl.ReadAccessor);
      if Proc.ProcType.Args.Count<>PropEl.Args.Count then
        RaiseMsg(20170216151847,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],PropEl.ReadAccessor);
      end
    else
      RaiseXExpectedButYFound(20170216151850,'variable',AccEl.ElementTypeName,PropEl.ReadAccessor);
    end;
  if PropEl.WriteAccessor<>nil then
    begin
    // check compatibility
    AccEl:=GetAccessor(PropEl.WriteAccessor);
    if AccEl is TPasVariable then
      begin
      if PropEl.Args.Count>0 then
        RaiseXExpectedButYFound(20170216151852,'procedure',AccEl.ElementTypeName,PropEl.WriteAccessor);
      if TPasVariable(AccEl).VarType<>PropType then
        RaiseIncompatibleType(20170216151855,nIncompatibleTypesGotExpected,
          [],PropType,TPasVariable(AccEl).VarType,PropEl.WriteAccessor);
      if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
        if vmClass in PropEl.VarModifiers then
          RaiseXExpectedButYFound(20170216151858,'class var','var',PropEl.WriteAccessor)
        else
          RaiseXExpectedButYFound(20170216151900,'var','class var',PropEl.WriteAccessor);
      end
    else if AccEl is TPasProcedure then
      begin
      // check procedure
      Proc:=TPasProcedure(AccEl);
      if (vmClass in PropEl.VarModifiers) then
        begin
        if Proc.ClassType<>TPasClassProcedure then
          RaiseXExpectedButYFound(20170216151903,'class procedure',Proc.ElementTypeName,PropEl.WriteAccessor);
          if Proc.IsStatic=(proClassPropertyNonStatic in Options) then
            if Proc.IsStatic then
              RaiseMsg(20170216151905,nClassPropertyAccessorMustNotBeStatic,sClassPropertyAccessorMustNotBeStatic,[],PropEl.WriteAccessor)
            else
              RaiseMsg(20170216151906,nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],PropEl.WriteAccessor);
        end
      else
        begin
        if Proc.ClassType<>TPasProcedure then
          RaiseXExpectedButYFound(20170216151910,'procedure',Proc.ElementTypeName,PropEl.WriteAccessor);
        end;
      // check args
      CheckArgs(Proc,PropEl.ReadAccessor);
      PropArgCount:=PropEl.Args.Count;
      if Proc.ProcType.Args.Count<>PropArgCount+1 then
        RaiseMsg(20170216151913,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],PropEl.WriteAccessor);
      Arg:=TPasArgument(Proc.ProcType.Args[PropArgCount]);
      if not (Arg.Access in [argDefault,argConst]) then
        RaiseMsg(20170216151917,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(PropArgCount+1),AccessNames[Arg.Access],AccessNames[argConst]],PropEl.WriteAccessor);
      if Arg.ArgType<>PropType then
        RaiseIncompatibleType(20170216151919,nIncompatibleTypeArgNo,
          [IntToStr(PropArgCount+1)],Arg.ArgType,PropType,PropEl.WriteAccessor);
      end
    else
      RaiseXExpectedButYFound(20170216151921,'variable',AccEl.ElementTypeName,PropEl.WriteAccessor);
    end;
  if PropEl.ImplementsFunc<>nil then
    begin
    ResolveExpr(PropEl.ImplementsFunc,rraRead);
    // ToDo: check compatibility

    end;
  if PropEl.StoredAccessor<>nil then
    begin
    // check compatibility
    AccEl:=GetAccessor(PropEl.StoredAccessor);
    if AccEl is TPasProcedure then
      begin
      // check function
      Proc:=TPasProcedure(AccEl);
      if Proc.ClassType<>TPasFunction then
        RaiseXExpectedButYFound(20170216151925,'function',Proc.ElementTypeName,PropEl.StoredAccessor);
      // check function result type
      ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
      if not IsBaseType(ResultType,btBoolean) then
        RaiseXExpectedButYFound(20170216151929,'function: boolean',
          'function:'+GetTypeDesc(ResultType),PropEl.StoredAccessor);
      // check arg count
      if Proc.ProcType.Args.Count<>0 then
        RaiseMsg(20170216151932,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],PropEl.StoredAccessor);
      end
    else
      RaiseXExpectedButYFound(20170216151935,'function: boolean',AccEl.ElementTypeName,PropEl.StoredAccessor);
    end;
  if PropEl.DefaultExpr<>nil then
    begin
    // check compatibility with type
    ResolveExpr(PropEl.DefaultExpr,rraRead);
    ComputeElement(PropEl.DefaultExpr,DefaultResolved,[rcConstant]);
    ComputeElement(PropType,PropTypeResolved,[rcType]);
    PropTypeResolved.IdentEl:=PropEl;
    PropTypeResolved.Flags:=[rrfReadable];
    CheckEqualResCompatibility(PropTypeResolved,DefaultResolved,PropEl.DefaultExpr,true);
    end;
  if PropEl.IsDefault then
    begin
    // set default array property
    if (ClassScope.DefaultProperty<>nil)
        and (ClassScope.DefaultProperty.Parent=PropEl.Parent) then
      RaiseMsg(20170216151938,nOnlyOneDefaultPropertyIsAllowed,sOnlyOneDefaultPropertyIsAllowed,[],PropEl);
    ClassScope.DefaultProperty:=PropEl;
    end;
end;

procedure TPasResolver.FinishArgument(El: TPasArgument);
begin
  if El.ValueExpr<>nil then
    begin
    ResolveExpr(El.ValueExpr,rraRead);
    if El.ArgType<>nil then
      CheckAssignCompatibility(El,El.ValueExpr,true);
    end;
end;

procedure TPasResolver.FinishAncestors(aClass: TPasClassType);
// called when the ancestor and interface list of a class has been parsed,
// before parsing the class elements
var
  AncestorEl: TPasClassType;
  ClassScope, AncestorClassScope: TPasClassScope;
  DirectAncestor, AncestorType, El: TPasType;
  i: Integer;
  aModifier: String;
  IsSealed: Boolean;
begin
  if aClass.IsForward then
    exit;
  if aClass.ObjKind<>okClass then
    RaiseNotYetImplemented(20161010174638,aClass,ObjKindNames[aClass.ObjKind]);

  IsSealed:=false;
  for i:=0 to aClass.Modifiers.Count-1 do
    begin
    aModifier:=lowercase(aClass.Modifiers[i]);
    case aModifier of
    'sealed': IsSealed:=true;
    else
      RaiseMsg(20170320190619,nIllegalQualifier,sIllegalQualifier,[aClass.Modifiers[i]],aClass);
    end;
    end;

  DirectAncestor:=aClass.AncestorType;
  AncestorType:=ResolveAliasType(DirectAncestor);

  if AncestorType=nil then
    begin
    if (CompareText(aClass.Name,'TObject')=0) or aClass.IsExternal then
      begin
        // ok, no ancestors
        AncestorEl:=nil;
      end else begin
        // search default ancestor TObject
        AncestorEl:=TPasClassType(FindElementWithoutParams('TObject',aClass,false));
        if not (AncestorEl is TPasClassType) then
          RaiseXExpectedButYFound(20170216151941,'class type',GetObjName(AncestorEl),aClass);
        if DirectAncestor=nil then
          DirectAncestor:=AncestorEl;
      end;
    end
  else if AncestorType.ClassType<>TPasClassType then
    RaiseXExpectedButYFound(20170216151944,'class type',GetTypeDesc(AncestorType),aClass)
  else
    AncestorEl:=TPasClassType(AncestorType);

  AncestorClassScope:=nil;
  if AncestorEl=nil then
    begin
    // root class e.g. TObject
    end
  else
    begin
    // inherited class
    if AncestorEl.IsForward then
      RaiseMsg(20170216151947,nCantUseForwardDeclarationAsAncestor,
        sCantUseForwardDeclarationAsAncestor,[AncestorEl.Name],aClass);
    if aClass.IsExternal and not AncestorEl.IsExternal then
      RaiseMsg(20170321144035,nAncestorIsNotExternal,sAncestorIsNotExternal,
        [AncestorEl.Name],aClass);
    AncestorClassScope:=AncestorEl.CustomData as TPasClassScope;
    if pcsfSealed in AncestorClassScope.Flags then
      RaiseMsg(20170320191735,nCannotCreateADescendantOfTheSealedClass,
        sCannotCreateADescendantOfTheSealedClass,[AncestorEl.Name],aClass);
    // check for cycle
    El:=AncestorEl;
    repeat
      if El=aClass then
        RaiseMsg(20170216151949,nAncestorCycleDetected,sAncestorCycleDetected,[],aClass);
      if (El.ClassType=TPasAliasType)
      or (El.ClassType=TPasTypeAliasType)
      then
        El:=TPasAliasType(El).DestType
      else if El.ClassType=TPasClassType then
        El:=TPasClassType(El).AncestorType;
    until El=nil;
    end;

  // start scope for elements
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.FinishAncestors ',GetObjName(aClass.CustomData));
  {$ENDIF}
  PushScope(aClass,ScopeClass_Class);
  ClassScope:=TPasClassScope(TopScope);
  ClassScope.VisibilityContext:=aClass;
  Include(ClassScope.Flags,pcsfAncestorResolved);
  if IsSealed then
    Include(ClassScope.Flags,pcsfSealed);
  ClassScope.DirectAncestor:=DirectAncestor;
  if AncestorEl<>nil then
    begin
    ClassScope.AncestorScope:=AncestorEl.CustomData as TPasClassScope;
    ClassScope.DefaultProperty:=ClassScope.AncestorScope.DefaultProperty;
    end;
end;

procedure TPasResolver.FinishParamExpressionAccess(Expr: TPasExpr;
  Access: TResolvedRefAccess);
// called after a call overload was found for each element
// to set the rraParamToUnknownProc to Access
var
  Ref: TResolvedReference;
  Bin: TBinaryExpr;
  Params: TParamsExpr;
begin
  if (Expr.CustomData is TResolvedReference) then
    begin
    Ref:=TResolvedReference(Expr.CustomData);
    if Ref.Access=rraParamToUnknownProc then
      begin
      Ref.Access:=Access;
      exit;
      end;
    end;

  if Expr.ClassType=TBinaryExpr then
    begin
    Bin:=TBinaryExpr(Expr);
    if Bin.OpCode in [eopSubIdent,eopNone] then
      FinishParamExpressionAccess(Bin.right,Access);
    exit;
    end
  else if Expr.ClassType=TParamsExpr then
    begin
    Params:=TParamsExpr(Expr);
    case Params.Kind of
    pekFuncParams:
      if IsTypeCast(Params) then
        FinishParamExpressionAccess(Params.Params[0],Access)
      else
        FinishParamExpressionAccess(Params.Value,Access);
    pekArrayParams:
      FinishParamExpressionAccess(Params.Value,Access);
    pekSet:
      if Access<>rraRead then
        RaiseMsg(20170306112306,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
    end;
    end
  else if ((Expr.ClassType=TPrimitiveExpr) and (TPrimitiveExpr(Expr).Kind=pekIdent))
      or (Expr.ClassType=TSelfExpr) then
    begin
    Ref:=Expr.CustomData as TResolvedReference;
    if Ref.Access<>Access then
      RaiseInternalError(20170306101244);
    end
  else if (Access=rraRead)
      and ((Expr.ClassType=TPrimitiveExpr)
        or (Expr.ClassType=TNilExpr)
        or (Expr.ClassType=TBoolConstExpr)
        or (Expr.ClassType=TUnaryExpr)) then
    // ok
  else
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishParamExpressionAccess Expr=',GetObjName(Expr),' Access=',Access,' Declaration="',Expr.GetDeclaration(false),'"');
    {$ENDIF}
    RaiseNotYetImplemented(20170306102158,Expr);
    end;
end;

procedure TPasResolver.ReplaceProcScopeImplArgsWithDeclArgs(
  ImplProcScope: TPasProcedureScope);
var
  DeclProc, ImplProc: TPasProcedure;
  DeclArgs, ImplArgs: TFPList;
  i: Integer;
  DeclArg, ImplArg: TPasArgument;
  Identifier: TPasIdentifier;
begin
  ImplProc:=ImplProcScope.Element as TPasProcedure;
  ImplArgs:=ImplProc.ProcType.Args;
  DeclProc:=ImplProcScope.DeclarationProc;
  DeclArgs:=DeclProc.ProcType.Args;
  for i:=0 to DeclArgs.Count-1 do
    begin
    DeclArg:=TPasArgument(DeclArgs[i]);
    if i<ImplArgs.Count then
      begin
      ImplArg:=TPasArgument(ImplArgs[i]);
      Identifier:=ImplProcScope.FindLocalIdentifier(DeclArg.Name);
      //writeln('TPasResolver.ReplaceProcScopeImplArgsWithDeclArgs i=',i,' replacing ',GetObjName(ImplArg),' with ',GetObjName(DeclArg));
      if Identifier.Element<>ImplArg then
        RaiseInternalError(20170203161659,GetObjName(DeclArg)+' '+GetObjName(ImplArg));
      Identifier.Element:=DeclArg;
      Identifier.Identifier:=DeclArg.Name;
      end
    else
      RaiseNotYetImplemented(20170203161826,ImplProc);
    end;
  if DeclProc is TPasFunction then
    begin
    // replace 'Result'
    Identifier:=ImplProcScope.FindLocalIdentifier(ResolverResultVar);
    if Identifier.Element is TPasResultElement then
      Identifier.Element:=TPasFunction(DeclProc).FuncType.ResultEl;
    end;
end;

procedure TPasResolver.CheckProcSignatureMatch(DeclProc, ImplProc: TPasProcedure
  );
var
  i: Integer;
  DeclArgs, ImplArgs: TFPList;
  DeclName, ImplName: String;
  ImplResult, DeclResult: TPasType;
begin
  if ImplProc.ClassType<>DeclProc.ClassType then
    RaiseXExpectedButYFound(20170216151729,DeclProc.TypeName,ImplProc.TypeName,ImplProc);
  if ImplProc.CallingConvention<>DeclProc.CallingConvention then
    RaiseMsg(20170216151731,nCallingConventionMismatch,sCallingConventionMismatch,[],ImplProc);
  if ImplProc is TPasFunction then
    begin
    // check result type
    ImplResult:=TPasFunction(ImplProc).FuncType.ResultEl.ResultType;
    DeclResult:=TPasFunction(DeclProc).FuncType.ResultEl.ResultType;

    if not CheckProcArgTypeCompatibility(ImplResult,DeclResult) then
      RaiseIncompatibleType(20170216151734,nResultTypeMismatchExpectedButFound,
        [],DeclResult,ImplResult,ImplProc);
    end;

  // check argument names
  DeclArgs:=DeclProc.ProcType.Args;
  ImplArgs:=ImplProc.ProcType.Args;
  for i:=0 to DeclArgs.Count-1 do
    begin
    DeclName:=TPasArgument(DeclArgs[i]).Name;
    ImplName:=TPasArgument(ImplArgs[i]).Name;
    if CompareText(DeclName,ImplName)<>0 then
      RaiseMsg(20170216151738,nFunctionHeaderMismatchForwardVarName,
        sFunctionHeaderMismatchForwardVarName,[DeclProc.Name,DeclName,ImplName],ImplProc);
    end;
end;

procedure TPasResolver.ResolveImplBlock(Block: TPasImplBlock);
var
  i: Integer;
begin
  if Block=nil then exit;
  for i:=0 to Block.Elements.Count-1 do
    ResolveImplElement(TPasImplElement(Block.Elements[i]));
end;

procedure TPasResolver.ResolveImplElement(El: TPasImplElement);
begin
  //writeln('TPasResolver.ResolveImplElement ',GetObjName(El));
  if El=nil then
  else if El.ClassType=TPasImplBeginBlock then
    ResolveImplBlock(TPasImplBeginBlock(El))
  else if El.ClassType=TPasImplAssign then
    ResolveImplAssign(TPasImplAssign(El))
  else if El.ClassType=TPasImplSimple then
    ResolveImplSimple(TPasImplSimple(El))
  else if El.ClassType=TPasImplBlock then
    ResolveImplBlock(TPasImplBlock(El))
  else if El.ClassType=TPasImplRepeatUntil then
    begin
    ResolveImplBlock(TPasImplBlock(El));
    ResolveStatementConditionExpr(TPasImplRepeatUntil(El).ConditionExpr);
    end
  else if El.ClassType=TPasImplIfElse then
    begin
    ResolveStatementConditionExpr(TPasImplIfElse(El).ConditionExpr);
    ResolveImplElement(TPasImplIfElse(El).IfBranch);
    ResolveImplElement(TPasImplIfElse(El).ElseBranch);
    end
  else if El.ClassType=TPasImplWhileDo then
    begin
    ResolveStatementConditionExpr(TPasImplWhileDo(El).ConditionExpr);
    ResolveImplElement(TPasImplWhileDo(El).Body);
    end
  else if El.ClassType=TPasImplCaseOf then
    ResolveImplCaseOf(TPasImplCaseOf(El))
  else if El.ClassType=TPasImplLabelMark then
    ResolveImplLabelMark(TPasImplLabelMark(El))
  else if El.ClassType=TPasImplForLoop then
    ResolveImplForLoop(TPasImplForLoop(El))
  else if El.ClassType=TPasImplTry then
    begin
    ResolveImplBlock(TPasImplTry(El));
    ResolveImplBlock(TPasImplTry(El).FinallyExcept);
    ResolveImplBlock(TPasImplTry(El).ElseBranch);
    end
  else if El.ClassType=TPasImplExceptOn then
    // handled in FinishExceptOnStatement
  else if El.ClassType=TPasImplRaise then
    ResolveImplRaise(TPasImplRaise(El))
  else if El.ClassType=TPasImplCommand then
    begin
    if TPasImplCommand(El).Command<>'' then
      RaiseNotYetImplemented(20160922163442,El,'TPasResolver.ResolveImplElement');
    end
  else if El.ClassType=TPasImplAsmStatement then
  else if El.ClassType=TPasImplWithDo then
    ResolveImplWithDo(TPasImplWithDo(El))
  else
    RaiseNotYetImplemented(20160922163445,El,'TPasResolver.ResolveImplElement');
end;

procedure TPasResolver.ResolveImplCaseOf(CaseOf: TPasImplCaseOf);
var
  i, j: Integer;
  El: TPasElement;
  Stat: TPasImplCaseStatement;
  CaseExprResolved, OfExprResolved: TPasResolverResult;
  OfExpr: TPasExpr;
  ok: Boolean;
begin
  ResolveExpr(CaseOf.CaseExpr,rraRead);
  ComputeElement(CaseOf.CaseExpr,CaseExprResolved,[]);
  ok:=false;
  if (rrfReadable in CaseExprResolved.Flags) then
    begin
    if (CaseExprResolved.BaseType in (btAllInteger+btAllBooleans+btAllStringAndChars)) then
      ok:=true
    else if CaseExprResolved.BaseType=btContext then
      begin
      if CaseExprResolved.TypeEl.ClassType=TPasEnumType then
        ok:=true;
      end;
    end;
  if not ok then
    RaiseXExpectedButYFound(20170216151952,'ordinal expression',
               GetTypeDesc(CaseExprResolved.TypeEl),CaseOf.CaseExpr);

  for i:=0 to CaseOf.Elements.Count-1 do
    begin
    El:=TPasElement(CaseOf.Elements[i]);
    if El.ClassType=TPasImplCaseStatement then
      begin
      Stat:=TPasImplCaseStatement(El);
      for j:=0 to Stat.Expressions.Count-1 do
        begin
        //writeln('TPasResolver.ResolveImplCaseOf Stat.Expr[',j,']=',GetObjName(El));
        OfExpr:=TPasExpr(Stat.Expressions[j]);
        ResolveExpr(OfExpr,rraRead);
        ComputeElement(OfExpr,OfExprResolved,[rcConstant]);
        if OfExprResolved.BaseType=btRange then
          ConvertRangeToFirstValue(OfExprResolved);
        CheckEqualResCompatibility(CaseExprResolved,OfExprResolved,OfExpr,true);
        end;
      ResolveImplElement(Stat.Body);
      end
    else if El.ClassType=TPasImplCaseElse then
      ResolveImplBlock(TPasImplCaseElse(El))
    else
      RaiseNotYetImplemented(20160922163448,El);
    end;
  // Note: CaseOf.ElseBranch was already resolved via Elements
end;

procedure TPasResolver.ResolveImplLabelMark(Mark: TPasImplLabelMark);
begin
  RaiseNotYetImplemented(20161014141636,Mark);
end;

procedure TPasResolver.ResolveImplForLoop(Loop: TPasImplForLoop);
var
  VarResolved, StartResolved, EndResolved: TPasResolverResult;
begin
  // loop var
  ResolveExpr(Loop.VariableName,rraAssign);
  ComputeElement(Loop.VariableName,VarResolved,[rcNoImplicitProc]);
  if ResolvedElCanBeVarParam(VarResolved)
      and ((VarResolved.BaseType in (btAllBooleans+btAllInteger+[btChar,btWideChar]))
        or ((VarResolved.BaseType=btContext) and (VarResolved.TypeEl.ClassType=TPasEnumType))) then
  else
    RaiseMsg(20170216151955,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Loop.VariableName);

  // start value
  ResolveExpr(Loop.StartExpr,rraRead);
  ComputeElement(Loop.StartExpr,StartResolved,[]);
  if CheckAssignResCompatibility(VarResolved,StartResolved,Loop.StartExpr,true)=cIncompatible then
    RaiseIncompatibleTypeRes(20170216151958,nIncompatibleTypesGotExpected,
      [],StartResolved,VarResolved,Loop.StartExpr);

  // end value
  ResolveExpr(Loop.EndExpr,rraRead);
  ComputeElement(Loop.EndExpr,EndResolved,[]);
  if CheckAssignResCompatibility(VarResolved,EndResolved,Loop.EndExpr,false)=cIncompatible then
    RaiseIncompatibleTypeRes(20170216152001,nIncompatibleTypesGotExpected,
      [],EndResolved,VarResolved,Loop.EndExpr);

  ResolveImplElement(Loop.Body);
end;

procedure TPasResolver.ResolveImplWithDo(El: TPasImplWithDo);
var
  i, OldScopeCount: Integer;
  Expr, ErrorEl: TPasExpr;
  ExprResolved: TPasResolverResult;
  TypeEl: TPasType;
  WithScope: TPasWithScope;
  WithExprScope: TPasWithExprScope;
  ExprScope: TPasScope;
  OnlyTypeMembers: Boolean;
  ClassEl: TPasClassType;
begin
  OldScopeCount:=ScopeCount;
  WithScope:=TPasWithScope(CreateScope(El,TPasWithScope));
  PushScope(WithScope);
  for i:=0 to El.Expressions.Count-1 do
    begin
    Expr:=TPasExpr(El.Expressions[i]);
    ResolveExpr(Expr,rraRead);
    ComputeElement(Expr,ExprResolved,[rcSkipTypeAlias]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveImplWithDo ExprResolved=',GetResolverResultDesc(ExprResolved));
    {$ENDIF}
    ErrorEl:=Expr;
    TypeEl:=ExprResolved.TypeEl;
    // ToDo: use last element in Expr for error position
    if TypeEl=nil then
      RaiseMsg(20170216152004,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [BaseTypeNames[ExprResolved.BaseType]],ErrorEl);

    OnlyTypeMembers:=false;
    if TypeEl.ClassType=TPasRecordType then
      begin
      ExprScope:=TPasRecordType(TypeEl).CustomData as TPasRecordScope;
      if ExprResolved.IdentEl is TPasType then
        // e.g. with TPoint do PointInCircle
        OnlyTypeMembers:=true;
      end
    else if TypeEl.ClassType=TPasClassType then
      begin
      ExprScope:=TPasClassType(TypeEl).CustomData as TPasClassScope;
      if ExprResolved.IdentEl is TPasType then
        // e.g. with TFPMemoryImage do FindHandlerFromExtension()
        OnlyTypeMembers:=true;
      end
    else if TypeEl.ClassType=TPasClassOfType then
      begin
      // e.g. with ImageClass do FindHandlerFromExtension()
      ClassEl:=ResolveAliasType(TPasClassOfType(TypeEl).DestType) as TPasClassType;
      ExprScope:=ClassEl.CustomData as TPasClassScope;
      OnlyTypeMembers:=true;
      end
    else
      RaiseMsg(20170216152007,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [TypeEl.ElementTypeName],ErrorEl);
    WithExprScope:=ScopeClass_WithExpr.Create;
    WithExprScope.WithScope:=WithScope;
    WithExprScope.Index:=i;
    WithExprScope.Expr:=Expr;
    WithExprScope.Scope:=ExprScope;
    if ExprResolved.IdentEl is TPasType then
      Include(WithExprScope.flags,wesfNeedTmpVar);
    if OnlyTypeMembers then
      Include(WithExprScope.flags,wesfOnlyTypeMembers);
    if (not (rrfWritable in ExprResolved.Flags))
        and (ExprResolved.BaseType=btContext)
        and (ExprResolved.TypeEl.ClassType=TPasRecordType) then
      Include(WithExprScope.flags,wesfConstParent);
    WithScope.ExpressionScopes.Add(WithExprScope);
    PushScope(WithExprScope);
    end;
  ResolveImplElement(El.Body);
  CheckTopScope(ScopeClass_WithExpr);
  if TopScope<>WithScope.ExpressionScopes[WithScope.ExpressionScopes.Count-1] then
    RaiseInternalError(20160923102846);
  while ScopeCount>OldScopeCount do
    PopScope;
end;

procedure TPasResolver.ResolveImplAssign(El: TPasImplAssign);
var
  LeftResolved, RightResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
  Access: TResolvedRefAccess;
begin
  if El.Kind=akDefault then
    Access:=rraAssign
  else
    Access:=rraReadAndAssign;
  ResolveExpr(El.left,Access);
  ResolveExpr(El.right,rraRead);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Kind=',El.Kind,' left=',GetObjName(El.left),' right=',GetObjName(el.right));
  {$ENDIF}
  // check LHS can be assigned
  ComputeElement(El.left,LeftResolved,[rcSkipTypeAlias,rcNoImplicitProc]);
  CheckCanBeLHS(LeftResolved,true,El.left);
  // compute RHS
  Flags:=[rcSkipTypeAlias];
  if IsProcedureType(LeftResolved) then
    if (msDelphi in CurrentParser.CurrentModeswitches) then
      Include(Flags,rcNoImplicitProc) // a proc type can use param less procs
    else
      Include(Flags,rcNoImplicitProcType); // a proc type can use a param less proc type
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Left=',GetResolverResultDesc(LeftResolved),' Flags=',dbgs(Flags));
  {$ENDIF}
  ComputeElement(El.right,RightResolved,Flags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Right=',GetResolverResultDesc(RightResolved));
  {$ENDIF}

  case El.Kind of
  akDefault:
    CheckAssignResCompatibility(LeftResolved,RightResolved,El.right,true);
  akAdd, akMinus,akMul,akDivision:
    begin
    if (El.Kind in [akAdd,akMinus,akMul]) and (LeftResolved.BaseType in btAllInteger) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in btAllInteger) then
        RaiseMsg(20170216152009,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.right);
      end
    else if (El.Kind=akAdd) and (LeftResolved.BaseType in btAllStrings) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in btAllStringAndChars) then
        RaiseMsg(20170216152012,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.right);
      end
    else if (El.Kind in [akAdd,akMinus,akMul,akDivision])
        and (LeftResolved.BaseType in btAllFloats) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in (btAllInteger+btAllFloats)) then
        RaiseMsg(20170216152107,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypes[RightResolved.BaseType],BaseTypes[LeftResolved.BaseType]],El.right);
      end
    else if (LeftResolved.BaseType=btSet) and (El.Kind in [akAdd,akMinus,akMul]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType=btSet) then
        RaiseMsg(20170216152110,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[RightResolved.BaseType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.right);
      if (LeftResolved.SubType=RightResolved.SubType)
          or ((LeftResolved.SubType in btAllInteger) and (RightResolved.SubType in btAllInteger))
          or ((LeftResolved.SubType in btAllBooleans) and (RightResolved.SubType in btAllBooleans))
      then
      else
        RaiseMsg(20170216152117,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[RightResolved.SubType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.right);
      end
    else
      RaiseMsg(20170216152125,nIllegalQualifier,sIllegalQualifier,[AssignKindNames[El.Kind]],El);
    end;
  else
    RaiseNotYetImplemented(20160927143649,El,'AssignKind '+AssignKindNames[El.Kind]);
  end;
end;

procedure TPasResolver.ResolveImplSimple(El: TPasImplSimple);
var
  ExprResolved: TPasResolverResult;
  Expr: TPasExpr;
begin
  Expr:=El.expr;
  ResolveExpr(Expr,rraRead);
  ComputeElement(Expr,ExprResolved,[rcSkipTypeAlias]);
  if (rrfCanBeStatement in ExprResolved.Flags) then
    exit;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplSimple El=',GetObjName(El),' El.Expr=',GetObjName(El.Expr),' ExprResolved=',GetResolverResultDesc(ExprResolved));
  {$ENDIF}
  RaiseMsg(20170216152127,nIllegalExpression,sIllegalExpression,[],El);
end;

procedure TPasResolver.ResolveImplRaise(El: TPasImplRaise);
var
  ResolvedEl: TPasResolverResult;
begin
  if El.ExceptObject<>nil then
    begin
    ResolveExpr(El.ExceptObject,rraRead);
    ComputeElement(El.ExceptObject,ResolvedEl,[rcSkipTypeAlias]);
    CheckIsClass(El.ExceptObject,ResolvedEl);
    if ResolvedEl.IdentEl<>nil then
      begin
      if (ResolvedEl.IdentEl is TPasVariable)
          or (ResolvedEl.IdentEl is TPasArgument) then
      else
        RaiseMsg(20170216152133,nXExpectedButYFound,sXExpectedButYFound,
                 ['variable',ResolvedEl.IdentEl.ElementTypeName],El.ExceptObject);
      end
    else if ResolvedEl.ExprEl<>nil then
    else
      RaiseMsg(201702303145230,nXExpectedButYFound,sXExpectedButYFound,
             ['variable',GetResolverResultDesc(ResolvedEl)],El.ExceptObject);
    if not (rrfReadable in ResolvedEl.Flags) then
      RaiseMsg(20170303145037,nNotReadable,sNotReadable,[],El.ExceptObject);
    end;
  if El.ExceptAddr<>nil then
    ResolveExpr(El.ExceptAddr,rraRead);
end;

procedure TPasResolver.ResolveExpr(El: TPasExpr; Access: TResolvedRefAccess);
var
  Primitive: TPrimitiveExpr;
  ElClass: TClass;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveExpr ',GetObjName(El));
  {$ENDIF}
  if El=nil then
    RaiseNotYetImplemented(20160922163453,El);
  ElClass:=El.ClassType;
  if ElClass=TPrimitiveExpr then
    begin
    Primitive:=TPrimitiveExpr(El);
    case Primitive.Kind of
    pekIdent: ResolveNameExpr(El,Primitive.Value,Access);
    pekNumber: ;
    pekString: ;
    pekNil,pekBoolConst: ;
    else
      RaiseNotYetImplemented(20160922163451,El);
    end;
    end
  else if ElClass=TUnaryExpr then
    ResolveExpr(TUnaryExpr(El).Operand,Access)
  else if ElClass=TBinaryExpr then
    ResolveBinaryExpr(TBinaryExpr(El),Access)
  else if ElClass=TParamsExpr then
    ResolveParamsExpr(TParamsExpr(El),Access)
  else if ElClass=TBoolConstExpr then
  else if ElClass=TNilExpr then
  else if ElClass=TSelfExpr then
    ResolveNameExpr(El,'Self',Access)
  else if ElClass=TInheritedExpr then
    ResolveInherited(TInheritedExpr(El),Access)
  else if ElClass=TArrayValues then
    begin
    if Access<>rraRead then
      RaiseMsg(20170303205743,nVariableIdentifierExpected,sVariableIdentifierExpected,
        [],El);
    ResolveArrayValues(TArrayValues(El));
    end
  else
    RaiseNotYetImplemented(20170222184329,El);

  if El.format1<>nil then
    ResolveExpr(El.format1,rraRead);
  if El.format2<>nil then
    ResolveExpr(El.format2,rraRead);
end;

procedure TPasResolver.ResolveStatementConditionExpr(El: TPasExpr);
var
  ResolvedCond: TPasResolverResult;
begin
  ResolveExpr(El,rraRead);
  ComputeElement(El,ResolvedCond,[rcSkipTypeAlias]);
  if ResolvedCond.BaseType<>btBoolean then
    RaiseMsg(20170216152135,nXExpectedButYFound,sXExpectedButYFound,
      [BaseTypeNames[btBoolean],BaseTypeNames[ResolvedCond.BaseType]],El);
end;

procedure TPasResolver.ResolveNameExpr(El: TPasExpr; const aName: string;
  Access: TResolvedRefAccess);
var
  FindData: TPRFindData;
  DeclEl: TPasElement;
  Proc: TPasProcedure;
  Ref: TResolvedReference;
  BuiltInProc: TResElDataBuiltInProc;
begin
  DeclEl:=FindElementWithoutParams(aName,FindData,El,false);
  Ref:=CreateReference(DeclEl,El,Access,@FindData);
  CheckFoundElement(FindData,Ref);
  if DeclEl is TPasProcedure then
    begin
    // identifier is a proc and args brackets are missing
    if El.Parent.ClassType=TPasProperty then
      // a property accessor does not need args -> ok
    else
      begin
      // examples: funca or @proca or a.funca or @a.funca ...
      Proc:=TPasProcedure(DeclEl);
      if ProcNeedsParams(Proc.ProcType) and not ExprIsAddrTarget(El) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveNameExpr ',GetObjName(El));
        {$ENDIF}
        RaiseMsg(20170216152138,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],El);
        end;
      end;
    end
  else if DeclEl.ClassType=TPasUnresolvedSymbolRef then
    begin
    if DeclEl.CustomData is TResElDataBuiltInProc then
      begin
      BuiltInProc:=TResElDataBuiltInProc(DeclEl.CustomData);
      BuiltInProc.GetCallCompatibility(BuiltInProc,El,true);
      end;
    end;
end;

procedure TPasResolver.ResolveInherited(El: TInheritedExpr;
  Access: TResolvedRefAccess);
var
  ProcScope, DeclProcScope: TPasProcedureScope;
  AncestorScope: TPasClassScope;
  DeclProc, AncestorProc: TPasProcedure;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveInherited El.Parent=',GetTreeDesc(El.Parent));
  {$ENDIF}
  if (El.Parent.ClassType=TBinaryExpr)
  and (TBinaryExpr(El.Parent).OpCode=eopNone) then
    begin
    // e.g. 'inherited Proc;'
    ResolveInheritedCall(TBinaryExpr(El.Parent),Access);
    exit;
    end;

  // 'inherited;' without expression
  CheckTopScope(TPasProcedureScope);
  ProcScope:=TPasProcedureScope(TopScope);
  if ProcScope.ClassScope=nil then
    RaiseMsg(20170216152141,nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);

  AncestorScope:=ProcScope.ClassScope.AncestorScope;
  if AncestorScope=nil then
    begin
    // 'inherited;' without ancestor class is silently ignored
    exit;
    end;

  // search ancestor in element, i.e. 'inherited' expression
  DeclProc:=ProcScope.DeclarationProc;
  DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
  AncestorProc:=DeclProcScope.OverriddenProc;
  if AncestorProc<>nil then
    begin
    CreateReference(AncestorProc,El,Access);
    if AncestorProc.IsAbstract then
      RaiseMsg(20170216152144,nAbstractMethodsCannotBeCalledDirectly,
        sAbstractMethodsCannotBeCalledDirectly,[],El);
    end
  else
    begin
    // 'inherited;' without ancestor method is silently ignored
    exit;
    end;
end;

procedure TPasResolver.ResolveInheritedCall(El: TBinaryExpr;
  Access: TResolvedRefAccess);
// El.OpCode=eopNone
// El.left is TInheritedExpr
// El.right is the identifier and parameters
var
  ProcScope: TPasProcedureScope;
  AncestorScope: TPasClassScope;
  AncestorClass: TPasClassType;
  InhScope: TPasDotClassScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveInheritedCall El=',GetTreeDesc(El));
  {$ENDIF}

  CheckTopScope(TPasProcedureScope);
  ProcScope:=TPasProcedureScope(TopScope);
  if ProcScope.ClassScope=nil then
    RaiseMsg(20170216152148,nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);

  AncestorScope:=ProcScope.ClassScope.AncestorScope;
  if AncestorScope=nil then
    RaiseMsg(20170216152151,nInheritedNeedsAncestor,sInheritedNeedsAncestor,[],El.left);

  // search call in ancestor
  AncestorClass:=TPasClassType(AncestorScope.Element);
  InhScope:=PushClassDotScope(AncestorClass);
  InhScope.InheritedExpr:=true;
  ResolveExpr(El.right,Access);
  PopScope;
end;

procedure TPasResolver.ResolveBinaryExpr(El: TBinaryExpr;
  Access: TResolvedRefAccess);
begin
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.ResolveBinaryExpr left=',GetObjName(El.left),' right=',GetObjName(El.right),' opcode=',OpcodeStrings[El.OpCode]);
  {$ENDIF}
  ResolveExpr(El.left,rraRead);
  if El.right=nil then exit;
  case El.OpCode of
  eopNone:
    case El.Kind of
    pekRange:
      ResolveExpr(El.right,rraRead);
    else
      if El.left.ClassType=TInheritedExpr then
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveBinaryExpr El.Kind=',ExprKindNames[El.Kind],' El.Left=',GetObjName(El.left),' El.Right=',GetObjName(El.right),' parent=',GetObjName(El.Parent));
        {$ENDIF}
        RaiseNotYetImplemented(20160922163456,El);
        end;
    end;
  eopAdd,
  eopSubtract,
  eopMultiply,
  eopDivide,
  eopDiv,
  eopMod,
  eopPower,
  eopShr,
  eopShl,
  eopNot,
  eopAnd,
  eopOr,
  eopXor,
  eopEqual,
  eopNotEqual,
  eopLessThan,
  eopGreaterThan,
  eopLessthanEqual,
  eopGreaterThanEqual,
  eopIn,
  eopIs,
  eopAs,
  eopSymmetricaldifference:
    ResolveExpr(El.right,rraRead);
  eopSubIdent:
    ResolveSubIdent(El,Access);
  else
    RaiseNotYetImplemented(20160922163459,El,OpcodeStrings[El.OpCode]);
  end;
end;

procedure TPasResolver.ResolveSubIdent(El: TBinaryExpr;
  Access: TResolvedRefAccess);
var
  aModule: TPasModule;
  ClassEl: TPasClassType;
  ClassScope: TPasDotClassScope;
  LeftResolved: TPasResolverResult;
  Left: TPasExpr;
  RecordEl: TPasRecordType;
  RecordScope: TPasDotRecordScope;
begin
  Left:=El.left;
  //writeln('TPasResolver.ResolveSubIdent Left=',GetObjName(Left));
  ComputeElement(Left,LeftResolved,[]);

  if LeftResolved.BaseType=btModule then
    begin
    // e.g. unitname.identifier
    // => search in interface and if this is our module in the implementation
    aModule:=LeftResolved.IdentEl as TPasModule;
    PushModuleDotScope(aModule);
    ResolveExpr(El.right,Access);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl=nil then
    begin
    // illegal qualifier, see below
    end
  else if LeftResolved.TypeEl.ClassType=TPasClassType then
    begin
    ClassEl:=TPasClassType(LeftResolved.TypeEl);
    ClassScope:=PushClassDotScope(ClassEl);
    if LeftResolved.IdentEl is TPasType then
      // e.g. TFPMemoryImage.FindHandlerFromExtension()
      ClassScope.OnlyTypeMembers:=true
    else
      // e.g. Image.Width
      ClassScope.OnlyTypeMembers:=false;
    ResolveExpr(El.right,Access);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl.ClassType=TPasClassOfType then
    begin
    // e.g. ImageClass.FindHandlerFromExtension()
    ClassEl:=ResolveAliasType(TPasClassOfType(LeftResolved.TypeEl).DestType) as TPasClassType;
    ClassScope:=PushClassDotScope(ClassEl);
    ClassScope.OnlyTypeMembers:=true;
    ResolveExpr(El.right,Access);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl.ClassType=TPasRecordType then
    begin
    RecordEl:=TPasRecordType(LeftResolved.TypeEl);
    RecordScope:=PushRecordDotScope(RecordEl);
    RecordScope.ConstParent:=not (rrfWritable in LeftResolved.Flags);
    if LeftResolved.IdentEl is TPasType then
      // e.g. TPoint.PointInCircle
      RecordScope.OnlyTypeMembers:=true
    else
      // e.g. aPoint.X
      RecordScope.OnlyTypeMembers:=false;
    ResolveExpr(El.right,Access);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl.ClassType=TPasEnumType then
    begin
    if LeftResolved.IdentEl is TPasType then
      begin
      // e.g. TShiftState.ssAlt
      PushEnumDotScope(TPasEnumType(LeftResolved.TypeEl));
      ResolveExpr(El.right,Access);
      PopScope;
      exit;
      end;
    end
  else
    RaiseMsg(20170216152541,nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
      [LeftResolved.TypeEl.ElementTypeName],El);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSubIdent left=',GetObjName(Left),' right=',GetObjName(El.right),' leftresolved=',GetResolverResultDesc(LeftResolved));
  {$ENDIF}
  RaiseMsg(20170216152157,nIllegalQualifier,sIllegalQualifier,['.'],El);
end;

procedure TPasResolver.ResolveParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);
var
  i, ScopeDepth: Integer;
  ParamAccess: TResolvedRefAccess;
begin
  if (Params.Kind=pekSet) and not (Access in [rraRead,rraParamToUnknownProc]) then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveParamsExpr SET literal Access=',Access);
    {$ENDIF}
    RaiseMsg(20170303211052,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
    end;

  // first resolve params
  ResetSubScopes(ScopeDepth);
  if Params.Kind in [pekFuncParams,pekArrayParams] then
    ParamAccess:=rraParamToUnknownProc
  else
    ParamAccess:=rraRead;
  for i:=0 to length(Params.Params)-1 do
    ResolveExpr(Params.Params[i],ParamAccess);
  RestoreSubScopes(ScopeDepth);

  // then resolve the call, typecast, array, set
  if (Params.Kind=pekFuncParams) then
    ResolveFuncParamsExpr(Params,Access)
  else if (Params.Kind=pekArrayParams) then
    ResolveArrayParamsExpr(Params,Access)
  else if (Params.Kind=pekSet) then
    ResolveSetParamsExpr(Params)
  else
    RaiseNotYetImplemented(20160922163501,Params);
end;

procedure TPasResolver.ResolveFuncParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);

  procedure FinishProcParams(ProcType: TPasProcedureType);
  var
    ParamAccess: TResolvedRefAccess;
    i: Integer;
  begin
    if not (Access in [rraRead,rraParamToUnknownProc]) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveFuncParamsExpr.FinishProcParams Params=',GetObjName(Params),' Value=',GetObjName(Params.Value),' Access=',Access);
      {$ENDIF}
      RaiseMsg(20170306104440,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Params);
      end;
    for i:=0 to length(Params.Params)-1 do
      begin
      ParamAccess:=rraRead;
      if i<ProcType.Args.Count then
        case TPasArgument(ProcType.Args[i]).Access of
        argVar: ParamAccess:=rraVarParam;
        argOut: ParamAccess:=rraOutParam;
        end;
      FinishParamExpressionAccess(Params.Params[i],ParamAccess);
      end;
  end;

var
  i: Integer;
  ElName, Msg: String;
  FindCallData: TFindCallElData;
  Abort: boolean;
  El, FoundEl: TPasElement;
  Ref: TResolvedReference;
  FindData: TPRFindData;
  BuiltInProc: TResElDataBuiltInProc;
  SubParams: TParamsExpr;
  ResolvedEl: TPasResolverResult;
  Value: TPasExpr;
  TypeEl: TPasType;
  C: TClass;
begin
  Value:=Params.Value;
  if (Value.ClassType=TSelfExpr)
      or ((Value.ClassType=TPrimitiveExpr)
        and (TPrimitiveExpr(Value).Kind=pekIdent)) then
    begin
    // e.g. Name() -> find compatible
    if Value.ClassType=TPrimitiveExpr then
      ElName:=TPrimitiveExpr(Value).Value
    else
      ElName:='Self';
    FindCallData:=Default(TFindCallElData);
    FindCallData.Params:=Params;
    Abort:=false;
    IterateElements(ElName,@OnFindCallElements,@FindCallData,Abort);
    if FindCallData.Found=nil then
      RaiseIdentifierNotFound(20170216152544,ElName,Value);
    if FindCallData.Distance=cIncompatible then
      begin
      // FoundEl one element, but it was incompatible => raise error
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveParamsExpr found one element, but it was incompatible => check again to raise error');
      {$ENDIF}
      if FindCallData.Found is TPasProcedure then
        CheckCallProcCompatibility(TPasProcedure(FindCallData.Found).ProcType,Params,true)
      else if FindCallData.Found is TPasProcedureType then
        CheckCallProcCompatibility(TPasProcedureType(FindCallData.Found),Params,true)
      else if FindCallData.Found.ClassType=TPasUnresolvedSymbolRef then
        begin
        if FindCallData.Found.CustomData is TResElDataBuiltInProc then
          begin
          BuiltInProc:=TResElDataBuiltInProc(FindCallData.Found.CustomData);
          BuiltInProc.GetCallCompatibility(BuiltInProc,Params,true);
          end
        else if FindCallData.Found.CustomData is TResElDataBaseType then
          CheckTypeCast(TPasUnresolvedSymbolRef(FindCallData.Found),Params,true)
        else
          RaiseNotYetImplemented(20161006132825,FindCallData.Found);
        end
      else if FindCallData.Found is TPasType then
        // Note: check TPasType after TPasUnresolvedSymbolRef
        CheckTypeCast(TPasType(FindCallData.Found),Params,true)
      else
        RaiseNotYetImplemented(20161003134755,FindCallData.Found);
      end;
    if FindCallData.Count>1 then
      begin
      // multiple overloads fit => search again and list the candidates
      FindCallData:=Default(TFindCallElData);
      FindCallData.Params:=Params;
      FindCallData.List:=TFPList.Create;
      try
        IterateElements(ElName,@OnFindCallElements,@FindCallData,Abort);
        Msg:='';
        for i:=0 to FindCallData.List.Count-1 do
          begin
          // ToDo: create a hint for each candidate
          El:=TPasElement(FindCallData.List[i]);
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ResolveParamsExpr Overload Candidate: ',GetElementSourcePosStr(El),' ',GetTreeDesc(El));
          {$ENDIF}
          Msg:=Msg+', ';
          Msg:=Msg+GetElementSourcePosStr(El);
          end;
        RaiseMsg(20170216152200,nCantDetermineWhichOverloadedFunctionToCall,
          sCantDetermineWhichOverloadedFunctionToCall+Msg,[ElName],Value);
      finally
        FindCallData.List.Free;
      end;
      end;

    // FoundEl compatible element -> create reference
    FoundEl:=FindCallData.Found;
    Ref:=CreateReference(FoundEl,Value,rraRead);
    if FindCallData.StartScope.ClassType=ScopeClass_WithExpr then
      Ref.WithExprScope:=TPasWithExprScope(FindCallData.StartScope);
    FindData:=Default(TPRFindData);
    FindData.ErrorPosEl:=Value;
    FindData.StartScope:=FindCallData.StartScope;
    FindData.ElScope:=FindCallData.ElScope;
    FindData.Found:=FoundEl;
    CheckFoundElement(FindData,Ref);

    // set param expression Access flags
    if FoundEl is TPasProcedure then
      // call proc
      FinishProcParams(TPasProcedure(FoundEl).ProcType)
    else if FoundEl is TPasType then
      begin
      TypeEl:=ResolveAliasType(TPasType(FoundEl));
      C:=TypeEl.ClassType;
      if (C=TPasClassType)
          or (C=TPasClassOfType)
          or (C=TPasEnumType)
          or (C=TPasArrayType) then
        begin
        // type cast
        for i:=0 to length(Params.Params)-1 do
          FinishParamExpressionAccess(Params.Params[i],Access);
        end
      else if C=TPasUnresolvedSymbolRef then
        begin
        if TypeEl.CustomData is TResElDataBuiltInProc then
          begin
          // call built-in proc
          BuiltInProc:=TResElDataBuiltInProc(TypeEl.CustomData);
          if Assigned(BuiltInProc.FinishParamsExpression) then
            BuiltInProc.FinishParamsExpression(BuiltInProc,Params)
          else
            for i:=0 to length(Params.Params)-1 do
              FinishParamExpressionAccess(Params.Params[i],rraRead);
          end
        else if TypeEl.CustomData is TResElDataBaseType then
          begin
          // type cast to base type
          for i:=0 to length(Params.Params)-1 do
            FinishParamExpressionAccess(Params.Params[i],Access);
          end
        else
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData));
          {$ENDIF}
          RaiseNotYetImplemented(20170325145720,Params);
          end;
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData));
        {$ENDIF}
        RaiseNotYetImplemented(20170306121908,Params);
        end;
      end
    else
      begin
      ComputeElement(FoundEl,ResolvedEl,[rcNoImplicitProc]);
      if ResolvedEl.TypeEl is TPasProcedureType then
        begin
        FinishProcParams(TPasProcedureType(ResolvedEl.TypeEl));
        exit;
        end;
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveFuncParamsExpr FoundEl=',GetObjName(FoundEl),' CustomData=',GetObjName(FoundEl.CustomData),' Resolvedel=',GetResolverResultDesc(ResolvedEl));
      {$ENDIF}
      RaiseNotYetImplemented(20170306104301,Params);
      end;
    end
  else if Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Value);
    if (SubParams.Kind in [pekArrayParams,pekFuncParams]) then
      begin
      // e.g. Name()() or Name[]()
      ResolveExpr(SubParams,rraRead);
      ComputeElement(SubParams,ResolvedEl,[rcNoImplicitProc]);
      if IsProcedureType(ResolvedEl) and (rrfReadable in ResolvedEl.Flags) then
        begin
        CheckCallProcCompatibility(TPasProcedureType(ResolvedEl.TypeEl),Params,true);
        CreateReference(ResolvedEl.TypeEl,Value,Access);
        exit;
        end
      end;
    RaiseMsg(20170216152202,nIllegalQualifier,sIllegalQualifier,['('],Params);
    end
  else
    RaiseNotYetImplemented(20161014085118,Params.Value);
end;

procedure TPasResolver.ResolveArrayParamsExpr(Params: TParamsExpr;
  Access: TResolvedRefAccess);

  procedure FinishPropertyParamAccess(Prop: TPasProperty);
  var
    i: Integer;
    ParamAccess: TResolvedRefAccess;
  begin
    for i:=0 to length(Params.Params)-1 do
      begin
      ParamAccess:=rraRead;
      if i<Prop.Args.Count then
        case TPasArgument(Prop.Args[i]).Access of
        argVar: ParamAccess:=rraVarParam;
        argOut: ParamAccess:=rraOutParam;
        end;
      FinishParamExpressionAccess(Params.Params[i],ParamAccess);
      end;
  end;

var
  ArrayName: String;
  FindData: TPRFindData;
  DeclEl: TPasElement;
  ResolvedEl, ResolvedArg: TPasResolverResult;
  ArgExp, Value: TPasExpr;
  Ref: TResolvedReference;
  PropEl: TPasProperty;
  ClassScope: TPasClassScope;
  SubParams: TParamsExpr;
  i: Integer;
begin
  DeclEl:=nil;
  Value:=Params.Value;
  if (Value.ClassType=TPrimitiveExpr)
      and (TPrimitiveExpr(Value).Kind=pekIdent) then
    begin
    // e.g. Name[]
    ArrayName:=TPrimitiveExpr(Value).Value;
    // find first
    DeclEl:=FindElementWithoutParams(ArrayName,FindData,Value,true);
    Ref:=CreateReference(DeclEl,Params.Value,Access,@FindData);
    CheckFoundElement(FindData,Ref);
    ComputeElement(Value,ResolvedEl,[rcSkipTypeAlias]);
    end
  else if (Value.ClassType=TSelfExpr) then
    begin
    // e.g. Self[]
    ResolveNameExpr(Value,'Self',Access);
    ComputeElement(Value,ResolvedEl,[rcSkipTypeAlias]);
    end
  else if Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Value);
    if (SubParams.Kind in [pekArrayParams,pekFuncParams]) then
      begin
      // e.g. Name()[] or Name[][]
      ResolveExpr(SubParams,rraRead);
      ComputeElement(SubParams,ResolvedEl,[rcSkipTypeAlias,rcNoImplicitProc]);
      CreateReference(ResolvedEl.TypeEl,Value,Access);
      end
    else
      RaiseNotYetImplemented(20161010194925,Value);
    end
  else
    RaiseNotYetImplemented(20160927212610,Value);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveArrayParamsExpr Value=',GetObjName(Value),' ',GetResolverResultDesc(ResolvedEl));
  {$ENDIF}
  if ResolvedEl.BaseType in btAllStrings then
    begin
    // string -> check that ResolvedEl is not merely a type, but has a value
    if not ResolvedElHasValue(ResolvedEl) then
      RaiseXExpectedButYFound(20170216152548,'variable',ResolvedEl.TypeEl.ElementTypeName,Params);
    // check single argument
    if length(Params.Params)<1 then
      RaiseMsg(20170216152204,nMissingParameterX,
        sMissingParameterX,['character index'],Params)
    else if length(Params.Params)>1 then
      RaiseMsg(20170216152551,nIllegalQualifier,sIllegalQualifier,[','],Params.Params[1]);
    // check argument is integer
    ArgExp:=Params.Params[0];
    ComputeElement(ArgExp,ResolvedArg,[rcSkipTypeAlias]);
    if not (ResolvedArg.BaseType in btAllInteger) then
      RaiseMsg(20170216152209,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [BaseTypeNames[ResolvedArg.BaseType],BaseTypeNames[BaseTypeStringIndex]],ArgExp);
    if not (rrfReadable in ResolvedArg.Flags) then
      RaiseMsg(20170216152211,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        ['type','value'],ArgExp);
    FinishParamExpressionAccess(ArgExp,rraRead);
    exit;
    end
  else if (ResolvedEl.IdentEl is TPasProperty)
      and (TPasProperty(ResolvedEl.IdentEl).Args.Count>0) then
    begin
    PropEl:=TPasProperty(ResolvedEl.IdentEl);
    CheckCallPropertyCompatibility(PropEl,Params,true);
    FinishPropertyParamAccess(PropEl);
    exit;
    end
  else if ResolvedEl.BaseType=btContext then
    begin
    if ResolvedEl.TypeEl.ClassType=TPasClassType then
      begin
      ClassScope:=ResolvedEl.TypeEl.CustomData as TPasClassScope;
      PropEl:=ClassScope.DefaultProperty;
      if PropEl<>nil then
        begin
        // class has default property
        if (ResolvedEl.IdentEl is TPasType) and (not PropEl.IsClass) then
          RaiseMsg(20170216152213,nIllegalQualifier,sIllegalQualifier,['['],Params);
        CheckCallPropertyCompatibility(PropEl,Params,true);
        FinishPropertyParamAccess(PropEl);
        exit;
        end;
      end
    else if ResolvedEl.TypeEl.ClassType=TPasArrayType then
      begin
      if ResolvedEl.IdentEl is TPasType then
        RaiseMsg(20170216152215,nIllegalQualifier,sIllegalQualifier,['['],Params);
      CheckCallArrayCompatibility(TPasArrayType(ResolvedEl.TypeEl),Params,true);
      for i:=0 to length(Params.Params)-1 do
        FinishParamExpressionAccess(Params.Params[i],rraRead);
      exit;
      end;
    end;
  RaiseMsg(20170216152217,nIllegalQualifier,sIllegalQualifier,['['],Params);
end;

procedure TPasResolver.ResolveSetParamsExpr(Params: TParamsExpr);
// e.g. resolving '[1,2..3]'
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSetParamsExpr ',GetTreeDesc(Params));
  {$ENDIF}
  if Params.Value<>nil then
    RaiseNotYetImplemented(20160930135910,Params);
end;

procedure TPasResolver.ResolveArrayValues(El: TArrayValues);
var
  i: Integer;
begin
  for i:=0 to length(El.Values)-1 do
    ResolveExpr(El.Values[i],rraRead);
end;

procedure TPasResolver.CheckPendingForwards(El: TPasElement);
var
  i: Integer;
  DeclEl: TPasElement;
  Proc: TPasProcedure;
  aClassType: TPasClassType;
begin
  if El is TPasDeclarations then
    begin
    for i:=0 to TPasDeclarations(El).Declarations.Count-1 do
      begin
      DeclEl:=TPasElement(TPasDeclarations(El).Declarations[i]);
      if DeclEl is TPasProcedure then
        begin
        Proc:=TPasProcedure(DeclEl);
        if ProcNeedsImplProc(Proc)
            and (TPasProcedureScope(Proc.CustomData).ImplProc=nil) then
          RaiseMsg(20170216152219,nForwardProcNotResolved,sForwardProcNotResolved,
            [Proc.ElementTypeName,Proc.Name],Proc);
        end;
      end;
    end
  else if El.ClassType=TPasClassType then
    begin
    aClassType:=TPasClassType(El);
    for i:=0 to aClassType.Members.Count-1 do
      begin
      DeclEl:=TPasElement(aClassType.Members[i]);
      if DeclEl is TPasProcedure then
        begin
        Proc:=TPasProcedure(DeclEl);
        if Proc.IsAbstract or Proc.IsExternal then continue;
        if TPasProcedureScope(Proc.CustomData).ImplProc=nil then
          RaiseMsg(20170216152221,nForwardProcNotResolved,sForwardProcNotResolved,
            [Proc.ElementTypeName,Proc.Name],Proc);
        end;
      end;
    end;
end;

procedure TPasResolver.AddModule(El: TPasModule);
begin
  if TopScope<>DefaultScope then
    RaiseInvalidScopeForElement(20160922163504,El);
  PushScope(El,TPasModuleScope);
  TPasModuleScope(TopScope).VisibilityContext:=El;
end;

procedure TPasResolver.AddSection(El: TPasSection);
// TInterfaceSection, TImplementationSection, TProgramSection, TLibrarySection
// Note: implementation scope is within the interface scope
begin
  FPendingForwards.Add(El); // check forward declarations at the end
  PushScope(El,TPasSectionScope);
end;

procedure TPasResolver.AddType(El: TPasType);
begin
  if (El.Name='') then exit; // sub type
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddType El=',GetObjName(El),' El.Parent=',GetObjName(El.Parent));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163506,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddRecordType(El: TPasRecordType);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddRecordType ',GetObjName(El),' Parent=',GetObjName(El.Parent));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163508,El);
  if El.Name<>'' then begin
    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
    FPendingForwards.Add(El); // check forward declarations at the end
  end;

  if El.Parent.ClassType<>TPasVariant then
    PushScope(El,TPasRecordScope);
end;

procedure TPasResolver.AddClassType(El: TPasClassType);
var
  Duplicate: TPasIdentifier;
  ForwardDecl: TPasClassType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddClassType ',GetObjName(El),' Parent=',GetObjName(El.Parent),' ',GetElementSourcePosStr(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163510,El);

  Duplicate:=TPasIdentifierScope(TopScope).FindIdentifier(El.Name);
  //if Duplicate<>nil then
    //writeln('  Duplicate=',GetObjName(Duplicate.Element),' ',ord(Duplicate.Kind));

  if (Duplicate<>nil)
      and (Duplicate.Kind=pikSimple)
      and (Duplicate.Element<>nil)
      and (Duplicate.Element.Parent=El.Parent)
      and (Duplicate.Element is TPasClassType)
      and TPasClassType(Duplicate.Element).IsForward
  then
    begin
    // forward declaration found
    ForwardDecl:=TPasClassType(Duplicate.Element);
    {$IFDEF VerbosePasResolver}
    writeln('  Resolving Forward=',GetObjName(ForwardDecl),' ',GetElementSourcePosStr(ForwardDecl));
    {$ENDIF}
    if ForwardDecl.CustomData<>nil then
      RaiseInternalError(20160922163513,'forward class has already customdata');
    // create a ref from the forward to the real declaration
    CreateReference(El,ForwardDecl,rraRead);
    // change the cache item
    Duplicate.Element:=El;
    end
  else
    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);

  FPendingForwards.Add(El); // check forward declarations at the end
end;

procedure TPasResolver.AddVariable(El: TPasVariable);
begin
  if (El.Name='') then exit; // anonymous var
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddVariable ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160929205730,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddEnumType(El: TPasEnumType);
var
  CanonicalSet: TPasSetType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddEnumType ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160929205732,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
  PushScope(El,TPasEnumTypeScope);
  // add canonical set
  CanonicalSet:=TPasSetType.Create('',El);
  CanonicalSet.EnumType:=El;
  El.AddRef;
  TPasEnumTypeScope(TopScope).CanonicalSet:=CanonicalSet;
end;

procedure TPasResolver.AddEnumValue(El: TPasEnumValue);
var
  i: Integer;
  Scope: TPasScope;
  Old: TPasIdentifier;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddEnumValue ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasEnumTypeScope) then
    RaiseInvalidScopeForElement(20160929205736,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);

  // propagate enum to parent scopes
  for i:=ScopeCount-2 downto 0 do
    begin
    Scope:=Scopes[i];
    if (Scope is TPasClassScope) or (Scope is TPasRecordScope) then
      begin
      // class or record: add if not duplicate
      Old:=TPasIdentifierScope(Scope).FindIdentifier(El.Name);
      if Old=nil then
        TPasIdentifierScope(Scope).AddIdentifier(El.Name,El,pikSimple);
      end
    else if (Scope is TPasProcedureScope) or (Scope is TPasSectionScope) then
      begin
      // procedure or section: check for duplicate and add
      Old:=TPasIdentifierScope(Scope).FindLocalIdentifier(El.Name);
      if Old<>nil then
        RaiseMsg(20170216152224,nDuplicateIdentifier,sDuplicateIdentifier,
                 [El.Name,GetElementSourcePosStr(Old.Element)],El);
      TPasIdentifierScope(Scope).AddIdentifier(El.Name,El,pikSimple);
      break;
      end
    else
      break;
    end;
end;

procedure TPasResolver.AddProperty(El: TPasProperty);
begin
  if (El.Name='') then
    RaiseNotYetImplemented(20160922163518,El);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProperty ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasClassScope) then
    RaiseInvalidScopeForElement(20160922163520,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
  PushScope(El,TPasPropertyScope);
end;

procedure TPasResolver.AddProcedure(El: TPasProcedure);
var
  ProcName, aClassName: String;
  p: SizeInt;
  CurClassType: TPasClassType;
  ProcScope: TPasProcedureScope;
  NeedPop, HasDot: Boolean;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProcedure ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163522,El);
  // Note: El.ProcType is nil !
  ProcName:=El.Name;
  HasDot:=Pos('.',ProcName)>1;
  if not HasDot then
    AddIdentifier(TPasIdentifierScope(TopScope),ProcName,El,pikProc);
  ProcScope:=TPasProcedureScope(PushScope(El,TPasProcedureScope));
  if HasDot then
    begin
    // method implementation -> search class
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.AddProcedure searching class of "',ProcName,'" ...');
    {$ENDIF}
    CurClassType:=nil;
    repeat
      p:=Pos('.',ProcName);
      if p<1 then
        begin
        if CurClassType=nil then
          RaiseInternalError(20161013170829);
        break;
        end;
      aClassName:=LeftStr(ProcName,p-1);
      Delete(ProcName,1,p);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.AddProcedure searching class "',aClassName,'" ProcName="',ProcName,'" ...');
      {$ENDIF}
      if not IsValidIdent(aClassName) then
        RaiseNotYetImplemented(20161013170844,El);

      if CurClassType<>nil then
        begin
        NeedPop:=true;
        PushClassDotScope(CurClassType);
        end
      else
        NeedPop:=false;

      CurClassType:=TPasClassType(FindElementWithoutParams(aClassName,El,false));
      if not (CurClassType is TPasClassType) then
        begin
        aClassName:=LeftStr(El.Name,length(El.Name)-length(ProcName));
        RaiseXExpectedButYFound(20170216152557,'class',aClassname+':'+CurClassType.ElementTypeName,El);
        end;

      // restore scope
      if NeedPop then
        PopScope;
    until false;

    if not IsValidIdent(ProcName) then
      RaiseNotYetImplemented(20161013170956,El);

    ProcScope.VisibilityContext:=CurClassType;
    ProcScope.ClassScope:=CurClassType.CustomData as TPasClassScope;
    end;
end;

procedure TPasResolver.AddArgument(El: TPasArgument);
var
  ProcType: TPasProcedureType;
  i: Integer;
  Arg: TPasArgument;
begin
  if (El.Name='') then
    RaiseInternalError(20160922163526,GetObjName(El));
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddArgument ',GetObjName(El));
  {$ENDIF}
  if (TopScope=nil) then
    RaiseInvalidScopeForElement(20160922163529,El);
  if El.Parent.ClassType=TPasProperty then
    begin
    if TopScope.ClassType<>TPasPropertyScope then
      RaiseInvalidScopeForElement(20161014124530,El);
    AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
    end
  else if El.Parent is TPasProcedureType then
    begin
    ProcType:=TPasProcedureType(El.Parent);
    if ProcType.Parent is TPasProcedure then
      begin
      if TopScope.ClassType<>TPasProcedureScope then
        RaiseInvalidScopeForElement(20160922163529,El);
      AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
      end
    else
      begin
      for i:=0 to ProcType.Args.Count-1 do
        begin
        Arg:=TPasArgument(ProcType.Args[i]);
        if (Arg<>El) and (CompareText(TPasArgument(ProcType.Args[i]).Name,El.Name)=0) then
          RaiseMsg(20170216152225,nDuplicateIdentifier,sDuplicateIdentifier,[Arg.Name,GetElementSourcePosStr(Arg)],El);
        end;
      end;
    end
  else
    RaiseNotYetImplemented(20161014124937,El);
end;

procedure TPasResolver.AddFunctionResult(El: TPasResultElement);
begin
  if TopScope.ClassType<>TPasProcedureScope then exit;
  AddIdentifier(TPasProcedureScope(TopScope),ResolverResultVar,El,pikSimple);
end;

procedure TPasResolver.AddExceptOn(El: TPasImplExceptOn);
begin
  PushScope(El,TPasExceptOnScope);
end;

procedure TPasResolver.AddProcedureBody(El: TProcedureBody);
begin
  if El=nil then ;
  CheckTopScope(TPasProcedureScope);
end;

procedure TPasResolver.WriteScopes;
var
  i: Integer;
  Scope: TPasScope;
begin
  writeln('TPasResolver.WriteScopes ScopeCount=',ScopeCount);
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    writeln('  ',i,'/',ScopeCount,' ',GetObjName(Scope));
    Scope.WriteIdentifiers('  ');
    end;
end;

procedure TPasResolver.ComputeBinaryExpr(Bin: TBinaryExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);

  procedure SetBaseType(BaseType: TResolverBaseType);
  begin
    SetResolverValueExpr(ResolvedEl,BaseType,FBaseTypes[BaseType],Bin,[rrfReadable]);
  end;

var
  LeftResolved, RightResolved: TPasResolverResult;
  LeftTypeEl, RightTypeEl: TPasType;
begin
  if (Bin.OpCode=eopSubIdent)
  or ((Bin.OpCode=eopNone) and (Bin.left is TInheritedExpr)) then
    begin
    // Note: bin.left was already resolved via ResolveSubIdent
    ComputeElement(Bin.right,ResolvedEl,Flags,StartEl);
    exit;
    end;

  if Bin.OpCode in [eopEqual,eopNotEqual] then
    begin
    if CheckEqualElCompatibility(Bin.left,Bin.right,nil,true)=cIncompatible then
      RaiseInternalError(20161007215912);
    SetBaseType(btBoolean);
    exit;
    end;

  ComputeElement(Bin.left,LeftResolved,Flags-[rcNoImplicitProc],StartEl);
  ComputeElement(Bin.right,RightResolved,Flags-[rcNoImplicitProc],StartEl);
  // ToDo: check operator overloading

  //writeln('TPasResolver.ComputeBinaryExpr ',OpcodeStrings[Bin.OpCode],' Left=',GetResolverResultDesc(LeftResolved),' Right=',GetResolverResultDesc(RightResolved));

  if LeftResolved.BaseType in btAllInteger then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
        if (RightResolved.BaseType in (btAllInteger+btAllFloats)) then
          case Bin.OpCode of
          eopNone:
            if (Bin.Kind=pekRange) then
              begin
              if not (RightResolved.BaseType in btAllInteger) then
                RaiseXExpectedButYFound(20170216152600,'integer',BaseTypeNames[RightResolved.BaseType],Bin.right);
              SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin,[rrfReadable]);
              exit;
              end;
          eopAdd, eopSubtract,
          eopMultiply, eopDiv, eopMod,
          eopPower,
          eopShl, eopShr,
          eopAnd, eopOr, eopXor:
            begin
            // use left type for result
            SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin,[rrfReadable]);
            exit;
            end;
          eopLessThan,
          eopGreaterThan,
          eopLessthanEqual,
          eopGreaterThanEqual:
            begin
            SetBaseType(btBoolean);
            exit;
            end;
          end
        else if (RightResolved.BaseType=btSet) and (RightResolved.SubType in btAllInteger)
            and (Bin.OpCode=eopIn) then
          begin
          SetBaseType(btBoolean);
          exit;
          end;
      end;
    end
  else if LeftResolved.BaseType in btAllBooleans then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType in btAllBooleans)
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopNone:
        if Bin.Kind=pekRange then
          begin
          SetResolverValueExpr(ResolvedEl,btRange,FBaseTypes[LeftResolved.BaseType],Bin,[rrfReadable]);
          ResolvedEl.SubType:=LeftResolved.BaseType;
          exit;
          end;
      eopAnd, eopOr, eopXor:
        begin
        // use left type for result
        SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin,[rrfReadable]);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType in btAllStringAndChars then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (rrfReadable in RightResolved.Flags) then
      begin
      if (RightResolved.BaseType in btAllStringAndChars) then
        case Bin.OpCode of
        eopNone:
          if (Bin.Kind=pekRange) and (LeftResolved.BaseType in [btChar]) then
            begin
            if RightResolved.BaseType<>btChar then
              RaiseXExpectedButYFound(20170216152603,'char',BaseTypeNames[RightResolved.BaseType],Bin.right);
            SetResolverValueExpr(ResolvedEl,btRange,FBaseTypes[btChar],Bin,[rrfReadable]);
            ResolvedEl.SubType:=LeftResolved.BaseType;
            exit;
            end;
        eopAdd:
          case LeftResolved.BaseType of
          btChar:
            begin
              case RightResolved.BaseType of
              btChar: SetBaseType(btString);
              btWideChar: SetBaseType(btUnicodeString);
              else
                // use right type for result
                SetResolverValueExpr(ResolvedEl,RightResolved.BaseType,RightResolved.TypeEl,Bin,[rrfReadable]);
              end;
              exit;
            end;
          btWideChar:
            begin
            SetBaseType(btUnicodeString);
            exit;
            end;
          btShortString:
            begin
              case RightResolved.BaseType of
              btChar,btShortString,btWideChar:
                // use left type for result
                SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin,[rrfReadable]);
              else
                // shortstring + string => string
                SetResolverValueExpr(ResolvedEl,RightResolved.BaseType,RightResolved.TypeEl,Bin,[rrfReadable]);
              end;
              exit;
            end;
          btString,btAnsiString,btUnicodeString:
            begin
              // string + x => string
              SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin,[rrfReadable]);
              exit;
            end;
          end;
        eopLessThan,
        eopGreaterThan,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end
      else if (RightResolved.BaseType=btSet) and (RightResolved.SubType=btChar)
          and (LeftResolved.BaseType=btChar) then
        begin
        case Bin.OpCode of
        eopIn:
          begin
          SetBaseType(btBoolean);
          exit;
          end;
        end;
        end
      end
    end
  else if LeftResolved.BaseType in btAllFloats then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType in (btAllInteger+btAllFloats))
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopAdd, eopSubtract,
      eopMultiply, eopDivide, eopMod,
      eopPower:
        begin
        SetResolverValueExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin,[rrfReadable]);
        exit;
        end;
      eopLessThan,
      eopGreaterThan,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        SetBaseType(btBoolean);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btPointer then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType in btAllInteger)
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopAdd,eopSubtract:
        begin
        SetResolverValueExpr(ResolvedEl,btPointer,LeftResolved.TypeEl,Bin,[rrfReadable]);
        exit;
        end;
      end
    else if RightResolved.BaseType=btPointer then
      case Bin.OpCode of
      eopLessThan,
      eopGreaterThan,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        SetBaseType(btBoolean);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btContext then
    case Bin.OpCode of
    eopNone:
      if Bin.Kind=pekRange then
        begin
        if (rrfReadable in LeftResolved.Flags)
            and (rrfReadable in RightResolved.Flags) then
          begin
          CheckSetElementsCompatible(Bin.left,Bin.right,LeftResolved,RightResolved);
          ResolvedEl:=LeftResolved;
          ResolvedEl.SubType:=ResolvedEl.BaseType;
          ResolvedEl.BaseType:=btRange;
          exit;
          end;
        end;
    eopIn:
      if (rrfReadable in LeftResolved.Flags)
      and (rrfReadable in RightResolved.Flags) then
        begin
        if LeftResolved.BaseType in (btAllInteger+[btChar]) then
          begin
          if (RightResolved.BaseType<>btSet) then
            RaiseXExpectedButYFound(20170216152607,'set of '+BaseTypeNames[LeftResolved.BaseType],LeftResolved.TypeEl.ElementTypeName,Bin.right);
          if LeftResolved.BaseType=btChar then
            begin
            if RightResolved.SubType<>btChar then
              RaiseXExpectedButYFound(20170216152609,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.right);
            end
          else if not (RightResolved.SubType in btAllInteger) then
            RaiseXExpectedButYFound(20170216152612,'set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.right);
          SetBaseType(btBoolean);
          exit;
          end
        else if (LeftResolved.BaseType=btContext) and (LeftResolved.TypeEl is TPasEnumType) then
          begin
          if (RightResolved.BaseType<>btSet) then
            RaiseXExpectedButYFound(20170216152615,'set of '+LeftResolved.TypeEl.Name,LeftResolved.TypeEl.ElementTypeName,Bin.right);
          if LeftResolved.TypeEl<>RightResolved.TypeEl then
            RaiseXExpectedButYFound(20170216152618,'set of '+LeftResolved.TypeEl.Name,'set of '+RightResolved.TypeEl.Name,Bin.right);
          SetBaseType(btBoolean);
          exit;
          end
        else
          RaiseMsg(20170216152228,nInOperatorExpectsSetElementButGot,
            sInOperatorExpectsSetElementButGot,[LeftResolved.TypeEl.ElementTypeName],Bin);
        end;
    eopIs:
      begin
      if (LeftResolved.TypeEl is TPasClassType) then
        begin
        if (LeftResolved.IdentEl=nil) or (LeftResolved.IdentEl is TPasType) then
          RaiseMsg(20170216152230,nIllegalQualifier,sIllegalQualifier,['is'],Bin);
        // left side is a class instance
        if RightResolved.IdentEl is TPasClassType then
          begin
          // e.g. if Image is TFPMemoryImage then ;
          // Note: at compile time the check is reversed: right must inherit from left
          if CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else if (RightResolved.TypeEl is TPasClassOfType)
            and (rrfReadable in RightResolved.Flags) then
          begin
          // e.g. if Image is ImageClass then ;
          if (CheckClassesAreRelated(LeftResolved.TypeEl,
              TPasClassOfType(RightResolved.TypeEl).DestType,Bin)<>cIncompatible) then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else
          RaiseXExpectedButYFound(20170216152625,'class type',RightResolved.TypeEl.ElementTypeName,Bin.right);
        end
      else if (proClassOfIs in Options) and (LeftResolved.TypeEl is TPasClassOfType)
          and (rrfReadable in LeftResolved.Flags) then
        begin
        if (LeftResolved.IdentEl=nil) or (LeftResolved.IdentEl is TPasType) then
          RaiseMsg(20170322101128,nIllegalQualifier,sIllegalQualifier,['is'],Bin);
        // left side is class-of variable
        LeftTypeEl:=TPasClassOfType(LeftResolved.TypeEl).DestType;
        if RightResolved.IdentEl is TPasClassType then
          begin
          // e.g. if ImageClass is TFPMemoryImage then ;
          // Note: at compile time the check is reversed: right must inherit from left
          if CheckClassIsClass(RightResolved.TypeEl,LeftTypeEl,Bin)<>cIncompatible then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else if (RightResolved.TypeEl is TPasClassOfType) then
          begin
          // e.g. if ImageClassA is ImageClassB then ;
          // or   if ImageClassA is TFPImageClass then ;
          RightTypeEl:=TPasClassOfType(RightResolved.TypeEl).DestType;
          if (CheckClassesAreRelated(LeftTypeEl,RightTypeEl,Bin)<>cIncompatible) then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else
          RaiseXExpectedButYFound(20170322105252,'class type',RightResolved.TypeEl.ElementTypeName,Bin.right);
        end
      else if LeftResolved.TypeEl=nil then
        RaiseMsg(20170216152232,nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [BaseTypeNames[LeftResolved.BaseType]],Bin.left)
      else
        RaiseMsg(20170216152234,nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [LeftResolved.TypeEl.ElementTypeName],Bin.left);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ComputeBinaryExpr is-operator: left=',GetResolverResultDesc(LeftResolved),' right=',GetResolverResultDesc(RightResolved));
      {$ENDIF}
      RaiseMsg(20170216152236,nTypesAreNotRelated,sTypesAreNotRelated,[],Bin);
      end;
    eopAs:
      begin
      if (LeftResolved.TypeEl is TPasClassType) then
        begin
        if (LeftResolved.IdentEl=nil) or (LeftResolved.IdentEl is TPasType)
            or (not (rrfReadable in LeftResolved.Flags)) then
          RaiseMsg(20170216152237,nIllegalQualifier,sIllegalQualifier,['as'],Bin);
        if RightResolved.IdentEl=nil then
          RaiseXExpectedButYFound(20170216152630,'class',RightResolved.TypeEl.ElementTypeName,Bin.right);
        if not (RightResolved.IdentEl is TPasType) then
          RaiseXExpectedButYFound(20170216152632,'class',RightResolved.IdentEl.Name,Bin.right);
        if (CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible) then
          begin
          SetResolverValueExpr(ResolvedEl,btContext,RightResolved.TypeEl,Bin,[rrfReadable]);
          exit;
          end;
        RaiseMsg(20170216152239,nTypesAreNotRelated,sTypesAreNotRelated,[],Bin);
        end;
      end;
    eopLessThan,eopGreaterThan, eopLessthanEqual,eopGreaterThanEqual:
      if (LeftResolved.TypeEl.ClassType=TPasEnumType)
          and (rrfReadable in LeftResolved.Flags)
          and (LeftResolved.TypeEl=RightResolved.TypeEl)
          and (rrfReadable in RightResolved.Flags)
      then
        begin
        SetBaseType(btBoolean);
        exit;
        end;
    eopSubIdent:
      begin
      ResolvedEl:=RightResolved;
      exit;
      end;
    end
  else if LeftResolved.BaseType=btSet then
    begin
    if (rrfReadable in LeftResolved.Flags)
        and (RightResolved.BaseType=btSet)
        and (rrfReadable in RightResolved.Flags) then
      case Bin.OpCode of
      eopAdd,
      eopSubtract,
      eopMultiply,
      eopSymmetricaldifference,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        if RightResolved.TypeEl=nil then
          begin
          // right is empty set
          if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
            SetBaseType(btBoolean)
          else
            begin
            ResolvedEl:=LeftResolved;
            ResolvedEl.IdentEl:=nil;
            ResolvedEl.ExprEl:=Bin;
            end;
          exit;
          end
        else if LeftResolved.TypeEl=nil then
          begin
          // left is empty set
          if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
            SetBaseType(btBoolean)
          else
            begin
            ResolvedEl:=RightResolved;
            ResolvedEl.IdentEl:=nil;
            ResolvedEl.ExprEl:=Bin;
            end;
          exit;
          end
        else if (LeftResolved.SubType=RightResolved.SubType)
            or ((LeftResolved.SubType in btAllBooleans)
              and (RightResolved.SubType in btAllBooleans))
            or ((LeftResolved.SubType in btAllInteger)
              and (RightResolved.SubType in btAllInteger)) then
          begin
          // compatible set
          if Bin.OpCode in [eopLessthanEqual,eopGreaterThanEqual] then
            SetBaseType(btBoolean)
          else
            begin
            ResolvedEl:=LeftResolved;
            ResolvedEl.IdentEl:=nil;
            ResolvedEl.ExprEl:=Bin;
            end;
          exit;
          end;
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeBinaryExpr + - * >< Sets LeftSubType='+BaseTypeNames[LeftResolved.SubType]
          +' RightSubType='+BaseTypeNames[RightResolved.SubType]);
        {$ENDIF}
        end;
      end;
    end
  else if LeftResolved.BaseType=btModule then
    begin
    if Bin.OpCode=eopSubIdent then
      begin
      ResolvedEl:=RightResolved;
      exit;
      end;
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeBinaryExpr OpCode=',OpcodeStrings[Bin.OpCode],' Kind=',Bin.Kind,' Left=',GetResolverResultDesc(LeftResolved),' Right=',GetResolverResultDesc(RightResolved));
  {$ENDIF}
  RaiseMsg(20170216152241,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[Bin.OpCode]],Bin);
end;

procedure TPasResolver.ComputeArrayParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);

  procedure ComputeIndexProperty(Prop: TPasProperty);
  begin
    if [rcConstant,rcType]*Flags<>[] then
      RaiseConstantExprExp(20170216152635,Params);
    ComputeElement(GetPasPropertyType(Prop),ResolvedEl,[rcType],StartEl);
    ResolvedEl.IdentEl:=Prop;
    ResolvedEl.Flags:=[];
    if GetPasPropertyGetter(Prop)<>nil then
      Include(ResolvedEl.Flags,rrfReadable);
    if GetPasPropertySetter(Prop)<>nil then
      Include(ResolvedEl.Flags,rrfWritable);
  end;

var
  TypeEl: TPasType;
  ClassScope: TPasClassScope;
  ArrayEl: TPasArrayType;
  ArgNo: Integer;
  OrigResolved: TPasResolverResult;
  SubParams: TParamsExpr;
begin
  if Params.Value.CustomData is TResolvedReference then
    begin
    // e.g. Name[]
    ComputeElement(Params.Value,ResolvedEl,
      Flags-[rcNoImplicitProc,rcNoImplicitProcType],StartEl);
    end
  else if Params.Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Params.Value);
    if SubParams.Kind in [pekArrayParams,pekFuncParams] then
      begin
      // e.g. Name()[] or Name[][]
      ComputeElement(SubParams,ResolvedEl,
        Flags-[rcNoImplicitProc,rcNoImplicitProcType],StartEl);
      end
    else
      RaiseNotYetImplemented(20161010195646,SubParams);
    end
  else
    RaiseNotYetImplemented(20160928174144,Params);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeArrayParams ResolvedEl=',GetResolverResultDesc(ResolvedEl));
  {$ENDIF}
  if ResolvedEl.BaseType in btAllStrings then
    begin
    // stringvar[] => char
    if ResolvedEl.BaseType in [btWideString,btUnicodeString] then
      ResolvedEl.BaseType:=btWideChar
    else
      ResolvedEl.BaseType:=btChar;
    // keep ResolvedEl.IdentEl the string var
    ResolvedEl.TypeEl:=FBaseTypes[ResolvedEl.BaseType];
    ResolvedEl.ExprEl:=Params;
    ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable,rrfCanBeStatement]+[rrfAssignable];
    end
  else if (ResolvedEl.IdentEl is TPasProperty)
      and (TPasProperty(ResolvedEl.IdentEl).Args.Count>0) then
    // property with args
    ComputeIndexProperty(TPasProperty(ResolvedEl.IdentEl))
  else if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.TypeEl;
    if TypeEl.ClassType=TPasClassType then
      begin
      ClassScope:=TypeEl.CustomData as TPasClassScope;
      if ClassScope.DefaultProperty=nil then
        RaiseInternalError(20161010151747);
      ComputeIndexProperty(ClassScope.DefaultProperty);
      end
    else if TypeEl.ClassType=TPasClassOfType then
      begin
      ClassScope:=TPasClassOfType(TypeEl).DestType.CustomData as TPasClassScope;
      if ClassScope.DefaultProperty=nil then
        RaiseInternalError(20161010174916);
      ComputeIndexProperty(ClassScope.DefaultProperty);
      end
    else if TypeEl.ClassType=TPasArrayType then
      begin
      ArrayEl:=TPasArrayType(TypeEl);
      ArgNo:=0;
      repeat
        if length(ArrayEl.Ranges)=0 then
          inc(ArgNo) // dynamic/open array has one dimension
        else
          inc(ArgNo,length(ArrayEl.Ranges)); // static array has several dimensions
        if ArgNo>length(Params.Params) then
          RaiseInternalError(20161010185535);
        if ArgNo=length(Params.Params) then
          break;
        // continue in sub array
        ArrayEl:=ResolveAliasType(ArrayEl.ElType) as TPasArrayType;
      until false;
      OrigResolved:=ResolvedEl;
      ComputeElement(ArrayEl.ElType,ResolvedEl,Flags,StartEl);
      // identifier and value is the array itself
      ResolvedEl.IdentEl:=OrigResolved.IdentEl;
      ResolvedEl.ExprEl:=OrigResolved.ExprEl;
      ResolvedEl.Flags:=OrigResolved.Flags*[rrfReadable,rrfWritable];
      if IsDynArray(ArrayEl) then
        // dyn array elements are writable independent of the array
        Include(ResolvedEl.Flags,rrfWritable);
      end
    else
      RaiseNotYetImplemented(20161010151727,Params,GetResolverResultDesc(ResolvedEl));
    end
  else
    RaiseNotYetImplemented(20160928174212,Params,GetResolverResultDesc(ResolvedEl));
end;

procedure TPasResolver.ComputeFuncParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
var
  DeclEl: TPasElement;
  BuiltInProc: TResElDataBuiltInProc;
  Proc: TPasProcedure;
  aClass: TPasClassType;
  ResolvedTypeEl: TPasResolverResult;
  Ref: TResolvedReference;
begin
  if Params.Value.CustomData is TResolvedReference then
    begin
    Ref:=TResolvedReference(Params.Value.CustomData);
    DeclEl:=Ref.Declaration;
    if DeclEl.ClassType=TPasUnresolvedSymbolRef then
      begin
      if DeclEl.CustomData.ClassType=TResElDataBuiltInProc then
        begin
        BuiltInProc:=TResElDataBuiltInProc(DeclEl.CustomData);
        if Assigned(BuiltInProc.GetCallResult) then
          // built in function
          BuiltInProc.GetCallResult(BuiltInProc,Params,ResolvedEl)
        else
          // built in procedure
          SetResolverIdentifier(ResolvedEl,btProc,BuiltInProc.Proc,BuiltInProc.Proc,[]);
        if bipfCanBeStatement in BuiltInProc.Flags then
          Include(ResolvedEl.Flags,rrfCanBeStatement);
        end
      else if DeclEl.CustomData is TResElDataBaseType then
        begin
        // type cast to base type
        if TResElDataBaseType(DeclEl.CustomData).BaseType=btCustom then
          // custom base type
          SetResolverValueExpr(ResolvedEl,
            btCustom,
            TPasUnresolvedSymbolRef(DeclEl),Params.Params[0],[rrfReadable])
        else
          SetResolverValueExpr(ResolvedEl,
            TResElDataBaseType(DeclEl.CustomData).BaseType,
            TPasUnresolvedSymbolRef(DeclEl),Params.Params[0],[rrfReadable]);
        end
      else
        RaiseNotYetImplemented(20161006133040,Params,GetResolverResultDesc(ResolvedEl));
      end
    else
      begin
      // normal identifier (not built-in)
      ComputeElement(DeclEl,ResolvedEl,Flags+[rcNoImplicitProc],StartEl);
      if ResolvedEl.BaseType=btProc then
        begin
        if not (ResolvedEl.IdentEl is TPasProcedure) then
          RaiseNotYetImplemented(20160928180201,Params,GetResolverResultDesc(ResolvedEl));
        Proc:=TPasProcedure(ResolvedEl.IdentEl);
        if rcConstant in Flags then
          RaiseConstantExprExp(20170216152637,Params);
        if Proc is TPasFunction then
          // function call => return result
          ComputeElement(TPasFunction(Proc).FuncType.ResultEl,ResolvedEl,
            Flags+[rcNoImplicitProc],StartEl)
        else if (Proc.ClassType=TPasConstructor)
            and (rrfNewInstance in Ref.Flags) then
          begin
          // new instance call -> return value of type class
          aClass:=GetReference_NewInstanceClass(Ref);
          SetResolverValueExpr(ResolvedEl,btContext,aClass,Params.Value,[rrfReadable]);
          end
        else
          // procedure call, result is neither readable nor writable
          SetResolverIdentifier(ResolvedEl,btProc,Proc,Proc.ProcType,[]);
        Include(ResolvedEl.Flags,rrfCanBeStatement);
        end
      else if ResolvedEl.TypeEl is TPasProcedureType then
        begin
        if rcConstant in Flags then
          RaiseConstantExprExp(20170216152639,Params);
        if ResolvedEl.TypeEl is TPasFunctionType then
          // function call => return result
          ComputeElement(TPasFunctionType(ResolvedEl.TypeEl).ResultEl,
            ResolvedEl,Flags+[rcNoImplicitProc],StartEl)
        else
          // procedure call, result is neither readable nor writable
          SetResolverTypeExpr(ResolvedEl,btProc,TPasProcedureType(ResolvedEl.TypeEl),[]);
        Include(ResolvedEl.Flags,rrfCanBeStatement);
        end
      else if (DeclEl is TPasType) then
        begin
        // type cast
        ResolvedTypeEl:=ResolvedEl;
        ComputeElement(Params.Params[0],ResolvedEl,Flags,StartEl);
        ResolvedEl.BaseType:=ResolvedTypeEl.BaseType;
        ResolvedEl.TypeEl:=ResolvedTypeEl.TypeEl;
        end
      else
        RaiseNotYetImplemented(20160928180048,Params,GetResolverResultDesc(ResolvedEl));
      end;
    end
  else
    RaiseNotYetImplemented(20160928174124,Params);
end;

procedure TPasResolver.ComputeSetParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);
// [param,param,...]
var
  ParamResolved, FirstResolved: TPasResolverResult;
  i: Integer;
  Param: TPasExpr;
begin
  if length(Params.Params)=0 then
    SetResolverValueExpr(ResolvedEl,btSet,nil,Params,[rrfReadable])
  else
    begin
    FirstResolved:=Default(TPasResolverResult);
    Flags:=Flags-[rcNoImplicitProc,rcNoImplicitProcType];
    for i:=0 to length(Params.Params)-1 do
      begin
      Param:=Params.Params[i];
      ComputeElement(Params.Params[0],ParamResolved,Flags,StartEl);
      if ParamResolved.BaseType=btRange then
        ConvertRangeToFirstValue(ParamResolved);
      if FirstResolved.BaseType=btNone then
        begin
        // first value -> check type usable in a set
        FirstResolved:=ParamResolved;
        CheckIsOrdinal(FirstResolved,Param,true);
        if not ResolvedElHasValue(FirstResolved) then
          RaiseXExpectedButYFound(20170216152554,'ordinal value','type',Param);
        end
      else
        begin
        // next value
        CheckSetElementsCompatible(Params.Params[0],Param,FirstResolved,ParamResolved);
        end;
      end;

    FirstResolved.IdentEl:=nil;
    if FirstResolved.ExprEl=nil then
      FirstResolved.ExprEl:=Params;
    FirstResolved.SubType:=FirstResolved.BaseType;
    FirstResolved.BaseType:=btSet;
    FirstResolved.Flags:=[rrfReadable];
    ResolvedEl:=FirstResolved;
    end;
end;

procedure TPasResolver.CheckIsClass(El: TPasElement;
  const ResolvedEl: TPasResolverResult);
begin
  if (ResolvedEl.BaseType<>btContext) then
    RaiseMsg(20170216152245,nXExpectedButYFound,sXExpectedButYFound,
      ['class',BaseTypeNames[ResolvedEl.BaseType]],El);
  if (ResolvedEl.TypeEl.ClassType<>TPasClassType) then
    RaiseMsg(20170216152246,nXExpectedButYFound,sXExpectedButYFound,
      ['class',ResolvedEl.TypeEl.ElementTypeName],El);
end;

function TPasResolver.CheckTypeCastClassInstanceToClass(const FromClassRes,
  ToClassRes: TPasResolverResult; ErrorEl: TPasElement): integer;
// called when type casting a class instance into an unrelated class
begin
  if FromClassRes.BaseType=btNone then ;
  if ToClassRes.BaseType=btNone then ;
  if ErrorEl=nil then ;
  Result:=cIncompatible;
end;

procedure TPasResolver.CheckRangeExpr(Left, Right: TPasExpr; out LeftResolved,
  RightResolved: TPasResolverResult);
begin
  ComputeElement(Left,LeftResolved,[rcSkipTypeAlias,rcConstant]);
  ComputeElement(Right,RightResolved,[rcSkipTypeAlias,rcConstant]);
  CheckSetElementsCompatible(Left,Right,LeftResolved,RightResolved);
end;

procedure TPasResolver.CheckSetElementsCompatible(Left, Right: TPasExpr;
  const LeftResolved, RightResolved: TPasResolverResult);
begin
  // check both are values
  if not ResolvedElHasValue(LeftResolved) then
    begin
    if LeftResolved.TypeEl<>nil then
      RaiseXExpectedButYFound(20170216152645,'ordinal',LeftResolved.TypeEl.ElementTypeName,Left)
    else
      RaiseXExpectedButYFound(20170216152648,'ordinal',BaseTypeNames[LeftResolved.BaseType],Left);
    end;
  if not ResolvedElHasValue(RightResolved) then
    begin
    if RightResolved.TypeEl<>nil then
      RaiseXExpectedButYFound(20170216152651,'ordinal',RightResolved.TypeEl.ElementTypeName,Right)
    else
      RaiseXExpectedButYFound(20170216152653,'ordinal',BaseTypeNames[RightResolved.BaseType],Right);
    end;
  // check both have the same ordinal type
  if LeftResolved.BaseType in btAllBooleans then
    begin
    if (RightResolved.BaseType in btAllBooleans) then
      exit;
    RaiseXExpectedButYFound(20170216152656,'boolean',BaseTypeNames[RightResolved.BaseType],Right);
    end
  else if LeftResolved.BaseType in btAllInteger then
    begin
    if (RightResolved.BaseType in btAllInteger) then
      exit;
    RaiseXExpectedButYFound(20170216152658,'integer',BaseTypeNames[RightResolved.BaseType],Right);
    end
  else if LeftResolved.BaseType=btChar then
    begin
    if (RightResolved.BaseType=btChar) then
      exit;
    RaiseXExpectedButYFound(20170216152702,'char',BaseTypeNames[RightResolved.BaseType],Right);
    end
  else if LeftResolved.BaseType=btContext then
    begin
    if LeftResolved.TypeEl.ClassType=TPasEnumType then
      begin
      if LeftResolved.TypeEl=RightResolved.TypeEl then
        exit;
      if RightResolved.TypeEl.ClassType<>TPasEnumType then
        RaiseXExpectedButYFound(20170216152707,LeftResolved.TypeEl.Parent.Name,RightResolved.TypeEl.ElementTypeName,Right);
      if LeftResolved.TypeEl.Parent<>RightResolved.TypeEl.Parent then
        RaiseXExpectedButYFound(20170216152710,LeftResolved.TypeEl.Parent.Name,RightResolved.TypeEl.Parent.Name,Right);
      end
    else
      RaiseXExpectedButYFound(20170216152712,'ordinal',BaseTypeNames[LeftResolved.BaseType],Left);
    end
  else
    RaiseXExpectedButYFound(20170216152714,'ordinal',BaseTypeNames[LeftResolved.BaseType],Left);
end;

function TPasResolver.CheckIsOrdinal(
  const ResolvedEl: TPasResolverResult; ErrorEl: TPasElement;
  RaiseOnError: boolean): boolean;
begin
  Result:=false;
  if ResolvedEl.BaseType in (btAllInteger+btAllBooleans+[btChar]) then
  else if (ResolvedEl.BaseType=btContext) then
    begin
    if ResolvedEl.TypeEl.ClassType=TPasEnumType then
    else if RaiseOnError then
      RaiseXExpectedButYFound(20170216152718,'ordinal value',ResolvedEl.TypeEl.ElementTypeName,ErrorEl)
    else
      exit;
    end
  else if RaiseOnError then
    RaiseXExpectedButYFound(20170216152720,'ordinal value',BaseTypeNames[ResolvedEl.BaseType],ErrorEl)
  else
    exit;
  Result:=true;
end;

procedure TPasResolver.ConvertRangeToFirstValue(
  var ResolvedEl: TPasResolverResult);
begin
  if ResolvedEl.BaseType<>btRange then
    RaiseInternalError(20161001155732);
  if ResolvedEl.TypeEl=nil then
    if ResolvedEl.IdentEl<>nil then
      RaiseNotYetImplemented(20161001155747,ResolvedEl.IdentEl)
    else
      RaiseNotYetImplemented(20161001155834,ResolvedEl.ExprEl);
  ResolvedEl.BaseType:=ResolvedEl.SubType;
  ResolvedEl.SubType:=btNone;
end;

function TPasResolver.IsCharLiteral(const Value: string): boolean;
var
  p: PChar;
begin
  Result:=false;
  p:=PChar(Value);
  if (p^='''') then
    begin
    inc(p);
    if p^ in [#32..#196] then
      begin
      inc(p);
      if p^='''' then
        exit(true);
      end;
    end;
end;

function TPasResolver.CheckBuiltInMinParamCount(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; MinCount: integer; RaiseOnError: boolean): boolean;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<MinCount) then
    begin
    if RaiseOnError then
      RaiseMsg(20170216152248,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(false);
    end;
  Result:=true;
end;

function TPasResolver.CheckBuiltInMaxParamCount(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; MaxCount: integer; RaiseOnError: boolean): integer;
begin
  if length(Params.Params)>MaxCount then
    begin
    if RaiseOnError then
      RaiseMsg(20170329154348,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[MaxCount]);
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

function TPasResolver.CheckRaiseTypeArgNo(id: int64; ArgNo: integer;
  Param: TPasExpr; const ParamResolved: TPasResolverResult; Expected: string;
  RaiseOnError: boolean): integer;
begin
  if RaiseOnError then
    RaiseMsg(id,nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
      [IntToStr(ArgNo),GetResolverResultDescription(ParamResolved,true),Expected],Param);
  Result:=cIncompatible;
end;

function TPasResolver.CheckAssignCompatibilityCustom(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean;
  var Handled: boolean): integer;
// called when LHS or RHS BaseType is btCustom
// if RaiseOnIncompatible=true you can raise an useful error.
begin
  Result:=cIncompatible;
  if LHS.BaseType=btNone then ;
  if RHS.BaseType=btNone then ;
  if ErrorEl=nil then ;
  if RaiseOnIncompatible then ;
  if Handled then ;
end;

function TPasResolver.CheckEqualCompatibilityCustomType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
begin
  Result:=cIncompatible;
  if LHS.BaseType=RHS.BaseType then;
  if ErrorEl=nil then;
  if RaiseOnIncompatible then ;
end;

function TPasResolver.BI_Length_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'length'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: string or dynamic array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType in btAllStringAndChars then
      Result:=cExact
    else if ParamResolved.BaseType=btContext then
      begin
      if (ParamResolved.TypeEl.ClassType=TPasArrayType) then
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170329160335,1,Param,ParamResolved,
      'string or array',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Length_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  if Params=nil then ;
  SetResolverIdentifier(ResolvedEl,btInt64,Proc.Proc,FBaseTypes[btInt64],[rrfReadable]);
end;

function TPasResolver.BI_SetLength_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'setlength'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: string or array variable
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact
    else if ParamResolved.BaseType=btContext then
      begin
      if IsDynArray(ParamResolved.TypeEl) then
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152250,1,Param,ParamResolved,
      'string or dynamic array variable',RaiseOnError));

  // second param: new length
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if (rrfReadable in ParamResolved.Flags)
      and (ParamResolved.BaseType in btAllInteger) then
    Result:=cExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170329160338,2,Param,ParamResolved,
      'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_SetLength_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishParamExpressionAccess(P[0],rraVarParam);
  FinishParamExpressionAccess(P[1],rraRead);
end;

function TPasResolver.BI_InExclude_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'include'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  EnumType: TPasEnumType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: variable of set of enumtype
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  EnumType:=nil;
  if ([rrfReadable,rrfWritable]*ParamResolved.Flags=[rrfReadable,rrfWritable])
      and ((ParamResolved.IdentEl is TPasVariable)
        or (ParamResolved.IdentEl is TPasArgument)) then
    begin
    if (ParamResolved.BaseType=btSet)
        and (ParamResolved.TypeEl is TPasEnumType) then
      EnumType:=TPasEnumType(ParamResolved.TypeEl);
    end;
  if EnumType=nil then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnGetCallCompatibility_InExclude ',GetResolverResultDesc(ParamResolved));
    {$ENDIF}
    exit(CheckRaiseTypeArgNo(20170216152301,1,Param,ParamResolved,
      'variable of set of enumtype',RaiseOnError));
    end;

  // second param: enum
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if (not (rrfReadable in ParamResolved.Flags))
      or (ParamResolved.TypeEl<>EnumType) then
    begin
    if RaiseOnError then
      RaiseIncompatibleType(20170216152302,nIncompatibleTypeArgNo,
        ['2'],ParamResolved.TypeEl,EnumType,Param);
    exit(cIncompatible);
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_InExclude_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishParamExpressionAccess(P[0],rraVarParam);
  FinishParamExpressionAccess(P[1],rraRead);
end;

function TPasResolver.BI_Break_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
begin
  if GetLoop(Expr)=nil then
    RaiseMsg(20170216152306,nMustBeInsideALoop,sMustBeInsideALoop,['Break'],Expr);
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Break Params=',length(Params.Params));
  {$ENDIF}
  Result:=CheckBuiltInMaxParamCount(Proc,Params,0,RaiseOnError);
end;

function TPasResolver.BI_Continue_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
begin
  if GetLoop(Expr)=nil then
    RaiseMsg(20170216152309,nMustBeInsideALoop,sMustBeInsideALoop,['Continue'],Expr);
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Continue Params=',length(Params.Params));
  {$ENDIF}
  Result:=CheckBuiltInMaxParamCount(Proc,Params,0,RaiseOnError);
end;

function TPasResolver.BI_Exit_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ResultResolved: TPasResolverResult;
  i: Integer;
  ProcScope: TPasProcedureScope;
  ResultEl: TPasResultElement;
  Flags: TPasResolverComputeFlags;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit Params=',length(Params.Params));
  {$ENDIF}

  // first param: result
  Param:=Params.Params[0];
  Result:=cIncompatible;
  i:=ScopeCount-1;
  while (i>0) and (not (Scopes[i] is TPasProcedureScope)) do dec(i);
  if i>0 then
    begin
    // first param is function result
    ProcScope:=TPasProcedureScope(Scopes[i]);
    if not (ProcScope.Element is TPasFunction) then
      begin
      if RaiseOnError then
        RaiseMsg(20170216152312,nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,['procedure exit'],Params.Params[0]);
      exit(cIncompatible);
      end;
    ResultEl:=(ProcScope.Element as TPasFunction).FuncType.ResultEl;
    ComputeElement(ResultEl,ResultResolved,[rcType]);
    end
  else
    begin
    // default: main program, param is an integer
    SetResolverTypeExpr(ResultResolved,btLongint,FBaseTypes[btLongint],[rrfReadable,rrfWritable]);
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ResultResolved=',GetResolverResultDesc(ResultResolved));
  {$ENDIF}

  Flags:=[];
  if IsProcedureType(ResultResolved) then
    Include(Flags,rcNoImplicitProc);
  ComputeElement(Param,ParamResolved,Flags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ParamResolved=',GetResolverResultDesc(ParamResolved));
  {$ENDIF}

  if rrfReadable in ParamResolved.Flags then
    Result:=CheckAssignResCompatibility(ResultResolved,ParamResolved,Param,false);
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152314,nIncompatibleTypeArgNo,
        ['1'],ParamResolved,ResultResolved,Param);
    exit;
    end;

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

function TPasResolver.BI_IncDec_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, IncrResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: var Integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProc]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_IncDec ParamResolved=',GetResolverResultDesc(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  // Expr must be a variable
  if not ResolvedElCanBeVarParam(ParamResolved) then
    begin
    if RaiseOnError then
      RaiseMsg(20170216152319,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
    exit;
    end;
  if ParamResolved.BaseType in btAllInteger then
    Result:=cExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152320,1,Param,ParamResolved,'integer',RaiseOnError));

  if length(Params.Params)=1 then
    exit;

  // second param: increment/decrement
  Param:=Params.Params[1];
  ComputeElement(Param,IncrResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in IncrResolved.Flags then
    begin
    if IncrResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152322,2,Param,IncrResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_IncDec_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishParamExpressionAccess(P[0],rraVarParam);
  if Length(P)>1 then
    FinishParamExpressionAccess(P[1],rraRead);
end;

function TPasResolver.BI_Assigned_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Assigned'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  C: TClass;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: pointer, class, class instance, proc type or array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcNoImplicitProcType]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in [btNil,btPointer] then
    Result:=cExact
  else if (ParamResolved.BaseType=btContext) then
    begin
    C:=ParamResolved.TypeEl.ClassType;
    if (C=TPasClassType)
        or (C=TPasClassOfType)
        or C.InheritsFrom(TPasProcedureType)
        or ((C=TPasArrayType) and (length(TPasArrayType(ParamResolved.TypeEl).Ranges)=0)) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152329,1,Param,ParamResolved,'class or array',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Assigned_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btBoolean,Proc.Proc,FBaseTypes[btBoolean],[rrfReadable]);
end;

function TPasResolver.BI_Chr_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170325185321,1,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Chr_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btChar,Proc.Proc,FBaseTypes[btChar],[rrfReadable]);
end;

function TPasResolver.BI_Ord_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: enum or char
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType=btChar then
      Result:=cExact
    else if (ParamResolved.BaseType=btContext) and (ParamResolved.TypeEl is TPasEnumType) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152334,1,Param,ParamResolved,'enum or char',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_Ord_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btSmallInt,Proc.Proc,FBaseTypes[btSmallInt],[rrfReadable]);
end;

function TPasResolver.BI_LowHigh_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Low' or 'High'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  TypeEl: TPasType;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: enum, range or char
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if CheckIsOrdinal(ParamResolved,Param,false) then
    Result:=cExact
  else if ParamResolved.BaseType=btSet then
    Result:=cExact
  else if (ParamResolved.BaseType=btContext) then
    begin
    TypeEl:=ParamResolved.TypeEl;
    if (TypeEl.ClassType=TPasArrayType)
        or (TypeEl.ClassType=TPasSetType) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152338,1,Param,ParamResolved,'enum or char',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_LowHigh_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
var
  ArrayEl: TPasArrayType;
  Param: TPasExpr;
  TypeEl: TPasType;
begin
  Param:=Params.Params[0];
  ComputeElement(Param,ResolvedEl,[]);
  if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.TypeEl;
    if TypeEl.ClassType=TPasArrayType then
      begin
      // array: result type is type of first dimension
      ArrayEl:=TPasArrayType(TypeEl);
      if length(ArrayEl.Ranges)=0 then
        SetResolverIdentifier(ResolvedEl,btInt64,Proc.Proc,FBaseTypes[btInt64],[rrfReadable])
      else
        begin
        ComputeElement(ArrayEl.Ranges[0],ResolvedEl,[rcConstant]);
        if ResolvedEl.BaseType=btRange then
          ConvertRangeToFirstValue(ResolvedEl);
        end;
      end
    else if TypeEl.ClassType=TPasSetType then
      begin
      ResolvedEl.TypeEl:=TPasSetType(TypeEl).EnumType;
      end;
    end
  else if ResolvedEl.BaseType=btSet then
    begin
    ResolvedEl.BaseType:=ResolvedEl.SubType;
    ResolvedEl.SubType:=btNone;
    end
  else
    ;// ordinal: result type is argument type
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable]+[rrfReadable];
end;

function TPasResolver.BI_PredSucc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'Pred' or 'Succ'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);

  // first param: enum, range, set, char or integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if CheckIsOrdinal(ParamResolved,Param,false) then
    Result:=cExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170216152343,1,Param,ParamResolved,'ordinal',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,1,RaiseOnError);
end;

procedure TPasResolver.BI_PredSucc_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
end;

function TPasResolver.BI_Str_CheckParam(IsFunc: boolean; Param: TPasExpr;
  const ParamResolved: TPasResolverResult; ArgNo: integer; RaiseOnError: boolean
  ): integer;

  function CheckFormat(FormatExpr: TPasExpr; Index: integer;
    const ParamResolved: TPasResolverResult): boolean;
  var
    ResolvedEl: TPasResolverResult;
    Ok: Boolean;
  begin
    if FormatExpr=nil then exit(true);
    Result:=false;
    Ok:=false;
    if ParamResolved.BaseType in btAllFloats then
      // floats supports value:Width:Precision
      Ok:=true
    else
      // all other only support only Width
      Ok:=Index<2;
    if not Ok then
      begin
      if RaiseOnError then
        RaiseMsg(20170319222319,nIllegalExpression,sIllegalExpression,[],FormatExpr);
      exit;
      end;
    ComputeElement(FormatExpr,ResolvedEl,[]);
    if not (ResolvedEl.BaseType in btAllInteger) then
      begin
      if RaiseOnError then
        RaiseMsg(20170319221515,nXExpectedButYFound,sXExpectedButYFound,
          ['integer',GetResolverResultDescription(ResolvedEl,true)],FormatExpr);
      exit;
      end;
    if not (rrfReadable in ResolvedEl.Flags) then
      begin
      if RaiseOnError then
        RaiseMsg(20170319221755,nNotReadable,sNotReadable,[],FormatExpr);
      exit;
      end;
    Result:=true;
  end;

var
  TypeEl: TPasType;
begin
  Result:=cIncompatible;
  if ParamResolved.BaseType in (btAllInteger+btAllBooleans+btAllFloats) then
    Result:=cExact
  else if IsFunc and (ParamResolved.BaseType in btAllStringAndChars) then
    Result:=cExact
  else if ParamResolved.BaseType=btContext then
    begin
      TypeEl:=ParamResolved.TypeEl;
      if TypeEl.ClassType=TPasEnumType then
        Result:=cExact
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170319220517,ArgNo,Param,ParamResolved,'boolean, integer, enum value',RaiseOnError));
  if not CheckFormat(Param.format1,1,ParamResolved) then
    exit(cIncompatible);
  if not CheckFormat(Param.format2,2,ParamResolved) then
    exit(cIncompatible);
end;

function TPasResolver.BI_StrProc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built-in procedure 'Str'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,2,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  if ParentNeedsExprResult(Params) then
    begin
    if RaiseOnError then
      RaiseMsg(20170326084331,nIncompatibleTypesGotExpected,
        sIncompatibleTypesGotExpected,['procedure str','function str'],Params);
    exit(cIncompatible);
    end;

  // first param: boolean, integer, enum, class instance
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  Result:=BI_Str_CheckParam(false,Param,ParamResolved,1,RaiseOnError);
  if Result=cIncompatible then
    exit;

  // second parameter: string variable
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170319220806,1,Param,ParamResolved,'string variable',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,2,RaiseOnError);
end;

procedure TPasResolver.BI_StrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishParamExpressionAccess(P[0],rraRead);
  FinishParamExpressionAccess(P[1],rraVarParam);
end;

function TPasResolver.BI_StrFunc_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  i: Integer;
begin
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit(cIncompatible);
  Params:=TParamsExpr(Expr);
  if not ParentNeedsExprResult(Params) then
    begin
    // not in an expression -> the 'procedure str' is needed, not the 'function str'
    if RaiseOnError then
      RaiseMsg(20170326084622,nIncompatibleTypesGotExpected,
        sIncompatibleTypesGotExpected,['function str','procedure str'],Params);
    exit(cIncompatible);
    end;

  // param: string, boolean, integer, enum, class instance
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    Result:=BI_Str_CheckParam(true,Param,ParamResolved,i+1,RaiseOnError);
    if Result=cIncompatible then
    exit;
    end;

  Result:=cExact;
end;

procedure TPasResolver.BI_StrFunc_OnGetCallResult(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  if Params=nil then ;
  SetResolverIdentifier(ResolvedEl,btString,Proc.Proc,FBaseTypes[btString],[rrfReadable]);
end;

function TPasResolver.BI_ConcatArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ElTypeResolved, FirstElTypeResolved: TPasResolverResult;
  i: Integer;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  FirstElTypeResolved:=Default(TPasResolverResult);
  for i:=0 to length(Params.Params)-1 do
    begin
    // all params: array
    Param:=Params.Params[i];
    ComputeElement(Param,ParamResolved,[]);
    if not (rrfReadable in ParamResolved.Flags)
        or (ParamResolved.BaseType<>btContext)
        or not IsDynArray(ParamResolved.TypeEl) then
      exit(CheckRaiseTypeArgNo(20170329181206,i+1,Param,ParamResolved,'dynamic array',RaiseOnError));
    ComputeElement(TPasArrayType(ParamResolved.TypeEl).ElType,ElTypeResolved,[rcType]);
    Include(ElTypeResolved.Flags,rrfReadable);
    if i=0 then
      begin
      FirstElTypeResolved:=ElTypeResolved;
      Include(ElTypeResolved.Flags,rrfWritable);
      end
    else if CheckAssignResCompatibility(FirstElTypeResolved,ElTypeResolved,Param,RaiseOnError)=cIncompatible then
      exit(cIncompatible);
    end;
end;

procedure TPasResolver.BI_ConcatArray_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
end;

function TPasResolver.BI_CopyArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,1,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // first param: array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if (rrfReadable in ParamResolved.Flags)
      and (ParamResolved.BaseType=btContext)
      and IsDynArray(ParamResolved.TypeEl) then
    Result:=cExact;
  if Result=cIncompatible then
    exit(CheckRaiseTypeArgNo(20170329153951,1,Param,ParamResolved,'dynamic array',RaiseOnError));
  if length(Params.Params)=1 then
    exit(cExact);

  // check optional Start index
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329164210,2,Param,ParamResolved,'integer',RaiseOnError));
  if length(Params.Params)=2 then
    exit(cExact);

  // check optional Count
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329164329,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_CopyArray_OnGetCallResult(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult);
begin
  ComputeElement(Params.Params[0],ResolvedEl,[]);
  ResolvedEl.Flags:=ResolvedEl.Flags-[rrfWritable];
end;

function TPasResolver.BI_InsertArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// Insert(Item,var Array,Index)
var
  Params: TParamsExpr;
  Param, ItemParam: TPasExpr;
  ItemResolved, ParamResolved, ElTypeResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,3,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check Item
  ItemParam:=Params.Params[0];
  ComputeElement(ItemParam,ItemResolved,[]);
  if not (rrfReadable in ItemResolved.Flags) then
    exit(CheckRaiseTypeArgNo(20170329171400,1,ItemParam,ItemResolved,'value',RaiseOnError));

  // check Array
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not ResolvedElCanBeVarParam(ParamResolved) then
    begin
    if RaiseOnError then
      RaiseMsg(20170329171514,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Param);
    exit;
    end;
  if (ParamResolved.BaseType<>btContext)
      or not IsDynArray(ParamResolved.TypeEl) then
    exit(CheckRaiseTypeArgNo(20170329172024,2,Param,ParamResolved,'dynamic array',RaiseOnError));
  ComputeElement(TPasArrayType(ParamResolved.TypeEl).ElType,ElTypeResolved,[rcType]);
  if CheckAssignResCompatibility(ElTypeResolved,ItemResolved,ItemParam,RaiseOnError)=cIncompatible then
    exit(cIncompatible);

  // check insert Index
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329172348,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_InsertArray_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishParamExpressionAccess(P[0],rraRead);
  FinishParamExpressionAccess(P[1],rraVarParam);
  FinishParamExpressionAccess(P[2],rraRead);
end;

function TPasResolver.BI_DeleteArray_OnGetCallCompatibility(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// Delete(var Array; Start, Count: integer)
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  Result:=cIncompatible;
  if not CheckBuiltInMinParamCount(Proc,Expr,3,RaiseOnError) then
    exit;
  Params:=TParamsExpr(Expr);

  // check Array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  if not ResolvedElCanBeVarParam(ParamResolved) then
    begin
    if RaiseOnError then
      RaiseMsg(20170329173421,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Param);
    exit;
    end;
  if (ParamResolved.BaseType<>btContext)
      or not IsDynArray(ParamResolved.TypeEl) then
    exit(CheckRaiseTypeArgNo(20170329173434,1,Param,ParamResolved,'dynamic array',RaiseOnError));

  // check param Start
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
     or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329173613,2,Param,ParamResolved,'integer',RaiseOnError));

  // check param Count
  Param:=Params.Params[2];
  ComputeElement(Param,ParamResolved,[]);
  if not (rrfReadable in ParamResolved.Flags)
      or not (ParamResolved.BaseType in btAllInteger) then
    exit(CheckRaiseTypeArgNo(20170329172348,3,Param,ParamResolved,'integer',RaiseOnError));

  Result:=CheckBuiltInMaxParamCount(Proc,Params,3,RaiseOnError);
end;

procedure TPasResolver.BI_DeleteArray_OnFinishParamsExpr(
  Proc: TResElDataBuiltInProc; Params: TParamsExpr);
var
  P: TPasExprArray;
begin
  if Proc=nil then ;
  P:=Params.Params;
  FinishParamExpressionAccess(P[0],rraVarParam);
  FinishParamExpressionAccess(P[1],rraRead);
  FinishParamExpressionAccess(P[2],rraRead);
end;

constructor TPasResolver.Create;
begin
  inherited Create;
  FDefaultScope:=TPasDefaultScope.Create;
  FPendingForwards:=TFPList.Create;
  FBaseTypeStringIndex:=btChar;
  FScopeClass_Class:=TPasClassScope;
  FScopeClass_WithExpr:=TPasWithExprScope;
  PushScope(FDefaultScope);
end;

function TPasResolver.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
var
  aScanner: TPascalScanner;
  SrcPos: TPasSourcePos;
begin
  // get source position for good error messages
  aScanner:=CurrentParser.Scanner;
  if (ASourceFilename='') or StoreSrcColumns then
    begin
    SrcPos.FileName:=aScanner.CurFilename;
    SrcPos.Row:=aScanner.CurRow;
    SrcPos.Column:=aScanner.CurColumn;
    end
  else
    begin
    SrcPos.FileName:=ASourceFilename;
    SrcPos.Row:=ASourceLinenumber;
    SrcPos.Column:=0;
    end;
  Result:=CreateElement(AClass,AName,AParent,AVisibility,SrcPos);
end;

function TPasResolver.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
var
  El: TPasElement;
  SrcY: integer;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateElement ',AClass.ClassName,' Name=',AName,' Parent=',GetObjName(AParent),' (',ASrcPos.Row,',',ASrcPos.Column,')');
  {$ENDIF}
  if (AParent=nil) and (FRootElement<>nil) then
    RaiseInternalError(20160922163535,'more than one root element Class="'+AClass.ClassName+'" Root='+GetObjName(FRootElement));

  if ASrcPos.FileName='' then
    RaiseInternalError(20160922163541,'missing filename');
  SrcY:=ASrcPos.Row;
  if StoreSrcColumns then
    begin
    if (ASrcPos.Column<ParserMaxEmbeddedColumn)
        and (SrcY<ParserMaxEmbeddedRow) then
      SrcY:=-(SrcY*ParserMaxEmbeddedColumn+integer(ASrcPos.Column));
    end;

  // create element
  El:=AClass.Create(AName,AParent);
  FLastElement:=El;
  Result:=FLastElement;
  El.Visibility:=AVisibility;
  El.SourceFilename:=ASrcPos.FileName;
  El.SourceLinenumber:=SrcY;
  if FRootElement=nil then
    FRootElement:=Result;

  // create scope
  if (AClass=TPasVariable)
      or (AClass=TPasConst) then
    AddVariable(TPasVariable(El))
  else if (AClass=TPasProperty) then
    AddProperty(TPasProperty(El))
  else if AClass=TPasArgument then
    AddArgument(TPasArgument(El))
  else if AClass=TPasEnumType then
    AddEnumType(TPasEnumType(El))
  else if AClass=TPasEnumValue then
    AddEnumValue(TPasEnumValue(El))
  else if (AClass=TUnresolvedPendingRef) then
  else if (AClass=TPasAliasType)
      or (AClass=TPasTypeAliasType)
      or (AClass=TPasClassOfType)
      or (AClass=TPasArrayType)
      or (AClass=TPasProcedureType)
      or (AClass=TPasFunctionType)
      or (AClass=TPasSetType)
      or (AClass=TPasRangeType) then
    AddType(TPasType(El))
  else if AClass=TPasRecordType then
    AddRecordType(TPasRecordType(El))
  else if AClass=TPasClassType then
    AddClassType(TPasClassType(El))
  else if AClass=TPasVariant then
  else if AClass.InheritsFrom(TPasProcedure) then
    AddProcedure(TPasProcedure(El))
  else if AClass=TPasResultElement then
    AddFunctionResult(TPasResultElement(El))
  else if AClass=TProcedureBody then
    AddProcedureBody(TProcedureBody(El))
  else if AClass=TPasImplExceptOn then
    AddExceptOn(TPasImplExceptOn(El))
  else if AClass=TPasImplLabelMark then
  else if AClass=TPasOverloadedProc then
  else if (AClass=TInterfaceSection)
      or (AClass=TImplementationSection)
      or (AClass=TProgramSection)
      or (AClass=TLibrarySection) then
    AddSection(TPasSection(El))
  else if (AClass=TPasModule)
      or (AClass=TPasProgram)
      or (AClass=TPasLibrary) then
    AddModule(TPasModule(El))
  else if AClass.InheritsFrom(TPasExpr) then
    // resolved when finished
  else if AClass.InheritsFrom(TPasImplBlock) then
    // resolved finished
  else
    RaiseNotYetImplemented(20160922163544,El);
end;

function TPasResolver.FindElement(const aName: String): TPasElement;
// called by TPasParser
var
  p: SizeInt;
  RightPath, CurName: String;
  NeedPop: Boolean;
  CurScopeEl, NextEl: TPasElement;
begin
  //writeln('TPasResolver.FindElement Name="',aName,'"');
  RightPath:=aName;
  p:=1;
  CurScopeEl:=nil;
  repeat
    p:=Pos('.',RightPath);
    if p<1 then
      begin
      CurName:=RightPath;
      RightPath:='';
      end
    else
      begin
      CurName:=LeftStr(RightPath,p-1);
      Delete(RightPath,1,p);
      if RightPath='' then
        RaiseMsg(20170328003146,nIllegalExpression,sIllegalExpression,[],LastElement);
      end;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FindElement searching scope "',CurName,'" RightPath="',RightPath,'" ...');
    {$ENDIF}
    if not IsValidIdent(CurName) then
      RaiseNotYetImplemented(20170328000033,LastElement);

    if CurScopeEl<>nil then
      begin
      NeedPop:=true;
      if CurScopeEl.ClassType=TPasClassType then
        // check visibility
        PushClassDotScope(TPasClassType(CurScopeEl))
      else if CurScopeEl is TPasModule then
        PushModuleDotScope(TPasModule(CurScopeEl));
      end
    else
      NeedPop:=false;

    NextEl:=FindElementWithoutParams(CurName,LastElement,true);
    if RightPath<>'' then
      begin
      if (NextEl is TPasModule) then
        begin
        if CurScopeEl is TPasModule then
          RaiseXExpectedButYFound(20170328001619,'class',NextEl.ElementTypeName+' '+NextEl.Name,LastElement);
        CurScopeEl:=NextEl;
        end
      else if (CurScopeEl is TPasClassType) then
        CurScopeEl:=NextEl
      else
        RaiseIdentifierNotFound(20170328001941,CurName,LastElement);
      end;

    // restore scope
    if NeedPop then
      PopScope;

    if RightPath='' then
      exit(NextEl);
  until false;
end;

function TPasResolver.FindElementWithoutParams(const AName: String;
  ErrorPosEl: TPasElement; NoProcsWithArgs: boolean): TPasElement;
var
  Data: TPRFindData;
begin
  Result:=FindElementWithoutParams(AName,Data,ErrorPosEl,NoProcsWithArgs);
  if Data.Found=nil then exit; // forward type: class-of or ^
  CheckFoundElement(Data,nil);
  if (Data.StartScope<>nil) and (Data.StartScope.ClassType=ScopeClass_WithExpr)
      and (wesfNeedTmpVar in TPasWithExprScope(Data.StartScope).Flags) then
    RaiseInternalError(20160923111727); // caller forgot to handle "With", use the other FindElementWithoutParams instead
end;

function TPasResolver.FindElementWithoutParams(const AName: String; out
  Data: TPRFindData; ErrorPosEl: TPasElement; NoProcsWithArgs: boolean
  ): TPasElement;
var
  Abort: boolean;
begin
  //writeln('TPasResolver.FindIdentifier Name="',AName,'"');
  Result:=Nil;
  Abort:=false;
  Data:=Default(TPRFindData);
  Data.ErrorPosEl:=ErrorPosEl;
  IterateElements(AName,@OnFindFirstElement,@Data,Abort);
  Result:=Data.Found;
  if Result=nil then
    begin
    if (ErrorPosEl.ClassType=TPasClassOfType)
        and (TPasClassOfType(ErrorPosEl).DestType=nil) then
      begin
      // 'class of' of a not yet defined class
      Result:=CreateElement(TUnresolvedPendingRef,AName,ErrorPosEl,visDefault,
                            CurrentParser.Scanner.CurSourcePos);
      exit;
      end;
    RaiseIdentifierNotFound(20170216152722,AName,ErrorPosEl);
    end;
  if NoProcsWithArgs and (Result is TPasProcedure)
      and ProcNeedsParams(TPasProcedure(Result).ProcType)
  then
    // proc needs parameters
    RaiseMsg(20170216152347,nWrongNumberOfParametersForCallTo,
      sWrongNumberOfParametersForCallTo,[GetProcDesc(TPasProcedure(Result).ProcType)],ErrorPosEl);
end;

procedure TPasResolver.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  i: Integer;
  Scope: TPasScope;
begin
  for i:=FScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    Scope.IterateElements(AName,Scope,OnIterateElement,Data,Abort);
    if Abort then
      exit;
    if Scope is TPasSubScope then break;
    end;
end;

procedure TPasResolver.CheckFoundElement(
  const FindData: TPRFindData; Ref: TResolvedReference);
// check visibility rules
// Call this method after finding an element by searching the scopes.
var
  Proc: TPasProcedure;
  Context: TPasElement;
  FoundContext: TPasClassType;
  StartScope: TPasScope;
  OnlyTypeMembers: Boolean;
  TypeEl: TPasType;
  C: TClass;
begin
  StartScope:=FindData.StartScope;
  OnlyTypeMembers:=false;
  if StartScope is TPasDotIdentifierScope then
    begin
    OnlyTypeMembers:=TPasDotIdentifierScope(StartScope).OnlyTypeMembers;
    Include(Ref.Flags,rrfDotScope);
    if TPasDotIdentifierScope(StartScope).ConstParent then
      Include(Ref.Flags,rrfConstInherited);
    end
  else if StartScope.ClassType=ScopeClass_WithExpr then
    begin
    OnlyTypeMembers:=wesfOnlyTypeMembers in TPasWithExprScope(StartScope).Flags;
    Include(Ref.Flags,rrfDotScope);
    if wesfConstParent in TPasWithExprScope(StartScope).Flags then
      Include(Ref.Flags,rrfConstInherited);
    end
  else if StartScope.ClassType=TPasProcedureScope then
    begin
    Proc:=TPasProcedureScope(StartScope).Element as TPasProcedure;
    //writeln('TPasResolver.CheckFoundElement ',GetObjName(Proc),' ',IsClassMethod(Proc),' ElScope=',GetObjName(FindData.ElScope));
    if (FindData.ElScope<>StartScope) and IsClassMethod(Proc) then
      OnlyTypeMembers:=true;
    end;

  //writeln('TPasResolver.CheckFoundElOnStartScope StartScope=',StartScope.ClassName,
  //    ' StartIsDot=',StartScope is TPasDotIdentifierScope,
  //    ' OnlyTypeMembers=',(StartScope is TPasDotIdentifierScope)
  //       and TPasDotIdentifierScope(StartScope).OnlyTypeMembers,
  //    ' FindData.Found=',GetObjName(FindData.Found));
  if OnlyTypeMembers then
    begin
    //writeln('TPasResolver.CheckFoundElOnStartScope ',GetObjName(FindData.Found),' ',(FindData.Found is TPasVariable)
    //    and (vmClass in TPasVariable(FindData.Found).VarModifiers));
    // only class vars/procs allowed
    if (FindData.Found.ClassType=TPasConstructor) then
      // constructor: ok
    else if IsClassMethod(FindData.Found)
    then
      // class proc: ok
    else if (FindData.Found is TPasVariable)
        and (vmClass in TPasVariable(FindData.Found).VarModifiers) then
      // class var/const/property: ok
    else
      RaiseMsg(20170216152348,nOnlyClassMembersCanBeReferredWithClassReferences,
        sOnlyClassMembersCanBeReferredWithClassReferences,[],FindData.ErrorPosEl);
    end
  else if (proExtClassInstanceNoTypeMembers in Options)
      and (StartScope.ClassType=TPasDotClassScope)
      and TPasClassType(TPasDotClassScope(StartScope).ClassScope.Element).IsExternal then
    begin
    // found member in external class instance
      C:=FindData.Found.ClassType;
      if (C=TPasProcedure) or (C=TPasFunction) then
        // ok
      else if C.InheritsFrom(TPasVariable)
          and (not (vmClass in TPasVariable(FindData.Found).VarModifiers)) then
        // ok
      else
        begin
        RaiseMsg(20170331184224,nExternalClassInstanceCannotAccessStaticX,
          sExternalClassInstanceCannotAccessStaticX,
          [FindData.Found.ElementTypeName+' '+FindData.Found.Name],
          FindData.ErrorPosEl);
        end;
    end;

  if (FindData.Found is TPasProcedure) then
    begin
    Proc:=TPasProcedure(FindData.Found);
    if Proc.IsVirtual or Proc.IsOverride then
      begin
      if (StartScope.ClassType=TPasDotClassScope)
      and TPasDotClassScope(StartScope).InheritedExpr then
        begin
        // call directly
        if Proc.IsAbstract then
          RaiseMsg(20170216152352,nAbstractMethodsCannotBeCalledDirectly,
            sAbstractMethodsCannotBeCalledDirectly,[],FindData.ErrorPosEl);
        end
      else
        begin
        // call via virtual method table
        if Ref<>nil then
          Ref.Flags:=Ref.Flags+[rrfVMT];
        end;
      end;

    // constructor: NewInstance or normal call
    //  it is a NewInstance iff the scope is a class, e.g. TObject.Create
    if (Proc.ClassType=TPasConstructor)
        and OnlyTypeMembers
        and (Ref<>nil) then
      begin
      Ref.Flags:=Ref.Flags+[rrfNewInstance]-[rrfConstInherited];
      // store the class in Ref.Context
      if Ref.Context<>nil then
        RaiseInternalError(20170131141936);
      Ref.Context:=TResolvedRefCtxConstructor.Create;
      if StartScope is TPasDotClassScope then
        TypeEl:=TPasDotClassScope(StartScope).ClassScope.Element as TPasType
      else if (StartScope is TPasWithExprScope)
          and (TPasWithExprScope(StartScope).Scope is TPasClassScope) then
        TypeEl:=TPasClassScope(TPasWithExprScope(StartScope).Scope).Element as TPasType
      else if (StartScope is TPasProcedureScope) then
        TypeEl:=TPasProcedureScope(StartScope).ClassScope.Element as TPasType
      else
        RaiseInternalError(20170131150855,GetObjName(StartScope));
      TResolvedRefCtxConstructor(Ref.Context).Typ:=TypeEl;
      end;
    {$IFDEF VerbosePasResolver}
    if (Proc.ClassType=TPasConstructor) then
      begin
      write('TPasResolver.CheckFoundElement ',GetObjName(Proc));
      if Ref=nil then
        write(' no ref!')
      else
        begin
        write(' rrfNewInstance=',rrfNewInstance in Ref.Flags,
          ' StartScope=',GetObjName(StartScope),
          ' OnlyTypeMembers=',OnlyTypeMembers);
        end;
      writeln;
      end;
    {$ENDIF}

    // destructor: FreeInstance or normal call
    // it is a normal call if 'inherited'
    if (Proc.ClassType=TPasDestructor) and (Ref<>nil) then
      if ((StartScope.ClassType<>TPasDotClassScope)
          or (not TPasDotClassScope(StartScope).InheritedExpr)) then
        Ref.Flags:=Ref.Flags+[rrfFreeInstance];
    {$IFDEF VerbosePasResolver}
    if (Proc.ClassType=TPasDestructor) then
      begin
      write('TPasResolver.CheckFoundElement ',GetObjName(Proc));
      if Ref=nil then
        write(' no ref!')
      else
        begin
        write(' rrfFreeInstance=',rrfFreeInstance in Ref.Flags,
          ' StartScope=',GetObjName(StartScope));
        if StartScope.ClassType=TPasDotClassScope then
          write(' InheritedExpr=',TPasDotClassScope(StartScope).InheritedExpr);
        end;
      writeln;
      end;
    {$ENDIF}
    end;

  // check class visibility
  if FindData.Found.Visibility in [visPrivate,visProtected,visStrictPrivate,visStrictProtected] then
    begin
    Context:=GetVisibilityContext;
    FoundContext:=FindData.Found.Parent as TPasClassType;
    case FindData.Found.Visibility of
      visPrivate:
        // private members can only be accessed in same module
        if FoundContext.GetModule<>Context.GetModule then
          RaiseMsg(20170216152354,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['private',FindData.Found.Name],FindData.ErrorPosEl);
      visProtected:
        // protected members can only be accessed in same module or descendant classes
        if FoundContext.GetModule=Context.GetModule then
          // same module -> ok
        else if (Context is TPasType)
            and (CheckClassIsClass(TPasType(Context),FoundContext,FindData.ErrorPosEl)<>cIncompatible) then
          // context in class or descendant
        else
          RaiseMsg(20170216152356,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['protected',FindData.Found.Name],FindData.ErrorPosEl);
      visStrictPrivate:
        // strict private members can only be accessed in their class
        if Context<>FoundContext then
          RaiseMsg(20170216152357,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['strict private',FindData.Found.Name],FindData.ErrorPosEl);
      visStrictProtected:
        // strict protected members can only be access in their and descendant classes
        if (Context is TPasType)
            and (CheckClassIsClass(TPasType(Context),FoundContext,FindData.ErrorPosEl)<>cIncompatible) then
          // context in class or descendant
        else
          RaiseMsg(20170216152400,nCantAccessPrivateMember,sCantAccessPrivateMember,
            ['strict protected',FindData.Found.Name],FindData.ErrorPosEl);
    end;
    end;
end;

function TPasResolver.GetVisibilityContext: TPasElement;
var
  i: Integer;
begin
  for i:=ScopeCount-1 downto 0 do
    begin
    Result:=Scopes[i].VisibilityContext;
    if Result<>nil then exit;
    end;
  Result:=nil;
end;

procedure TPasResolver.FinishScope(ScopeType: TPasScopeType; El: TPasElement);
begin
  case ScopeType of
  stModule: FinishModule(El as TPasModule);
  stUsesList: FinishUsesList;
  stTypeSection: FinishTypeSection(El as TPasDeclarations);
  stTypeDef: FinishTypeDef(El as TPasType);
  stConstDef: FinishConstDef(El as TPasConst);
  stProcedure: FinishProcedure(El as TPasProcedure);
  stProcedureHeader: FinishProcedureType(El as TPasProcedureType);
  stExceptOnExpr: FinishExceptOnExpr;
  stExceptOnStatement: FinishExceptOnStatement;
  stDeclaration: FinishDeclaration(El);
  stAncestors: FinishAncestors(El as TPasClassType);
  else
    RaiseMsg(20170216152401,nNotYetImplemented,sNotYetImplemented+' FinishScope',[IntToStr(ord(ScopeType))],nil);
  end;
end;

class procedure TPasResolver.UnmangleSourceLineNumber(LineNumber: integer; out
  Line, Column: integer);
begin
  Line:=Linenumber;
  Column:=0;
  if Line<0 then begin
    Line:=-Line;
    Column:=Line mod ParserMaxEmbeddedColumn;
    Line:=Line div ParserMaxEmbeddedColumn;
  end;
end;

class function TPasResolver.GetElementSourcePosStr(El: TPasElement): string;
var
  Line, Column: integer;
begin
  if El=nil then exit('nil');
  UnmangleSourceLineNumber(El.SourceLinenumber,Line,Column);
  Result:=El.SourceFilename+'('+IntToStr(Line);
  if Column>0 then
    Result:=Result+','+IntToStr(Column);
  Result:=Result+')';
end;

destructor TPasResolver.Destroy;
begin
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy START ',ClassName);
  {$ENDIF}
  Clear;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy PopScope...');
  {$ENDIF}
  PopScope; // free default scope
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy FPendingForwards...');
  {$ENDIF}
  FreeAndNil(FPendingForwards);
  inherited Destroy;
  {$IFDEF VerbosePasResolverMem}
  writeln('TPasResolver.Destroy END ',ClassName);
  {$ENDIF}
end;

procedure TPasResolver.Clear;
begin
  RestoreSubScopes(0);
  // clear stack, keep DefaultScope
  while (FScopeCount>0) and (FTopScope<>DefaultScope) do
    PopScope;
  ClearResolveDataList(lkModule);
end;

procedure TPasResolver.ClearBuiltInIdentifiers;
var
  bt: TResolverBaseType;
begin
  ClearResolveDataList(lkBuiltIn);
  for bt in TResolverBaseType do
    FBaseTypes[bt]:=nil;
end;

procedure TPasResolver.AddObjFPCBuiltInIdentifiers(
  const TheBaseTypes: TResolveBaseTypes;
  const TheBaseProcs: TResolverBuiltInProcs);
var
  bt: TResolverBaseType;
begin
  for bt in TheBaseTypes do
    AddBaseType(BaseTypeNames[bt],bt);
  if bfLength in TheBaseProcs then
    AddBuiltInProc('Length','function Length(const String or Array): sizeint',
        @BI_Length_OnGetCallCompatibility,@BI_Length_OnGetCallResult,nil,bfLength);
  if bfSetLength in TheBaseProcs then
    AddBuiltInProc('SetLength','procedure SetLength(var String or Array; NewLength: sizeint)',
        @BI_SetLength_OnGetCallCompatibility,nil,
        @BI_SetLength_OnFinishParamsExpr,bfSetLength,[bipfCanBeStatement]);
  if bfInclude in TheBaseProcs then
    AddBuiltInProc('Include','procedure Include(var Set of Enum; const Enum)',
        @BI_InExclude_OnGetCallCompatibility,nil,
        @BI_InExclude_OnFinishParamsExpr,bfInclude,[bipfCanBeStatement]);
  if bfExclude in TheBaseProcs then
    AddBuiltInProc('Exclude','procedure Exclude(var Set of Enum; const Enum)',
        @BI_InExclude_OnGetCallCompatibility,nil,
        @BI_InExclude_OnFinishParamsExpr,bfExclude,[bipfCanBeStatement]);
  if bfBreak in TheBaseProcs then
    AddBuiltInProc('Break','procedure Break',
        @BI_Break_OnGetCallCompatibility,nil,nil,bfBreak,[bipfCanBeStatement]);
  if bfContinue in TheBaseProcs then
    AddBuiltInProc('Continue','procedure Continue',
        @BI_Continue_OnGetCallCompatibility,nil,nil,bfContinue,[bipfCanBeStatement]);
  if bfExit in TheBaseProcs then
    AddBuiltInProc('Exit','procedure Exit(result)',
        @BI_Exit_OnGetCallCompatibility,nil,nil,bfExit,[bipfCanBeStatement]);
  if bfInc in TheBaseProcs then
    AddBuiltInProc('Inc','procedure Inc(var Integer; const Incr: Integer = 1)',
        @BI_IncDec_OnGetCallCompatibility,nil,
        @BI_IncDec_OnFinishParamsExpr,bfInc,[bipfCanBeStatement]);
  if bfDec in TheBaseProcs then
    AddBuiltInProc('Dec','procedure Dec(var Integer; const Decr: Integer = 1)',
        @BI_IncDec_OnGetCallCompatibility,nil,
        @BI_IncDec_OnFinishParamsExpr,bfDec,[bipfCanBeStatement]);
  if bfAssigned in TheBaseProcs then
    AddBuiltInProc('Assigned','function Assigned(const Pointer or Class or Class-of): boolean',
        @BI_Assigned_OnGetCallCompatibility,@BI_Assigned_OnGetCallResult,nil,bfAssigned);
  if bfChr in TheBaseProcs then
    AddBuiltInProc('Chr','function Chr(const Integer): char',
        @BI_Chr_OnGetCallCompatibility,@BI_Chr_OnGetCallResult,nil,bfChr);
  if bfOrd in TheBaseProcs then
    AddBuiltInProc('Ord','function Ord(const Enum or Char): integer',
        @BI_Ord_OnGetCallCompatibility,@BI_Ord_OnGetCallResult,nil,bfOrd);
  if bfLow in TheBaseProcs then
    AddBuiltInProc('Low','function Low(const array or ordinal): ordinal or integer',
        @BI_LowHigh_OnGetCallCompatibility,@BI_LowHigh_OnGetCallResult,nil,bfLow);
  if bfHigh in TheBaseProcs then
    AddBuiltInProc('High','function High(const array or ordinal): ordinal or integer',
        @BI_LowHigh_OnGetCallCompatibility,@BI_LowHigh_OnGetCallResult,nil,bfHigh);
  if bfPred in TheBaseProcs then
    AddBuiltInProc('Pred','function Pred(const ordinal): ordinal',
        @BI_PredSucc_OnGetCallCompatibility,@BI_PredSucc_OnGetCallResult,nil,bfPred);
  if bfSucc in TheBaseProcs then
    AddBuiltInProc('Succ','function Succ(const ordinal): ordinal',
        @BI_PredSucc_OnGetCallCompatibility,@BI_PredSucc_OnGetCallResult,nil,bfSucc);
  if bfStrProc in TheBaseProcs then
    AddBuiltInProc('Str','procedure Str(const var; var String)',
        @BI_StrProc_OnGetCallCompatibility,nil,
        @BI_StrProc_OnFinishParamsExpr,bfStrProc,[bipfCanBeStatement]);
  if bfStrFunc in TheBaseProcs then
    AddBuiltInProc('Str','function Str(const var): String',
        @BI_StrFunc_OnGetCallCompatibility,@BI_StrFunc_OnGetCallResult,nil,bfStrFunc);
  if bfConcatArray in TheBaseProcs then
    AddBuiltInProc('Concat','function Concat(const Array1, Array2, ...): Array',
        @BI_ConcatArray_OnGetCallCompatibility,@BI_ConcatArray_OnGetCallResult,nil,bfConcatArray);
  if bfCopyArray in TheBaseProcs then
    AddBuiltInProc('Copy','function Copy(const Array; Start: integer = 0; Count: integer = all): Array',
        @BI_CopyArray_OnGetCallCompatibility,@BI_CopyArray_OnGetCallResult,nil,bfCopyArray);
  if bfInsertArray in TheBaseProcs then
    AddBuiltInProc('Insert','procedure Insert(const Element; var Array; Index: integer)',
        @BI_InsertArray_OnGetCallCompatibility,nil,
        @BI_InsertArray_OnFinishParamsExpr,bfInsertArray,[bipfCanBeStatement]);
  if bfDeleteArray in TheBaseProcs then
    AddBuiltInProc('Delete','procedure Delete(var Array; Start, Count: integer)',
        @BI_DeleteArray_OnGetCallCompatibility,nil,
        @BI_DeleteArray_OnFinishParamsExpr,bfDeleteArray,[bipfCanBeStatement]);
end;

function TPasResolver.AddBaseType(const aName: string; Typ: TResolverBaseType
  ): TResElDataBaseType;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef.Create(aName,nil);
  if not (Typ in [btNone,btCustom]) then
    FBaseTypes[Typ]:=El;
  Result:=TResElDataBaseType.Create;
  Result.BaseType:=Typ;
  AddResolveData(El,Result,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,El,pikBaseType);
end;

function TPasResolver.AddCustomBaseType(const aName: string;
  aClass: TResElDataBaseTypeClass): TPasUnresolvedSymbolRef;
var
  CustomData: TResElDataBaseType;
begin
  Result:=TPasUnresolvedSymbolRef.Create(aName,nil);
  CustomData:=aClass.Create;
  CustomData.BaseType:=btCustom;
  AddResolveData(Result,CustomData,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,Result,pikBaseType);
end;

function TPasResolver.IsBaseType(aType: TPasType; BaseType: TResolverBaseType
  ): boolean;
begin
  Result:=false;
  if aType=nil then exit;
  if aType.ClassType<>TPasUnresolvedSymbolRef then exit;
  Result:=CompareText(aType.Name,BaseTypeNames[BaseType])=0;
end;

function TPasResolver.AddBuiltInProc(const aName: string; Signature: string;
  const GetCallCompatibility: TOnGetCallCompatibility;
  const GetCallResult: TOnGetCallResult;
  const FinishParamsExpr: TOnFinishParamsExpr;
  const BuiltIn: TResolverBuiltInProc; const Flags: TBuiltInProcFlags
  ): TResElDataBuiltInProc;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef.Create(aName,nil);
  Result:=TResElDataBuiltInProc.Create;
  Result.Proc:=El;
  Result.Signature:=Signature;
  Result.BuiltIn:=BuiltIn;
  Result.GetCallCompatibility:=GetCallCompatibility;
  Result.GetCallResult:=GetCallResult;
  Result.FinishParamsExpression:=FinishParamsExpr;
  Result.Flags:=Flags;
  AddResolveData(El,Result,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,El,pikBuiltInProc);
end;

procedure TPasResolver.AddResolveData(El: TPasElement; Data: TResolveData;
  Kind: TResolveDataListKind);
begin
  Data.Element:=El;
  Data.Owner:=Self;
  Data.Next:=FLastCreatedData[Kind];
  FLastCreatedData[Kind]:=Data;
  El.CustomData:=Data;
end;

function TPasResolver.CreateReference(DeclEl, RefEl: TPasElement;
  Access: TResolvedRefAccess; FindData: PPRFindData): TResolvedReference;

  procedure RaiseAlreadySet;
  var
    FormerDeclEl: TPasElement;
  begin
    writeln('RaiseAlreadySet RefEl=',GetObjName(RefEl),' DeclEl=',GetObjName(DeclEl));
    writeln('  RefEl at ',GetElementSourcePosStr(RefEl));
    writeln('  RefEl.CustomData=',GetObjName(RefEl.CustomData));
    if RefEl.CustomData is TResolvedReference then
      begin
        FormerDeclEl:=TResolvedReference(RefEl.CustomData).Declaration;
      writeln('  TResolvedReference(RefEl.CustomData).Declaration=',GetObjName(FormerDeclEl),
       ' IsSame=',FormerDeclEl=DeclEl);
      end;
    RaiseInternalError(20160922163554,'customdata<>nil');
  end;

begin
  if RefEl.CustomData<>nil then
    RaiseAlreadySet;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateReference RefEl=',GetObjName(RefEl),' DeclEl=',GetObjName(DeclEl));
  {$ENDIF}
  Result:=TResolvedReference.Create;
  Result.Access:=Access;
  if FindData<>nil then
    begin
    if FindData^.StartScope.ClassType=ScopeClass_WithExpr then
      Result.WithExprScope:=TPasWithExprScope(FindData^.StartScope);
    end;
  AddResolveData(RefEl,Result,lkModule);
  Result.Declaration:=DeclEl;
end;

function TPasResolver.CreateScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  if not ScopeClass.IsStoredInElement then
    RaiseInternalError(20160923121858);
  if El.CustomData<>nil then
    RaiseInternalError(20160923121849);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateScope El=',GetObjName(El),' ScopeClass=',ScopeClass.ClassName);
  {$ENDIF}
  Result:=ScopeClass.Create;
  if Result.FreeOnPop then
    begin
    Result.Element:=El;
    El.CustomData:=Result;
    Result.Owner:=Self;
    end
  else
    // add to free list
    AddResolveData(El,Result,lkModule);
end;

procedure TPasResolver.PopScope;
var
  Scope: TPasScope;
begin
  if FScopeCount=0 then
    RaiseInternalError(20160922163557);
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.PopScope ',FScopeCount,' ',FTopScope<>nil,' IsDefault=',FTopScope=FDefaultScope);
  writeln('TPasResolver.PopScope ',FTopScope.ClassName,' IsStoredInElement=',FTopScope.IsStoredInElement,' Element=',GetObjName(FTopScope.Element),' FreeOnPop=',FTopScope.FreeOnPop);
  {$ENDIF}
  dec(FScopeCount);
  if FTopScope.FreeOnPop then
    begin
    Scope:=FScopes[FScopeCount];
    if (Scope.Element<>nil) and (Scope.Element.CustomData=Scope) then
      Scope.Element.CustomData:=nil;
    if Scope=FDefaultScope then
      FDefaultScope:=nil;
    FScopes[FScopeCount]:=nil;
    Scope.Free;
    end;
  if FScopeCount>0 then
    FTopScope:=FScopes[FScopeCount-1]
  else
    FTopScope:=nil;
end;

procedure TPasResolver.PushScope(Scope: TPasScope);
begin
  if Scope=nil then
    RaiseInternalError(20160922163601);
  if length(FScopes)=FScopeCount then
    SetLength(FScopes,FScopeCount*2+10);
  FScopes[FScopeCount]:=Scope;
  inc(FScopeCount);
  FTopScope:=Scope;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.PushScope ScopeCount=',ScopeCount,' ',GetObjName(FTopScope));
  {$ENDIF}
end;

function TPasResolver.PushScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  Result:=CreateScope(El,ScopeClass);
  PushScope(Result);
end;

function TPasResolver.PushModuleDotScope(aModule: TPasModule): TPasModuleDotScope;
begin
  Result:=TPasModuleDotScope.Create;
  Result.Owner:=Self;
  Result.Module:=aModule;
  if aModule is TPasProgram then
    begin // program
    if TPasProgram(aModule).ProgramSection<>nil then
      Result.InterfaceScope:=
        TPasProgram(aModule).ProgramSection.CustomData as TPasSectionScope;
    end
  else if aModule is TPasLibrary then
    begin // library
    if TPasLibrary(aModule).LibrarySection<>nil then
      Result.InterfaceScope:=
        TPasLibrary(aModule).LibrarySection.CustomData as TPasSectionScope;
    end
  else
    begin // unit
    if aModule.InterfaceSection<>nil then
      Result.InterfaceScope:=
        aModule.InterfaceSection.CustomData as TPasSectionScope;
    if (aModule=CurrentParser.CurModule)
        and (aModule.ImplementationSection<>nil)
        and (aModule.ImplementationSection.CustomData<>nil)
    then
      Result.ImplementationScope:=aModule.ImplementationSection.CustomData as TPasSectionScope;
    end;

  PushScope(Result);
end;

function TPasResolver.PushClassDotScope(var CurClassType: TPasClassType
  ): TPasDotClassScope;
var
  ClassScope: TPasClassScope;
  Ref: TResolvedReference;
begin
  if CurClassType.IsForward then
    begin
    Ref:=CurClassType.CustomData as TResolvedReference;
    CurClassType:=Ref.Declaration as TPasClassType;
    end;
  if CurClassType.CustomData=nil then
    RaiseInternalError(20160922163611);
  ClassScope:=CurClassType.CustomData as TPasClassScope;
  Result:=TPasDotClassScope.Create;
  Result.Owner:=Self;
  Result.ClassScope:=ClassScope;
  PushScope(Result);
end;

function TPasResolver.PushRecordDotScope(CurRecordType: TPasRecordType
  ): TPasDotRecordScope;
var
  RecScope: TPasRecordScope;
begin
  RecScope:=CurRecordType.CustomData as TPasRecordScope;
  Result:=TPasDotRecordScope.Create;
  Result.Owner:=Self;
  Result.IdentifierScope:=RecScope;
  PushScope(Result);
end;

function TPasResolver.PushEnumDotScope(CurEnumType: TPasEnumType
  ): TPasDotEnumTypeScope;
var
  EnumScope: TPasEnumTypeScope;
begin
  EnumScope:=CurEnumType.CustomData as TPasEnumTypeScope;
  Result:=TPasDotEnumTypeScope.Create;
  Result.Owner:=Self;
  Result.IdentifierScope:=EnumScope;
  PushScope(Result);
end;

procedure TPasResolver.ResetSubScopes(out Depth: integer);
// move all sub scopes from Scopes to SubScopes
begin
  Depth:=FSubScopeCount;
  while TopScope is TPasSubScope do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResetSubScopes moving ',TopScope.ClassName,' ScopeCount=',ScopeCount,' SubScopeCount=',FSubScopeCount);
    {$ENDIF}
    if FSubScopeCount=length(FSubScopes) then
      SetLength(FSubScopes,FSubScopeCount+4);
    FSubScopes[FSubScopeCount]:=TopScope;
    inc(FSubScopeCount);
    dec(FScopeCount);
    FScopes[FScopeCount]:=nil;
    if FScopeCount>0 then
      FTopScope:=FScopes[FScopeCount-1]
    else
      FTopScope:=nil;
    end;
end;

procedure TPasResolver.RestoreSubScopes(Depth: integer);
// restore sub scopes
begin
  while FSubScopeCount>Depth do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.RestoreSubScopes moving ',FSubScopes[FSubScopeCount-1].ClassName,' ScopeCount=',ScopeCount,' SubScopeCount=',FSubScopeCount);
    {$ENDIF}
    if FScopeCount=length(FScopes) then
      SetLength(FScopes,FScopeCount+4);
    dec(FSubScopeCount);
    FScopes[FScopeCount]:=FSubScopes[FSubScopeCount];
    FTopScope:=FScopes[FScopeCount];
    FSubScopes[FSubScopeCount]:=nil;
    inc(FScopeCount);
    end;
end;

procedure TPasResolver.SetLastMsg(const id: int64; MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String; Args: array of const;
  Element: TPasElement);
{$IFDEF VerbosePasResolver}
var
  s: string;
{$ENDIF}
begin
  FLastMsgId := id;
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := SafeFormat(Fmt,Args);
  FLastElement := Element;
  CreateMsgArgs(FLastMsgArgs,Args);
  {$IFDEF VerbosePasResolver}
  write('TPasResolver.SetLastMsg ',id,' ',GetElementSourcePosStr(Element),' ');
  s:='';
  str(MsgType,s);
  write(s);
  writeln(': [',MsgNumber,'] ',FLastMsg);
  {$ENDIF}
end;

procedure TPasResolver.RaiseMsg(const Id: int64; MsgNumber: integer;
  const Fmt: String; Args: array of const; ErrorPosEl: TPasElement);
var
  E: EPasResolve;
begin
  SetLastMsg(Id,mtError,MsgNumber,Fmt,Args,ErrorPosEl);
  E:=EPasResolve.Create(FLastMsg);
  E.Id:=Id;
  E.MsgType:=mtError;
  E.MsgNumber:=MsgNumber;
  E.PasElement:=ErrorPosEl;
  E.Args:=FLastMsgArgs;
  raise E;
end;

procedure TPasResolver.RaiseNotYetImplemented(id: int64; El: TPasElement;
  Msg: string);
var
  s: String;
begin
  s:=sNotYetImplemented+' ['+IntToStr(id)+']';
  if Msg<>'' then
    s:=s+' '+Msg;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseNotYetImplemented s="',s,'" El=',GetObjName(El));
  {$ENDIF}
  RaiseMsg(id,nNotYetImplemented,s,[GetObjName(El)],El);
end;

procedure TPasResolver.RaiseInternalError(id: int64; const Msg: string);
begin
  raise Exception.Create('Internal error: ['+IntToStr(id)+'] '+Msg);
end;

procedure TPasResolver.RaiseInvalidScopeForElement(id: int64; El: TPasElement;
  const Msg: string);
var
  i: Integer;
  s: String;
begin
  s:='['+IntToStr(id)+'] invalid scope for "'+GetObjName(El)+'": ';
  for i:=0 to ScopeCount-1 do
    begin
    if i>0 then s:=s+',';
    s:=s+Scopes[i].ClassName;
    end;
  if Msg<>'' then
    s:=s+': '+Msg;
  RaiseInternalError(id,s);
end;

procedure TPasResolver.RaiseIdentifierNotFound(id: int64; Identifier: string;
  El: TPasElement);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseIdentifierNotFound START "',Identifier,'"');
  WriteScopes;
  {$ENDIF}
  RaiseMsg(id,nIdentifierNotFound,sIdentifierNotFound,[Identifier],El);
end;

procedure TPasResolver.RaiseXExpectedButYFound(id: int64; const X, Y: string;
  El: TPasElement);
begin
  RaiseMsg(id,nXExpectedButYFound,sXExpectedButYFound,[X,Y],El);
end;

procedure TPasResolver.RaiseConstantExprExp(id: int64; ErrorEl: TPasElement);
begin
  RaiseMsg(id,nConstantExpressionExpected,sConstantExpressionExpected,[],ErrorEl);
end;

procedure TPasResolver.RaiseIncompatibleTypeDesc(id: int64; MsgNumber: integer;
  const Args: array of const; const DescA, DescB: String; ErrorEl: TPasElement);

  function GetString(ArgNo: integer): string;
  begin
    case Args[ArgNo].VType of
    vtAnsiString: Result:=AnsiString(Args[ArgNo].VAnsiString);
    else
      Result:='invalid param '+IntToStr(Ord(Args[ArgNo].VType));
    end;
  end;

begin
  case MsgNumber of
    nIllegalTypeConversionTo:
      RaiseMsg(id,MsgNumber,sIllegalTypeConversionTo,[DescA,DescB],ErrorEl);
    nIncompatibleTypesGotExpected:
      RaiseMsg(id,MsgNumber,sIncompatibleTypesGotExpected,[DescA,DescB],ErrorEl);
    nIncompatibleTypeArgNo:
      RaiseMsg(id,MsgNumber,sIncompatibleTypeArgNo,[GetString(0),DescA,DescB],ErrorEl);
    nIncompatibleTypeArgNoVarParamMustMatchExactly:
      RaiseMsg(id,MsgNumber,sIncompatibleTypeArgNoVarParamMustMatchExactly,
               [GetString(0),DescA,DescB],ErrorEl);
    nResultTypeMismatchExpectedButFound:
      RaiseMsg(id,MsgNumber,sResultTypeMismatchExpectedButFound,[DescA,DescB],ErrorEl);
  else
    RaiseInternalError(20170329112911);
  end;
end;

procedure TPasResolver.RaiseIncompatibleType(id: int64; MsgNumber: integer;
  const Args: array of const; TypeA, TypeB: TPasType; ErrorEl: TPasElement);
var
  DescA, DescB: String;
begin
  DescA:=GetTypeDesc(TypeA);
  DescB:=GetTypeDesc(TypeB);
  if DescA=DescB then
    begin
    DescA:=GetTypeDesc(TypeA,true);
    DescB:=GetTypeDesc(TypeB,true);
    end;
  RaiseIncompatibleTypeDesc(id,MsgNumber,Args,DescA,DescB,ErrorEl);
end;

procedure TPasResolver.RaiseIncompatibleTypeRes(id: int64; MsgNumber: integer;
  const Args: array of const; const TypeA, TypeB: TPasResolverResult;
  ErrorEl: TPasElement);
var
  DescA, DescB: String;
begin
  if TypeA.BaseType<>TypeB.BaseType then
    begin
    if TypeA.BaseType=btContext then
      DescA:=GetTypeDesc(TypeA.TypeEl)
    else
      DescA:=BaseTypeNames[TypeA.BaseType];
    if TypeB.BaseType=btContext then
      DescB:=GetTypeDesc(TypeB.TypeEl)
    else
      DescB:=BaseTypeNames[TypeB.BaseType];
    if DescA=DescB then
      begin
      if TypeA.BaseType=btContext then
        DescA:=GetTypeDesc(TypeA.TypeEl,true);
      if TypeB.BaseType=btContext then
        DescB:=GetTypeDesc(TypeB.TypeEl,true);
      end;
    end
  else if (TypeA.TypeEl<>nil) and (TypeB.TypeEl<>nil) then
    begin
    DescA:=GetTypeDesc(TypeA.TypeEl);
    DescB:=GetTypeDesc(TypeB.TypeEl);
    if DescA=DescB then
      begin
      DescA:=GetTypeDesc(TypeA.TypeEl,true);
      DescB:=GetTypeDesc(TypeB.TypeEl,true);
      end;
    end
  else
    begin
    DescA:=GetResolverResultDescription(TypeA,true);
    DescB:=GetResolverResultDescription(TypeA,true);
    if DescA=DescB then
      begin
      DescA:=GetResolverResultDescription(TypeA,false);
      DescB:=GetResolverResultDescription(TypeA,false);
      end;
    end;
  RaiseIncompatibleTypeDesc(id,MsgNumber,Args,DescA,DescB,ErrorEl);
end;

procedure TPasResolver.LogMsg(const id: int64; MsgType: TMessageType;
  MsgNumber: integer; const Fmt: String; Args: array of const;
  PosEl: TPasElement);
begin
  SetLastMsg(id,MsgType,MsgNumber,Fmt,Args,PosEl);
  if Assigned(OnLog) then
    OnLog(Self,FLastMsg)
  else if Assigned(CurrentParser.OnLog) then
    CurrentParser.OnLog(Self,FLastMsg);
end;

function TPasResolver.CheckCallProcCompatibility(ProcType: TPasProcedureType;
  Params: TParamsExpr; RaiseOnError: boolean): integer;
var
  ProcArgs: TFPList;
  i, ParamCnt, ParamCompatibility: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  IsVarArgs: Boolean;
begin
  Result:=cExact;
  ProcArgs:=ProcType.Args;
  // check args
  ParamCnt:=length(Params.Params);
  IsVarArgs:=false;
  i:=0;
  while i<ParamCnt do
    begin
    Param:=Params.Params[i];
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckCallProcCompatibility ',i,'/',ParamCnt);
    {$ENDIF}
    if i<ProcArgs.Count then
      begin
      ParamCompatibility:=CheckParamCompatibility(Param,TPasArgument(ProcArgs[i]),i,RaiseOnError);
      if ParamCompatibility=cIncompatible then
        exit(cIncompatible);
      end
    else
      begin
      IsVarArgs:=IsVarArgs or ((ProcType.Parent is TPasProcedure)
        and (pmVarargs in TPasProcedure(ProcType.Parent).Modifiers));
      if IsVarArgs then
        begin
        ComputeElement(Param,ParamResolved,[],Param);
        if not (rrfReadable in ParamResolved.Flags) then
          begin
          if RaiseOnError then
            RaiseMsg(20170318234957,nVariableIdentifierExpected,
              sVariableIdentifierExpected,[],Param);
          exit(cIncompatible);
          end;
        ParamCompatibility:=cExact;
        end
      else
        begin
        // too many arguments
        if RaiseOnError then
          RaiseMsg(20170216152408,nWrongNumberOfParametersForCallTo,
            sWrongNumberOfParametersForCallTo,[GetProcDesc(ProcType)],Param);
        exit(cIncompatible);
        end;
      end;
    inc(Result,ParamCompatibility);
    inc(i);
    end;
  if (i<ProcArgs.Count) and (TPasArgument(ProcArgs[i]).ValueExpr=nil) then
    begin
    // not enough arguments
    if RaiseOnError then
      // ToDo: position cursor on identifier
      RaiseMsg(20170216152410,nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[GetProcDesc(ProcType)],Params.Value);
    exit(cIncompatible);
    end;
end;

function TPasResolver.CheckCallPropertyCompatibility(PropEl: TPasProperty;
  Params: TParamsExpr; RaiseOnError: boolean): integer;
var
  PropArg: TPasArgument;
  ArgNo, ParamComp: Integer;
  Param: TPasExpr;
begin
  Result:=cExact;
  if PropEl.Args.Count<length(Params.Params) then
    begin
    if not RaiseOnError then exit(cIncompatible);
    RaiseMsg(20170216152412,nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
      [PropEl.Name],Params)
    end
  else if PropEl.Args.Count>length(Params.Params) then
    begin
    if not RaiseOnError then exit(cIncompatible);
    RaiseMsg(20170216152413,nMissingParameterX,sMissingParameterX,
      [TPasArgument(PropEl.Args[length(Params.Params)]).Name],Params);
    end;
  for ArgNo:=0 to PropEl.Args.Count-1 do
    begin
    PropArg:=TPasArgument(PropEl.Args[ArgNo]);
    Param:=Params.Params[ArgNo];
    ParamComp:=CheckParamCompatibility(Param,PropArg,ArgNo,RaiseOnError);
    if ParamComp=cIncompatible then
      exit(cIncompatible);
    inc(Result,ParamComp);
    end;
end;

function TPasResolver.CheckCallArrayCompatibility(ArrayEl: TPasArrayType;
  Params: TParamsExpr; RaiseOnError: boolean): integer;
var
  ArgNo: Integer;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;

  procedure GetNextParam;
  begin
    if ArgNo>=length(Params.Params) then
      RaiseMsg(20170216152415,nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
    Param:=Params.Params[ArgNo];
    ComputeElement(Param,ParamResolved,[]);
    inc(ArgNo);
  end;

var
  DimNo: integer;
  RangeResolved: TPasResolverResult;
  bt: TResolverBaseType;
  NextType: TPasType;
begin
  ArgNo:=0;
  repeat
    if length(ArrayEl.Ranges)=0 then
      begin
      // dynamic/open array -> needs exactly one integer
      GetNextParam;
      if (not (rrfReadable in ParamResolved.Flags))
          or not (ParamResolved.BaseType in btAllInteger) then
        exit(CheckRaiseTypeArgNo(20170216152417,ArgNo,Param,ParamResolved,'integer',RaiseOnError));
      end
    else
      begin
      // static array
      for DimNo:=0 to length(ArrayEl.Ranges)-1 do
        begin
        GetNextParam;
        ComputeElement(ArrayEl.Ranges[DimNo],RangeResolved,[]);
        bt:=RangeResolved.BaseType;
        if bt=btRange then
          bt:=RangeResolved.SubType;
        if not (rrfReadable in ParamResolved.Flags) then
          begin
          if not RaiseOnError then exit(cIncompatible);
          RaiseIncompatibleTypeRes(20170216152421,nIncompatibleTypeArgNo,
            [IntToStr(ArgNo)],ParamResolved,RangeResolved,Param);
          end;
        if (bt in btAllBooleans) and (ParamResolved.BaseType in btAllBooleans) then
          continue
        else if (bt in btAllInteger) and (ParamResolved.BaseType in btAllInteger) then
          continue
        else if (bt in [btChar,btWideChar]) and (ParamResolved.BaseType in [btChar,btWideChar]) then
          continue
        else if (bt=btContext) and (ParamResolved.BaseType=btContext) then
          begin
          if (RangeResolved.TypeEl.ClassType=TPasEnumType)
              and (RangeResolved.TypeEl=ParamResolved.TypeEl) then
            continue;
          end;
        // incompatible
        if not RaiseOnError then exit(cIncompatible);
        RaiseIncompatibleTypeRes(20170216152422,nIncompatibleTypeArgNo,
          [IntToStr(ArgNo)],ParamResolved,RangeResolved,Param);
        end;
      end;
    if ArgNo=length(Params.Params) then exit(cExact);

    // there are more parameters -> continue in sub array
    NextType:=ResolveAliasType(ArrayEl.ElType);
    if NextType.ClassType<>TPasArrayType then
      RaiseMsg(20170216152424,nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
    ArrayEl:=TPasArrayType(NextType);
  until false;
end;

function TPasResolver.CheckOverloadProcCompatibility(Proc1, Proc2: TPasProcedure
  ): boolean;
// returns if number and type of arguments fit
// does not check calling convention
var
  ProcArgs1, ProcArgs2: TFPList;
  i: Integer;
begin
  Result:=false;
  ProcArgs1:=Proc1.ProcType.Args;
  ProcArgs2:=Proc2.ProcType.Args;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckOverloadProcCompatibility START Count=',ProcArgs1.Count,' ',ProcArgs2.Count);
  {$ENDIF}
  // check args
  if ProcArgs1.Count<>ProcArgs2.Count then
    exit;
  for i:=0 to ProcArgs1.Count-1 do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckOverloadProcCompatibility ',i,'/',ProcArgs1.Count);
    {$ENDIF}
    if not CheckProcArgCompatibility(TPasArgument(ProcArgs1[i]),TPasArgument(ProcArgs2[i])) then
      exit;
    end;
  Result:=true;
end;

function TPasResolver.CheckProcAssignCompatibility(Proc1,
  Proc2: TPasProcedureType): boolean;
var
  ProcArgs1, ProcArgs2: TFPList;
  i: Integer;
  Result1Resolved, Result2Resolved: TPasResolverResult;
begin
  Result:=false;
  if Proc1.ClassType<>Proc2.ClassType then exit;
  if Proc1.IsOfObject<>Proc2.IsOfObject then exit;
  if Proc1.IsNested<>Proc2.IsNested then exit;
  if Proc1.CallingConvention<>Proc2.CallingConvention then exit;
  ProcArgs1:=Proc1.Args;
  ProcArgs2:=Proc2.Args;
  if ProcArgs1.Count<>ProcArgs2.Count then exit;
  for i:=0 to ProcArgs1.Count-1 do
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckProcAssignCompatibility ',i,'/',ProcArgs1.Count);
    {$ENDIF}
    if not CheckProcArgCompatibility(TPasArgument(ProcArgs1[i]),TPasArgument(ProcArgs2[i])) then
      exit;
    end;
  if Proc1 is TPasFunctionType then
    begin
    ComputeElement(TPasFunctionType(Proc1).ResultEl.ResultType,Result1Resolved,[rcType]);
    ComputeElement(TPasFunctionType(Proc2).ResultEl.ResultType,Result2Resolved,[rcType]);
    if (Result1Resolved.BaseType<>Result2Resolved.BaseType)
        or (Result1Resolved.TypeEl=nil)
        or (Result1Resolved.TypeEl<>Result2Resolved.TypeEl) then
      exit;
    end;
  Result:=true;
end;

function TPasResolver.CheckProcArgCompatibility(Arg1, Arg2: TPasArgument): boolean;
begin
  Result:=false;

  // check access: var, const, ...
  if Arg1.Access<>Arg2.Access then exit;

  // check untyped
  if Arg1.ArgType=nil then
    exit(Arg2.ArgType=nil);
  if Arg2.ArgType=nil then exit;

  Result:=CheckProcArgTypeCompatibility(Arg1.ArgType,Arg2.ArgType);
end;

function TPasResolver.CheckProcArgTypeCompatibility(Arg1, Arg2: TPasType
  ): boolean;
var
  Arg1Resolved, Arg2Resolved: TPasResolverResult;
  C: TClass;
  Arr1, Arr2: TPasArrayType;
begin
  ComputeElement(Arg1,Arg1Resolved,[rcType]);
  ComputeElement(Arg2,Arg2Resolved,[rcType]);
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.CheckProcArgTypeCompatibility Arg1=',GetResolverResultDesc(Arg1Resolved),' Arg2=',GetResolverResultDesc(Arg2Resolved));
  {$ENDIF}

  if (Arg1Resolved.BaseType<>Arg2Resolved.BaseType)
      or (Arg1Resolved.TypeEl=nil)
      or (Arg2Resolved.TypeEl=nil) then
    exit(false);
  if (Arg1Resolved.BaseType=Arg2Resolved.BaseType)
      and IsSameType(Arg1Resolved.TypeEl,Arg2Resolved.TypeEl) then
    exit(true);
  C:=Arg1Resolved.TypeEl.ClassType;
  if (C=TPasArrayType) and (Arg2Resolved.TypeEl.ClassType=TPasArrayType) then
    begin
    Arr1:=TPasArrayType(Arg1Resolved.TypeEl);
    Arr2:=TPasArrayType(Arg2Resolved.TypeEl);
    if length(Arr1.Ranges)<>length(Arr2.Ranges) then
      exit(false);
    if length(Arr1.Ranges)>0 then
      RaiseNotYetImplemented(20170328093733,Arr1.Ranges[0],'anonymous static array');
    Result:=CheckProcArgTypeCompatibility(Arr1.ElType,Arr2.ElType);
    exit;
    end;

  Result:=false;
end;

function TPasResolver.CheckCanBeLHS(const ResolvedEl: TPasResolverResult;
  ErrorOnFalse: boolean; ErrorEl: TPasElement): boolean;
var
  El: TPasElement;
begin
  Result:=false;
  El:=ResolvedEl.IdentEl;
  if El=nil then
    begin
    if ErrorOnFalse then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckCanBeLHS ',GetResolverResultDesc(ResolvedEl));
      {$ENDIF}
      if (ResolvedEl.TypeEl<>nil) and (ResolvedEl.ExprEl<>nil) then
        RaiseXExpectedButYFound(20170216152727,'identifier',ResolvedEl.TypeEl.ElementTypeName,ResolvedEl.ExprEl)
      else
        RaiseMsg(20170216152426,nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
      end;
    exit;
    end;
  if [rrfWritable,rrfAssignable]*ResolvedEl.Flags<>[] then
    exit(true);
  // not writable
  if not ErrorOnFalse then exit;
  if ResolvedEl.IdentEl is TPasProperty then
    RaiseMsg(20170216152427,nPropertyNotWritable,sPropertyNotWritable,[],ErrorEl)
  else
    RaiseMsg(20170216152429,nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
end;

function TPasResolver.CheckAssignCompatibility(const LHS, RHS: TPasElement;
  RaiseOnIncompatible: boolean): integer;
var
  LeftResolved, RightResolved: TPasResolverResult;
  Flags: TPasResolverComputeFlags;
  IsProcType: Boolean;
begin
  ComputeElement(LHS,LeftResolved,[rcNoImplicitProc]);
  Flags:=[];
  IsProcType:=IsProcedureType(LeftResolved);
  if IsProcType then
    if msDelphi in CurrentParser.CurrentModeswitches then
      Include(Flags,rcNoImplicitProc)
    else
      Include(Flags,rcNoImplicitProcType);
  ComputeElement(RHS,RightResolved,Flags);
  Result:=CheckAssignResCompatibility(LeftResolved,RightResolved,RHS,RaiseOnIncompatible);
end;

function TPasResolver.CheckAssignResCompatibility(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  TypeEl: TPasType;
  Handled: Boolean;
begin
  // check if the RHS can be converted to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignResCompatibility START LHS='+GetResolverResultDesc(LHS)+' RHS='+GetResolverResultDesc(RHS));
  {$ENDIF}
  Result:=-1;

  Handled:=false;
  Result:=CheckAssignCompatibilityCustom(LHS,RHS,ErrorEl,RaiseOnIncompatible,Handled);
  if not Handled then
    begin
    if LHS.TypeEl=nil then
      begin
      if LHS.BaseType=btUntyped then
        begin
        // untyped parameter
        Result:=cExact+1;
        end
      else
        RaiseNotYetImplemented(20160922163631,LHS.IdentEl);
      end
    else if LHS.BaseType=RHS.BaseType then
      begin
      if LHS.BaseType=btContext then
        exit(CheckAssignCompatibilityUserType(LHS,RHS,ErrorEl,RaiseOnIncompatible))
      else
        Result:=cExact; // same base type, maybe not same type name (e.g. longint and integer)
      end
    else if (LHS.BaseType in btAllInteger)
        and (RHS.BaseType in btAllInteger) then
      Result:=cExact+1
    else if (LHS.BaseType in btAllBooleans)
        and (RHS.BaseType in btAllBooleans) then
      Result:=cExact+1
    else if (LHS.BaseType in btAllStringAndChars)
        and (RHS.BaseType in btAllStringAndChars) then
      Result:=cExact+1
    else if (LHS.BaseType in btAllFloats)
        and (RHS.BaseType in btAllFloats+btAllInteger) then
      Result:=cExact+1
    else if LHS.BaseType=btNil then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152431,nCantAssignValuesToAnAddress,sCantAssignValuesToAnAddress,
          [],ErrorEl);
      exit(cIncompatible);
      end
    else if LHS.BaseType in [btRange,btSet,btModule,btArray] then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152432,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      exit(cIncompatible);
      end
    else if (LHS.IdentEl=nil) and (LHS.ExprEl=nil) then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170216152434,nIllegalExpression,sIllegalExpression,[],ErrorEl);
      exit(cIncompatible);
      end
    else if RHS.BaseType=btNil then
      begin
      if LHS.BaseType=btPointer then
        Result:=cExact
      else if LHS.BaseType=btContext then
        begin
        TypeEl:=LHS.TypeEl;
        if (TypeEl.ClassType=TPasClassType)
            or (TypeEl.ClassType=TPasClassOfType)
            or (TypeEl.ClassType=TPasPointerType)
            or (TypeEl is TPasProcedureType)
            or IsDynArray(TypeEl) then
          Result:=cExact;
        end;
      end
    else if RHS.BaseType=btSet then
      begin
      if (LHS.BaseType=btSet) then
        begin
        if RHS.TypeEl=nil then
          Result:=cExact // empty set
        else if (LHS.SubType=RHS.SubType) and (LHS.SubType in (btAllBooleans+btAllInteger+[btChar])) then
          Result:=cExact
        else if ((LHS.SubType in btAllBooleans) and (RHS.SubType in btAllBooleans))
            or ((LHS.SubType in btAllInteger) and (RHS.SubType in btAllInteger)) then
          Result:=cExact+1
        else if (LHS.SubType=btContext) and (LHS.TypeEl is TPasEnumType)
            and (LHS.TypeEl=RHS.TypeEl) then
          Result:=cExact;
        end;
      end
    else if RHS.BaseType=btProc then
      begin
      if (msDelphi in CurrentParser.CurrentModeswitches)
          and (LHS.TypeEl is TPasProcedureType)
          and (RHS.IdentEl is TPasProcedure) then
        begin
        if CheckProcAssignCompatibility(TPasProcedureType(LHS.TypeEl),
            TPasProcedure(RHS.IdentEl).ProcType) then
          Result:=cExact;
        end;
      end
    else if (LHS.BaseType=btContext) and (LHS.TypeEl is TPasArrayType) then
      Result:=CheckAssignCompatibilityArrayType(LHS,RHS,ErrorEl,RaiseOnIncompatible);
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignResCompatibility incompatible LHS='+GetResolverResultDesc(LHS)+' RHS='+GetResolverResultDesc(RHS));
  {$ENDIF}

  if (Result>=0) and (Result<cIncompatible) then
    begin
    // type fits -> check readable
    if not (rrfReadable in RHS.Flags) then
      begin
      if RaiseOnIncompatible then
        RaiseMsg(20170318235637,nVariableIdentifierExpected,
          sVariableIdentifierExpected,[],ErrorEl);
      exit(cIncompatible);
      end;
    exit;
    end;

  // incompatible
  if not RaiseOnIncompatible then
    exit(cIncompatible);

  // create error messages
  RaiseIncompatibleTypeRes(20170216152437,nIncompatibleTypesGotExpected,
    [],RHS,LHS,ErrorEl);
end;

function TPasResolver.CheckEqualElCompatibility(Left, Right: TPasElement;
  ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
// check if the RightResolved is type compatible to LeftResolved
var
  Flags: TPasResolverComputeFlags;
  LeftResolved, RightResolved: TPasResolverResult;
  LeftErrorEl, RightErrorEl: TPasElement;
begin
  Result:=cIncompatible;
  // Delphi resolves both sides, so it forbids "if procvar=procvar then"
  // FPC is more clever. It supports "if procvar=@proc then", "function=value"
  if msDelphi in CurrentParser.CurrentModeswitches then
    Flags:=[]
  else
    Flags:=[rcNoImplicitProcType];
  ComputeElement(Left,LeftResolved,Flags);
  if not (msDelphi in CurrentParser.CurrentModeswitches) then
    begin
    if LeftResolved.BaseType=btNil then
      Flags:=[rcNoImplicitProcType]
    else if IsProcedureType(LeftResolved) then
      Flags:=[rcNoImplicitProcType]
    else
      Flags:=[];
    end;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckEqualElCompatibility Left=',GetResolverResultDesc(LeftResolved),' Flags=',dbgs(Flags),' Delphi=',msDelphi in CurrentParser.CurrentModeswitches);
  {$ENDIF}
  ComputeElement(Right,RightResolved,Flags);
  if ErrorEl=nil then
    begin
    LeftErrorEl:=Left;
    RightErrorEl:=Right;
    end
  else
    begin
    LeftErrorEl:=ErrorEl;
    RightErrorEl:=ErrorEl;
    end;
  Result:=CheckEqualResCompatibility(LeftResolved,RightResolved,LeftErrorEl,
    RaiseOnIncompatible,RightErrorEl);
end;

function TPasResolver.CheckEqualResCompatibility(const LHS,
  RHS: TPasResolverResult; LErrorEl: TPasElement; RaiseOnIncompatible: boolean;
  RErrorEl: TPasElement): integer;
var
  TypeEl: TPasType;
  ok: Boolean;
begin
  Result:=cIncompatible;
  if RErrorEl=nil then RErrorEl:=LErrorEl;
  // check if the RHS is type compatible to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckEqualCompatibility LHS=',GetResolverResultDesc(LHS),' RHS=',GetResolverResultDesc(RHS));
  {$ENDIF}
  if not (rrfReadable in LHS.Flags) then
    begin
    ok:=false;
    if (LHS.BaseType=btContext) and (LHS.TypeEl.ClassType=TPasClassType)
        and (LHS.IdentEl=LHS.TypeEl) then
      begin
      if RHS.BaseType=btNil then
        ok:=true
      else if (RHS.BaseType=btContext) and (RHS.TypeEl.ClassType=TPasClassOfType)
          and (rrfReadable in RHS.Flags) then
        // for example  if TImage=ImageClass then
        ok:=true;
      end;
    if not ok then
      RaiseMsg(20170216152438,nNotReadable,sNotReadable,[],LErrorEl);
    end;
  if not (rrfReadable in RHS.Flags) then
    begin
    ok:=false;
    if (RHS.BaseType=btContext) and (RHS.TypeEl.ClassType=TPasClassType)
        and (RHS.IdentEl=RHS.TypeEl) then
      begin
      if LHS.BaseType=btNil then
        ok:=true
      else if (LHS.BaseType=btContext) and (LHS.TypeEl.ClassType=TPasClassOfType)
          and (rrfReadable in LHS.Flags) then
        // for example  if ImageClass=TImage then
        ok:=true;
      end;
    if not ok then
      RaiseMsg(20170216152440,nNotReadable,sNotReadable,[],RErrorEl);
    end;

  if (LHS.BaseType=btCustom) or (RHS.BaseType=btCustom) then
    begin
    Result:=CheckEqualCompatibilityCustomType(LHS,RHS,LErrorEl,RaiseOnIncompatible);
    if (Result=cIncompatible) and RaiseOnIncompatible then
      RaiseIncompatibleTypeRes(20170330010727,nIncompatibleTypesGotExpected,
        [],RHS,LHS,LErrorEl);
    exit;
    end
  else if LHS.BaseType=RHS.BaseType then
    begin
    if LHS.BaseType=btContext then
      exit(CheckEqualCompatibilityUserType(LHS,RHS,LErrorEl,RaiseOnIncompatible))
    else
      exit(cExact); // same base type, maybe not same type name (e.g. longint and integer)
    end
  else if (LHS.BaseType in btAllInteger+btAllFloats)
      and (RHS.BaseType in btAllInteger+btAllFloats) then
    exit(cExact+1) // ToDo: range check for Expr
  else if (LHS.BaseType in btAllBooleans)
      and (RHS.BaseType in btAllBooleans) then
    exit(cExact+1)
  else if (LHS.BaseType in btAllStringAndChars)
      and (RHS.BaseType in btAllStringAndChars) then
    exit(cExact+1)
  else if LHS.BaseType=btNil then
    begin
      if RHS.BaseType in [btPointer,btNil] then
        exit(cExact)
      else if RHS.BaseType=btContext then
        begin
        TypeEl:=RHS.TypeEl;
        if (TypeEl.ClassType=TPasClassType)
            or (TypeEl.ClassType=TPasClassOfType)
            or (TypeEl.ClassType=TPasPointerType)
            or (TypeEl is TPasProcedureType)
            or IsDynArray(TypeEl) then
          exit(cExact);
        end
      else if RaiseOnIncompatible then
        RaiseMsg(20170216152442,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[RHS.BaseType],BaseTypeNames[LHS.BaseType]],LErrorEl)
      else
        exit(cIncompatible);
    end
  else if RHS.BaseType=btNil then
    begin
      if LHS.BaseType=btPointer then
        exit(cExact)
      else if LHS.BaseType=btContext then
        begin
        TypeEl:=LHS.TypeEl;
        if (TypeEl.ClassType=TPasClassType)
            or (TypeEl.ClassType=TPasClassOfType)
            or (TypeEl.ClassType=TPasPointerType)
            or (TypeEl is TPasProcedureType)
            or IsDynArray(TypeEl) then
          exit(cExact);
        end
      else if RaiseOnIncompatible then
        RaiseMsg(20170216152444,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[LHS.BaseType],BaseTypeNames[RHS.BaseType]],LErrorEl)
      else
        exit(cIncompatible);
    end
  else if LHS.BaseType=btSet then
    begin
    if RHS.BaseType=btSet then
      begin
      if LHS.TypeEl=nil then
        exit(cExact); // empty set
      if RHS.TypeEl=nil then
        exit(cExact); // empty set
      if (LHS.SubType=RHS.SubType) and (LHS.SubType in (btAllBooleans+btAllInteger+[btChar])) then
        exit(cExact);
      if ((LHS.SubType in btAllBooleans) and (RHS.SubType in btAllBooleans))
          or ((LHS.SubType in btAllInteger) and (RHS.SubType in btAllInteger)) then
        exit(cExact+1);
      if (LHS.SubType=btContext) and (LHS.TypeEl is TPasEnumType)
          and (LHS.TypeEl=RHS.TypeEl) then
        exit(cExact);
      if RaiseOnIncompatible then
        RaiseMsg(20170216152446,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[LHS.SubType],'set of '+BaseTypeNames[RHS.SubType]],LErrorEl)
      else
        exit(cIncompatible);
      end;
    end
  else if RaiseOnIncompatible then
    RaiseMsg(20170216152449,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
      [BaseTypeNames[LHS.BaseType],BaseTypeNames[RHS.BaseType]],LErrorEl)
  else
    exit(cIncompatible);
  RaiseNotYetImplemented(20161007101041,LErrorEl,'LHS='+GetResolverResultDesc(LHS)+' RHS='+GetResolverResultDesc(RHS));
end;

function TPasResolver.ResolvedElHasValue(const ResolvedEl: TPasResolverResult
  ): boolean;
begin
  if not (rrfReadable in ResolvedEl.Flags) then
    Result:=false
  else if ResolvedEl.ExprEl<>nil then
    Result:=true
  else if (ResolvedEl.IdentEl<>nil) then
    Result:=not (ResolvedEl.IdentEl is TPasType)
  else
    Result:=false;
end;

function TPasResolver.ResolvedElCanBeVarParam(
  const ResolvedEl: TPasResolverResult): boolean;
begin
  Result:=false;
  if [rrfReadable,rrfWritable]*ResolvedEl.Flags<>[rrfReadable,rrfWritable] then
    exit;
  if ResolvedEl.IdentEl=nil then exit;
  if ResolvedEl.IdentEl.ClassType=TPasVariable then
    exit(true);
  if (ResolvedEl.IdentEl.ClassType=TPasArgument) then
    begin
    Result:=(TPasArgument(ResolvedEl.IdentEl).Access in [argDefault, argVar, argOut]);
    exit;
    end;
  if ResolvedEl.IdentEl.ClassType=TPasResultElement then
    exit(true);
  if (ResolvedEl.IdentEl.ClassType=TPasConst) then
    begin
    // typed const are writable
    Result:=(TPasConst(ResolvedEl.IdentEl).VarType<>nil);
    exit;
    end;
  if (proPropertyAsVarParam in Options)
      and (ResolvedEl.IdentEl.ClassType=TPasProperty) then
    exit(true);
end;

function TPasResolver.ResolvedElIsClassInstance(
  const ResolvedEl: TPasResolverResult): boolean;
begin
  Result:=false;
  if ResolvedEl.BaseType<>btContext then exit;
  if ResolvedEl.TypeEl=nil then exit;
  if ResolvedEl.TypeEl.ClassType<>TPasClassType then exit;
  if (ResolvedEl.IdentEl is TPasVariable)
      or (ResolvedEl.IdentEl.ClassType=TPasArgument)
      or (ResolvedEl.IdentEl.ClassType=TPasResultElement) then
    exit(true);
end;

function TPasResolver.GetPasPropertyType(El: TPasProperty): TPasType;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.VarType<>nil then
      exit(El.VarType);
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertyAncestor(El: TPasProperty): TPasProperty;
begin
  Result:=nil;
  if El=nil then exit;
  if El.CustomData=nil then exit;
  Result:=TPasPropertyScope(El.CustomData).AncestorProp;
end;

function TPasResolver.GetPasPropertyGetter(El: TPasProperty): TPasElement;
// search the member variable or getter function of a property
var
  DeclEl: TPasElement;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.ReadAccessor<>nil then
      begin
      DeclEl:=(El.ReadAccessor.CustomData as TResolvedReference).Declaration;
      Result:=DeclEl;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.GetPasPropertySetter(El: TPasProperty): TPasElement;
// search the member variable or setter procedure of a property
var
  DeclEl: TPasElement;
begin
  Result:=nil;
  while El<>nil do
    begin
    if El.WriteAccessor<>nil then
      begin
      DeclEl:=(El.WriteAccessor.CustomData as TResolvedReference).Declaration;
      Result:=DeclEl;
      exit;
      end;
    El:=GetPasPropertyAncestor(El);
    end;
end;

function TPasResolver.CheckParamCompatibility(Expr: TPasExpr;
  Param: TPasArgument; ParamNo: integer; RaiseOnError: boolean): integer;
var
  ExprResolved, ParamResolved: TPasResolverResult;
  NeedVar: Boolean;
  RHSFlags: TPasResolverComputeFlags;
begin
  Result:=cIncompatible;

  NeedVar:=Param.Access in [argVar, argOut];

  ComputeElement(Param,ParamResolved,[]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Param=',GetTreeDesc(Param,2),' ParamResolved=',GetResolverResultDesc(ParamResolved));
  {$ENDIF}
  if (ParamResolved.TypeEl=nil) and (Param.ArgType<>nil) then
    RaiseInternalError(20160922163628,'GetResolvedType returned TypeEl=nil for '+GetTreeDesc(Param));
  RHSFlags:=[];
  if NeedVar then
    Include(RHSFlags,rcNoImplicitProc)
  else if IsProcedureType(ParamResolved) then
    Include(RHSFlags,rcNoImplicitProcType);

  if (Expr is TParamsExpr) and (TParamsExpr(Expr).Kind=pekSet) then
    begin
    // passing a const set
    if NeedVar then
      begin
      if RaiseOnError then
        RaiseMsg(20170216152450,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
      exit;
      end;
    if ParamResolved.TypeEl is TPasArrayType then
      begin
      Result:=CheckConstArrayCompatibility(TParamsExpr(Expr),ParamResolved,
                                           RaiseOnError,RHSFlags,Expr);
      if (Result=cIncompatible) and RaiseOnError then
        RaiseInternalError(20170326211129);
      exit;
      end;
    end;

  ComputeElement(Expr,ExprResolved,RHSFlags);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Expr=',GetTreeDesc(Expr,2),' ResolvedExpr=',GetResolverResultDesc(ExprResolved),' RHSFlags=',dbgs(RHSFlags));
  {$ENDIF}

  if NeedVar then
    begin
    // Expr must be a variable
    if not ResolvedElCanBeVarParam(ExprResolved) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckParamCompatibility NeedWritable: Identifier=',GetObjName(ExprResolved.IdentEl),' Type=',GetObjName(ExprResolved.TypeEl),' Expr=',GetObjName(ExprResolved.ExprEl),' Flags=',ResolverResultFlagsToStr(ExprResolved.Flags));
      {$ENDIF}
      if RaiseOnError then
        RaiseMsg(20170216152450,nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
      exit;
      end;
    if (ParamResolved.BaseType=ExprResolved.BaseType) then
      begin
      if IsSameType(ParamResolved.TypeEl,ExprResolved.TypeEl) then
        exit(cExact);
      end;
    if (Param.ArgType=nil) then
      exit(cExact); // untyped argument
    if RaiseOnError then
      RaiseIncompatibleType(20170216152452,nIncompatibleTypeArgNoVarParamMustMatchExactly,
        [IntToStr(ParamNo+1)],ExprResolved.TypeEl,ParamResolved.TypeEl,
        Expr);
    exit(cIncompatible);
    end;

  Result:=CheckAssignResCompatibility(ParamResolved,ExprResolved,Expr,false);
  if (Result=cIncompatible) and RaiseOnError then
    RaiseIncompatibleTypeRes(20170216152454,nIncompatibleTypeArgNo,
      [IntToStr(ParamNo+1)],ExprResolved,ParamResolved,Expr);
end;

function TPasResolver.CheckAssignCompatibilityUserType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  RTypeEl, LTypeEl: TPasType;
  SrcResolved, DstResolved: TPasResolverResult;
  LArray, RArray: TPasArrayType;
  function RaiseIncompatType: integer;
  begin
    if not RaiseOnIncompatible then exit(cIncompatible);
    RaiseIncompatibleTypeRes(20170216152505,nIncompatibleTypesGotExpected,
      [],RHS,LHS,ErrorEl);
  end;

begin
  if (RHS.TypeEl=nil) then
    RaiseInternalError(20160922163645);
  if (LHS.TypeEl=nil) then
    RaiseInternalError(20160922163648);
  LTypeEl:=LHS.TypeEl;
  RTypeEl:=RHS.TypeEl;
  if LTypeEl=RTypeEl then
    exit(cExact);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckCustomTypeCompatibility LTypeEl=',GetObjName(LTypeEl),' RTypeEl=',GetObjName(RTypeEl));
  {$ENDIF}
  Result:=-1;
  if LTypeEl.ClassType=TPasClassType then
    begin
    if RHS.BaseType=btNil then
      Result:=cExact
    else if RTypeEl.ClassType=TPasClassType then
      begin
      Result:=CheckSrcIsADstType(RHS,LHS,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseIncompatibleType(20170216152458,nIncompatibleTypesGotExpected,
          [],RTypeEl,LTypeEl,ErrorEl);
      end
    else
      exit(RaiseIncompatType);
    end
  else if LTypeEl.ClassType=TPasClassOfType then
    begin
    if RHS.BaseType=btNil then
      Result:=cExact
    else if (RTypeEl.ClassType=TPasClassOfType) then
      begin
      // e.g. ImageClass:=AnotherImageClass;
      Result:=CheckClassIsClass(TPasClassOfType(RTypeEl).DestType,
        TPasClassOfType(LTypeEl).DestType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(20170216152500,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['class of '+TPasClassOfType(RTypeEl).DestType.FullName,'class of '+TPasClassOfType(LTypeEl).DestType.FullName],ErrorEl);
      end
    else if (RHS.IdentEl is TPasClassType) then
      begin
      // e.g. ImageClass:=TFPMemoryImage;
      Result:=CheckClassIsClass(RTypeEl,TPasClassOfType(LTypeEl).DestType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(20170216152501,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [RTypeEl.Name,'class of '+TPasClassOfType(LTypeEl).DestType.FullName],ErrorEl);
      // do not check rrfReadable -> exit
      exit;
      end;
    end
  else if LTypeEl is TPasProcedureType then
    begin
    if RHS.BaseType=btNil then
      Result:=cExact
    else if (LTypeEl.ClassType=RTypeEl.ClassType)
        and (rrfReadable in RHS.Flags) then
      begin
      if CheckProcAssignCompatibility(TPasProcedureType(LTypeEl),TPasProcedureType(RTypeEl)) then
        Result:=cExact;
      end;
    end
  else if LTypeEl.ClassType=TPasArrayType then
    begin
    // arrays of different types
    if IsOpenArray(LTypeEl) then
      begin
      LArray:=TPasArrayType(LTypeEl);
      RArray:=TPasArrayType(RTypeEl);
      if length(LArray.Ranges)=length(RArray.Ranges) then
        begin
        if CheckProcArgTypeCompatibility(LArray.ElType,RArray.ElType) then
          Result:=cExact
        else if RaiseOnIncompatible then
          RaiseMsg(20170328110050,nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
            ['array of '+LArray.ElType.FullName,
             'array of '+RArray.ElType.FullName],ErrorEl)
        else
          exit(cIncompatible);
        end;
      end;
    end
  else if RTypeEl.ClassType=TPasEnumType then
    begin
    // enums of different type
    end
  else if RTypeEl.ClassType=TPasSetType then
    begin
    // sets of different type are compatible if enum types are compatible
    if LTypeEl.ClassType=TPasSetType then
      begin
      ComputeElement(TPasSetType(LTypeEl).EnumType,DstResolved,[]);
      ComputeElement(TPasSetType(RTypeEl).EnumType,SrcResolved,[]);
      if (SrcResolved.TypeEl<>nil)
      and (SrcResolved.TypeEl=DstResolved.TypeEl) then
        Result:=cExact
      else if (SrcResolved.TypeEl.CustomData is TResElDataBaseType)
          and (DstResolved.TypeEl.CustomData is TResElDataBaseType)
          and (CompareText(SrcResolved.TypeEl.Name,DstResolved.TypeEl.Name)=0) then
        Result:=cExact
      else if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152510,nIncompatibleTypesGotExpected,
          [],SrcResolved,DstResolved,ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(RaiseIncompatType);
    end
  else
    RaiseNotYetImplemented(20160922163654,ErrorEl);

  if Result=-1 then
    exit(RaiseIncompatType);
  if not (rrfReadable in RHS.Flags) then
    exit(RaiseIncompatType);
end;

function TPasResolver.CheckAssignCompatibilityArrayType(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;

  procedure CheckRange(ArrType: TPasArrayType; RangeIndex: integer;
    Values: TPasResolverResult; ErrorEl: TPasElement);
  var
    Range, Value: TPasExpr;
    RangeResolved, ValueResolved, ElTypeResolved: TPasResolverResult;
    i, Count: Integer;
    IsLastRange: Boolean;
    ArrayValues: TPasExprArray;
  begin
    Range:=ArrType.Ranges[RangeIndex];
    ComputeElement(Range,RangeResolved,[rcConstant]);
    Count:=GetRangeLength(RangeResolved);
    if Count=0 then
      RaiseNotYetImplemented(20170222232409,Values.ExprEl,'range '+GetResolverResultDesc(RangeResolved));

    IsLastRange:=RangeIndex+1=length(ArrType.Ranges);
    if IsLastRange then
      begin
      ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
      ElTypeResolved.IdentEl:=Range;
      Include(ElTypeResolved.Flags,rrfWritable);
      end
    else
      ElTypeResolved.BaseType:=btNone;

    if Values.ExprEl.ClassType=TArrayValues then
      begin
      ArrayValues:=TArrayValues(Values.ExprEl).Values;
      // check each value
      for i:=0 to Count-1 do
        begin
        if i=length(ArrayValues) then
          begin
          // not enough values
          if length(ArrayValues)>0 then
            ErrorEl:=ArrayValues[length(ArrayValues)-1];
          RaiseMsg(20170222233001,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
            [IntToStr(Count),IntToStr(length(ArrayValues))],ErrorEl);
          end;
        Value:=ArrayValues[i];
        ComputeElement(Value,ValueResolved,[rcConstant]);
        if IsLastRange then
          begin
          // last dimension -> check element type
          Result:=CheckAssignResCompatibility(ElTypeResolved,ValueResolved,Value,RaiseOnIncompatible);
          if Result=cIncompatible then
            exit;
          end
        else
          begin
          // multi dimensional array -> check next range
          CheckRange(ArrType,RangeIndex+1,ValueResolved,Value);
          end;
        end;
      if Count<length(ArrayValues) then
        begin
        // too many values
        ErrorEl:=ArrayValues[Count];
        RaiseMsg(20170222233605,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
          [IntToStr(Count),IntToStr(length(ArrayValues))],ErrorEl);
        end;
      end
    else
      begin
      // single value
      // Note: the parser does not store the difference between (1) and 1
      if (not IsLastRange) or (Count>1) then
        RaiseMsg(20170223095307,nExpectXArrayElementsButFoundY,sExpectXArrayElementsButFoundY,
          [IntToStr(Count),'1'],ErrorEl);
      // check element type
      Result:=CheckAssignResCompatibility(ElTypeResolved,Values,ErrorEl,RaiseOnIncompatible);
      if Result=cIncompatible then
        exit;
      end;
  end;

var
  ArrType: TPasArrayType;
begin
  Result:=cIncompatible;
  if (LHS.BaseType<>btContext) or (not (LHS.TypeEl is TPasArrayType)) then
    RaiseInternalError(20170222230012);
  ArrType:=TPasArrayType(LHS.TypeEl);
  if RHS.ExprEl=nil then
    RaiseNotYetImplemented(20170222230246,ErrorEl);
  CheckRange(ArrType,0,RHS,ErrorEl);
end;

function TPasResolver.CheckConstArrayCompatibility(Params: TParamsExpr;
  const ArrayResolved: TPasResolverResult; RaiseOnError: boolean;
  Flags: TPasResolverComputeFlags; StartEl: TPasElement): integer;
// check that each Param fits the array element type
var
  i, ParamComp: Integer;
  Param: TPasExpr;
  ArrayType: TPasArrayType;
  ElTypeResolved, ParamResolved: TPasResolverResult;
  ElTypeIsArray: boolean;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckConstArrayCompatibility Params.length=',length(Params.Params),
    ' ArrayResolved=',GetResolverResultDesc(ArrayResolved),' Flags=',dbgs(Flags));
  {$ENDIF}
  if not (ArrayResolved.TypeEl is TPasArrayType) then
    RaiseInternalError(20170326204957);
  ArrayType:=TPasArrayType(ArrayResolved.TypeEl);
  ComputeElement(ArrayType.ElType,ElTypeResolved,Flags+[rcType]);
  ElTypeIsArray:=ResolveAliasType(ElTypeResolved.TypeEl) is TPasArrayType;
  Result:=cExact;
  for i:=0 to length(Params.Params)-1 do
    begin
    Param:=Params.Params[i];
    if ElTypeIsArray and (Param is TParamsExpr) and (TParamsExpr(Param).Kind=pekSet) then
      ParamComp:=CheckConstArrayCompatibility(TParamsExpr(Param),ElTypeResolved,
                                              RaiseOnError,Flags,StartEl)
    else
      begin
      ComputeElement(Param,ParamResolved,Flags,StartEl);
      ParamComp:=CheckAssignResCompatibility(ElTypeResolved,ParamResolved,Param,RaiseOnError);
      end;
    if ParamComp=cIncompatible then
      exit(cIncompatible);
    inc(Result,ParamComp);
    end;
end;

function TPasResolver.CheckEqualCompatibilityUserType(const TypeA,
  TypeB: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  ElA, ElB: TPasType;
  AResolved, BResolved: TPasResolverResult;

  function IncompatibleElements: integer;
  begin
    Result:=cIncompatible;
    if not RaiseOnIncompatible then exit;
    RaiseIncompatibleType(20170216152513,nIncompatibleTypesGotExpected,
      [],ElA,ElB,ErrorEl);
  end;

begin
  if (TypeA.TypeEl=nil) then
    RaiseInternalError(20161007223118);
  if (TypeB.TypeEl=nil) then
    RaiseInternalError(20161007223119);
  ElA:=TypeA.TypeEl;
  ElB:=TypeB.TypeEl;
  if ElA=ElB then
    exit(cExact);

  if ElA.ClassType=TPasClassType then
    begin
    if TypeA.IdentEl is TPasType then
      begin
      if (TypeB.IdentEl is TPasType) and (ElA=ElB) then
        // e.g. if TFPMemoryImage=TFPMemoryImage then ;
        exit(cExact);
      if ElB.ClassType=TPasClassOfType then
        begin
        // e.g. if TFPMemoryImage=ImageClass then ;
        Result:=CheckClassIsClass(ElA,TPasClassOfType(ElB).DestType,ErrorEl);
        if (Result=cIncompatible) and RaiseOnIncompatible then
          RaiseMsg(20170216152515,nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
        exit;
        end;
      end
    else if ElB.ClassType=TPasClassType then
      begin
      // e.g. if Sender=Button1 then
      Result:=CheckSrcIsADstType(TypeA,TypeB,ErrorEl);
      if Result=cIncompatible then
        Result:=CheckSrcIsADstType(TypeB,TypeA,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(20170216152517,nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
      exit;
      end;
    exit(IncompatibleElements);
    end
  else if ElA.ClassType=TPasClassOfType then
    begin
    if ElB.ClassType=TPasClassOfType then
      begin
      // for example: if ImageClass=ImageClass then
      Result:=CheckClassIsClass(TPasClassOfType(ElA).DestType,
                                TPasClassOfType(ElB).DestType,ErrorEl);
      if Result=cIncompatible then
        Result:=CheckClassIsClass(TPasClassOfType(ElB).DestType,
                                  TPasClassOfType(ElA).DestType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(20170216152519,nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
      exit;
      end
    else if TypeB.IdentEl is TPasClassType then
      begin
      // for example: if ImageClass=TFPMemoryImage then
      Result:=CheckClassIsClass(TPasClassType(TypeB.IdentEl),TPasClassOfType(ElA).DestType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(20170216152520,nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
      exit;
      end;
    exit(IncompatibleElements);
    end
  else if ElA.ClassType=TPasEnumType then
    begin
    // enums of different type
    if not RaiseOnIncompatible then
      exit(cIncompatible);
    if ElB.ClassType=TPasEnumValue then
      RaiseIncompatibleType(20170216152523,nIncompatibleTypesGotExpected,
        [],TPasEnumType(ElA),TPasEnumType(ElB),ErrorEl)
    else
      exit(IncompatibleElements);
    end
  else if ElA.ClassType=TPasSetType then
    begin
    if ElB.ClassType=TPasSetType then
      begin
      ComputeElement(TPasSetType(ElA).EnumType,AResolved,[]);
      ComputeElement(TPasSetType(ElB).EnumType,BResolved,[]);
      if (AResolved.TypeEl<>nil)
      and (AResolved.TypeEl=BResolved.TypeEl) then
        exit(cExact);
      if (AResolved.TypeEl.CustomData is TResElDataBaseType)
          and (BResolved.TypeEl.CustomData is TResElDataBaseType)
          and (CompareText(AResolved.TypeEl.Name,BResolved.TypeEl.Name)=0) then
        exit(cExact);
      if RaiseOnIncompatible then
        RaiseIncompatibleTypeRes(20170216152524,nIncompatibleTypesGotExpected,
          [],AResolved,BResolved,ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(IncompatibleElements);
    end
  else if (ElA is TPasProcedureType) and (rrfReadable in TypeA.Flags) then
    begin
    if (ElB is TPasProcedureType) and (rrfReadable in TypeB.Flags) then
      begin
      if CheckProcAssignCompatibility(TPasProcedureType(ElA),TPasProcedureType(ElB)) then
        exit(cExact);

      end
    else
      exit(IncompatibleElements);
    end;
  exit(IncompatibleElements);
end;

function TPasResolver.CheckTypeCast(El: TPasType; Params: TParamsExpr;
  RaiseOnError: boolean): integer;
// for example  if TClassA(AnObject)=nil then ;
var
  Param: TPasExpr;
  ParamResolved, ResolvedEl: TPasResolverResult;
begin
  if length(Params.Params)<>1 then
    begin
    if RaiseOnError then
      RaiseMsg(20170216152526,nWrongNumberOfParametersForTypeCast,
        sWrongNumberOfParametersForTypeCast,[El.Name],Params);
    exit(cIncompatible);
    end;
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  ComputeElement(El,ResolvedEl,[]);
  Result:=CheckTypeCastRes(ParamResolved,ResolvedEl,Param,RaiseOnError);
end;

function TPasResolver.CheckTypeCastRes(const FromResolved,
  ToResolved: TPasResolverResult; ErrorEl: TPasElement; RaiseOnError: boolean
  ): integer;
var
  ToTypeEl, ToClassType, FromClassType: TPasType;
  ToTypeBaseType: TResolverBaseType;
  C: TClass;
begin
  Result:=cIncompatible;
  ToTypeEl:=ToResolved.TypeEl;
  if (ToTypeEl<>nil)
      and (rrfReadable in FromResolved.Flags) then
    begin
    C:=ToTypeEl.ClassType;
    if FromResolved.BaseType=btUntyped then
      begin
      // typecast an untyped parameter
      Result:=cExact+1;
      end
    else if C=TPasUnresolvedSymbolRef then
      begin
      if ToTypeEl.CustomData is TResElDataBaseType then
        begin
        // base type cast, e.g. double(aninteger)
        if ToTypeEl=FromResolved.TypeEl then
          exit(cExact);
        ToTypeBaseType:=(ToTypeEl.CustomData as TResElDataBaseType).BaseType;
        if ToTypeBaseType=FromResolved.BaseType then
          Result:=cExact
        else if ToTypeBaseType in btAllInteger then
          begin
          if FromResolved.BaseType in (btAllInteger+btAllBooleans) then
            Result:=cExact+1;
          end
        else if ToTypeBaseType in btAllFloats then
          begin
          if FromResolved.BaseType in (btAllInteger+btAllFloats) then
            Result:=cExact+1;
          end
        else if ToTypeBaseType in btAllBooleans then
          begin
          if FromResolved.BaseType in (btAllBooleans+btAllInteger) then
            Result:=cExact+1;
          end
        else if ToTypeBaseType in btAllStrings then
          begin
          if FromResolved.BaseType in btAllStringAndChars then
            Result:=cExact+1;
          end;
        end;
      end
    else if C=TPasClassType then
      begin
      // to class
      if FromResolved.BaseType=btNil then
        Result:=cExact
      else if (FromResolved.BaseType=btContext)
          and (FromResolved.TypeEl.ClassType=TPasClassType)
          and (not (FromResolved.IdentEl is TPasType)) then
        begin
        // type cast upwards or downwards
        Result:=CheckSrcIsADstType(FromResolved,ToResolved,ErrorEl);
        if Result=cIncompatible then
          Result:=CheckSrcIsADstType(ToResolved,FromResolved,ErrorEl);
        if Result=cIncompatible then
          Result:=CheckTypeCastClassInstanceToClass(FromResolved,ToResolved,ErrorEl);
        end;
      end
    else if C=TPasClassOfType then
      begin
      //writeln('TPasResolver.CheckTypeCast class-of FromRes.TypeEl=',GetObjName(FromResolved.TypeEl),' FromRes.IdentEl=',GetObjName(FromResolved.IdentEl));
      if (FromResolved.BaseType=btContext) then
        begin
        if (FromResolved.TypeEl.ClassType=TPasClassOfType)
            and (not (FromResolved.IdentEl is TPasType)) then
          begin
          // type cast  classof(classof-var)  upwards or downwards
          ToClassType:=TPasClassOfType(ToTypeEl).DestType;
          FromClassType:=TPasClassOfType(FromResolved.TypeEl).DestType;
          Result:=CheckClassesAreRelated(ToClassType,FromClassType,ErrorEl);
          end;
        end;
      end
    else if C=TPasEnumType then
      begin
      if CheckIsOrdinal(FromResolved,ErrorEl,true) then
        Result:=cExact;
      end
    else if C=TPasArrayType then
      begin
      if (FromResolved.BaseType=btContext)
          and (FromResolved.TypeEl.ClassType=TPasArrayType) then
        Result:=CheckTypeCastArray(TPasArrayType(FromResolved.TypeEl),
          TPasArrayType(ToTypeEl),ErrorEl,RaiseOnError);
      end;
    end
  else if ToTypeEl<>nil then
    begin
    // FromResolved is not readable
    if (FromResolved.BaseType=btContext)
        and (FromResolved.TypeEl.ClassType=TPasClassType)
        and (FromResolved.TypeEl=FromResolved.IdentEl)
        and (ToResolved.BaseType=btContext)
        and (ToResolved.TypeEl.ClassType=TPasClassOfType)
        and (ToResolved.TypeEl=ToResolved.IdentEl) then
      begin
      // for example  class-of(Self) in a class function
      ToClassType:=TPasClassOfType(ToTypeEl).DestType;
      FromClassType:=TPasClassType(FromResolved.TypeEl);
      Result:=CheckClassesAreRelated(ToClassType,FromClassType,ErrorEl);
      end;
    end;

  if Result=cIncompatible then
    begin
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckTypeCastRes From=',GetResolverResultDesc(FromResolved),' To=',GetResolverResultDesc(ToResolved));
    {$ENDIF}
    if RaiseOnError then
      RaiseIncompatibleTypeRes(20170216152528,nIllegalTypeConversionTo,
        [],FromResolved,ToResolved,ErrorEl);
    exit;
    end;
end;

function TPasResolver.CheckTypeCastArray(FromType, ToType: TPasArrayType;
  ErrorEl: TPasElement; RaiseOnError: boolean): integer;

  function NextDim(var ArrType: TPasArrayType; var NextIndex: integer;
    out ElTypeResolved: TPasResolverResult): boolean;
  begin
    inc(NextIndex);
    if NextIndex<length(ArrType.Ranges) then
      begin
      ElTypeResolved.BaseType:=btNone;
      exit(true);
      end;
    ComputeElement(ArrType.ElType,ElTypeResolved,[rcType]);
    if (ElTypeResolved.BaseType<>btContext)
        or (ElTypeResolved.TypeEl.ClassType<>TPasArrayType) then
      exit(false);
    ArrType:=TPasArrayType(ElTypeResolved.TypeEl);
    NextIndex:=0;
    Result:=true;
  end;

var
  FromIndex, ToIndex: Integer;
  FromElTypeRes, ToElTypeRes: TPasResolverResult;
  StartFromType, StartToType: TPasArrayType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckTypeCastArray From=',GetTypeDesc(FromType),' ToType=',GetTypeDesc(ToType));
  {$ENDIF}
  StartFromType:=FromType;
  StartToType:=ToType;
  Result:=cIncompatible;
  // check dimensions
  FromIndex:=0;
  ToIndex:=0;
  repeat
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckTypeCastArray From=',GetTypeDesc(FromType),' FromIndex=',FromIndex,' ToType=',GetTypeDesc(ToType),' ToIndex=',ToIndex);
    {$ENDIF}
    if length(ToType.Ranges)=0 then
      // ToType is dynamic/open array -> fits any size
    else
      begin
      // ToType is ranged
      // ToDo: check size of dimension
      end;
    // check next dimension
    if not NextDim(FromType,FromIndex,FromElTypeRes) then
      begin
      // at end of FromType
      if NextDim(ToType,ToIndex,ToElTypeRes) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckTypeCastArray To has more dims than From: From=',GetTypeDesc(FromType),' FromIndex=',FromIndex,', ToType=',GetTypeDesc(ToType),' ToIndex=',ToIndex);
        {$ENDIF}
        break; // ToType has more dimensions
        end;
      // have same dimension -> check ElType
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckTypeCastArray check ElType From=',GetResolverResultDesc(FromElTypeRes),' To=',GetResolverResultDesc(ToElTypeRes));
      {$ENDIF}
      Include(FromElTypeRes.Flags,rrfReadable);
      Result:=CheckTypeCastRes(FromElTypeRes,ToElTypeRes,ErrorEl,false);
      break;
      end
    else
      begin
      // FromType has more dimensions
      if not NextDim(ToType,ToIndex,ToElTypeRes) then
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.CheckTypeCastArray From has more dims than To: From=',GetTypeDesc(FromType),' FromIndex=',FromIndex,', ToType=',GetTypeDesc(ToType),' ToIndex=',ToIndex);
        {$ENDIF}
        break; // ToType has less dimensions
        end;
      end;
  until false;
  if (Result=cIncompatible) and RaiseOnError then
    RaiseIncompatibleType(20170331124643,nIllegalTypeConversionTo,
      [],StartFromType,StartToType,ErrorEl);
end;

procedure TPasResolver.ComputeElement(El: TPasElement; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
  StartEl: TPasElement);

  procedure ComputeIdentifier;
  var
    Ref: TResolvedReference;
    Proc: TPasProcedure;
    ProcType: TPasProcedureType;
    aClass: TPasClassType;
  begin
    Ref:=TResolvedReference(El.CustomData);
    ComputeElement(Ref.Declaration,ResolvedEl,Flags+[rcNoImplicitProc],StartEl);
    if rrfConstInherited in Ref.Flags then
      Exclude(ResolvedEl.Flags,rrfWritable);
    {$IFDEF VerbosePasResolver}
    if El is TPrimitiveExpr then
      writeln('TPasResolver.ComputeElement.ComputeIdentifier TPrimitiveExpr "',TPrimitiveExpr(El).Value,'" ',GetResolverResultDesc(ResolvedEl),' Flags=',dbgs(Flags))
    else
      writeln('TPasResolver.ComputeElement.ComputeIdentifier "',GetObjName(El),'" ',GetResolverResultDesc(ResolvedEl),' Flags=',dbgs(Flags));
    {$ENDIF}
    if (ResolvedEl.BaseType=btProc) then
      begin
      if [rcNoImplicitProc,rcConstant,rcType]*Flags=[] then
        begin
        // a proc and implicit call without params is allowed -> check if possible
        Proc:=ResolvedEl.IdentEl as TPasProcedure;
        if not ProcNeedsParams(Proc.ProcType) then
          begin
          // parameter less proc -> implicit call
          Include(Ref.Flags,rrfImplicitCallWithoutParams);
          if ResolvedEl.IdentEl is TPasFunction then
            // function => return result
            ComputeElement(TPasFunction(ResolvedEl.IdentEl).FuncType.ResultEl,
              ResolvedEl,Flags+[rcType],StartEl)
          else if (ResolvedEl.IdentEl.ClassType=TPasConstructor)
              and (rrfNewInstance in Ref.Flags) then
            begin
            // new instance constructor -> return value of type class
            aClass:=GetReference_NewInstanceClass(Ref);
            SetResolverValueExpr(ResolvedEl,btContext,aClass,TPrimitiveExpr(El),[rrfReadable]);
            end;
          Include(ResolvedEl.Flags,rrfCanBeStatement);
          end;
        end;
      end
    else if IsProcedureType(ResolvedEl) then
      begin
      if [rcNoImplicitProc,rcNoImplicitProcType,rcConstant,rcType]*Flags=[] then
        begin
        // a proc type and implicit call without params is allowed -> check if possible
        ProcType:=TPasProcedureType(ResolvedEl.TypeEl);
        if not ProcNeedsParams(ProcType) then
          begin
          // parameter less proc -> implicit call
          Include(Ref.Flags,rrfImplicitCallWithoutParams);
          if ResolvedEl.TypeEl is TPasFunctionType then
            // function => return result
            ComputeElement(TPasFunctionType(ResolvedEl.TypeEl).ResultEl,
              ResolvedEl,Flags+[rcType],StartEl);
          Include(ResolvedEl.Flags,rrfCanBeStatement);
          end;
        end;
      end;
  end;

var
  DeclEl: TPasElement;
  ElClass: TClass;
begin
  if StartEl=nil then StartEl:=El;
  ResolvedEl:=Default(TPasResolverResult);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeElement El=',GetObjName(El),' SkipTypeAlias=',rcSkipTypeAlias in Flags);
  {$ENDIF}
  if El=nil then
    exit;
  ElClass:=El.ClassType;
  if ElClass=TPrimitiveExpr then
    begin
    case TPrimitiveExpr(El).Kind of
      pekIdent,pekSelf:
        begin
        if not (El.CustomData is TResolvedReference) then
          RaiseNotYetImplemented(20160922163658,El,'Value="'+TPrimitiveExpr(El).Value+'" CustomData='+GetObjName(El.CustomData)+' '+GetElementSourcePosStr(El));
        ComputeIdentifier;
        end;
      pekNumber:
        // ToDo: check if btByte, btSmallInt, btSingle, ...
        if Pos('.',TPrimitiveExpr(El).Value)>0 then
          SetResolverValueExpr(ResolvedEl,btDouble,FBaseTypes[btDouble],TPrimitiveExpr(El),[rrfReadable])
        else
          SetResolverValueExpr(ResolvedEl,btLongint,FBaseTypes[btLongint],TPrimitiveExpr(El),[rrfReadable]);
      pekString:
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ComputeElement pekString Value="',TPrimitiveExpr(El).Value,'"');
        {$ENDIF}
        if IsCharLiteral(TPrimitiveExpr(El).Value) then
          SetResolverValueExpr(ResolvedEl,btChar,FBaseTypes[btChar],TPrimitiveExpr(El),[rrfReadable])
        else
          SetResolverValueExpr(ResolvedEl,btString,FBaseTypes[btString],TPrimitiveExpr(El),[rrfReadable]);
        end;
      pekNil:
        SetResolverValueExpr(ResolvedEl,btNil,FBaseTypes[btNil],TPrimitiveExpr(El),[rrfReadable]);
      pekBoolConst:
        SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],TPrimitiveExpr(El),[rrfReadable]);
    else
      RaiseNotYetImplemented(20160922163701,El);
    end;
    end
  else if ElClass=TSelfExpr then
    begin
    // self is just an identifier
    if not (El.CustomData is TResolvedReference) then
      RaiseNotYetImplemented(20170216150017,El,' El="'+GetObjName(El)+'" CustomData='+GetObjName(El.CustomData)+' '+GetElementSourcePosStr(El));
    ComputeIdentifier;
    end
  else if ElClass=TPasUnresolvedSymbolRef then
    begin
    // built-in type
    if El.CustomData is TResElDataBaseType then
      SetResolverIdentifier(ResolvedEl,TResElDataBaseType(El.CustomData).BaseType,
        El,TPasUnresolvedSymbolRef(El),[])
    else if El.CustomData is TResElDataBuiltInProc then
      begin
      SetResolverIdentifier(ResolvedEl,btBuiltInProc,El,TPasUnresolvedSymbolRef(El),[]);
      if bipfCanBeStatement in TResElDataBuiltInProc(El.CustomData).Flags then
        Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else
      RaiseNotYetImplemented(20160926194756,El);
    end
  else if ElClass=TBoolConstExpr then
    SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],TBoolConstExpr(El),[rrfReadable])
  else if ElClass=TBinaryExpr then
    ComputeBinaryExpr(TBinaryExpr(El),ResolvedEl,Flags,StartEl)
  else if ElClass=TUnaryExpr then
    begin
    if TUnaryExpr(El).OpCode=eopAddress then
      ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags+[rcNoImplicitProc],StartEl)
    else
      ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags,StartEl);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ComputeElement Unary Kind=',TUnaryExpr(El).Kind,' OpCode=',TUnaryExpr(El).OpCode,' OperandResolved=',GetResolverResultDesc(ResolvedEl),' ',GetElementSourcePosStr(El));
    {$ENDIF}
    case TUnaryExpr(El).OpCode of
      eopAdd, eopSubtract:
        if ResolvedEl.BaseType in (btAllInteger+btAllFloats) then
          exit
        else
          RaiseMsg(20170216152532,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TUnaryExpr(El).OpCode]],El);
      eopNot:
        if ResolvedEl.BaseType in (btAllInteger+btAllBooleans) then
          exit
        else
          RaiseMsg(20170216152534,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TUnaryExpr(El).OpCode]],El);
      eopAddress:
        if (ResolvedEl.BaseType=btProc) and (ResolvedEl.IdentEl is TPasProcedure) then
          begin
          SetResolverValueExpr(ResolvedEl,btContext,ResolvedEl.TypeEl,TUnaryExpr(El).Operand,[rrfReadable]);
          exit;
          end
        else
          RaiseMsg(20170216152535,nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TUnaryExpr(El).OpCode]],El);
    end;
    RaiseNotYetImplemented(20160926142426,El);
    end
  else if ElClass=TParamsExpr then
    case TParamsExpr(El).Kind of
      pekArrayParams:
        ComputeArrayParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
      pekFuncParams:
        ComputeFuncParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
      pekSet:
        ComputeSetParams(TParamsExpr(El),ResolvedEl,Flags,StartEl);
    else
      RaiseNotYetImplemented(20161010184559,El);
    end
  else if ElClass=TInheritedExpr then
    begin
    // writeln('TPasResolver.ComputeElement TInheritedExpr El.CustomData=',GetObjName(El.CustomData));
    if El.CustomData is TResolvedReference then
      begin
        // "inherited;"
        DeclEl:=TResolvedReference(El.CustomData).Declaration as TPasProcedure;
        SetResolverIdentifier(ResolvedEl,btProc,DeclEl,
          TPasProcedure(DeclEl).ProcType,[rrfCanBeStatement]);
      end
    else
      // no ancestor proc
      SetResolverIdentifier(ResolvedEl,btBuiltInProc,nil,nil,[rrfCanBeStatement]);
    end
  else if ElClass=TPasAliasType then
    begin
    // e.g. 'type a = b' -> compute b
    ComputeElement(TPasAliasType(El).DestType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    end
  else if (ElClass=TPasTypeAliasType) then
    begin
    // e.g. 'type a = type b;' -> compute b
    ComputeElement(TPasTypeAliasType(El).DestType,ResolvedEl,Flags+[rcType],StartEl);
    if not (rcSkipTypeAlias in Flags) then
      ResolvedEl.IdentEl:=El;
    end
  else if (ElClass=TPasVariable) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152737,StartEl);
    ComputeElement(TPasVariable(El).VarType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[rrfReadable,rrfWritable];
    end
  else if (ElClass=TPasConst) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if TPasConst(El).VarType<>nil then
      begin
      // typed const -> just like a var
      if rcConstant in Flags then
        RaiseConstantExprExp(20170216152739,StartEl);
      ComputeElement(TPasConst(El).VarType,ResolvedEl,Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[rrfReadable,rrfWritable];
      end
    else
      begin
      // untyped const
      ComputeElement(TPasConst(El).Expr,ResolvedEl,Flags+[rcConstant],StartEl);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[rrfReadable];
      end;
    end
  else if (ElClass=TPasEnumValue) then
    SetResolverIdentifier(ResolvedEl,btContext,El,El.Parent as TPasEnumType,[rrfReadable])
  else if (ElClass=TPasEnumType) then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasEnumType(El),[rrfReadable])
  else if (ElClass=TPasProperty) then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152741,StartEl);
    if TPasProperty(El).Args.Count=0 then
      begin
      ComputeElement(GetPasPropertyType(TPasProperty(El)),ResolvedEl,
        Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[];
      if GetPasPropertyGetter(TPasProperty(El))<>nil then
        Include(ResolvedEl.Flags,rrfReadable);
      if GetPasPropertySetter(TPasProperty(El))<>nil then
        Include(ResolvedEl.Flags,rrfWritable);
      if IsProcedureType(ResolvedEl) then
        Include(ResolvedEl.Flags,rrfCanBeStatement);
      end
    else
      // index property
      SetResolverIdentifier(ResolvedEl,btContext,El,nil,[]);
    end
  else if ElClass=TPasArgument then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152744,StartEl);
    if TPasArgument(El).ArgType=nil then
      // untyped parameter
      SetResolverIdentifier(ResolvedEl,btUntyped,El,nil,[])
    else
      begin
      // typed parameter -> use param as IdentEl, compute type
      ComputeElement(TPasArgument(El).ArgType,ResolvedEl,Flags+[rcType],StartEl);
      ResolvedEl.IdentEl:=El;
      end;
    ResolvedEl.Flags:=[rrfReadable];
    if TPasArgument(El).Access in [argDefault, argVar, argOut] then
      Include(ResolvedEl.Flags,rrfWritable);
    if IsProcedureType(ResolvedEl) then
      Include(ResolvedEl.Flags,rrfCanBeStatement);
    end
  else if ElClass=TPasClassType then
    begin
    if TPasClassType(El).IsForward then
      begin
      DeclEl:=(TPasClassType(El).CustomData as TResolvedReference).Declaration;
      ResolvedEl.TypeEl:=DeclEl as TPasClassType;
      end
    else
      ResolvedEl.TypeEl:=TPasClassType(El);
    SetResolverIdentifier(ResolvedEl,btContext,
                          ResolvedEl.TypeEl,ResolvedEl.TypeEl,[]);
    //if not TPasClassType(El).IsExternal then
    //  Include(ResolvedEl.Flags,rrfReadable);
    // Note: rrfReadable because a class has a vmt as value
    end
  else if ElClass=TPasClassOfType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasClassOfType(El),[])
  else if ElClass=TPasRecordType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasRecordType(El),[])
  else if ElClass=TPasRangeType then
    begin
    ComputeElement(TPasRangeType(El).RangeExpr,ResolvedEl,[rcConstant],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[];
    end
  else if ElClass=TPasSetType then
    begin
    ComputeElement(TPasSetType(El).EnumType,ResolvedEl,[rcConstant],StartEl);
    if ResolvedEl.BaseType=btRange then
      ConvertRangeToFirstValue(ResolvedEl);
    ResolvedEl.SubType:=ResolvedEl.BaseType;
    ResolvedEl.BaseType:=btSet;
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[];
    end
  else if ElClass=TPasResultElement then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(20170216152746,StartEl);
    ComputeElement(TPasResultElement(El).ResultType,ResolvedEl,Flags+[rcType],StartEl);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[rrfReadable,rrfWritable];
    end
  else if El is TPasModule then
    SetResolverIdentifier(ResolvedEl,btModule,El,nil,[])
  else if ElClass=TNilExpr then
    SetResolverValueExpr(ResolvedEl,btNil,FBaseTypes[btNil],TNilExpr(El),[rrfReadable])
  else if El is TPasProcedure then
    begin
    SetResolverIdentifier(ResolvedEl,btProc,El,TPasProcedure(El).ProcType,[rrfCanBeStatement]);
    if El is TPasFunction then
      Include(ResolvedEl.Flags,rrfReadable);
    // Note: the readability of TPasConstructor depends on the context
    // Note: implicit calls are handled in TPrimitiveExpr
    end
  else if El is TPasProcedureType then
    begin
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasProcedureType(El),[rrfCanBeStatement]);
    // Note: implicit calls are handled in TPrimitiveExpr
    end
  else if ElClass=TPasArrayType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasArrayType(El),[])
  else if ElClass=TArrayValues then
    SetResolverValueExpr(ResolvedEl,btArray,nil,TArrayValues(El),[rrfReadable])
  else
    RaiseNotYetImplemented(20160922163705,El);
end;

function TPasResolver.IsSameType(TypeA, TypeB: TPasType): boolean;
begin
  if TypeA=nil then exit(false);
  if TypeA=TypeB then exit(true);
  if (TypeA.ClassType=TPasUnresolvedSymbolRef)
      and (TypeB.ClassType=TPasUnresolvedSymbolRef) then
    begin
    Result:=CompareText(TypeA.Name,TypeB.Name)=0;
    exit;
    end;
  Result:=false;
end;

function TPasResolver.GetPasClassAncestor(ClassEl: TPasClassType;
  SkipAlias: boolean): TPasType;
var
  DeclEl: TPasElement;
  ClassScope: TPasClassScope;
begin
  Result:=nil;
  if ClassEl=nil then
    exit;
  if ClassEl.CustomData=nil then
    exit;
  if ClassEl.IsForward then
    begin
    DeclEl:=(ClassEl.CustomData as TResolvedReference).Declaration;
    ClassEl:=DeclEl as TPasClassType;
    Result:=ClassEl;
    end
  else
    begin
    ClassScope:=ClassEl.CustomData as TPasClassScope;
    if not (pcsfAncestorResolved in ClassScope.Flags) then
      exit;
    if SkipAlias then
      begin
      if ClassScope.AncestorScope=nil then
        exit;
      Result:=TPasClassType(ClassScope.AncestorScope.Element);
      end
    else
      Result:=ClassScope.DirectAncestor;
    end;
end;

function TPasResolver.GetLoop(El: TPasElement): TPasImplElement;
begin
  while El<>nil do
    begin
    if (El.ClassType=TPasImplRepeatUntil)
        or (El.ClassType=TPasImplWhileDo)
        or (El.ClassType=TPasImplForLoop) then
      exit(TPasImplElement(El));
    El:=El.Parent;
    end;
  Result:=nil;
end;

function TPasResolver.ResolveAliasType(aType: TPasType): TPasType;
begin
  Result:=aType;
  while (Result<>nil)
      and ((Result.ClassType=TPasAliasType) or (Result.ClassType=TPasTypeAliasType)) do
    Result:=TPasAliasType(Result).DestType;
end;

function TPasResolver.ExprIsAddrTarget(El: TPasExpr): boolean;
{ returns true if El is
  a) the last element of an @ operator expression
  e.g. '@p().o[].El' or '@El[]'
  b) mode delphi: the last element of a right side of an assignment
  c) an accessor function, e.g. property P read El;
}
var
  Parent: TPasElement;
  Prop: TPasProperty;
begin
  Result:=false;
  if El=nil then exit;
  if not ((El.ClassType=TParamsExpr) or (El.ClassType=TPrimitiveExpr)
      or (El.ClassType=TSelfExpr)) then
    exit;
  repeat
    Parent:=El.Parent;
    //writeln('TPasResolver.ExprIsAddrTarget El=',GetObjName(El),' Parent=',GetObjName(Parent));
    if Parent.ClassType=TUnaryExpr then
      begin
      if TUnaryExpr(Parent).OpCode=eopAddress then exit(true);
      end
    else if Parent.ClassType=TBinaryExpr then
      begin
      if TBinaryExpr(Parent).right<>El then exit;
      if TBinaryExpr(Parent).OpCode<>eopSubIdent then exit;
      end
    else if Parent.ClassType=TParamsExpr then
      begin
      if TParamsExpr(Parent).Value<>El then exit;
      end
    else if Parent.ClassType=TPasProperty then
      begin
      Prop:=TPasProperty(Parent);
      Result:=(Prop.ReadAccessor=El) or (Prop.WriteAccessor=El) or (Prop.StoredAccessor=El);
      exit;
      end
    else if Parent.ClassType=TPasImplAssign then
      begin
      if TPasImplAssign(Parent).right<>El then exit;
      if (msDelphi in CurrentParser.CurrentModeswitches) then exit(true);
      exit;
      end
    else
      exit;
    El:=TPasExpr(Parent);
  until false;
end;

function TPasResolver.GetLastExprIdentifier(El: TPasExpr): TPasExpr;
begin
  Result:=El;
  while Result<>nil do
    begin
    if Result is TParamsExpr then
      Result:=TParamsExpr(Result).Value
    else if Result is TBinaryExpr then
      Result:=TBinaryExpr(Result).right;
    end;
end;

function TPasResolver.ParentNeedsExprResult(El: TPasExpr): boolean;
var
  C: TClass;
  P: TPasElement;
begin
  if (El=nil) or (El.Parent=nil) then exit(false);
  Result:=false;
  P:=El.Parent;
  C:=P.ClassType;
  if C.InheritsFrom(TPasExpr) then
    Result:=true
  else if (C=TPasEnumValue)
      or (C=TPasArgument)
      or (C=TPasVariable)
      or (C=TPasExportSymbol) then
    Result:=true
  else if C=TPasClassType then
    Result:=TPasClassType(P).GUIDExpr=El
  else if C=TPasProperty then
    Result:=(TPasProperty(P).IndexExpr=El)
        or (TPasProperty(P).DispIDExpr=El)
        or (TPasProperty(P).DefaultExpr=El)
  else if C=TPasProcedure then
    Result:=(TPasProcedure(P).LibraryExpr=El)
         or (TPasProcedure(P).DispIDExpr=El)
  else if C=TPasImplRepeatUntil then
    Result:=(TPasImplRepeatUntil(P).ConditionExpr=El)
  else if C=TPasImplIfElse then
    Result:=(TPasImplIfElse(P).ConditionExpr=El)
  else if C=TPasImplWhileDo then
    Result:=(TPasImplWhileDo(P).ConditionExpr=El)
  else if C=TPasImplWithDo then
    Result:=(TPasImplWithDo(P).Expressions.IndexOf(El)>=0)
  else if C=TPasImplCaseOf then
    Result:=(TPasImplCaseOf(P).CaseExpr=El)
  else if C=TPasImplCaseStatement then
    Result:=(TPasImplCaseStatement(P).Expressions.IndexOf(El)>=0)
  else if C=TPasImplForLoop then
    Result:=(TPasImplForLoop(P).StartExpr=El)
         or (TPasImplForLoop(P).EndExpr=El)
  else if C=TPasImplAssign then
    Result:=(TPasImplAssign(P).right=El)
  else if C=TPasImplRaise then
    Result:=(TPasImplRaise(P).ExceptAddr=El);
end;

function TPasResolver.GetReference_NewInstanceClass(Ref: TResolvedReference
  ): TPasClassType;
begin
  Result:=(Ref.Context as TResolvedRefCtxConstructor).Typ as TPasClassType;
end;

function TPasResolver.IsDynArray(TypeEl: TPasType): boolean;
begin
  if (TypeEl=nil) or (TypeEl.ClassType<>TPasArrayType)
      or (length(TPasArrayType(TypeEl).Ranges)<>0) then
    exit(false);
  if proOpenAsDynArrays in Options then
    Result:=true
  else
    Result:=(TypeEl.Parent=nil) or (TypeEl.Parent.ClassType<>TPasArgument);
end;

function TPasResolver.IsOpenArray(TypeEl: TPasType): boolean;
begin
  Result:=(TypeEl<>nil) and (TypeEl.ClassType=TPasArrayType)
      and (length(TPasArrayType(TypeEl).Ranges)=0)
      and (TypeEl.Parent<>nil)
      and (TypeEl.Parent.ClassType=TPasArgument);
end;

function TPasResolver.IsDynOrOpenArray(TypeEl: TPasType): boolean;
begin
  Result:=(TypeEl<>nil) and (TypeEl.ClassType=TPasArrayType)
      and (length(TPasArrayType(TypeEl).Ranges)=0);
end;

function TPasResolver.IsClassMethod(El: TPasElement): boolean;
begin
  Result:=(El<>nil)
     and ((El.ClassType=TPasClassConstructor)
       or (El.ClassType=TPasClassDestructor)
       or (El.ClassType=TPasClassProcedure)
       or (El.ClassType=TPasClassFunction)
       or (El.ClassType=TPasClassOperator));
end;

function TPasResolver.IsProcedureType(const ResolvedEl: TPasResolverResult
  ): boolean;
begin
  Result:=(ResolvedEl.BaseType=btContext) and (ResolvedEl.TypeEl is TPasProcedureType);
end;

function TPasResolver.IsArrayType(const ResolvedEl: TPasResolverResult
  ): boolean;
begin
  Result:=(ResolvedEl.BaseType=btContext) and (ResolvedEl.TypeEl is TPasArrayType);
end;

function TPasResolver.IsTypeCast(Params: TParamsExpr): boolean;
var
  Value: TPasExpr;
  Ref: TResolvedReference;
  Decl: TPasElement;
begin
  Result:=false;
  if (Params=nil) or (Params.Kind<>pekFuncParams) then exit;
  Value:=Params.Value;
  if (Value.ClassType<>TSelfExpr)
      and ((Value.ClassType<>TPrimitiveExpr) or (TPrimitiveExpr(Value).Kind<>pekIdent)) then
    exit;
  if not (Value.CustomData is TResolvedReference) then exit;
  Ref:=TResolvedReference(Value.CustomData);
  Decl:=Ref.Declaration;
  if (Decl.ClassType=TPasAliasType) or (Decl.ClassType=TPasTypeAliasType) then
    Decl:=ResolveAliasType(TPasAliasType(Decl));
  if (Decl.ClassType=TPasClassType)
      or (Decl.ClassType=TPasClassOfType)
      or (Decl.ClassType=TPasEnumType) then
    exit(true);
  if (Decl.ClassType=TPasUnresolvedSymbolRef)
      and (Decl.CustomData is TResElDataBaseType) then
    exit(true);
end;

function TPasResolver.ProcNeedsParams(El: TPasProcedureType): boolean;
begin
  Result:=(El.Args.Count>0) and (TPasArgument(El.Args[0]).ValueExpr=nil);
end;

function TPasResolver.GetRangeLength(RangeResolved: TPasResolverResult
  ): integer;
begin
  Result:=0;
  if RangeResolved.BaseType=btContext then
    begin
    if RangeResolved.IdentEl is TPasEnumType then
      Result:=TPasEnumType(RangeResolved.IdentEl).Values.Count;
    end
  else if RangeResolved.BaseType in btAllBooleans then
    Result:=2;
end;

function TPasResolver.CheckSrcIsADstType(const ResolvedSrcType,
  ResolvedDestType: TPasResolverResult; ErrorEl: TPasElement): integer;
// finds distance between classes SrcType and DestType
begin
  Result:=CheckClassIsClass(ResolvedSrcType.TypeEl,ResolvedDestType.TypeEl,ErrorEl);
end;

function TPasResolver.CheckClassIsClass(SrcType, DestType: TPasType;
  ErrorEl: TPasElement): integer;
// check if Src is equal or descends from Dest
var
  ClassEl: TPasClassType;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckClassIsClass SrcType=',GetObjName(SrcType),' DestType=',GetObjName(DestType));
  {$ENDIF}
  if DestType=nil then exit(cIncompatible);
  // skip Dest alias
  while (DestType.ClassType=TPasAliasType) do
    DestType:=TPasAliasType(DestType).DestType;

  Result:=cExact;
  while SrcType<>nil do
    begin
    {$IFDEF VerbosePasResolver}
    writeln(' Step=',Result,' SrcType=',GetObjName(SrcType),' DestType=',GetObjName(DestType));
    {$ENDIF}
    if SrcType=DestType then
      exit
    else if SrcType.ClassType=TPasAliasType then
      // alias -> skip
      SrcType:=TPasAliasType(SrcType).DestType
    else if SrcType.ClassType=TPasTypeAliasType then
      begin
      // type alias -> increases distance
      SrcType:=TPasAliasType(SrcType).DestType;
      inc(Result);
      end
    else if SrcType.ClassType=TPasClassType then
      begin
      ClassEl:=TPasClassType(SrcType);
      if ClassEl.IsForward then
        // class forward -> skip
        SrcType:=(ClassEl.CustomData as TResolvedReference).Declaration as TPasType
      else
        begin
        // class ancestor -> increase distance
        SrcType:=(ClassEl.CustomData as TPasClassScope).DirectAncestor;
        inc(Result);
        end;
      end
    else
      exit(cIncompatible);
    end;
  if ErrorEl=nil then ;
  Result:=cIncompatible;
end;

function TPasResolver.CheckClassesAreRelated(TypeA, TypeB: TPasType;
  ErrorEl: TPasElement): integer;
begin
  Result:=CheckClassIsClass(TypeA,TypeB,ErrorEl);
  if Result<>cIncompatible then exit;
  Result:=CheckClassIsClass(TypeB,TypeA,ErrorEl);
end;

end.

