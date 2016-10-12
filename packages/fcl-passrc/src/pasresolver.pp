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
    - constructor result type
    - type cast
    - class of
    - class method, property, var, const
    - class-of.constructor
    - property with params
    - default property
  - with..do
  - enums - TPasEnumType, TPasEnumValue
     - propagate to parent scopes
     - function ord(): integer
  - sets - TPasSetType
    - set of char
    - set of integer
    - set of boolean
    - set of enum
    - ranges 'a'..'z'
    - operators: +, -, *, ><
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

 ToDo:
  - check if var initexpr fits vartype: var a: type = expr;
  - const TArrayValues
  - procedure type
  - method type
  - built-in functions high, low
  - classes - TPasClassType
     - visibility
     - nested var, const
     - nested types
  - char constant #0, #10, #13, UTF-8 char
  - check if constant is longint or int64
  - for..in..do
  - pointer TPasPointerType
  - records - TPasRecordType,
     - variant - TPasVariant
     - const  TRecordValues
     - function default(record type): record
  - proc: check if forward and impl default values match
  - untyped parameters
  - pointer type, ^type, @ operator, [] operator
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
  nOnlyOneDefaultPropertyIsAllowed = 3040;
  nWrongNumberOfParametersForArray = 3041;

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
  sOnlyOneDefaultPropertyIsAllowed = 'Only one default property is allowed';
  sWrongNumberOfParametersForArray = 'Wrong number of parameters for array';

type
  TResolverBaseType = (
    btNone,        // undefined
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
    btSet,
    btRange
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
    'PasProcedure',
    '[set]',
    '..range..'
    );

type
  TResolverBuiltInProc = (
    bfLength,
    bfSetLength,
    bfInclude,
    bfExclude,
    bfOrd,
    bfExit,
    bfInc,
    bfDec,
    bfAssigned
    );
  TResolverBuiltInProcs = set of TResolverBuiltInProc;
const
  ResolverBuiltInProcNames: array[TResolverBuiltInProc] of shortstring = (
    'Length',
    'SetLength',
    'Include',
    'Exclude',
    'Ord',
    'Exit',
    'Inc',
    'Dec',
    'Assigned'
    );
  bfAllStandardProcs = [low(TResolverBuiltInProc)..high(TResolverBuiltInProc)];

const
  ResolverResultVar = 'Result';

type

  { EPasResolve }

  EPasResolve = class(Exception)
  private
    FPasElement: TPasElement;
    procedure SetPasElement(AValue: TPasElement);
  public
    MsgNumber: integer;
    Args: TMessageArgs;
    destructor Destroy; override;
    property PasElement: TPasElement read FPasElement write SetPasElement;
  end;

  { TResolveData - base class for data stored in TPasElement.CustomData }

  TResolveData = Class
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    Owner: TObject; // e.g. a TPasResolver
    Next: TResolveData;
    CustomData: TObject;
    constructor Create; virtual;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
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

  { TPasScope - CustomData for elements with sub identifiers }

  TPasScope = Class(TResolveData)
  public
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

  { TPasClassScope }

  TPasClassScope = Class(TPasIdentifierScope)
  public
    AncestorResolved: boolean;
    AncestorScope: TPasClassScope;
    DirectAncestor: TPasType; // TPasClassType or TPasAliasType or TPasTypeAliasType
    DefaultProperty: TPasProperty;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

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

  { TPasWithExprScope }

  TPasWithExprScope = Class(TPasScope)
  public
    WithScope: TPasWithScope;
    Index: integer;
    NeedTmpVar: boolean;
    Expr: TPasExpr;
    Scope: TPasScope;
    class function IsStoredInElement: boolean; override;
    class function FreeOnPop: boolean; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

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

  { TPasSubModuleScope - scope for searching unitname.<identifier> }

  TPasSubModuleScope = Class(TPasSubScope)
  private
    FCurModule: TPasModule;
    procedure OnInternalIterate(El: TPasElement; ElScope, StartScope: TPasScope;
      Data: Pointer; var Abort: boolean);
    procedure SetCurModule(AValue: TPasModule);
  public
    InterfaceScope: TPasSectionScope;
    ImplementationScope: TPasSectionScope;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    property CurModule: TPasModule read FCurModule write SetCurModule;
  end;

  { TPasDotIdentifierScope }

  TPasDotIdentifierScope = Class(TPasSubScope)
  public
    IdentifierScope: TPasIdentifierScope;
    OnlyTypeMembers: boolean; // true=only class var/procs, false=default=all
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
    rrfVMT, // use VMT for call
    rrfNewInstance // constructor call
    );
  TResolvedReferenceFlags = set of TResolvedReferenceFlag;

  { TResolvedReference - CustomData for normal references }

  TResolvedReference = Class(TResolveData)
  private
    FDeclaration: TPasElement;
    procedure SetDeclaration(AValue: TPasElement);
  public
    WithExprScope: TPasWithExprScope;
    Flags: TResolvedReferenceFlags;
    destructor Destroy; override;
    property Declaration: TPasElement read FDeclaration write SetDeclaration;
  end;

  TPasResolverResultFlag = (
    rrfReadable,
    rrfWritable
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
    rcReturnFuncResult, // if there is a choice call the function
    rcConstant  // resolve a constant expresson
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

  TResElDataBuiltInProc = Class;

  TOnGetCallCompatibility = function(Proc: TResElDataBuiltInProc;
    Exp: TPasExpr; RaiseOnError: boolean): integer of object;
  TOnGetCallResult = procedure(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
    out ResolvedEl: TPasResolverResult) of object;

  { TResElDataBuiltInProc - CustomData for compiler built-in procs like 'length' }

  TResElDataBuiltInProc = Class(TResElDataBuiltInSymbol)
  public
    Proc: TPasUnresolvedSymbolRef;
    Signature: string;
    GetCallCompatibility: TOnGetCallCompatibility;
    GetCallResult: TOnGetCallResult;
  end;

  TPRFindData = record
    ErrorPosEl: TPasElement;
    Found: TPasElement;
    ElScope, StartScope: TPasScope;
  end;
  PPRFindData = ^TPRFindData;

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
    FLastElement: TPasElement;
    FLastCreatedData: array[TResolveDataListKind] of TResolveData;
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgElement: TPasElement;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FScopes: array of TPasScope; // stack of scopes
    FScopeCount: integer;
    FSubScopes: array of TPasScope; // stack of scopes
    FSubScopeCount: integer;
    FStoreSrcColumns: boolean;
    FRootElement: TPasElement;
    FTopScope: TPasScope;
    FPendingForwards: TFPList; // list of TPasElement needed to check for forward procs
    function GetBaseType(bt: TResolverBaseType): TPasUnresolvedSymbolRef; inline;
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
    procedure AddModule(El: TPasModule);
    procedure AddSection(El: TPasSection);
    procedure AddType(El: TPasType);
    procedure AddRecordType(El: TPasRecordType);
    procedure AddClassType(El: TPasClassType);
    procedure AddVariable(El: TPasVariable);
    procedure AddEnumType(El: TPasEnumType);
    procedure AddEnumValue(El: TPasEnumValue);
    procedure AddProperty(El: TPasProperty);
    procedure AddProcedure(El: TPasProcedure);
    procedure AddArgument(El: TPasArgument);
    procedure AddFunctionResult(El: TPasResultElement);
    procedure AddExceptOn(El: TPasImplExceptOn);
    procedure ResolveImplBlock(Block: TPasImplBlock);
    procedure ResolveImplElement(El: TPasImplElement);
    procedure ResolveImplCaseOf(CaseOf: TPasImplCaseOf);
    procedure ResolveImplLabelMark(Mark: TPasImplLabelMark);
    procedure ResolveImplForLoop(Loop: TPasImplForLoop);
    procedure ResolveImplWithDo(El: TPasImplWithDo);
    procedure ResolveImplAssign(El: TPasImplAssign);
    procedure ResolveImplRaise(El: TPasImplRaise);
    procedure ResolveExpr(El: TPasExpr);
    procedure ResolveBooleanExpr(El: TPasExpr);
    procedure ResolveNameExpr(El: TPasExpr; const aName: string);
    procedure ResolveInherited(El: TInheritedExpr);
    procedure ResolveInheritedCall(El: TBinaryExpr);
    procedure ResolveBinaryExpr(El: TBinaryExpr);
    procedure ResolveSubIdent(El: TBinaryExpr);
    procedure ResolveParamsExpr(Params: TParamsExpr);
    procedure ResolveFuncParamsExpr(Params: TParamsExpr);
    procedure ResolveArrayParamsExpr(Params: TParamsExpr);
    procedure ResolveSetParamsExpr(Params: TParamsExpr);
    procedure StartProcedureBody(El: TProcedureBody);
    procedure FinishModule(CurModule: TPasModule);
    procedure FinishUsesList;
    procedure FinishTypeSection(El: TPasDeclarations);
    procedure FinishTypeDef(El: TPasType);
    procedure FinishSetType(El: TPasSetType);
    procedure FinishRangeType(El: TPasRangeType);
    procedure FinishClassOf(El: TPasClassOfType);
    procedure FinishArray(El: TPasArrayType);
    procedure FinishConstDef(El: TPasConst);
    procedure FinishProcedure;
    procedure FinishProcedureHeader(El: TPasProcedureType);
    procedure FinishMethodDeclHeader(Proc: TPasProcedure);
    procedure FinishMethodImplHeader(ImplProc: TPasProcedure);
    procedure CheckProcSignatureMatch(DeclProc, ImplProc: TPasProcedure);
    procedure FinishExceptOnExpr;
    procedure FinishExceptOnStatement;
    procedure FinishDeclaration(El: TPasElement);
    procedure FinishPropertyOfClass(PropEl: TPasProperty);
    procedure FinishAncestors(aClass: TPasClassType);
    procedure CheckPendingForwards(El: TPasElement);
    procedure ComputeBinaryExpr(Bin: TBinaryExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
    procedure ComputeArrayParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
    procedure ComputeFuncParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
    procedure ComputeSetParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
    procedure ComputeFuncCallWithoutParams(var ResolvedEl: TPasResolverResult; ErrorEl: TPasExpr);
    procedure CheckIsClass(El: TPasElement; const ResolvedEl: TPasResolverResult);
    procedure CheckRangeExpr(Left, Right: TPasExpr;
      out LeftResolved, RightResolved: TPasResolverResult);
    procedure CheckSetElementsCompatible(Left, Right: TPasExpr;
      const LeftResolved, RightResolved: TPasResolverResult);
    procedure ConvertRangeToFirstValue(var ResolvedEl: TPasResolverResult);
    function IsCharLiteral(const Value: string): boolean; virtual;
  protected
    function OnGetCallCompatibility_Length(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure OnGetCallResult_Length(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function OnGetCallCompatibility_SetLength(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function OnGetCallCompatibility_InExclude(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function OnGetCallCompatibility_Ord(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure OnGetCallResult_Ord(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function OnGetCallCompatibility_Exit(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function OnGetCallCompatibility_IncDec(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    function OnGetCallCompatibility_Assigned(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure OnGetCallResult_Assigned(Proc: TResElDataBuiltInProc;
      {%H-}Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
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
    function FindElement(const AName: String): TPasElement; override; // used by TPasParser
    function FindElementWithoutParams(const AName: String; ErrorPosEl: TPasElement): TPasElement;
    function FindElementWithoutParams(const AName: String; out Data: TPRFindData;
      ErrorPosEl: TPasElement): TPasElement;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure CheckFoundElOnStartScope(const FindData: TPRFindData;
      Ref: TResolvedReference); virtual;
    procedure FinishScope(ScopeType: TPasScopeType; El: TPasElement); override;
    // built in types and functions
    procedure ClearBuiltInIdentifiers;
    procedure AddObjFPCBuiltInIdentifiers(
      BaseTypes: TResolveBaseTypes = btAllStandardTypes;
      BaseProcs: TResolverBuiltInProcs = bfAllStandardProcs);
    function AddBaseType(aName: shortstring; Typ: TResolverBaseType): TResElDataBaseType;
    function IsBaseType(aType: TPasType; BaseType: TResolverBaseType): boolean;
    function AddBuiltInProc(aName: shortstring; Signature: string;
      const GetCallCompatibility: TOnGetCallCompatibility;
      const GetCallResult: TOnGetCallResult): TResElDataBuiltInProc;
    // add extra TResolveData (E.CustomData) to free list
    procedure AddResolveData(El: TPasElement; Data: TResolveData;
      Kind: TResolveDataListKind);
    function CreateReference(DeclEl, RefEl: TPasElement;
      FindData: PPRFindData = nil): TResolvedReference; virtual;
    // scopes
    function CreateScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; virtual;
    procedure PopScope;
    procedure PushScope(Scope: TPasScope); overload;
    function PushScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; overload;
    function PushClassDotScope(var CurClassType: TPasClassType): TPasDotClassScope;
    function PushRecordDotScope(CurRecordType: TPasRecordType): TPasDotRecordScope;
    function PushEnumDotScope(CurEnumType: TPasEnumType): TPasDotEnumTypeScope;
    procedure ResetSubScopes(out Depth: integer);
    procedure RestoreSubScopes(Depth: integer);
    // log and messages
    class procedure UnmangleSourceLineNumber(LineNumber: integer;
      out Line, Column: integer);
    class function GetElementSourcePosStr(El: TPasElement): string;
    procedure SetLastMsg(MsgType: TMessageType; MsgNumber: integer;
      Const Fmt : String; Args : Array of const; Element: TPasElement);
    procedure LogMsg(MsgType: TMessageType; MsgNumber: integer;
      const Fmt: String; Args: Array of const; PosEl: TPasElement); overload;
    procedure RaiseMsg(MsgNumber: integer; const Fmt: String;
      Args: Array of const; ErrorPosEl: TPasElement);
    procedure RaiseNotYetImplemented(id: int64; El: TPasElement; Msg: string = ''); virtual;
    procedure RaiseInternalError(id: int64; const Msg: string = '');
    procedure RaiseInvalidScopeForElement(id: int64; El: TPasElement; const Msg: string = '');
    procedure RaiseIdentifierNotFound(Identifier: string; El: TPasElement);
    procedure RaiseXExpectedButYFound(X,Y: string; El: TPasElement);
    procedure WriteScopes;
    // find value and type of an element
    procedure ComputeElement(El: TPasElement; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags);
    // checking compatibilility
    function CheckCallProcCompatibility(Proc: TPasProcedure;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckCallPropertyCompatibility(PropEl: TPasProperty;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckCallArrayCompatibility(ArrayEl: TPasArrayType;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckParamCompatibility(Expr: TPasExpr; Param: TPasArgument;
      ParamNo: integer; RaiseOnError: boolean): integer;
    function CheckAssignCompatibilityCustomType(
      const SrcType, DestType: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckEqualCompatibilityCustomType(
      const TypeA, TypeB: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckTypeCast(El: TPasType; Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckSrcIsADstType(
      const ResolvedSrcType, ResolvedDestType: TPasResolverResult;
      ErrorEl: TPasElement): integer;
    function CheckOverloadProcCompatibility(Proc1, Proc2: TPasProcedure): boolean;
    function CheckProcArgCompatibility(Proc1, Proc2: TPasProcedure;
      ArgNo: integer): boolean;
    function CheckCanBeLHS(const ResolvedEl: TPasResolverResult;
      ErrorOnFalse: boolean; ErrorEl: TPasElement): boolean;
    function CheckAssignCompatibility(const LHS, RHS: TPasResolverResult;
      ErrorEl: TPasElement): integer;
    function CheckEqualCompatibility(const LHS, RHS: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
    function ResolvedElHasValue(const ResolvedEl: TPasResolverResult): boolean;
    function ResolvedElCanBeVarParam(const ResolvedEl: TPasResolverResult): boolean;
    // uility functions
    function GetPasPropertyType(El: TPasProperty): TPasType;
    function GetPasPropertyAncestor(El: TPasProperty): TPasProperty;
    function GetPasPropertyGetter(El: TPasProperty): TPasElement;
    function GetPasPropertySetter(El: TPasProperty): TPasElement;
    function GetPasClassAncestor(ClassEl: TPasClassType; SkipAlias: boolean): TPasType;
    function ResolveAliasType(aType: TPasType): TPasType;
  public
    property BaseType[bt: TResolverBaseType]: TPasUnresolvedSymbolRef read GetBaseType;
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
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
    property LastMsgElement: TPasElement read FLastMsgElement write FLastMsgElement;
  end;

function GetObjName(o: TObject): string;
function GetProcDesc(Proc: TPasProcedure): string;
function GetTypeDesc(aType: TPasType): string;
function GetTreeDesc(El: TPasElement; Indent: integer = 0): string;
function GetResolverResultDesc(const T: TPasResolverResult): string;
function GetResolverResultDescription(const T: TPasResolverResult): string;
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

function GetProcDesc(Proc: TPasProcedure): string;
var
  Args: TFPList;
  i: Integer;
  Arg: TPasArgument;
begin
  if Proc=nil then exit('nil');
  Result:=Proc.Name+'(';
  Args:=Proc.ProcType.Args;
  for i:=0 to Args.Count-1 do
    begin
    if i>0 then Result:=Result+';';
    Arg:=TPasArgument(Args[i]);
    if AccessNames[Arg.Access]<>'' then
      Result:=Result+AccessNames[Arg.Access];
    if Arg.ArgType=nil then
      Result:=Result+'untyped'
    else
      Result:=Result+GetTypeDesc(Arg.ArgType);
    end;
  Result:=Result+')';
  if cCallingConventions[Proc.ProcType.CallingConvention]<>'' then
    Result:=Result+';'+cCallingConventions[Proc.ProcType.CallingConvention];
end;

function GetTypeDesc(aType: TPasType): string;
begin
  if aType=nil then exit('nil');
  if (aType.ClassType=TPasUnresolvedSymbolRef) then
    begin
    Result:=aType.Name;
    if TPasUnresolvedSymbolRef(aType).CustomData is TResElDataBuiltInProc then
      Result:=Result+'()';
    end
  else if (aType.ClassType=TPasUnresolvedTypeRef) then
    Result:=aType.Name
  else if aType.ClassType=TPasPointerType then
    Result:='^'+GetTypeDesc(TPasPointerType(aType).DestType)
  else if aType.ClassType=TPasAliasType then
    Result:=GetTypeDesc(TPasAliasType(aType).DestType)
  else if aType.ClassType=TPasTypeAliasType then
    Result:='type '+GetTypeDesc(TPasTypeAliasType(aType).DestType)
  else if aType.ClassType=TPasClassOfType then
    Result:='class of '+TPasClassOfType(aType).DestType.Name
  else if aType.ClassType=TPasArrayType then
    Result:='array['+TPasArrayType(aType).IndexRange+'] of '+GetTypeDesc(TPasArrayType(aType).ElType)
  else
    Result:=aType.ElementTypeName+' '+aType.Name;
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
  Result:=BaseTypeNames[T.BaseType];
  if T.BaseType in [btSet,btRange] then
    Result:=Result+' of '+BaseTypeNames[T.SubType];
  if T.IdentEl<>nil then
    begin
    // named element
    if T.IdentEl=T.TypeEl then
      Result:=Result+',type '+GetTypeDesc(T.TypeEl)
    else
      Result:=Result+','+GetObjName(T.IdentEl)+':'+GetTypeDesc(T.TypeEl);
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

function GetResolverResultDescription(const T: TPasResolverResult): string;
var
  ArrayEl: TPasArrayType;
begin
  case T.BaseType of
  btModule: Result:=T.IdentEl.ElementTypeName+' '+T.IdentEl.Name;
  btNil: Result:='nil';
  btRange:
    begin
    if T.SubType=btContext then
      Result:='range of '+T.TypeEl.Name
    else
      Result:='range of '+BaseTypeNames[T.SubType];
    if (T.TypeEl<>T.IdentEl) and (T.IdentEl<>nil) then
      Result:=T.IdentEl.Name+':'+Result;
    end;
  btSet:
    begin
    if T.SubType=btContext then
      Result:='set of '+T.TypeEl.Name
    else
      Result:='set of '+BaseTypeNames[T.SubType];
    if (T.TypeEl<>T.IdentEl) and (T.IdentEl<>nil) then
      Result:=T.IdentEl.Name+':'+Result;
    end;
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
    else
      Result:=T.TypeEl.ElementTypeName;
    if (T.TypeEl<>T.IdentEl) and (T.IdentEl<>nil) then
      Result:=T.IdentEl.Name+':'+Result;
    end;
  else
    Result:=BaseTypeNames[T.BaseType];
    if (T.TypeEl<>T.IdentEl) and (T.IdentEl<>nil) then
      Result:=T.IdentEl.Name+':'+Result;
  end;
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

{ TPasPropertyScope }

destructor TPasPropertyScope.Destroy;
begin
  ReleaseAndNil(TPasElement(AncestorProp));
  inherited Destroy;
end;

{ TPasEnumTypeScope }

destructor TPasEnumTypeScope.Destroy;
begin
  ReleaseAndNil(TPasElement(CanonicalSet));
  inherited Destroy;
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
  FreeAndNil(ExpressionScopes);
  inherited Destroy;
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
  inherited Destroy;
  ReleaseAndNil(TPasElement(SelfArg));
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
  Element:=nil;
  inherited Destroy;
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
  PasElement:=nil;
  inherited Destroy;
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
  Declaration:=nil;
  inherited Destroy;
end;

{ TPasSubScope }

class function TPasSubScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TPasSubModuleScope }

procedure TPasSubModuleScope.OnInternalIterate(El: TPasElement; ElScope,
  StartScope: TPasScope; Data: Pointer; var Abort: boolean);
var
  FilterData: PPasIterateFilterData absolute Data;
begin
  if El.ClassType=TPasModule then
    exit; // skip used units
  // call the original iterator
  FilterData^.OnIterate(El,ElScope,StartScope,FilterData^.Data,Abort);
end;

procedure TPasSubModuleScope.SetCurModule(AValue: TPasModule);
begin
  if FCurModule=AValue then Exit;
  if CurModule<>nil then
    CurModule.Release;
  FCurModule:=AValue;
  if CurModule<>nil then
    CurModule.AddRef;
end;

destructor TPasSubModuleScope.Destroy;
begin
  CurModule:=nil;
  inherited Destroy;
end;

function TPasSubModuleScope.FindIdentifier(const Identifier: String
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

procedure TPasSubModuleScope.IterateElements(const aName: string;
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

procedure TPasSubModuleScope.WriteIdentifiers(Prefix: string);
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
  FreeAndNil(UsesList);
  inherited Destroy;
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
  Element:=nil;
  Owner:=nil;
  Next:=nil;
  inherited Destroy;
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

{ TPasResolver }

// inline
function TPasResolver.GetBaseType(bt: TResolverBaseType
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
      and (TPasProcedure(El).ProcType.Args.Count>0)
      and (TPasArgument(TPasProcedure(El).ProcType.Args[0]).ValueExpr=nil) then
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

      // check if previous found proc is override of found proc
      if (PrevProc.IsOverride)
          and (TPasProcedureScope(PrevProc.CustomData).OverriddenProc=Proc) then
        begin
        // previous found proc is override of found proc -> skip
        exit;
        end;
      end;

    Distance:=CheckCallProcCompatibility(Proc,Data^.Params,false);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements Proc Compatible=',Distance,' Data^.Found=',Data^.Found<>nil,' Data^.Compatible=',ord(Data^.Distance));
    {$ENDIF}
    CandidateFound:=true;
    end
  else if El.ClassType=TPasUnresolvedSymbolRef then
    begin
    if El.CustomData.ClassType=TResElDataBuiltInProc then
      begin
      // call of built-in proc
      BuiltInProc:=TResElDataBuiltInProc(El.CustomData);
      Distance:=BuiltInProc.GetCallCompatibility(BuiltInProc,Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements BuiltInProc=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end
    else if El.CustomData.ClassType=TResElDataBaseType then
      begin
      // type cast to base type
      Distance:=CheckTypeCast(TPasUnresolvedSymbolRef(El),Data^.Params,false);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallElements Base type cast=',El.Name,' Distance=',Distance);
      {$ENDIF}
      CandidateFound:=true;
      end;
    end
  else if El.ClassType=TPasClassType then
    begin
    // type cast to a class
    Distance:=CheckTypeCast(TPasClassType(El),Data^.Params,false);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallElements type cast to class=',El.Name,' Distance=',Distance);
    {$ENDIF}
    CandidateFound:=true;
    end;

  if not CandidateFound then
    begin
    Abort:=true;
    if Data^.Found=nil then
      begin
      // ToDo: use the ( as error position
      RaiseMsg(nIllegalQualifier,sIllegalQualifier,['('],Data^.Params);
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
  //writeln('TPasResolver.OnFindOverloadProc START ',El.Name,':',El.ElementTypeName);
  if not (El is TPasProcedure) then
    begin
    // identifier is not a proc
    Data^.FoundNonProc:=El;
    Abort:=true;
    exit;
    end;
  // identifier is a proc
  if El=Data^.Proc then
    exit; // found itself -> normal when searching for overloads

  //writeln('TPasResolver.OnFindOverloadProc Data^.OnlyScope=',GetObjName(Data^.OnlyScope),' Scope=',GetObjName(Scope),' ',Data^.OnlyScope=Scope);
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
begin
  Identifier:=Scope.AddIdentifier(aName,El,Kind);
  OlderIdentifier:=Identifier.NextSameIdentifier;
  // check duplicate
  if (OlderIdentifier<>nil) then
    if (Identifier.Kind=pikSimple) or (OlderIdentifier.Kind=pikSimple) then
      begin
      if (OlderIdentifier.Element.ClassType=TPasEnumValue)
          and (OlderIdentifier.Element.Parent.Parent<>Scope.Element) then
        // this enum was propagated from a sub type -> remove enum
        Scope.RemoveLocalIdentifier(OlderIdentifier.Element);
      RaiseMsg(nDuplicateIdentifier,sDuplicateIdentifier,
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
begin
  // resolve pending forwards
  for i:=0 to El.Declarations.Count-1 do
    begin
    Decl:=TPasElement(El.Declarations[i]);
    if Decl is TPasClassType then
      begin
      if TPasClassType(Decl).IsForward and (TPasClassType(Decl).CustomData=nil) then
        RaiseMsg(nForwardTypeNotResolved,sForwardTypeNotResolved,[Decl.Name],Decl);
      end
    else if (Decl.ClassType=TPasClassOfType)
        and (TPasClassOfType(Decl).DestType.ClassType=TUnresolvedPendingRef) then
      begin
      ClassOfEl:=TPasClassOfType(Decl);
      UnresolvedEl:=TUnresolvedPendingRef(ClassOfEl.DestType);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.FinishTypeSection resolving "',ClassOfEl.Name,'" = class of "',UnresolvedEl.Name,'"');
      {$ENDIF}
      Data:=Default(TPRFindData);
      Data.ErrorPosEl:=UnresolvedEl;
      Abort:=false;
      (TopScope as TPasIdentifierScope).IterateElements(UnresolvedEl.Name,
        TopScope,@OnFindFirstElement,@Data,Abort);
      if (Data.Found=nil) then
        RaiseIdentifierNotFound(UnresolvedEl.Name,UnresolvedEl);
      if Data.Found.ClassType<>TPasClassType then
        RaiseXExpectedButYFound('class',Data.Found.ElementTypeName,UnresolvedEl);
      ClassOfEl.DestType:=TPasClassType(Data.Found);
      ClassOfEl.DestType.AddRef;
      UnresolvedEl.Release;
      end;
    end;
end;

procedure TPasResolver.FinishTypeDef(El: TPasType);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishTypeDef El=',GetObjName(El));
  {$ENDIF}
  if El.ClassType=TPasSetType then
    FinishSetType(TPasSetType(El))
  else if El.ClassType=TPasRangeType then
    FinishRangeType(TPasRangeType(El))
  else if El.ClassType=TPasClassOfType then
    FinishClassOf(TPasClassOfType(El))
  else if El.ClassType=TPasArrayType then
    FinishArray(TPasArrayType(El))
  else if TopScope.Element=El then
    begin
    if (TopScope.ClassType=TPasEnumTypeScope)
        or (TopScope.ClassType=TPasRecordScope)
        or (TopScope.ClassType=TPasClassScope) then
      PopScope;
    end;
end;

procedure TPasResolver.FinishSetType(El: TPasSetType);
var
  BaseTypeData: TResElDataBaseType;
  StartResolved, EndResolved: TPasResolverResult;
  RangeExpr: TBinaryExpr;
begin
  if El.EnumType.ClassType=TPasEnumType then
    exit
  else if El.EnumType.ClassType=TPasRangeType then
    begin
    RangeExpr:=TPasRangeType(El.EnumType).RangeExpr;
    if RangeExpr.Parent=El then
      CheckRangeExpr(RangeExpr.left,RangeExpr.right,StartResolved,EndResolved);
    exit;
    end
  else if El.EnumType.ClassType=TPasUnresolvedSymbolRef then
    begin
    if El.EnumType.CustomData is TResElDataBaseType then
      begin
      BaseTypeData:=TResElDataBaseType(El.EnumType.CustomData);
      if BaseTypeData.BaseType in [btChar,btBoolean] then
        exit;
      RaiseXExpectedButYFound('char or boolean',El.EnumType.ElementTypeName,El.EnumType);
      end;
    end;
  RaiseXExpectedButYFound('enum type',El.EnumType.ElementTypeName,El.EnumType);
end;

procedure TPasResolver.FinishRangeType(El: TPasRangeType);
var
  StartResolved, EndResolved: TPasResolverResult;
begin
  CheckRangeExpr(El.RangeExpr.left,El.RangeExpr.right,StartResolved,EndResolved);
end;

procedure TPasResolver.FinishClassOf(El: TPasClassOfType);
begin
  if El.DestType is TUnresolvedPendingRef then exit;
  if El.DestType is TPasClassType then exit;
  RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
    [El.DestType.Name,'class'],El);
end;

procedure TPasResolver.FinishArray(El: TPasArrayType);
var
  i: Integer;
  Expr: TPasExpr;
  RangeResolved: TPasResolverResult;
begin
  for i:=0 to length(El.Ranges)-1 do
    begin
    Expr:=El.Ranges[i];
    ResolveExpr(Expr);
    ComputeElement(Expr,RangeResolved,[]);
    if (RangeResolved.IdentEl<>nil) and not (RangeResolved.IdentEl is TPasType) then
      RaiseXExpectedButYFound('range',RangeResolved.IdentEl.ElementTypeName,Expr);
    if (RangeResolved.BaseType=btRange) and (RangeResolved.SubType in btArrayRangeTypes) then
    else if RangeResolved.BaseType in btArrayRangeTypes then
    else
      RaiseXExpectedButYFound('range',RangeResolved.IdentEl.ElementTypeName,Expr);
    end;
end;

procedure TPasResolver.FinishConstDef(El: TPasConst);
var
  TypeResolved, ExprResolved: TPasResolverResult;
begin
  ResolveExpr(El.Expr);
  if El.VarType<>nil then
    begin
    ComputeElement(El.VarType,TypeResolved,[]);
    ComputeElement(El.Expr,ExprResolved,[rcReturnFuncResult]);
    if CheckAssignCompatibility(TypeResolved,ExprResolved,El.Expr)=cIncompatible then
      RaiseXExpectedButYFound(GetTypeDesc(El.VarType),GetTypeDesc(ExprResolved.TypeEl),El.Expr);
    end;
end;

procedure TPasResolver.FinishProcedure;
var
  aProc: TPasProcedure;
  i: Integer;
  Body: TProcedureBody;
  SubEl: TPasElement;
  SubProcScope: TPasProcedureScope;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishProcedure START');
  {$ENDIF}
  CheckTopScope(TPasProcedureScope);
  aProc:=TPasProcedureScope(TopScope).Element as TPasProcedure;
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
          RaiseMsg(nForwardProcNotResolved,sForwardProcNotResolved,
            [SubEl.ElementTypeName,SubEl.Name],SubEl);
        end;
      end;
    end;
  PopScope;
end;

procedure TPasResolver.FinishProcedureHeader(El: TPasProcedureType);
var
  ProcName: String;
  p: SizeInt;
  FindData: TFindOverloadProcData;
  DeclProc, Proc: TPasProcedure;
  Abort: boolean;
  DeclProcScope, ProcScope: TPasProcedureScope;
  FoundInScope: TPasIdentifierScope;
begin
  CheckTopScope(TPasProcedureScope);

  // search the best fitting proc
  if El.Parent is TPasProcedure then
    begin
    Proc:=TPasProcedure(El.Parent);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishProcedureHeader El=',GetTreeDesc(El),' ',GetElementSourcePosStr(El),' IsForward=',Proc.IsForward,' Parent=',GetObjName(El.Parent));
    {$ENDIF}
    ProcName:=Proc.Name;

    if Proc.IsForward and Proc.IsExternal then
      RaiseMsg(nInvalidProcModifiers,
        sInvalidProcModifiers,[Proc.ElementTypeName,'forward, external'],Proc);

    if Proc.IsDynamic then
      // 'dynamic' is not supported
      RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'dynamic'],Proc);

    if Proc.Parent is TPasClassType then
      begin
      // method declaration
      if Proc.IsAbstract then
        begin
        if not Proc.IsVirtual then
          RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'abstract without virtual'],Proc);
        if Proc.IsOverride then
          RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'abstract, override'],Proc);
        end;
      if Proc.IsVirtual and Proc.IsOverride then
        RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'virtual, override'],Proc);
      if Proc.IsForward then
        RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'forward'],Proc);
      if Proc.IsStatic then
        if (Proc.ClassType<>TPasClassProcedure) and (Proc.ClassType<>TPasClassFunction) then
          RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'static'],Proc);
      end
    else
      begin
      // intf proc, forward proc, proc body, method body
      if Proc.IsAbstract then
        RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'abstract'],Proc);
      if Proc.IsVirtual then
        RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'virtual'],Proc);
      if Proc.IsOverride then
        RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'override'],Proc);
      if Proc.IsMessage then
        RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'message'],Proc);
      if Proc.IsStatic then
        RaiseMsg(nInvalidProcModifiers,sInvalidProcModifiers,[Proc.ElementTypeName,'static'],Proc);
      end;

    p:=Pos('.',ProcName);
    if p>1 then
      begin
      FinishMethodImplHeader(Proc);
      exit;
      end;

    // finish non method, i.e. interface/implementation/nested procedure/method declaration
    if not IsValidIdent(ProcName) then
      RaiseNotYetImplemented(20160922163407,El);

    if Proc.Parent is TPasClassType then
      begin
      FinishMethodDeclHeader(Proc);
      exit;
      end;

    FindData:=Default(TFindOverloadProcData);
    FindData.Proc:=Proc;
    FindData.Args:=Proc.ProcType.Args;
    Abort:=false;
    IterateElements(ProcName,@OnFindOverloadProc,@FindData,Abort);
    if FindData.FoundNonProc<>nil then
      begin
      // proc hides a non proc -> forbidden within module
      if (Proc.GetModule=FindData.FoundNonProc.GetModule) then
        RaiseMsg(nDuplicateIdentifier,sDuplicateIdentifier,
          [FindData.FoundNonProc.Name,GetElementSourcePosStr(FindData.FoundNonProc)],Proc.ProcType);
      end;
    if FindData.Found=nil then
      exit; // no overload -> ok

    // overload found
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
        RaiseMsg(nDuplicateIdentifier,sDuplicateIdentifier,
                 [ProcName,GetElementSourcePosStr(DeclProc)],Proc.ProcType);
      CheckProcSignatureMatch(DeclProc,Proc);
      DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
      DeclProcScope.ImplProc:=Proc;
      ProcScope:=Proc.CustomData as TPasProcedureScope;
      ProcScope.DeclarationProc:=DeclProc;
      // remove DeclProc from scope
      FoundInScope:=FindData.ElScope as TPasIdentifierScope;
      FoundInScope.RemoveLocalIdentifier(DeclProc);
      end
    else
      begin
      // give a hint, that proc is hiding DeclProc
      LogMsg(mtHint,nFunctionHidesIdentifier,sFunctionHidesIdentifier,
        [DeclProc.Name,GetElementSourcePosStr(DeclProc)],Proc.ProcType);
      end;
    end
  else
    RaiseNotYetImplemented(20160922163411,El.Parent);
end;

procedure TPasResolver.FinishMethodDeclHeader(Proc: TPasProcedure);
var
  Abort: boolean;
  ClassScope: TPasClassScope;
  FindData: TFindOverloadProcData;
  OverloadProc: TPasProcedure;
  ProcScope: TPasProcedureScope;
begin
  ProcScope:=TopScope as TPasProcedureScope;
  ClassScope:=Scopes[ScopeCount-2] as TPasClassScope;
  FindData:=Default(TFindOverloadProcData);
  FindData.Proc:=Proc;
  FindData.Args:=Proc.ProcType.Args;
  Abort:=false;
  ClassScope.IterateElements(Proc.Name,ClassScope,@OnFindOverloadProc,@FindData,Abort);
  if FindData.FoundNonProc<>nil then
    // proc hides a non proc -> duplicate
    RaiseMsg(nDuplicateIdentifier,sDuplicateIdentifier,
      [FindData.FoundNonProc.Name,GetElementSourcePosStr(FindData.FoundNonProc)],Proc.ProcType);
  if FindData.Found=nil then
    begin
    // no overload
    if Proc.IsOverride then
      RaiseMsg(nNoMethodInAncestorToOverride,
        sNoMethodInAncestorToOverride,[GetProcDesc(Proc)],Proc.ProcType);
    end
  else
    begin
    // overload found
    OverloadProc:=FindData.Found;
    if Proc.Parent=OverloadProc.Parent then
      // overload in same scope -> duplicate
      RaiseMsg(nDuplicateIdentifier,sDuplicateIdentifier,
        [OverloadProc.Name,GetElementSourcePosStr(OverloadProc)],Proc.ProcType);
    ProcScope.OverriddenProc:=OverloadProc;
    if Proc.IsOverride then
      begin
      if (not OverloadProc.IsVirtual) and (not OverloadProc.IsOverride) then
        // the OverloadProc fits the signature, but is not virtual
        RaiseMsg(nNoMethodInAncestorToOverride,
          sNoMethodInAncestorToOverride,[GetProcDesc(Proc)],Proc.ProcType);
      // override a virtual method
      CheckProcSignatureMatch(OverloadProc,Proc);
      end
    else if not Proc.IsReintroduced then
      begin
        // give a hint, that proc is hiding OverloadProc
        LogMsg(mtHint,nFunctionHidesIdentifier,sFunctionHidesIdentifier,
          [OverloadProc.Name,GetElementSourcePosStr(OverloadProc)],Proc.ProcType);
      end;
    end;
end;

procedure TPasResolver.FinishMethodImplHeader(ImplProc: TPasProcedure);
var
  p: SizeInt;
  ProcName, aClassName: String;
  CurClassType: TPasClassType;
  OldScopeCount: Integer;
  FindData: TFindOverloadProcData;
  Abort: boolean;
  ImplProcScope, DeclProcScope: TPasProcedureScope;
  DeclProc: TPasProcedure;
  CurClassScope: TPasClassScope;
  SelfArg: TPasArgument;
begin
  // search class
  ProcName:=ImplProc.Name;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishMethodBodyHeader searching declaration "',ProcName,'" ...');
  {$ENDIF}
  OldScopeCount:=ScopeCount;
  CurClassType:=nil;
  repeat
    p:=Pos('.',ProcName);
    if p<1 then
      begin
      if CurClassType=nil then
        RaiseInternalError(20160922163415);
      break;
      end;
    aClassName:=LeftStr(ProcName,p-1);
    Delete(ProcName,1,p);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishMethodBodyHeader searching class "',aClassName,'" ProcName="',ProcName,'" ...');
    {$ENDIF}
    if not IsValidIdent(aClassName) then
      RaiseNotYetImplemented(20160922163417,ImplProc.ProcType);

    if CurClassType<>nil then
      PushClassDotScope(CurClassType);

    CurClassType:=TPasClassType(FindElementWithoutParams(aClassName,ImplProc.ProcType));
    if not (CurClassType is TPasClassType) then
      begin
      aClassName:=LeftStr(ImplProc.Name,length(ImplProc.Name)-length(ProcName));
      RaiseXExpectedButYFound('class',aClassname+':'+CurClassType.ElementTypeName,ImplProc.ProcType);
      end;

    // restore scope
    if ScopeCount>OldScopeCount then
      PopScope;
  until false;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishMethodBodyHeader searching proc "',ProcName,'" ...');
  {$ENDIF}
  // search ImplProc in class
  if not IsValidIdent(ProcName) then
    RaiseNotYetImplemented(20160922163421,ImplProc.ProcType);

  CurClassScope:=CurClassType.CustomData as TPasClassScope;
  FindData:=Default(TFindOverloadProcData);
  FindData.Proc:=ImplProc;
  FindData.Args:=ImplProc.ProcType.Args;
  FindData.OnlyScope:=CurClassScope;
  Abort:=false;
  CurClassScope.IterateElements(ProcName,CurClassScope,@OnFindOverloadProc,@FindData,Abort);
  if FindData.Found=nil then
    RaiseIdentifierNotFound(ImplProc.Name,ImplProc.ProcType);

  // connect method declaration and body
  DeclProc:=FindData.Found;
  if DeclProc.IsAbstract then
    RaiseMsg(nAbstractMethodsMustNotHaveImplementation,sAbstractMethodsMustNotHaveImplementation,[],ImplProc);
  if DeclProc.IsExternal then
    RaiseXExpectedButYFound('method','external method',ImplProc);
  CheckProcSignatureMatch(DeclProc,ImplProc);
  ImplProcScope:=ImplProc.CustomData as TPasProcedureScope;
  ImplProcScope.DeclarationProc:=DeclProc;
  ImplProcScope.ClassScope:=CurClassScope;
  DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
  DeclProcScope.ImplProc:=ImplProc;

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

procedure TPasResolver.CheckProcSignatureMatch(DeclProc, ImplProc: TPasProcedure
  );
var
  i: Integer;
  DeclArgs, ImplArgs: TFPList;
  DeclName, ImplName: String;
  ImplResult, DeclResult: TPasType;
begin
  if ImplProc.ClassType<>DeclProc.ClassType then
    RaiseXExpectedButYFound(DeclProc.TypeName,ImplProc.TypeName,ImplProc);
  if ImplProc.CallingConvention<>DeclProc.CallingConvention then
    RaiseMsg(nCallingConventionMismatch,sCallingConventionMismatch,[],ImplProc);
  if ImplProc is TPasFunction then
    begin
    // check result type
    ImplResult:=TPasFunction(ImplProc).FuncType.ResultEl.ResultType;
    DeclResult:=TPasFunction(DeclProc).FuncType.ResultEl.ResultType;
    if (ImplResult=nil)
    or (ImplResult<>DeclResult) then
      RaiseMsg(nResultTypeMismatchExpectedButFound,
        sResultTypeMismatchExpectedButFound,[GetTypeDesc(DeclResult),GetTypeDesc(ImplResult)],
        ImplProc);
    end;

  // check argument names
  DeclArgs:=DeclProc.ProcType.Args;
  ImplArgs:=ImplProc.ProcType.Args;
  for i:=0 to DeclArgs.Count-1 do
    begin
    DeclName:=TPasArgument(DeclArgs[i]).Name;
    ImplName:=TPasArgument(ImplArgs[i]).Name;
    if CompareText(DeclName,ImplName)<>0 then
      RaiseMsg(nFunctionHeaderMismatchForwardVarName,
        sFunctionHeaderMismatchForwardVarName,[DeclProc.Name,DeclName,ImplName],ImplProc);
    end;
end;

procedure TPasResolver.FinishExceptOnExpr;
var
  El: TPasImplExceptOn;
  ResolvedType: TPasResolverResult;
begin
  CheckTopScope(TPasExceptOnScope);
  El:=TPasImplExceptOn(FTopScope.Element);
  ComputeElement(El.TypeEl,ResolvedType,[rcSkipTypeAlias]);
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
  if El.ClassType=TPasProperty then
    FinishPropertyOfClass(TPasProperty(El));
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
        RaiseMsg(nNoPropertyFoundToOverride,sNoPropertyFoundToOverride,[],PropEl);
      // found -> create reference
      AncProp:=TPasProperty(AncEl);
      (PropEl.CustomData as TPasPropertyScope).AncestorProp:=AncProp;
      AncProp.AddRef;
      // check property versus class property
      if PropEl.ClassType<>AncProp.ClassType then
        RaiseXExpectedButYFound(AncProp.ElementTypeName,PropEl.ElementTypeName,PropEl);
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
          RaiseXExpectedButYFound('class',Prim.Value,Prim);
        Scope:=TopScope as TPasIdentifierScope;
        // search in class and ancestors, not in unit interface
        Identifier:=Scope.FindIdentifier(Prim.Value);
        if Identifier=nil then
          RaiseIdentifierNotFound(Prim.Value,Prim);
        DeclEl:=Identifier.Element;
        if DeclEl.ClassType<>TPasClassType then
          RaiseXExpectedButYFound('class',DeclEl.ElementTypeName,Prim);
        CreateReference(DeclEl,Prim);
        end
      else
        RaiseMsg(nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
      if TBinaryExpr(Expr).OpCode<>eopSubIdent then
        RaiseMsg(nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TBinaryExpr(Expr).OpCode]],Expr);
      PushClassDotScope(TPasClassType(DeclEl));
      Expr:=TBinaryExpr(Expr).right;
      Result:=GetAccessor(Expr);
      PopScope;
      end
    else if Expr.ClassType=TPrimitiveExpr then
      begin
      Prim:=TPrimitiveExpr(Expr);
      if Prim.Kind<>pekIdent then
        RaiseXExpectedButYFound('identifier',Prim.Value,Prim);
      Scope:=TopScope as TPasIdentifierScope;
      // search in class and ancestors, not in unit interface
      Identifier:=Scope.FindIdentifier(Prim.Value);
      if Identifier=nil then
        RaiseIdentifierNotFound(Prim.Value,Prim);
      DeclEl:=Identifier.Element;
      CreateReference(DeclEl,Prim);
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
        RaiseMsg(nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],ErrorEl);
      PropArg:=TPasArgument(PropEl.Args[ArgNo]);
      ProcArg:=TPasArgument(Proc.ProcType.Args[ArgNo]);
      inc(ArgNo);

      // check access: var, const, ...
      if PropArg.Access<>ProcArg.Access then
        RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),AccessNames[ProcArg.Access],AccessNames[PropArg.Access]],ErrorEl);

      // check typed
      if PropArg.ArgType=nil then
        begin
        if ProcArg.ArgType<>nil then
          RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),ProcArg.ArgType.ElementTypeName,'untyped'],ErrorEl);
        end
      else if ProcArg.ArgType=nil then
        RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),'untyped',PropArg.ArgType.ElementTypeName],ErrorEl)
      else
        begin
        ComputeElement(PropArg,PropArgResolved,[]);
        ComputeElement(ProcArg,ProcArgResolved,[]);

        if (PropArgResolved.BaseType<>ProcArgResolved.BaseType) then
          RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),BaseTypeNames[ProcArgResolved.BaseType],BaseTypeNames[PropArgResolved.BaseType]],ErrorEl);
        if PropArgResolved.TypeEl=nil then
          RaiseInternalError(20161010125255);
        if ProcArgResolved.TypeEl=nil then
          RaiseInternalError(20161010125304);
        if (PropArgResolved.TypeEl<>ProcArgResolved.TypeEl) then
          RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),ProcArgResolved.TypeEl.Name,PropArgResolved.TypeEl.Name],ErrorEl);
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
    ResolveExpr(PropEl.IndexExpr);
    RaiseNotYetImplemented(20160922163439,PropEl.IndexExpr);
    end;
  if PropEl.ReadAccessor<>nil then
    begin
    // check compatibility
    AccEl:=GetAccessor(PropEl.ReadAccessor);
    if AccEl is TPasVariable then
      begin
      if PropEl.Args.Count>0 then
        RaiseXExpectedButYFound('function',AccEl.ElementTypeName,PropEl.ReadAccessor);
      if TPasVariable(AccEl).VarType<>PropType then
        RaiseXExpectedButYFound(GetTypeDesc(PropType),
          GetTypeDesc(TPasVariable(AccEl).VarType),PropEl.ReadAccessor);
      if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
        if vmClass in PropEl.VarModifiers then
          RaiseXExpectedButYFound('class var','var',PropEl.ReadAccessor)
        else
          RaiseXExpectedButYFound('var','class var',PropEl.ReadAccessor);
      end
    else if AccEl is TPasProcedure then
      begin
      // check function
      Proc:=TPasProcedure(AccEl);
      if (vmClass in PropEl.VarModifiers) then
        begin
        if Proc.ClassType<>TPasClassFunction then
          RaiseXExpectedButYFound('class function',Proc.ElementTypeName,PropEl.ReadAccessor);
        if not Proc.IsStatic then
          RaiseMsg(nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],PropEl.ReadAccessor);
        end
      else
        begin
        if Proc.ClassType<>TPasFunction then
          RaiseXExpectedButYFound('function',Proc.ElementTypeName,PropEl.ReadAccessor);
        end;
      // check function result type
      ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
      if ResultType<>PropType then
        RaiseXExpectedButYFound('function result '+GetTypeDesc(PropType),
          GetTypeDesc(ResultType),PropEl.ReadAccessor);
      // check args
      CheckArgs(Proc,PropEl.ReadAccessor);
      if Proc.ProcType.Args.Count<>PropEl.Args.Count then
        RaiseMsg(nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],PropEl.ReadAccessor);
      end
    else
      RaiseXExpectedButYFound('variable',AccEl.ElementTypeName,PropEl.ReadAccessor);
    end;
  if PropEl.WriteAccessor<>nil then
    begin
    // check compatibility
    AccEl:=GetAccessor(PropEl.WriteAccessor);
    if AccEl is TPasVariable then
      begin
      if PropEl.Args.Count>0 then
        RaiseXExpectedButYFound('procedure',AccEl.ElementTypeName,PropEl.WriteAccessor);
      if TPasVariable(AccEl).VarType<>PropType then
        RaiseXExpectedButYFound(GetTypeDesc(PropType),
          GetTypeDesc(TPasVariable(AccEl).VarType),PropEl.WriteAccessor);
      if (vmClass in PropEl.VarModifiers)<>(vmClass in TPasVariable(AccEl).VarModifiers) then
        if vmClass in PropEl.VarModifiers then
          RaiseXExpectedButYFound('class var','var',PropEl.WriteAccessor)
        else
          RaiseXExpectedButYFound('var','class var',PropEl.WriteAccessor);
      end
    else if AccEl is TPasProcedure then
      begin
      // check procedure
      Proc:=TPasProcedure(AccEl);
      if (vmClass in PropEl.VarModifiers) then
        begin
        if Proc.ClassType<>TPasClassProcedure then
          RaiseXExpectedButYFound('class procedure',Proc.ElementTypeName,PropEl.WriteAccessor);
        if not Proc.IsStatic then
          RaiseMsg(nClassPropertyAccessorMustBeStatic,sClassPropertyAccessorMustBeStatic,[],PropEl.WriteAccessor);
        end
      else
        begin
        if Proc.ClassType<>TPasProcedure then
          RaiseXExpectedButYFound('procedure',Proc.ElementTypeName,PropEl.WriteAccessor);
        end;
      // check args
      CheckArgs(Proc,PropEl.ReadAccessor);
      PropArgCount:=PropEl.Args.Count;
      if Proc.ProcType.Args.Count<>PropArgCount+1 then
        RaiseMsg(nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],PropEl.WriteAccessor);
      Arg:=TPasArgument(Proc.ProcType.Args[PropArgCount]);
      if not (Arg.Access in [argDefault,argConst]) then
        RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(PropArgCount+1),AccessNames[Arg.Access],AccessNames[argConst]],PropEl.WriteAccessor);
      if Arg.ArgType<>PropType then
        RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(PropArgCount+1),GetTypeDesc(Arg.ArgType),GetTypeDesc(PropType)],PropEl.WriteAccessor);
      end
    else
      RaiseXExpectedButYFound('variable',AccEl.ElementTypeName,PropEl.WriteAccessor);
    end;
  if PropEl.ImplementsFunc<>nil then
    begin
    ResolveExpr(PropEl.ImplementsFunc);
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
        RaiseXExpectedButYFound('function',Proc.ElementTypeName,PropEl.StoredAccessor);
      // check function result type
      ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
      if not IsBaseType(ResultType,btBoolean) then
        RaiseXExpectedButYFound('function: boolean',
          'fucntion:'+GetTypeDesc(ResultType),PropEl.StoredAccessor);
      // check arg count
      if Proc.ProcType.Args.Count<>0 then
        RaiseMsg(nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
          [Proc.Name],PropEl.StoredAccessor);
      end
    else
      RaiseXExpectedButYFound('function: boolean',AccEl.ElementTypeName,PropEl.StoredAccessor);
    end;
  if PropEl.DefaultExpr<>nil then
    begin
    // check compatibility with type
    ResolveExpr(PropEl.DefaultExpr);
    ComputeElement(PropEl.DefaultExpr,DefaultResolved,[rcConstant]);
    ComputeElement(PropType,PropTypeResolved,[]);
    PropTypeResolved.IdentEl:=PropEl;
    PropTypeResolved.Flags:=[rrfReadable];
    CheckEqualCompatibility(PropTypeResolved,DefaultResolved,PropEl.DefaultExpr,true);
    end;
  if PropEl.IsDefault then
    begin
    // set default array property
    if (ClassScope.DefaultProperty<>nil)
        and (ClassScope.DefaultProperty.Parent=PropEl.Parent) then
      RaiseMsg(nOnlyOneDefaultPropertyIsAllowed,sOnlyOneDefaultPropertyIsAllowed,[],PropEl);
    ClassScope.DefaultProperty:=PropEl;
    end;
end;

procedure TPasResolver.FinishAncestors(aClass: TPasClassType);
// called when the ancestor and interface list of a class has been parsed,
// before parsing the class elements
var
  AncestorEl: TPasClassType;
  ClassScope: TPasClassScope;
  DirectAncestor, AncestorType, El: TPasType;
begin
  if aClass.IsForward then
    exit;
  if aClass.ObjKind<>okClass then
    RaiseNotYetImplemented(20161010174638,aClass,ObjKindNames[aClass.ObjKind]);

  DirectAncestor:=aClass.AncestorType;
  AncestorType:=DirectAncestor;
  while (AncestorType<>nil)
      and ((AncestorType.ClassType=TPasAliasType) or (AncestorType.ClassType=TPasTypeAliasType))
  do
    AncestorType:=TPasAliasType(AncestorType).DestType;

  if AncestorType=nil then
    begin
    if CompareText(aClass.Name,'TObject')=0 then
      begin
        // ok, no ancestors
        AncestorEl:=nil;
      end else begin
        // search default ancestor TObject
        AncestorEl:=TPasClassType(FindElementWithoutParams('TObject',aClass));
        if not (AncestorEl is TPasClassType) then
          RaiseXExpectedButYFound('class type',GetObjName(AncestorEl),aClass);
        if DirectAncestor=nil then
          DirectAncestor:=AncestorEl;
      end;
    end
  else if AncestorType.ClassType<>TPasClassType then
    RaiseXExpectedButYFound('class type',GetTypeDesc(AncestorType),aClass)
  else
    AncestorEl:=TPasClassType(AncestorType);
  if AncestorEl=nil then
    begin
    // root class TObject
    end
  else
    begin
    // inherited class -> check for cycle
    if AncestorEl.IsForward then
      RaiseMsg(nCantUseForwardDeclarationAsAncestor,
        sCantUseForwardDeclarationAsAncestor,[AncestorEl.Name],aClass);
    El:=AncestorEl;
    repeat
      if El=aClass then
        RaiseMsg(nAncestorCycleDetected,sAncestorCycleDetected,[],aClass);
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
  writeln('TPasResolver.FinishAncestors ',GetObjName(aClass.CustomData));
  {$ENDIF}
  PushScope(aClass,TPasClassScope);
  ClassScope:=TPasClassScope(TopScope);
  ClassScope.AncestorResolved:=true;
  ClassScope.DirectAncestor:=DirectAncestor;
  if AncestorEl<>nil then
    begin
    ClassScope.AncestorScope:=AncestorEl.CustomData as TPasClassScope;
    ClassScope.DefaultProperty:=ClassScope.AncestorScope.DefaultProperty;
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
    ResolveExpr(TPasImplSimple(El).expr)
  else if El.ClassType=TPasImplBlock then
    ResolveImplBlock(TPasImplBlock(El))
  else if El.ClassType=TPasImplRepeatUntil then
    begin
    ResolveImplBlock(TPasImplBlock(El));
    ResolveBooleanExpr(TPasImplRepeatUntil(El).ConditionExpr);
    end
  else if El.ClassType=TPasImplIfElse then
    begin
    ResolveBooleanExpr(TPasImplIfElse(El).ConditionExpr);
    ResolveImplElement(TPasImplIfElse(El).IfBranch);
    ResolveImplElement(TPasImplIfElse(El).ElseBranch);
    end
  else if El.ClassType=TPasImplWhileDo then
    begin
    ResolveBooleanExpr(TPasImplWhileDo(El).ConditionExpr);
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
  ResolveExpr(CaseOf.CaseExpr);
  ComputeElement(CaseOf.CaseExpr,CaseExprResolved,[rcReturnFuncResult]);
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
    RaiseXExpectedButYFound('ordinal expression',GetTypeDesc(CaseExprResolved.TypeEl),CaseOf.CaseExpr);

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
        ResolveExpr(OfExpr);
        ComputeElement(OfExpr,OfExprResolved,[rcConstant]);
        if OfExprResolved.BaseType=btRange then
          ConvertRangeToFirstValue(OfExprResolved);
        CheckEqualCompatibility(CaseExprResolved,OfExprResolved,OfExpr,true);
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
var
  DeclEl: TPasElement;
begin
  DeclEl:=FindElementWithoutParams(Mark.LabelId,Mark);
  // ToDo: check if DeclEl is a label and check duplicate
  CreateReference(DeclEl,Mark);
end;

procedure TPasResolver.ResolveImplForLoop(Loop: TPasImplForLoop);
var
  VarResolved, StartResolved, EndResolved: TPasResolverResult;
begin
  // loop var
  ResolveExpr(Loop.VariableName);
  ComputeElement(Loop.VariableName,VarResolved,[]);
  if (not ResolvedElCanBeVarParam(VarResolved))
      or not (VarResolved.BaseType in (btAllBooleans+btAllInteger+[btChar,btWideChar])) then
    RaiseMsg(nVariableIdentifierExpected,sVariableIdentifierExpected,[],Loop.VariableName);

  // start value
  ResolveExpr(Loop.StartExpr);
  ComputeElement(Loop.StartExpr,StartResolved,[rcReturnFuncResult]);
  if CheckAssignCompatibility(VarResolved,StartResolved,Loop.StartExpr)=cIncompatible then
    RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
      [GetTypeDesc(StartResolved.TypeEl),GetTypeDesc(VarResolved.TypeEl)],Loop.StartExpr);

  ResolveExpr(Loop.EndExpr);
  ComputeElement(Loop.EndExpr,EndResolved,[rcReturnFuncResult]);
  if CheckAssignCompatibility(VarResolved,EndResolved,Loop.EndExpr)=cIncompatible then
    RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
      [GetTypeDesc(EndResolved.TypeEl),GetTypeDesc(VarResolved.TypeEl)],Loop.EndExpr);

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
begin
  OldScopeCount:=ScopeCount;
  WithScope:=TPasWithScope(CreateScope(El,TPasWithScope));
  PushScope(WithScope);
  for i:=0 to El.Expressions.Count-1 do
    begin
    Expr:=TPasExpr(El.Expressions[i]);
    ResolveExpr(Expr);
    ComputeElement(Expr,ExprResolved,[rcSkipTypeAlias,rcReturnFuncResult]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveImplWithDo ExprResolved=',GetResolverResultDesc(ExprResolved));
    {$ENDIF}
    ErrorEl:=Expr;
    TypeEl:=ExprResolved.TypeEl;
    // ToDo: use last element in Expr for error position
    if TypeEl=nil then
      RaiseMsg(nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [BaseTypeNames[ExprResolved.BaseType]],ErrorEl);

    if TypeEl.ClassType=TPasRecordType then
      ExprScope:=TPasRecordType(TypeEl).CustomData as TPasRecordScope
    else if TypeEl.ClassType=TPasClassType then
      ExprScope:=TPasClassType(TypeEl).CustomData as TPasClassScope
    else
      RaiseMsg(nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
        [TypeEl.ElementTypeName],ErrorEl);
    WithExprScope:=TPasWithExprScope.Create;
    WithExprScope.WithScope:=WithScope;
    WithExprScope.Index:=i;
    WithExprScope.Expr:=Expr;
    WithExprScope.Scope:=ExprScope;
    WithExprScope.NeedTmpVar:=not (ExprResolved.IdentEl is TPasType);
    WithScope.ExpressionScopes.Add(WithExprScope);
    PushScope(WithExprScope);
    end;
  ResolveImplElement(El.Body);
  CheckTopScope(TPasWithExprScope);
  if TopScope<>WithScope.ExpressionScopes[WithScope.ExpressionScopes.Count-1] then
    RaiseInternalError(20160923102846);
  while ScopeCount>OldScopeCount do
    PopScope;
end;

procedure TPasResolver.ResolveImplAssign(El: TPasImplAssign);
var
  LeftResolved, RightResolved: TPasResolverResult;
  Compatible: Integer;
  Actual, Expected: String;
begin
  ResolveExpr(El.left);
  ResolveExpr(El.right);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveImplAssign Kind=',El.Kind,' left=',GetObjName(El.left),' right=',GetObjName(el.right));
  {$ENDIF}
  // check LHS can be assigned
  ComputeElement(El.left,LeftResolved,[rcSkipTypeAlias]);
  CheckCanBeLHS(LeftResolved,true,El.left);
  // compute RHS
  ComputeElement(El.right,RightResolved,[rcSkipTypeAlias]);

  if RightResolved.BaseType=btProc then
    ComputeFuncCallWithoutParams(RightResolved,El.right);

  case El.Kind of
  akDefault:
    begin
    Compatible:=CheckAssignCompatibility(LeftResolved,RightResolved,El.right);
    if Compatible=cIncompatible then
      begin
      Expected:=GetResolverResultDescription(LeftResolved);
      Actual:=GetResolverResultDescription(RightResolved);
      if LeftResolved.BaseType<>RightResolved.BaseType then
        begin
        if (LeftResolved.BaseType=btContext)
        and (LeftResolved.TypeEl<>nil) then
          Expected:=LeftResolved.TypeEl.ElementTypeName
        else
          Expected:=BaseTypeNames[LeftResolved.BaseType];
        if (RightResolved.BaseType=btContext)
        and (RightResolved.TypeEl<>nil) then
          Actual:=RightResolved.TypeEl.ElementTypeName
        else
          Actual:=BaseTypeNames[RightResolved.BaseType];
       end
      else if (LeftResolved.TypeEl<>nil) and (RightResolved.TypeEl<>nil) then
        begin
        if LeftResolved.TypeEl.ElementTypeName<>RightResolved.TypeEl.ElementTypeName then
          begin
          Expected:=LeftResolved.TypeEl.ElementTypeName;
          Actual:=RightResolved.TypeEl.ElementTypeName;
          end
        else if LeftResolved.TypeEl.Name<>RightResolved.TypeEl.Name then
          begin
          Expected:=LeftResolved.TypeEl.Name;
          Actual:=RightResolved.TypeEl.Name;
          end;
        end;
      RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [Actual,Expected],El.right);
      end;
    end;
  akAdd, akMinus,akMul,akDivision:
    begin
    if (El.Kind in [akAdd,akMinus,akMul]) and (LeftResolved.BaseType in btAllInteger) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in btAllInteger) then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseType[RightResolved.BaseType],BaseType[LeftResolved.BaseType]],El.right);
      end
    else if (El.Kind=akAdd) and (LeftResolved.BaseType in btAllStrings) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in btAllStringAndChars) then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseType[RightResolved.BaseType],BaseType[LeftResolved.BaseType]],El.right);
      end
    else if (El.Kind in [akAdd,akMinus,akMul,akDivision])
        and (LeftResolved.BaseType in btAllFloats) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType in (btAllInteger+btAllFloats)) then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseType[RightResolved.BaseType],BaseType[LeftResolved.BaseType]],El.right);
      end
    else if (LeftResolved.BaseType=btSet) and (El.Kind in [akAdd,akMinus,akMul]) then
      begin
      if (not (rrfReadable in RightResolved.Flags))
          or not (RightResolved.BaseType=btSet) then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[RightResolved.BaseType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.right);
      if (LeftResolved.SubType=RightResolved.SubType)
          or ((LeftResolved.SubType in btAllInteger) and (RightResolved.SubType in btAllInteger))
          or ((LeftResolved.SubType in btAllBooleans) and (RightResolved.SubType in btAllBooleans))
      then
      else
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[RightResolved.SubType],'set of '+BaseTypeNames[LeftResolved.SubType]],El.right);
      end
    else
      RaiseMsg(nIllegalQualifier,sIllegalQualifier,[AssignKindNames[El.Kind]],El);
    end;
  else
    RaiseNotYetImplemented(20160927143649,El,'AssignKind '+AssignKindNames[El.Kind]);
  end;
end;

procedure TPasResolver.ResolveImplRaise(El: TPasImplRaise);
var
  ResolvedEl: TPasResolverResult;
begin
  ResolveExpr(El.ExceptObject);
  ResolveExpr(El.ExceptAddr);
  ComputeElement(El.ExceptObject,ResolvedEl,[rcSkipTypeAlias,rcReturnFuncResult]);
  if (ResolvedEl.IdentEl=nil) then
    RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
             ['variable',ResolvedEl.TypeEl.ElementTypeName],El.ExceptObject);
  if (ResolvedEl.IdentEl.ClassType<>TPasVariable)
      and (ResolvedEl.IdentEl.ClassType<>TPasArgument) then
    RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
             ['variable',ResolvedEl.IdentEl.ElementTypeName],El.ExceptObject);
  CheckIsClass(El.ExceptObject,ResolvedEl);
end;

procedure TPasResolver.ResolveExpr(El: TPasExpr);
var
  Primitive: TPrimitiveExpr;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveExpr ',GetObjName(El));
  {$ENDIF}
  if El=nil then
  else if El.ClassType=TPrimitiveExpr then
    begin
    Primitive:=TPrimitiveExpr(El);
    case Primitive.Kind of
    pekIdent: ResolveNameExpr(El,Primitive.Value);
    pekNumber: exit;
    pekString: exit;
    pekNil,pekBoolConst: exit;
    else
      RaiseNotYetImplemented(20160922163451,El);
    end;
    end
  else if El.ClassType=TUnaryExpr then
    ResolveExpr(TUnaryExpr(El).Operand)
  else if El.ClassType=TBinaryExpr then
    ResolveBinaryExpr(TBinaryExpr(El))
  else if El.ClassType=TParamsExpr then
    ResolveParamsExpr(TParamsExpr(El))
  else if El.ClassType=TBoolConstExpr then
  else if El.ClassType=TNilExpr then
  else if El.ClassType=TSelfExpr then
    ResolveNameExpr(El,'Self')
  else if El.ClassType=TInheritedExpr then
    ResolveInherited(TInheritedExpr(El))
  else
    RaiseNotYetImplemented(20160922163453,El);
end;

procedure TPasResolver.ResolveBooleanExpr(El: TPasExpr);
var
  ResolvedCond: TPasResolverResult;
begin
  ResolveExpr(El);
  ComputeElement(El,ResolvedCond,[rcSkipTypeAlias,rcReturnFuncResult]);
  if ResolvedCond.BaseType<>btBoolean then
    RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
      [BaseTypeNames[btBoolean],BaseTypeNames[ResolvedCond.BaseType]],El);
end;

procedure TPasResolver.ResolveNameExpr(El: TPasExpr; const aName: string);
var
  FindData: TPRFindData;
  DeclEl: TPasElement;
  Proc: TPasProcedure;
  Ref: TResolvedReference;
  BuiltInProc: TResElDataBuiltInProc;
begin
  DeclEl:=FindElementWithoutParams(aName,FindData,El);
  if DeclEl is TPasProcedure then
    begin
    // identifier is a call and args brackets are missing
    if El.Parent.ClassType=TPasProperty then
      // a property modifier -> ok
    else
      begin
      Proc:=TPasProcedure(DeclEl);
      if (Proc.ProcType.Args.Count>0)
          and (TPasArgument(Proc.ProcType.Args[0]).ValueExpr=nil)
      then
        RaiseMsg(nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[Proc.Name],El);
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
  Ref:=CreateReference(DeclEl,El,@FindData);
  CheckFoundElOnStartScope(FindData,Ref);
end;

procedure TPasResolver.ResolveInherited(El: TInheritedExpr);
var
  ProcScope, DeclProcScope: TPasProcedureScope;
  AncestorScope: TPasClassScope;
  DeclProc, AncestorProc: TPasProcedure;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveInheritedDefault El.Parent=',GetTreeDesc(El.Parent));
  {$ENDIF}
  if (El.Parent.ClassType=TBinaryExpr)
  and (TBinaryExpr(El.Parent).OpCode=eopNone) then
    begin
    ResolveInheritedCall(TBinaryExpr(El.Parent));
    exit;
    end;
  CheckTopScope(TPasProcedureScope);
  ProcScope:=TPasProcedureScope(TopScope);
  if ProcScope.ClassScope=nil then
    RaiseMsg(nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);

  AncestorScope:=ProcScope.ClassScope.AncestorScope;
  if AncestorScope=nil then
    begin
    // 'inherited;' without ancestor is ignored
    exit;
    end;

  // search in ancestor
  DeclProc:=ProcScope.DeclarationProc;
  DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
  AncestorProc:=DeclProcScope.OverriddenProc;
  if AncestorProc<>nil then
    begin
    CreateReference(AncestorProc,El);
    if AncestorProc.IsAbstract then
      RaiseMsg(nAbstractMethodsCannotBeCalledDirectly,
        sAbstractMethodsCannotBeCalledDirectly,[],El);
    end
  else
    begin
    // 'inherited;' without ancestor is ignored
    exit;
    end;
end;

procedure TPasResolver.ResolveInheritedCall(El: TBinaryExpr);
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
    RaiseMsg(nInheritedOnlyWorksInMethods,sInheritedOnlyWorksInMethods,[],El);

  AncestorScope:=ProcScope.ClassScope.AncestorScope;
  if AncestorScope=nil then
    RaiseMsg(nInheritedNeedsAncestor,sInheritedNeedsAncestor,[],El.left);

  // search call in ancestor
  AncestorClass:=TPasClassType(AncestorScope.Element);
  InhScope:=PushClassDotScope(AncestorClass);
  InhScope.InheritedExpr:=true;
  ResolveExpr(El.right);
  PopScope;
end;

procedure TPasResolver.ResolveBinaryExpr(El: TBinaryExpr);
begin
  //writeln('TPasResolver.ResolveBinaryExpr left=',GetObjName(El.left),' right=',GetObjName(El.right),' opcode=',OpcodeStrings[El.OpCode]);
  ResolveExpr(El.left);
  if El.right=nil then exit;
  case El.OpCode of
  eopNone:
    case El.Kind of
    pekRange:
      ResolveExpr(El.right);
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
    begin
    ResolveExpr(El.right);
    end;
  //eopAddress: ;
  //eopDeref: ;
  eopSubIdent:
    ResolveSubIdent(El);
  else
    RaiseNotYetImplemented(20160922163459,El,OpcodeStrings[El.OpCode]);
  end;
end;

procedure TPasResolver.ResolveSubIdent(El: TBinaryExpr);
var
  ModuleScope: TPasSubModuleScope;
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
  ComputeElement(Left,LeftResolved,[rcReturnFuncResult]);

  if LeftResolved.BaseType=btModule then
    begin
    // e.g. unitname.identifier
    // => search in interface and if this is our module in the implementation
    aModule:=LeftResolved.IdentEl as TPasModule;
    ModuleScope:=TPasSubModuleScope.Create;
    ModuleScope.Owner:=Self;
    ModuleScope.CurModule:=aModule;
    if aModule is TPasProgram then
      begin // program
      if TPasProgram(aModule).ProgramSection<>nil then
        ModuleScope.InterfaceScope:=
          TPasProgram(aModule).ProgramSection.CustomData as TPasSectionScope;
      end
    else if aModule is TPasLibrary then
      begin // library
      if TPasLibrary(aModule).LibrarySection<>nil then
        ModuleScope.InterfaceScope:=
          TPasLibrary(aModule).LibrarySection.CustomData as TPasSectionScope;
      end
    else
      begin // unit
      if aModule.InterfaceSection<>nil then
        ModuleScope.InterfaceScope:=
          aModule.InterfaceSection.CustomData as TPasSectionScope;
      if (aModule=CurrentParser.CurModule)
          and (aModule.ImplementationSection<>nil)
          and (aModule.ImplementationSection.CustomData<>nil)
      then
        ModuleScope.ImplementationScope:=aModule.ImplementationSection.CustomData as TPasSectionScope;
      end;
    PushScope(ModuleScope);
    ResolveExpr(El.right);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl=nil then
    begin
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
    ResolveExpr(El.right);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl.ClassType=TPasClassOfType then
    begin
    // e.g. ImageClass.FindHandlerFromExtension()
    ClassEl:=ResolveAliasType(TPasClassOfType(LeftResolved.TypeEl).DestType) as TPasClassType;
    ClassScope:=PushClassDotScope(ClassEl);
    ClassScope.OnlyTypeMembers:=true;
    ResolveExpr(El.right);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl.ClassType=TPasRecordType then
    begin
    RecordEl:=TPasRecordType(LeftResolved.TypeEl);
    RecordScope:=PushRecordDotScope(RecordEl);
    if LeftResolved.IdentEl is TPasType then
      // e.g. TPoint.PointInCircle
      RecordScope.OnlyTypeMembers:=true
    else
      // e.g. aPoint.X
      RecordScope.OnlyTypeMembers:=false;
    ResolveExpr(El.right);
    PopScope;
    exit;
    end
  else if LeftResolved.TypeEl.ClassType=TPasEnumType then
    begin
    if LeftResolved.IdentEl is TPasType then
      begin
      // e.g. TShiftState.ssAlt
      PushEnumDotScope(TPasEnumType(LeftResolved.TypeEl));
      ResolveExpr(El.right);
      PopScope;
      exit;
      end;
    end
  else
    RaiseMsg(nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
      [LeftResolved.TypeEl.ElementTypeName],El);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSubIdent left=',GetObjName(Left),' right=',GetObjName(El.right),' leftresolved=',GetResolverResultDesc(LeftResolved));
  {$ENDIF}
  RaiseMsg(nIllegalQualifier,sIllegalQualifier,['.'],El);
end;

procedure TPasResolver.ResolveParamsExpr(Params: TParamsExpr);
var
  i, ScopeDepth: Integer;
begin
  // first resolve params
  ResetSubScopes(ScopeDepth);
  for i:=0 to length(Params.Params)-1 do
    ResolveExpr(Params.Params[i]);
  RestoreSubScopes(ScopeDepth);

  // then resolve the call, typecast, array, set
  if (Params.Kind=pekFuncParams) then
    ResolveFuncParamsExpr(Params)
  else if (Params.Kind=pekArrayParams) then
    ResolveArrayParamsExpr(Params)
  else if (Params.Kind=pekSet) then
    ResolveSetParamsExpr(Params)
  else
    RaiseNotYetImplemented(20160922163501,Params);
end;

procedure TPasResolver.ResolveFuncParamsExpr(Params: TParamsExpr);
var
  i: Integer;
  ElName, Msg: String;
  FindCallData: TFindCallElData;
  Abort: boolean;
  El: TPasElement;
  Ref: TResolvedReference;
  FindData: TPRFindData;
  BuiltInProc: TResElDataBuiltInProc;
begin
  if not (Params.Value.ClassType=TPrimitiveExpr) then
    RaiseNotYetImplemented(20160927212525,Params.Value);
  ElName:=TPrimitiveExpr(Params.Value).Value;
  FindCallData:=Default(TFindCallElData);
  FindCallData.Params:=Params;
  Abort:=false;
  IterateElements(ElName,@OnFindCallElements,@FindCallData,Abort);
  if FindCallData.Found=nil then
    RaiseIdentifierNotFound(ElName,Params.Value);
  if FindCallData.Distance=cIncompatible then
    begin
    // found one element, but it was incompatible => raise error
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveParamsExpr found one element, but it was incompatible => check again to raise error');
    {$ENDIF}
    if FindCallData.Found is TPasProcedure then
      CheckCallProcCompatibility(TPasProcedure(FindCallData.Found),Params,true)
    else if FindCallData.Found.ClassType=TPasUnresolvedSymbolRef then
      begin
      if FindCallData.Found.CustomData is TResElDataBuiltInProc then
        begin
        BuiltInProc:=TResElDataBuiltInProc(FindCallData.Found.CustomData);
        BuiltInProc.GetCallCompatibility(BuiltInProc,Params,true);
        end
      else if FindCallData.Found.CustomData.ClassType=TResElDataBaseType then
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
      RaiseMsg(nCantDetermineWhichOverloadedFunctionToCall,
        sCantDetermineWhichOverloadedFunctionToCall+Msg,[ElName],Params.Value);
    finally
      FindCallData.List.Free;
    end;
    end;

  // found compatible element -> create reference
  Ref:=CreateReference(FindCallData.Found,Params.Value);
  FindData:=Default(TPRFindData);
  FindData.ErrorPosEl:=Params.Value;
  FindData.StartScope:=FindCallData.StartScope;
  FindData.ElScope:=FindCallData.ElScope;
  FindData.Found:=FindCallData.Found;
  CheckFoundElOnStartScope(FindData,Ref);
end;

procedure TPasResolver.ResolveArrayParamsExpr(Params: TParamsExpr);
var
  ArrayName: String;
  FindData: TPRFindData;
  DeclEl: TPasElement;
  ResolvedEl, ResolvedArg: TPasResolverResult;
  ArgExp: TPasExpr;
  Ref: TResolvedReference;
  PropEl: TPasProperty;
  ClassScope: TPasClassScope;
  SubParams: TParamsExpr;
begin
  DeclEl:=nil;
  if (Params.Value.ClassType=TPrimitiveExpr)
      and (TPrimitiveExpr(Params.Value).Kind=pekIdent) then
    begin
    // e.g. Name[]
    ArrayName:=TPrimitiveExpr(Params.Value).Value;
    // find first
    DeclEl:=FindElementWithoutParams(ArrayName,FindData,Params.Value);
    Ref:=CreateReference(DeclEl,Params.Value,@FindData);
    CheckFoundElOnStartScope(FindData,Ref);
    ComputeElement(Params.Value,ResolvedEl,[rcSkipTypeAlias,rcReturnFuncResult]);
    end
  else if Params.Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Params.Value);
    if (SubParams.Kind in [pekArrayParams,pekFuncParams]) then
      begin
      // e.g. Name()[] or Name[][]
      ResolveExpr(SubParams);
      ComputeElement(SubParams,ResolvedEl,[rcSkipTypeAlias,rcReturnFuncResult]);
      end
    else
      RaiseNotYetImplemented(20161010194925,Params.Value);
    end
  else
    RaiseNotYetImplemented(20160927212610,Params.Value);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveArrayParamsExpr Params.Value=',GetObjName(Params.Value),' ',GetResolverResultDesc(ResolvedEl));
  {$ENDIF}
  if ResolvedEl.BaseType in btAllStrings then
    begin
    // string -> check that ResolvedEl is not merely a type, but has a value
    if not ResolvedElHasValue(ResolvedEl) then
      RaiseXExpectedButYFound('variable',ResolvedEl.TypeEl.ElementTypeName,Params);
    // check single argument
    if length(Params.Params)<1 then
      RaiseMsg(nMissingParameterX,
        sMissingParameterX,['character index'],Params)
    else if length(Params.Params)>1 then
      RaiseMsg(nIllegalQualifier,sIllegalQualifier,[','],Params.Params[1]);
    // check argument is integer
    ArgExp:=Params.Params[0];
    ComputeElement(ArgExp,ResolvedArg,[rcSkipTypeAlias,rcReturnFuncResult]);
    if not (ResolvedArg.BaseType in btAllInteger) then
      RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [BaseTypeNames[ResolvedArg.BaseType],BaseTypeNames[BaseTypeStringIndex]],ArgExp);
    if not (rrfReadable in ResolvedArg.Flags) then
      RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        ['type','value'],ArgExp);
    exit;
    end
  else if (ResolvedEl.IdentEl is TPasProperty)
      and (TPasProperty(ResolvedEl.IdentEl).Args.Count>0) then
    begin
    PropEl:=TPasProperty(ResolvedEl.IdentEl);
    CheckCallPropertyCompatibility(PropEl,Params,true);
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
          RaiseMsg(nIllegalQualifier,sIllegalQualifier,['['],Params);
        CheckCallPropertyCompatibility(PropEl,Params,true);
        exit;
        end;
      end
    else if ResolvedEl.TypeEl.ClassType=TPasArrayType then
      begin
      if ResolvedEl.IdentEl is TPasType then
        RaiseMsg(nIllegalQualifier,sIllegalQualifier,['['],Params);
      CheckCallArrayCompatibility(TPasArrayType(ResolvedEl.TypeEl),Params,true);
      exit;
      end;
    end;
  RaiseMsg(nIllegalQualifier,sIllegalQualifier,['['],Params);
end;

procedure TPasResolver.ResolveSetParamsExpr(Params: TParamsExpr);
// e.g. resolving '[1,2..3]'
var
  FirstResolved, ResolvedEl: TPasResolverResult;
  i: Integer;
  Param: TPasExpr;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveSetParamsExpr ',GetTreeDesc(Params));
  {$ENDIF}
  if Params.Value<>nil then
    RaiseNotYetImplemented(20160930135910,Params);
  if length(Params.Params)=0 then
    // empty set
  else
    begin
    FirstResolved:=Default(TPasResolverResult);
    for i:=0 to length(Params.Params)-1 do
      begin
      Param:=Params.Params[i];
      ComputeElement(Params.Params[0],ResolvedEl,[rcReturnFuncResult]);
      if ResolvedEl.BaseType=btRange then
        ConvertRangeToFirstValue(ResolvedEl);
      if FirstResolved.BaseType=btNone then
        begin
        // first value -> check type usable in a set
        FirstResolved:=ResolvedEl;
        if FirstResolved.BaseType in (btAllInteger+btAllBooleans+[btChar]) then
        else if (FirstResolved.BaseType=btContext) then
          begin
          if FirstResolved.TypeEl.ClassType=TPasEnumType then
          else
            RaiseXExpectedButYFound('ordinal value',FirstResolved.TypeEl.ElementTypeName,Param);
          end
        else
          RaiseXExpectedButYFound('ordinal value',BaseTypeNames[FirstResolved.BaseType],Param);
        if not ResolvedElHasValue(FirstResolved) then
          RaiseXExpectedButYFound('ordinal value','type',Param);
        end
      else
        begin
        // next value
        CheckSetElementsCompatible(Params.Params[0],Param,FirstResolved,ResolvedEl);
        end;
      end;
    end;
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
          RaiseMsg(nForwardProcNotResolved,sForwardProcNotResolved,
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
        if Proc.IsAbstract then continue;
        if TPasProcedureScope(Proc.CustomData).ImplProc=nil then
          RaiseMsg(nForwardProcNotResolved,sForwardProcNotResolved,
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
    CreateReference(El,ForwardDecl);
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
        RaiseMsg(nDuplicateIdentifier,sDuplicateIdentifier,
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
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProcedure ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(20160922163522,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikProc);
  PushScope(El,TPasProcedureScope);
end;

procedure TPasResolver.AddArgument(El: TPasArgument);
begin
  if (El.Name='') then
    RaiseInternalError(20160922163526,GetObjName(El));
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddArgument ',GetObjName(El));
  {$ENDIF}
  if (TopScope=nil)
      or ((TopScope.ClassType<>TPasProcedureScope)
        and (TopScope.ClassType<>TPasPropertyScope)) then
    RaiseInvalidScopeForElement(20160922163529,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
end;

procedure TPasResolver.AddFunctionResult(El: TPasResultElement);
begin
  if TopScope.ClassType<>TPasProcedureScope then
    RaiseInvalidScopeForElement(20160922163531,El);
  AddIdentifier(TPasProcedureScope(TopScope),ResolverResultVar,El,pikSimple);
end;

procedure TPasResolver.AddExceptOn(El: TPasImplExceptOn);
begin
  PushScope(El,TPasExceptOnScope);
end;

procedure TPasResolver.StartProcedureBody(El: TProcedureBody);
begin
  if El=nil then ;
  // ToDo: check if all nested forward procs are resolved
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
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);

  procedure SetBaseType(BaseType: TResolverBaseType);
  begin
    SetResolverValueExpr(ResolvedEl,BaseType,FBaseTypes[BaseType],Bin,[rrfReadable]);
  end;

var
  LeftResolved, RightResolved, RightClassResolved: TPasResolverResult;
begin
  ComputeElement(Bin.left,LeftResolved,Flags);
  ComputeElement(Bin.right,RightResolved,Flags);
  // ToDo: check operator overloading

  if LeftResolved.BaseType=btProc then
    ComputeFuncCallWithoutParams(LeftResolved,Bin.left);
  if RightResolved.BaseType=btProc then
    ComputeFuncCallWithoutParams(RightResolved,Bin.right);

  if Bin.OpCode in [eopEqual,eopNotEqual] then
    begin
    if CheckEqualCompatibility(LeftResolved,RightResolved,Bin,true)=cIncompatible then
      RaiseInternalError(20161007215912);
    if not (rrfReadable in LeftResolved.Flags) then
      RaiseMsg(nNotReadable,sNotReadable,[],Bin.left);
    if not (rrfReadable in RightResolved.Flags) then
      RaiseMsg(nNotReadable,sNotReadable,[],Bin.right);
    SetBaseType(btBoolean);
    exit;
    end;

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
                RaiseXExpectedButYFound('integer',BaseTypeNames[RightResolved.BaseType],Bin.right);
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
              RaiseXExpectedButYFound('char',BaseTypeNames[RightResolved.BaseType],Bin.right);
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
            RaiseXExpectedButYFound('set of '+BaseTypeNames[LeftResolved.BaseType],LeftResolved.TypeEl.ElementTypeName,Bin.right);
          if LeftResolved.BaseType=btChar then
            begin
            if RightResolved.SubType<>btChar then
              RaiseXExpectedButYFound('set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.right);
            end
          else if not (RightResolved.SubType in btAllInteger) then
            RaiseXExpectedButYFound('set of '+BaseTypeNames[LeftResolved.BaseType],'set of '+BaseTypeNames[RightResolved.SubType],Bin.right);
          SetBaseType(btBoolean);
          exit;
          end
        else if (LeftResolved.BaseType=btContext) and (LeftResolved.TypeEl is TPasEnumType) then
          begin
          if (RightResolved.BaseType<>btSet) then
            RaiseXExpectedButYFound('set of '+LeftResolved.TypeEl.Name,LeftResolved.TypeEl.ElementTypeName,Bin.right);
          if LeftResolved.TypeEl<>RightResolved.TypeEl then
            RaiseXExpectedButYFound('set of '+LeftResolved.TypeEl.Name,'set of '+RightResolved.TypeEl.Name,Bin.right);
          SetBaseType(btBoolean);
          exit;
          end
        else
          RaiseMsg(nInOperatorExpectsSetElementButGot,
            sInOperatorExpectsSetElementButGot,[LeftResolved.TypeEl.ElementTypeName],Bin);
        end;
    eopIs:
      begin
      if (LeftResolved.TypeEl is TPasClassType) then
        begin
        if (LeftResolved.IdentEl=nil) or (LeftResolved.IdentEl is TPasType) then
          RaiseMsg(nIllegalQualifier,sIllegalQualifier,['is'],Bin);
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
          SetResolverTypeExpr(RightClassResolved,btContext,TPasClassOfType(RightResolved.TypeEl).DestType,[]);
          if (CheckSrcIsADstType(LeftResolved,RightClassResolved,Bin)<>cIncompatible)
          or (CheckSrcIsADstType(RightClassResolved,LeftResolved,Bin)<>cIncompatible) then
            begin
            SetBaseType(btBoolean);
            exit;
            end
          end
        else
          RaiseXExpectedButYFound('class type',RightResolved.TypeEl.ElementTypeName,Bin.right);
        end
      else if LeftResolved.TypeEl=nil then
        RaiseMsg(nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [BaseTypeNames[LeftResolved.BaseType]],Bin.left)
      else
        RaiseMsg(nLeftSideOfIsOperatorExpectsAClassButGot,sLeftSideOfIsOperatorExpectsAClassButGot,
                 [LeftResolved.TypeEl.ElementTypeName],Bin.left);
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ComputeBinaryExpr is-operator: left=',GetResolverResultDesc(LeftResolved),' right=',GetResolverResultDesc(RightResolved));
      {$ENDIF}
      RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],Bin);
      end;
    eopAs:
      begin
      if (LeftResolved.TypeEl is TPasClassType) then
        begin
        if (LeftResolved.IdentEl=nil) or (LeftResolved.IdentEl is TPasType)
            or (not (rrfReadable in LeftResolved.Flags)) then
          RaiseMsg(nIllegalQualifier,sIllegalQualifier,['as'],Bin);
        if RightResolved.IdentEl=nil then
          RaiseXExpectedButYFound('class',RightResolved.TypeEl.ElementTypeName,Bin.right);
        if not (RightResolved.IdentEl is TPasType) then
          RaiseXExpectedButYFound('class',RightResolved.IdentEl.Name,Bin.right);
        if (CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible) then
          begin
          SetResolverValueExpr(ResolvedEl,btContext,RightResolved.TypeEl,Bin,[rrfReadable]);
          exit;
          end;
        RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],Bin);
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
      eopSymmetricaldifference:
        begin
        if RightResolved.TypeEl=nil then
          begin
          // right is empty set
          ResolvedEl:=LeftResolved;
          exit;
          end;
        if LeftResolved.TypeEl=nil then
          begin
          // left is empty set
          ResolvedEl:=RightResolved;
          exit;
          end;
        if (LeftResolved.SubType=RightResolved.SubType)
            or ((LeftResolved.SubType in btAllBooleans)
              and (RightResolved.SubType in btAllBooleans))
            or ((LeftResolved.SubType in btAllInteger)
              and (RightResolved.SubType in btAllInteger)) then
          begin
          ResolvedEl:=LeftResolved;
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
  RaiseMsg(nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[Bin.OpCode]],Bin);
end;

procedure TPasResolver.ComputeArrayParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
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
    ComputeElement(Params.Value,ResolvedEl,Flags+[rcReturnFuncResult]);
    end
  else if Params.Value.ClassType=TParamsExpr then
    begin
    SubParams:=TParamsExpr(Params.Value);
    if SubParams.Kind in [pekArrayParams,pekFuncParams] then
      begin
      // e.g. Name()[] or Name[][]
      ComputeElement(SubParams,ResolvedEl,Flags+[rcReturnFuncResult]);
      end
    else
      RaiseNotYetImplemented(20161010195646,SubParams);
    end
  else
    RaiseNotYetImplemented(20160928174144,Params);

  if ResolvedEl.BaseType in btAllStrings then
    begin
    // stringvar[] => char
    if ResolvedEl.BaseType in [btWideString,btUnicodeString] then
      ResolvedEl.BaseType:=btWideChar
    else
      ResolvedEl.BaseType:=btChar;
    ResolvedEl.IdentEl:=nil;
    ResolvedEl.TypeEl:=FBaseTypes[ResolvedEl.BaseType];
    ResolvedEl.ExprEl:=Params;
    end
  else if ResolvedEl.IdentEl is TPasProperty then
    // property with args
  else if ResolvedEl.BaseType=btContext then
    begin
    TypeEl:=ResolvedEl.TypeEl;
    if TypeEl.ClassType=TPasClassType then
      begin
      ClassScope:=TypeEl.CustomData as TPasClassScope;
      if ClassScope.DefaultProperty=nil then
        RaiseInternalError(20161010151747);
      ComputeElement(ClassScope.DefaultProperty,ResolvedEl,[]);
      end
    else if TypeEl.ClassType=TPasClassOfType then
      begin
      ClassScope:=TPasClassOfType(TypeEl).DestType.CustomData as TPasClassScope;
      if ClassScope.DefaultProperty=nil then
        RaiseInternalError(20161010174916);
      ComputeElement(ClassScope.DefaultProperty,ResolvedEl,[]);
      end
    else if TypeEl.ClassType=TPasArrayType then
      begin
      ArrayEl:=TPasArrayType(TypeEl);
      ArgNo:=0;
      repeat
        if length(ArrayEl.Ranges)=0 then
          inc(ArgNo) // dynamic array has one dimension
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
      ComputeElement(ArrayEl.ElType,ResolvedEl,Flags);
      // identifier and value is the array itself
      ResolvedEl.IdentEl:=OrigResolved.IdentEl;
      ResolvedEl.ExprEl:=OrigResolved.ExprEl;
      ResolvedEl.Flags:=OrigResolved.Flags*[rrfReadable,rrfWritable];
      end
    else
      RaiseNotYetImplemented(20161010151727,Params,GetResolverResultDesc(ResolvedEl));
    end
  else
    RaiseNotYetImplemented(20160928174212,Params,GetResolverResultDesc(ResolvedEl));
end;

procedure TPasResolver.ComputeFuncParams(Params: TParamsExpr; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
var
  DeclEl: TPasElement;
  BuiltInProc: TResElDataBuiltInProc;
  Proc: TPasProcedure;
  aClass: TPasClassType;
  ResolvedTypeEl: TPasResolverResult;
begin
  if Params.Value.CustomData is TResolvedReference then
    begin
    DeclEl:=TResolvedReference(Params.Value.CustomData).Declaration;
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
        end
      else if DeclEl.CustomData.ClassType=TResElDataBaseType then
        begin
        // type case to base type
        SetResolverValueExpr(ResolvedEl,
          TResElDataBaseType(DeclEl.CustomData).BaseType,
          TPasUnresolvedSymbolRef(DeclEl),Params.Params[0],[rrfReadable]);
        end
      else
        RaiseNotYetImplemented(20161006133040,Params,GetResolverResultDesc(ResolvedEl));
      end
    else
      begin
      ComputeElement(DeclEl,ResolvedEl,Flags-[rcReturnFuncResult]);
      if ResolvedEl.BaseType=btProc then
        begin
        if not (ResolvedEl.IdentEl is TPasProcedure) then
          RaiseNotYetImplemented(20160928180201,Params,GetResolverResultDesc(ResolvedEl));
        Proc:=TPasProcedure(ResolvedEl.IdentEl);
        if rcConstant in Flags then
          RaiseMsg(nConstantExpressionExpected,sConstantExpressionExpected,[],Params);
        if Proc is TPasFunction then
          // function => return result
          ComputeElement(TPasFunction(Proc).FuncType.ResultEl,ResolvedEl,Flags-[rcReturnFuncResult])
        else if Proc.ClassType=TPasConstructor then
          begin
          // constructor -> return value of type class
          aClass:=Proc.Parent as TPasClassType;
          SetResolverValueExpr(ResolvedEl,btContext,aClass,Params.Value,[rrfReadable]);
          end
        else
          // procedure, neither readable nor writable
          SetResolverIdentifier(ResolvedEl,btProc,Proc,Proc.ProcType,[]);
        end
      else if (DeclEl is TPasType) then
        begin
        // type cast
        ResolvedTypeEl:=ResolvedEl;
        ComputeElement(Params.Params[0],ResolvedEl,[rcReturnFuncResult]);
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
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
begin
  if length(Params.Params)=0 then
    SetResolverValueExpr(ResolvedEl,btSet,nil,Params,[rrfReadable])
  else
    begin
    ComputeElement(Params.Params[0],ResolvedEl,Flags+[rcReturnFuncResult]);
    if ResolvedEl.BaseType=btRange then
      ConvertRangeToFirstValue(ResolvedEl);
    ResolvedEl.SubType:=ResolvedEl.BaseType;
    ResolvedEl.BaseType:=btSet;
    ResolvedEl.Flags:=[rrfReadable];
    end;
end;

procedure TPasResolver.ComputeFuncCallWithoutParams(
  var ResolvedEl: TPasResolverResult; ErrorEl: TPasExpr);
var
  aClass: TPasClassType;
  Proc: TPasProcedure;
begin
  // call without arguments
  if ResolvedEl.IdentEl=nil then
    RaiseNotYetImplemented(20160928183455,ErrorEl,GetResolverResultDesc(ResolvedEl));
  if not (ResolvedEl.IdentEl is TPasProcedure) then
    RaiseXExpectedButYFound('function',ResolvedEl.IdentEl.ElementTypeName,ErrorEl);
  Proc:=TPasProcedure(ResolvedEl.IdentEl);
  if (Proc.ProcType.Args.Count>0)
      and (TPasArgument(Proc.ProcType.Args[0]).ValueExpr=nil) then
    RaiseMsg(nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
      [GetProcDesc(Proc)],ErrorEl);
  if (ResolvedEl.IdentEl is TPasFunction) then
    ComputeElement(TPasFunction(ResolvedEl.IdentEl).FuncType.ResultEl,ResolvedEl,[])
  else if ResolvedEl.IdentEl.ClassType=TPasConstructor then
    begin
    aClass:=Proc.Parent as TPasClassType;
    SetResolverValueExpr(ResolvedEl,btContext,aClass,ErrorEl,[rrfReadable]);
    end
  else
    RaiseXExpectedButYFound('function',ResolvedEl.IdentEl.ElementTypeName,ErrorEl);
end;

procedure TPasResolver.CheckIsClass(El: TPasElement;
  const ResolvedEl: TPasResolverResult);
begin
  if (ResolvedEl.BaseType<>btContext) then
    RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
      ['class',BaseTypeNames[ResolvedEl.BaseType]],El);
  if (ResolvedEl.TypeEl.ClassType<>TPasClassType) then
    RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
      ['class',ResolvedEl.TypeEl.ElementTypeName],El);
end;

procedure TPasResolver.CheckRangeExpr(Left, Right: TPasExpr; out LeftResolved,
  RightResolved: TPasResolverResult);
begin
  ComputeElement(Left,LeftResolved,[rcSkipTypeAlias,rcReturnFuncResult]);
  ComputeElement(Right,RightResolved,[rcSkipTypeAlias,rcReturnFuncResult]);
  CheckSetElementsCompatible(Left,Right,LeftResolved,RightResolved);
end;

procedure TPasResolver.CheckSetElementsCompatible(Left, Right: TPasExpr;
  const LeftResolved, RightResolved: TPasResolverResult);
begin
  // check both are values
  if not ResolvedElHasValue(LeftResolved) then
    begin
    if LeftResolved.TypeEl<>nil then
      RaiseXExpectedButYFound('ordinal',LeftResolved.TypeEl.ElementTypeName,Left)
    else
      RaiseXExpectedButYFound('ordinal',BaseTypeNames[LeftResolved.BaseType],Left);
    end;
  if not ResolvedElHasValue(RightResolved) then
    begin
    if RightResolved.TypeEl<>nil then
      RaiseXExpectedButYFound('ordinal',RightResolved.TypeEl.ElementTypeName,Right)
    else
      RaiseXExpectedButYFound('ordinal',BaseTypeNames[RightResolved.BaseType],Right);
    end;
  // check both have the same ordinal type
  if LeftResolved.BaseType in btAllBooleans then
    begin
    if (RightResolved.BaseType in btAllBooleans) then
      exit;
    RaiseXExpectedButYFound('boolean',BaseTypeNames[RightResolved.BaseType],Right);
    end
  else if LeftResolved.BaseType in btAllInteger then
    begin
    if (RightResolved.BaseType in btAllInteger) then
      exit;
    RaiseXExpectedButYFound('integer',BaseTypeNames[RightResolved.BaseType],Right);
    end
  else if LeftResolved.BaseType=btChar then
    begin
    if (RightResolved.BaseType=btChar) then
      exit;
    RaiseXExpectedButYFound('char',BaseTypeNames[RightResolved.BaseType],Right);
    end
  else if LeftResolved.BaseType=btContext then
    begin
    if LeftResolved.TypeEl.ClassType=TPasEnumType then
      begin
      if LeftResolved.TypeEl=RightResolved.TypeEl then
        exit;
      if RightResolved.TypeEl.ClassType<>TPasEnumType then
        RaiseXExpectedButYFound(LeftResolved.TypeEl.Parent.Name,RightResolved.TypeEl.ElementTypeName,Right);
      if LeftResolved.TypeEl.Parent<>RightResolved.TypeEl.Parent then
        RaiseXExpectedButYFound(LeftResolved.TypeEl.Parent.Name,RightResolved.TypeEl.Parent.Name,Right);
      end
    else
      RaiseXExpectedButYFound('ordinal',BaseTypeNames[LeftResolved.BaseType],Left);
    end
  else
    RaiseXExpectedButYFound('ordinal',BaseTypeNames[LeftResolved.BaseType],Left);
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

function TPasResolver.OnGetCallCompatibility_Length(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'length'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<1) then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(cIncompatible);
    end;
  Params:=TParamsExpr(Expr);

  // first param: string or dynamic array
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
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
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['1',GetTypeDesc(ParamResolved.TypeEl),'string or array'],
        Param);
    exit;
    end;

  if length(Params.Params)>1 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[1]);
    exit(cIncompatible);
    end;
end;

procedure TPasResolver.OnGetCallResult_Length(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btInt64,Proc.Proc,FBaseTypes[btInt64],[rrfReadable]);
end;

function TPasResolver.OnGetCallCompatibility_SetLength(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'setlength'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<2) then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(cIncompatible);
    end;
  Params:=TParamsExpr(Expr);

  // first param: string or array variable
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
  Result:=cIncompatible;
  if ResolvedElCanBeVarParam(ParamResolved) then
    begin
    if ParamResolved.BaseType in btAllStrings then
      Result:=cExact
    else if ParamResolved.BaseType=btContext then
      begin
      if ParamResolved.TypeEl.ClassType=TPasArrayType then
        Result:=cExact;
      end;
    end;
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['1',GetTypeDesc(ParamResolved.TypeEl),'string or array variable'],
        Param);
    exit(cIncompatible);
    end;

  // second param: new length
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
  Result:=cIncompatible;
  if (rrfReadable in ParamResolved.Flags)
      and (ParamResolved.BaseType in btAllInteger) then
    Result:=cExact;
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['2',GetTypeDesc(ParamResolved.TypeEl),'integer'],Param);
    exit(cIncompatible);
    end;

  if length(Params.Params)>2 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[2]);
    exit(cIncompatible);
    end;
end;

function TPasResolver.OnGetCallCompatibility_InExclude(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
// check params of built in proc 'include'
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
  EnumType: TPasEnumType;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<2) then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(cIncompatible);
    end;
  Params:=TParamsExpr(Expr);

  // first param: variable of set of enumtype
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
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
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['1',GetTypeDesc(ParamResolved.TypeEl),'variable of set of enumtype'],
        Param);
    exit(cIncompatible);
    end;

  // second param: enum
  Param:=Params.Params[1];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
  if (not (rrfReadable in ParamResolved.Flags))
      or (ParamResolved.TypeEl<>EnumType) then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['2',GetTypeDesc(ParamResolved.TypeEl),EnumType.Name],
        Param);
    exit(cIncompatible);
    end;

  if length(Params.Params)>2 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[2]);
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

function TPasResolver.OnGetCallCompatibility_Ord(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<1) then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(cIncompatible);
    end;
  Params:=TParamsExpr(Expr);

  // first param: enum or char
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
  Result:=cIncompatible;
  if rrfReadable in ParamResolved.Flags then
    begin
    if ParamResolved.BaseType=btChar then
      Result:=cExact
    else if (ParamResolved.BaseType=btContext) and (ParamResolved.TypeEl is TPasEnumType) then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['1',GetTypeDesc(ParamResolved.TypeEl),'enum or char'],
        Param);
    exit;
    end;

  if length(Params.Params)>1 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[1]);
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

procedure TPasResolver.OnGetCallResult_Ord(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btSmallInt,Proc.Proc,FBaseTypes[btSmallInt],[rrfReadable]);
end;

function TPasResolver.OnGetCallCompatibility_Exit(Proc: TResElDataBuiltInProc;
  Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, ResultResolved: TPasResolverResult;
  i: Integer;
  ProcScope: TPasProcedureScope;
  ResultEl: TPasResultElement;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)=0) then
    exit(cExact);
  Params:=TParamsExpr(Expr);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit Params=',length(Params.Params));
  {$ENDIF}

  // first param: result
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ParamResolved=',GetResolverResultDesc(ParamResolved));
  {$ENDIF}
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
        RaiseMsg(nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,['procedure exit'],Params.Params[0]);
      exit(cIncompatible);
      end;
    ResultEl:=(ProcScope.Element as TPasFunction).FuncType.ResultEl;
    ComputeElement(ResultEl,ResultResolved,[]);
    end
  else
    begin
    // default: main program, param is an integer
    SetResolverTypeExpr(ResultResolved,btLongint,FBaseTypes[btLongint],[rrfReadable,rrfWritable]);
    end;

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_Exit ResultResolved=',GetResolverResultDesc(ResultResolved));
  {$ENDIF}
  if rrfReadable in ParamResolved.Flags then
    Result:=CheckAssignCompatibility(ResultResolved,ParamResolved,Params.Params[0]);
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['1',GetTypeDesc(ParamResolved.TypeEl),GetTypeDesc(ResultResolved.TypeEl)],
        Param);
    exit;
    end;

  if length(Params.Params)>1 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[1]);
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

function TPasResolver.OnGetCallCompatibility_IncDec(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved, IncrResolved: TPasResolverResult;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<1) then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(cIncompatible);
    end;
  Params:=TParamsExpr(Expr);

  // first param: var Integer
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnGetCallCompatibility_IncDec ParamResolved=',GetResolverResultDesc(ParamResolved));
  {$ENDIF}
  Result:=cIncompatible;
  // Expr must be a variable
  if not ResolvedElCanBeVarParam(ParamResolved) then
    begin
    if RaiseOnError then
      RaiseMsg(nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
    exit;
    end;
  if ParamResolved.BaseType in btAllInteger then
    Result:=cExact;
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['1',GetTypeDesc(ParamResolved.TypeEl),BaseTypeNames[btLongint]],
        Param);
    exit;
    end;

  if length(Params.Params)=1 then
    exit;

  // second param: increment/decrement
  Param:=Params.Params[1];
  ComputeElement(Param,IncrResolved,[rcReturnFuncResult]);
  Result:=cIncompatible;
  if rrfReadable in IncrResolved.Flags then
    begin
    if IncrResolved.BaseType in btAllInteger then
      Result:=cExact;
    end;
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['2',GetTypeDesc(IncrResolved.TypeEl),BaseTypeNames[btLongint]],
        Param);
    exit;
    end;

  if length(Params.Params)>2 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[2]);
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

function TPasResolver.OnGetCallCompatibility_Assigned(
  Proc: TResElDataBuiltInProc; Expr: TPasExpr; RaiseOnError: boolean): integer;
var
  Params: TParamsExpr;
  Param: TPasExpr;
  ParamResolved: TPasResolverResult;
begin
  if (not (Expr is TParamsExpr)) or (length(TParamsExpr(Expr).Params)<1) then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Expr);
    exit(cIncompatible);
    end;
  Params:=TParamsExpr(Expr);

  // first param: enum or char
  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
  Result:=cIncompatible;
  if ParamResolved.BaseType in [btNil,btPointer] then
    Result:=cExact
  else if (ParamResolved.BaseType=btContext)
      and ((ParamResolved.TypeEl.ClassType=TPasClassType)
        or (ParamResolved.TypeEl.ClassType=TPasClassOfType)) then
    Result:=cExact;
  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
        ['1',GetTypeDesc(ParamResolved.TypeEl),'enum or char'],
        Param);
    exit;
    end;

  if length(Params.Params)>1 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[Proc.Signature],Params.Params[1]);
    exit(cIncompatible);
    end;

  Result:=cExact;
end;

procedure TPasResolver.OnGetCallResult_Assigned(Proc: TResElDataBuiltInProc;
  Params: TParamsExpr; out ResolvedEl: TPasResolverResult);
begin
  SetResolverIdentifier(ResolvedEl,btBoolean,Proc.Proc,FBaseTypes[btBoolean],[rrfReadable]);
end;

constructor TPasResolver.Create;
begin
  inherited Create;
  FDefaultScope:=TPasDefaultScope.Create;
  FPendingForwards:=TFPList.Create;
  FBaseTypeStringIndex:=btComp;
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
    StartProcedureBody(TProcedureBody(El))
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

function TPasResolver.FindElement(const AName: String): TPasElement;
begin
  //writeln('TPasResolver.FindElement Name="',AName,'"');
  Result:=FindElementWithoutParams(AName,LastElement);
end;

function TPasResolver.FindElementWithoutParams(const AName: String;
  ErrorPosEl: TPasElement): TPasElement;
var
  Data: TPRFindData;
begin
  Result:=FindElementWithoutParams(AName,Data,ErrorPosEl);
  if (Data.StartScope<>nil) and (Data.StartScope.ClassType=TPasWithExprScope)
      and TPasWithExprScope(Data.StartScope).NeedTmpVar then
    RaiseInternalError(20160923111727); // caller forgot to handle "With", use the other FindElementWithoutParams instead
end;

function TPasResolver.FindElementWithoutParams(const AName: String; out
  Data: TPRFindData; ErrorPosEl: TPasElement): TPasElement;
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
    RaiseIdentifierNotFound(AName,ErrorPosEl);
    end;
  if (Result is TPasProcedure) and (TPasProcedure(Result).ProcType.Args.Count>0)
    and (TPasArgument(TPasProcedure(Result).ProcType.Args[0]).ValueExpr=nil)
  then
    // identifier needs parameters
    RaiseMsg(nWrongNumberOfParametersForCallTo,
      sWrongNumberOfParametersForCallTo,[GetProcDesc(TPasProcedure(Result))],ErrorPosEl);

  CheckFoundElOnStartScope(Data,nil);
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

procedure TPasResolver.CheckFoundElOnStartScope(
  const FindData: TPRFindData; Ref: TResolvedReference);
begin
  //writeln('TPasResolver.CheckFoundElOnStartScope StartScope=',FindData.StartScope.ClassName,' ',FindData.StartScope is TPasDotIdentifierScope,' ',(FindData.StartScope is TPasDotIdentifierScope)
  //    and TPasDotIdentifierScope(FindData.StartScope).OnlyTypeMembers,
  //    ' FindData.Found=',GetObjName(FindData.Found));
  if (FindData.StartScope is TPasDotIdentifierScope)
      and TPasDotIdentifierScope(FindData.StartScope).OnlyTypeMembers then
    begin
    //writeln('TPasResolver.CheckFoundElOnStartScope ',GetObjName(FindData.Found),' ',(FindData.Found is TPasVariable)
    //    and (vmClass in TPasVariable(FindData.Found).VarModifiers));
    // only class vars/procs allowed
    if (FindData.Found.ClassType=TPasConstructor) then
      // constructor: ok
    else if (FindData.Found.ClassType=TPasClassConstructor)
        or (FindData.Found.ClassType=TPasClassDestructor)
        or (FindData.Found.ClassType=TPasClassProcedure)
        or (FindData.Found.ClassType=TPasClassFunction)
        or (FindData.Found.ClassType=TPasClassOperator)
    then
      // class proc: ok
    else if (FindData.Found is TPasVariable)
        and (vmClass in TPasVariable(FindData.Found).VarModifiers) then
      // class var/const/property: ok
    else
      RaiseMsg(nOnlyClassMembersCanBeReferredWithClassReferences,
        sOnlyClassMembersCanBeReferredWithClassReferences,[],FindData.ErrorPosEl);
    end;

  if (FindData.Found is TPasProcedure)
      and (TPasProcedure(FindData.Found).IsVirtual
        or TPasProcedure(FindData.Found).IsOverride) then
    begin
      if (FindData.StartScope.ClassType=TPasDotClassScope)
      and TPasDotClassScope(FindData.StartScope).InheritedExpr then
        begin
        // call directly
        if TPasProcedure(FindData.Found).IsAbstract then
          RaiseMsg(nAbstractMethodsCannotBeCalledDirectly,
            sAbstractMethodsCannotBeCalledDirectly,[],FindData.ErrorPosEl);
        end
      else
        begin
        // call via method table
        if Ref<>nil then
          Ref.Flags:=Ref.Flags+[rrfVMT];
        end;
    end;
  if (FindData.Found.ClassType=TPasConstructor)
      and (FindData.StartScope.ClassType=TPasDotClassScope)
      and TPasDotClassScope(FindData.StartScope).OnlyTypeMembers
      and (Ref<>nil) then
    Ref.Flags:=Ref.Flags+[rrfNewInstance];
end;

procedure TPasResolver.FinishScope(ScopeType: TPasScopeType; El: TPasElement);
begin
  case ScopeType of
  stModule: FinishModule(El as TPasModule);
  stUsesList: FinishUsesList;
  stTypeSection: FinishTypeSection(El as TPasDeclarations);
  stTypeDef: FinishTypeDef(El as TPasType);
  stConstDef: FinishConstDef(El as TPasConst);
  stProcedure: FinishProcedure;
  stProcedureHeader: FinishProcedureHeader(El as TPasProcedureType);
  stExceptOnExpr: FinishExceptOnExpr;
  stExceptOnStatement: FinishExceptOnStatement;
  stDeclaration: FinishDeclaration(El);
  stAncestors: FinishAncestors(El as TPasClassType);
  else
    RaiseMsg(nNotYetImplemented,sNotYetImplemented+' FinishScope',[IntToStr(ord(ScopeType))],nil);
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
  Clear;
  PopScope; // free default scope
  FreeAndNil(FPendingForwards);
  inherited Destroy;
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
  BaseTypes: TResolveBaseTypes; BaseProcs: TResolverBuiltInProcs);
var
  bt: TResolverBaseType;
begin
  for bt in BaseTypes do
    AddBaseType(BaseTypeNames[bt],bt);
  if bfLength in BaseProcs then
    AddBuiltInProc('Length','function Length(const String or Array): sizeint',
        @OnGetCallCompatibility_Length,@OnGetCallResult_Length);
  if bfSetLength in BaseProcs then
    AddBuiltInProc('SetLength','procedure SetLength(var String or Array; NewLength: sizeint)',
        @OnGetCallCompatibility_SetLength,nil);
  if bfInclude in BaseProcs then
    AddBuiltInProc('Include','procedure Include(var Set of Enum; const Enum)',
        @OnGetCallCompatibility_InExclude,nil);
  if bfExclude in BaseProcs then
    AddBuiltInProc('Exclude','procedure Exclude(var Set of Enum; const Enum)',
        @OnGetCallCompatibility_InExclude,nil);
  if bfOrd in BaseProcs then
    AddBuiltInProc('Ord','function Ord(const Enum or Char): integer',
        @OnGetCallCompatibility_Ord,@OnGetCallResult_Ord);
  if bfExit in BaseProcs then
    AddBuiltInProc('Exit','procedure Exit(result)',
        @OnGetCallCompatibility_Exit,nil);
  if bfInc in BaseProcs then
    AddBuiltInProc('Inc','procedure Inc(var Integer; const Incr: Integer = 1)',
        @OnGetCallCompatibility_IncDec,nil);
  if bfDec in BaseProcs then
    AddBuiltInProc('Dec','procedure Dec(var Integer; const Decr: Integer = 1)',
        @OnGetCallCompatibility_IncDec,nil);
  if bfAssigned in BaseProcs then
    AddBuiltInProc('Assigned','function Assigned(const Pointer or Class or Class-of): boolean',
        @OnGetCallCompatibility_Assigned,@OnGetCallResult_Assigned);
end;

function TPasResolver.AddBaseType(aName: shortstring; Typ: TResolverBaseType
  ): TResElDataBaseType;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef.Create(aName,nil);
  if Typ<>btNone then
    FBaseTypes[Typ]:=El;
  Result:=TResElDataBaseType.Create;
  Result.BaseType:=Typ;
  AddResolveData(El,Result,lkBuiltIn);
  FDefaultScope.AddIdentifier(aName,El,pikBaseType);
end;

function TPasResolver.IsBaseType(aType: TPasType; BaseType: TResolverBaseType
  ): boolean;
begin
  Result:=false;
  if aType=nil then exit;
  if aType.ClassType<>TPasUnresolvedSymbolRef then exit;
  Result:=CompareText(aType.Name,BaseTypeNames[BaseType])=0;
end;

function TPasResolver.AddBuiltInProc(aName: shortstring; Signature: string;
  const GetCallCompatibility: TOnGetCallCompatibility;
  const GetCallResult: TOnGetCallResult): TResElDataBuiltInProc;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef.Create(aName,nil);
  Result:=TResElDataBuiltInProc.Create;
  Result.Proc:=El;
  Result.Signature:=Signature;
  Result.GetCallCompatibility:=GetCallCompatibility;
  Result.GetCallResult:=GetCallResult;
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
  FindData: PPRFindData): TResolvedReference;

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
  if FindData<>nil then
    begin
    if FindData^.StartScope.ClassType=TPasWithExprScope then
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

procedure TPasResolver.SetLastMsg(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const; Element: TPasElement);
{$IFDEF VerbosePasResolver}
var
  s: string;
{$ENDIF}
begin
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := Format(Fmt,Args);
  FLastElement := Element;
  CreateMsgArgs(FLastMsgArgs,Args);
  {$IFDEF VerbosePasResolver}
  write('TPasResolver.SetLastMsg ',GetElementSourcePosStr(Element),' ');
  s:='';
  str(MsgType,s);
  write(s);
  writeln(': [',MsgNumber,'] ',FLastMsg);
  {$ENDIF}
end;

procedure TPasResolver.RaiseMsg(MsgNumber: integer; const Fmt: String;
  Args: array of const; ErrorPosEl: TPasElement);
var
  E: EPasResolve;
begin
  SetLastMsg(mtError,MsgNumber,Fmt,Args,ErrorPosEl);
  E:=EPasResolve.Create(FLastMsg);
  E.PasElement:=ErrorPosEl;
  E.MsgNumber:=MsgNumber;
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
  RaiseMsg(nNotYetImplemented,s,[GetObjName(El)],El);
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

procedure TPasResolver.RaiseIdentifierNotFound(Identifier: string;
  El: TPasElement);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseIdentifierNotFound START "',Identifier,'"');
  WriteScopes;
  {$ENDIF}
  RaiseMsg(nIdentifierNotFound,sIdentifierNotFound,[Identifier],El);
end;

procedure TPasResolver.RaiseXExpectedButYFound(X, Y: string; El: TPasElement);
begin
  RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,[X,Y],El);
end;

procedure TPasResolver.LogMsg(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const; PosEl: TPasElement);
begin
  SetLastMsg(MsgType,MsgNumber,Fmt,Args,PosEl);
  if Assigned(CurrentParser.OnLog) then
    CurrentParser.OnLog(Self,Format(Fmt,Args));
end;

function TPasResolver.CheckCallProcCompatibility(Proc: TPasProcedure;
  Params: TParamsExpr; RaiseOnError: boolean): integer;
var
  ProcArgs: TFPList;
  i, ParamCnt, ParamCompatibility: Integer;
  Param: TPasExpr;
begin
  Result:=cExact;
  ProcArgs:=Proc.ProcType.Args;
  // check args
  ParamCnt:=length(Params.Params);
  i:=0;
  while i<ParamCnt do
    begin
    Param:=Params.Params[i];
    if i>=ProcArgs.Count then
      begin
      // too many arguments
      if RaiseOnError then
        RaiseMsg(nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[GetProcDesc(Proc)],Param);
      exit(cIncompatible);
      end;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckCallProcCompatibility ',i,'/',ParamCnt);
    {$ENDIF}
    ParamCompatibility:=CheckParamCompatibility(Param,TPasArgument(ProcArgs[i]),i,RaiseOnError);
    if ParamCompatibility=cIncompatible then
      exit(cIncompatible);
    inc(Result,ParamCompatibility);
    inc(i);
    end;
  if (i<ProcArgs.Count) and (TPasArgument(ProcArgs[i]).ValueExpr=nil) then
    begin
    // not enough arguments
    if RaiseOnError then
      // ToDo: position cursor on identifier
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[GetProcDesc(Proc)],Params.Value);
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
    RaiseMsg(nWrongNumberOfParametersForCallTo,sWrongNumberOfParametersForCallTo,
      [PropEl.Name],Params)
    end
  else if PropEl.Args.Count>length(Params.Params) then
    begin
    if not RaiseOnError then exit(cIncompatible);
    RaiseMsg(nMissingParameterX,sMissingParameterX,
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
      RaiseMsg(nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
    Param:=Params.Params[ArgNo];
    ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
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
      // dynamic array -> needs exactly one integer
      GetNextParam;
      if not (ParamResolved.BaseType in btAllInteger) then
        begin
        if not RaiseOnError then exit(cIncompatible);
        RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),BaseTypeNames[ParamResolved.BaseType],BaseTypeNames[btLongint]],
          Param);
        end;
      if not (rrfReadable in ParamResolved.Flags) then
        begin
        if not RaiseOnError then exit(cIncompatible);
        RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
          [IntToStr(ArgNo),GetResolverResultDescription(ParamResolved),BaseTypeNames[btLongint]],
          Param);
        end;
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
          RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),GetResolverResultDescription(ParamResolved),
             GetResolverResultDescription(RangeResolved)],
            Param);
          end;
        if (bt in btAllBooleans) and (ParamResolved.BaseType in btAllBooleans) then
        else if (bt in btAllInteger) and (ParamResolved.BaseType in btAllInteger) then
        else if (bt in [btChar,btWideChar]) and (ParamResolved.BaseType in [btChar,btWideChar]) then
        else
          begin
          if not RaiseOnError then exit(cIncompatible);
          RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
            [IntToStr(ArgNo),GetResolverResultDescription(ParamResolved),
             GetResolverResultDescription(RangeResolved)],
            Param);
          end;
        end;
      end;
    if ArgNo=length(Params.Params) then exit(cExact);

    // there are more parameters -> continue in sub array
    NextType:=ResolveAliasType(ArrayEl.ElType);
    if NextType.ClassType<>TPasArrayType then
      RaiseMsg(nWrongNumberOfParametersForArray,sWrongNumberOfParametersForArray,
        [],Params);
    ArrayEl:=TPasArrayType(NextType);
  until false;
end;

function TPasResolver.CheckOverloadProcCompatibility(Proc1, Proc2: TPasProcedure
  ): boolean;
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
    if not CheckProcArgCompatibility(Proc1,Proc2,i) then
      exit;
    end;
  Result:=true;
end;

function TPasResolver.CheckProcArgCompatibility(Proc1, Proc2: TPasProcedure;
  ArgNo: integer): boolean;
var
  Arg1, Arg2: TPasArgument;
  Arg1Resolved, Arg2Resolved: TPasResolverResult;
begin
  Result:=false;
  Arg1:=TPasArgument(Proc1.ProcType.Args[ArgNo]);
  Arg2:=TPasArgument(Proc2.ProcType.Args[ArgNo]);

  // check access: var, const, ...
  if Arg1.Access<>Arg2.Access then exit;

  // check untyped
  if Arg1.ArgType=nil then
    exit(Arg2.ArgType=nil);
  if Arg2.ArgType=nil then exit;

  ComputeElement(Arg1,Arg1Resolved,[]);
  ComputeElement(Arg2,Arg2Resolved,[]);

  if (Arg1Resolved.BaseType<>Arg2Resolved.BaseType)
      or (Arg1Resolved.TypeEl=nil)
      or (Arg1Resolved.TypeEl<>Arg2Resolved.TypeEl) then
    exit;

  // ToDo: check Arg1.ValueExpr
  Result:=true;
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
      if (ResolvedEl.TypeEl<>nil) and (ResolvedEl.ExprEl<>nil) then
        RaiseXExpectedButYFound('identifier',ResolvedEl.TypeEl.ElementTypeName,ResolvedEl.ExprEl)
      else
        RaiseMsg(nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
      end;
    exit;
    end;
  if (rrfWritable in ResolvedEl.Flags) then
    exit(true);
  // not writable
  if not ErrorOnFalse then exit;
  if ResolvedEl.IdentEl is TPasProperty then
    RaiseMsg(nPropertyNotWritable,sPropertyNotWritable,[],ErrorEl)
  else
    RaiseMsg(nVariableIdentifierExpected,sVariableIdentifierExpected,[],ErrorEl);
end;

function TPasResolver.CheckAssignCompatibility(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement): integer;
begin
  // check if the RHS can be converted to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckAssignCompatibility ');
  {$ENDIF}
  if LHS.TypeEl=nil then
    begin
    // ToDo: untyped parameter
    RaiseNotYetImplemented(20160922163631,LHS.IdentEl);
    end
  else if LHS.BaseType=RHS.BaseType then
    begin
    if LHS.BaseType=btContext then
      exit(CheckAssignCompatibilityCustomType(RHS,LHS,ErrorEl,false))
    else
      exit(cExact); // same base type, maybe not same type name (e.g. longint and integer)
    end
  else if (LHS.BaseType in btAllInteger)
      and (RHS.BaseType in btAllInteger) then
    exit(cExact+1) // ToDo: range check for Expr
  else if (LHS.BaseType in btAllBooleans)
      and (RHS.BaseType in btAllBooleans) then
    exit(cExact+1)
  else if (LHS.BaseType in btAllStringAndChars)
      and (RHS.BaseType in btAllStringAndChars) then
    exit(cExact+1)
  else if (LHS.BaseType in btAllFloats)
      and (RHS.BaseType in btAllFloats+btAllInteger) then
    exit(cExact+1)
  else if RHS.BaseType=btNil then
    begin
      if LHS.BaseType=btPointer then
        exit(cExact)
      else if LHS.BaseType=btContext then
        begin
        if (LHS.TypeEl.ClassType=TPasClassType)
            or (LHS.TypeEl.ClassType=TPasClassOfType)
            or (LHS.TypeEl.ClassType=TPasPointerType) then
          exit(cExact);
        end
      else
        exit(cIncompatible);
    end
  else if RHS.BaseType=btSet then
    begin
    if (LHS.BaseType=btSet) then
      begin
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
      exit(cIncompatible);
      end;
    end
  else
    exit(cIncompatible);
  RaiseNotYetImplemented(20160922163634,ErrorEl,'LHS='+GetResolverResultDesc(LHS)+' RHS='+GetResolverResultDesc(RHS));
end;

function TPasResolver.CheckEqualCompatibility(const LHS,
  RHS: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
begin
  Result:=cIncompatible;
  // check if the RHS is type compatible to LHS
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckEqualCompatibility ');
  {$ENDIF}
  if LHS.BaseType=RHS.BaseType then
    begin
    if LHS.BaseType=btContext then
      exit(CheckEqualCompatibilityCustomType(LHS,RHS,ErrorEl,RaiseOnIncompatible))
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
        if (RHS.TypeEl.ClassType=TPasClassType)
            or (RHS.TypeEl.ClassType=TPasClassOfType)
            or (RHS.TypeEl.ClassType=TPasPointerType) then
          exit(cExact);
        end
      else if RaiseOnIncompatible then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[RHS.BaseType],BaseTypeNames[LHS.BaseType]],ErrorEl)
      else
        exit(cIncompatible);
    end
  else if RHS.BaseType=btNil then
    begin
      if LHS.BaseType=btPointer then
        exit(cExact)
      else if LHS.BaseType=btContext then
        begin
        if (LHS.TypeEl.ClassType=TPasClassType)
            or (LHS.TypeEl.ClassType=TPasClassOfType)
            or (LHS.TypeEl.ClassType=TPasPointerType) then
          exit(cExact);
        end
      else if RaiseOnIncompatible then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [BaseTypeNames[LHS.BaseType],BaseTypeNames[RHS.BaseType]],ErrorEl)
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
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['set of '+BaseTypeNames[LHS.SubType],'set of '+BaseTypeNames[RHS.SubType]],ErrorEl)
      else
        exit(cIncompatible);
      end;
    end
  else if RaiseOnIncompatible then
    RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
      [BaseTypeNames[LHS.BaseType],BaseTypeNames[RHS.BaseType]],ErrorEl)
  else
    exit(cIncompatible);
  RaiseNotYetImplemented(20161007101041,ErrorEl,'LHS='+GetResolverResultDesc(LHS)+' RHS='+GetResolverResultDesc(RHS));
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
  if (ResolvedEl.IdentEl.ClassType=TPasConst) then
    begin
    // typed const are writable
    Result:=(TPasConst(ResolvedEl.IdentEl).VarType<>nil);
    exit;
    end;
  if ResolvedEl.IdentEl.ClassType=TPasResultElement then
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
var
  MustFitExactly: Boolean;
  ComputeFlags: TPasResolverComputeFlags;
begin
  Result:=cIncompatible;

  ComputeFlags:=[];
  MustFitExactly:=Param.Access in [argVar, argOut];

  ComputeElement(Expr,ExprResolved,ComputeFlags);
  if ExprResolved.BaseType=btProc then
    ComputeFuncCallWithoutParams(ExprResolved,Expr);

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Expr=',GetTreeDesc(Expr,2),' ResolvedExpr=',GetResolverResultDesc(ExprResolved));
  {$ENDIF}

  if MustFitExactly then
    begin
    // Expr must be a variable
    if not ResolvedElCanBeVarParam(ExprResolved) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.CheckParamCompatibility MustFitExactly: Identifier=',GetObjName(ExprResolved.IdentEl),' Type=',GetObjName(ExprResolved.TypeEl),' Expr=',GetObjName(ExprResolved.ExprEl),' Flags=',ResolverResultFlagsToStr(ExprResolved.Flags));
      {$ENDIF}
      if RaiseOnError then
        RaiseMsg(nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
      exit;
      end;
    end;

  ComputeElement(Param,ParamResolved,ComputeFlags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Param=',GetTreeDesc(Param,2),' ResolvedParam=',GetResolverResultDesc(ParamResolved));
  {$ENDIF}
  if (ParamResolved.TypeEl=nil) and (Param.ArgType<>nil) then
    RaiseInternalError(20160922163628,'GetResolvedType returned TypeEl=nil for '+GetTreeDesc(Param));

  if MustFitExactly then
    begin
    if (ParamResolved.BaseType=ExprResolved.BaseType) then
      begin
      if (ParamResolved.TypeEl<>nil) and (ParamResolved.TypeEl=ExprResolved.TypeEl) then
        exit(cExact);
      end;
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNoVarParamMustMatchExactly,
        sIncompatibleTypeArgNoVarParamMustMatchExactly,
        [IntToStr(ParamNo+1),GetTypeDesc(ExprResolved.TypeEl),GetTypeDesc(ParamResolved.TypeEl)],
        Expr);
    exit(cIncompatible);
    end;

  Result:=CheckAssignCompatibility(ParamResolved,ExprResolved,Expr);
  if (Result=cIncompatible) and RaiseOnError then
    RaiseMsg(nIncompatibleTypeArgNo,sIncompatibleTypeArgNo,
      [IntToStr(ParamNo+1),GetTypeDesc(ExprResolved.TypeEl),GetTypeDesc(ParamResolved.TypeEl)],
      Expr);
end;

function TPasResolver.CheckAssignCompatibilityCustomType(const SrcType,
  DestType: TPasResolverResult; ErrorEl: TPasElement;
  RaiseOnIncompatible: boolean): integer;
var
  SrcTypeEl, DstTypeEl: TPasType;
  SrcResolved, DstResolved, DstClassType, SrcClassType: TPasResolverResult;

  function RaiseIncompatibleType: integer;
  begin
    if not RaiseOnIncompatible then exit(cIncompatible);
    RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [SrcTypeEl.ElementTypeName,DstTypeEl.ElementTypeName],ErrorEl);
  end;

begin
  if (SrcType.TypeEl=nil) then
    RaiseInternalError(20160922163645);
  if (DestType.TypeEl=nil) then
    RaiseInternalError(20160922163648);
  SrcTypeEl:=SrcType.TypeEl;
  DstTypeEl:=DestType.TypeEl;
  if DstTypeEl=SrcTypeEl then
    exit(cExact);

  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.CheckCustomTypeCompatibility SrcTypeEl=',GetObjName(SrcTypeEl),' DstTypeEl=',GetObjName(DstTypeEl));
  {$ENDIF}
  if DstTypeEl.ClassType=TPasClassType then
    begin
    if SrcType.BaseType=btNil then
      exit(cExact)
    else if SrcTypeEl.ClassType=TPasClassType then
      begin
      Result:=CheckSrcIsADstType(SrcType,DestType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [SrcTypeEl.Name,DstTypeEl.Name],ErrorEl);
      exit;
      end
    else
      exit(RaiseIncompatibleType);
    end
  else if DstTypeEl.ClassType=TPasClassOfType then
    begin
    if SrcType.BaseType=btNil then
      exit(cExact);
    SetResolverTypeExpr(DstClassType,btContext,TPasClassOfType(DstTypeEl).DestType,[]);
    if (SrcTypeEl.ClassType=TPasClassOfType) then
      begin
      SetResolverTypeExpr(SrcClassType,btContext,TPasClassOfType(SrcTypeEl).DestType,[]);
      // e.g. ImageClass:=AnotherImageClass;
      Result:=CheckSrcIsADstType(SrcClassType,DstClassType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          ['class of '+TPasClassOfType(SrcTypeEl).DestType.Name,'class of '+TPasClassOfType(DstTypeEl).DestType.Name],ErrorEl);
      exit;
      end
    else if (SrcType.IdentEl is TPasClassType) then
      begin
      // e.g. ImageClass:=TFPMemoryImage;
      Result:=CheckSrcIsADstType(SrcType,DstClassType,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [SrcTypeEl.Name,'class of '+TPasClassOfType(DstTypeEl).DestType.Name],ErrorEl);
      exit;
      end;
    end
  else if SrcTypeEl.ClassType=TPasEnumType then
    begin
    // enums of different type
    if not RaiseOnIncompatible then
      exit(cIncompatible);
    if DstTypeEl.ClassType=TPasEnumValue then
      RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [SrcTypeEl.Name,DstTypeEl.Name],ErrorEl)
    else
      exit(RaiseIncompatibleType);
    end
  else if SrcTypeEl.ClassType=TPasSetType then
    begin
    if DstTypeEl.ClassType=TPasSetType then
      begin
      ComputeElement(TPasSetType(SrcTypeEl).EnumType,SrcResolved,[]);
      ComputeElement(TPasSetType(DstTypeEl).EnumType,DstResolved,[]);
      if (SrcResolved.TypeEl<>nil)
      and (SrcResolved.TypeEl=DstResolved.TypeEl) then
        exit(cExact);
      if (SrcResolved.TypeEl.CustomData is TResElDataBaseType)
          and (DstResolved.TypeEl.CustomData is TResElDataBaseType)
          and (SrcResolved.BaseType=DstResolved.BaseType) then
        exit(cExact);
      if RaiseOnIncompatible then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [GetResolverResultDescription(SrcResolved),
           GetResolverResultDescription(DstResolved)],ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(RaiseIncompatibleType);
    end
  else
    RaiseNotYetImplemented(20160922163654,ErrorEl);
end;

function TPasResolver.CheckEqualCompatibilityCustomType(const TypeA,
  TypeB: TPasResolverResult; ErrorEl: TPasElement; RaiseOnIncompatible: boolean
  ): integer;
var
  ElA, ElB: TPasType;
  AResolved, BResolved, ClassTypeA, ClassTypeB: TPasResolverResult;

  function IncompatibleElements: integer;
  begin
    Result:=cIncompatible;
    if RaiseOnIncompatible then
      RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [ElA.ElementTypeName,ElB.ElementTypeName],ErrorEl)
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
        SetResolverTypeExpr(ClassTypeB,btContext,TPasClassOfType(ElB).DestType,[]);
        Result:=CheckSrcIsADstType(TypeA,ClassTypeB,ErrorEl);
        if (Result=cIncompatible) and RaiseOnIncompatible then
          RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
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
        RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
      exit;
      end;
    exit(IncompatibleElements);
    end
  else if ElA.ClassType=TPasClassOfType then
    begin
    SetResolverTypeExpr(ClassTypeA,btContext,TPasClassOfType(ElA).DestType,[]);
    if ElB.ClassType=TPasClassOfType then
      begin
      // for example: if ImageClass=ImageClass then
      SetResolverTypeExpr(ClassTypeB,btContext,TPasClassOfType(ElB).DestType,[]);
      Result:=CheckSrcIsADstType(ClassTypeA,ClassTypeB,ErrorEl);
      if Result=cIncompatible then
        Result:=CheckSrcIsADstType(ClassTypeB,ClassTypeA,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
      exit;
      end
    else if TypeB.IdentEl is TPasClassType then
      begin
      // for example: if ImageClass=TFPMemoryImage then
      Result:=CheckSrcIsADstType(TypeB,ClassTypeA,ErrorEl);
      if (Result=cIncompatible) and RaiseOnIncompatible then
        RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],ErrorEl);
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
      RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
        [ElA.Name,ElB.Name],ErrorEl)
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
          and (AResolved.BaseType=BResolved.BaseType) then
        exit(cExact);
      if RaiseOnIncompatible then
        RaiseMsg(nIncompatibleTypesGotExpected,sIncompatibleTypesGotExpected,
          [GetResolverResultDescription(AResolved),
           GetResolverResultDescription(BResolved)],ErrorEl)
      else
        exit(cIncompatible);
      end
    else
      exit(IncompatibleElements);
    end
  else
    RaiseNotYetImplemented(20161007102250,ErrorEl);
end;

function TPasResolver.CheckTypeCast(El: TPasType; Params: TParamsExpr;
  RaiseOnError: boolean): integer;
// for example  if TClassA(AnObject)=nil then ;
var
  Param: TPasExpr;
  ParamResolved, ResolvedEl: TPasResolverResult;
  Data: TResElDataBaseType;
begin
  if length(Params.Params)<1 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForTypeCast,
        sWrongNumberOfParametersForTypeCast,[El.Name],Params);
    exit(cIncompatible);
    end;

  Param:=Params.Params[0];
  ComputeElement(Param,ParamResolved,[rcReturnFuncResult]);
  Result:=cIncompatible;
  ComputeElement(El,ResolvedEl,[]);

  if (ResolvedEl.TypeEl<>nil)
      and (rrfReadable in ParamResolved.Flags) then
    begin
    if ResolvedEl.TypeEl.ClassType=TPasUnresolvedSymbolRef then
      begin
      if ResolvedEl.TypeEl.CustomData.ClassType=TResElDataBaseType then
        begin
        // base type cast, e.g. double(aninteger)
        Data:=ResolvedEl.TypeEl.CustomData as TResElDataBaseType;
        if Data.BaseType=ParamResolved.BaseType then
          Result:=cExact
        else if Data.BaseType in btAllInteger then
          begin
          if ParamResolved.BaseType in btAllInteger then
            Result:=cExact+1;
          end
        else if Data.BaseType in btAllFloats then
          begin
          if ParamResolved.BaseType in (btAllInteger+btAllFloats) then
            Result:=cExact+1;
          end
        else if Data.BaseType in btAllBooleans then
          begin
          if ParamResolved.BaseType in (btAllBooleans+btAllInteger) then
            Result:=cExact+1;
          end
        else if Data.BaseType in btAllStrings then
          begin
          if ParamResolved.BaseType in btAllStringAndChars then
            Result:=cExact+1;
          end;
        end;
      end
    else if ResolvedEl.TypeEl.ClassType=TPasClassType then
      begin
      if ParamResolved.BaseType=btNil then
        Result:=cExact
      else if (ParamResolved.BaseType=btContext)
          and (ParamResolved.TypeEl.ClassType=TPasClassType)
          and (not (ParamResolved.IdentEl is TPasType)) then
        begin
        Result:=CheckSrcIsADstType(ResolvedEl,ParamResolved,Param);
        if Result=cIncompatible then
          Result:=CheckSrcIsADstType(ParamResolved,ResolvedEl,Param);
        end;
      end;
    end;

  if Result=cIncompatible then
    begin
    if RaiseOnError then
      RaiseMsg(nIllegalTypeConversionTo,sIllegalTypeConversionTo,
        [GetTypeDesc(ParamResolved.TypeEl),El.Name],Param);
    exit;
    end;

  if length(Params.Params)>1 then
    begin
    if RaiseOnError then
      RaiseMsg(nWrongNumberOfParametersForTypeCast,
        sWrongNumberOfParametersForTypeCast,[El.Name],Params);
    exit(cIncompatible);
    end;
end;

procedure TPasResolver.ComputeElement(El: TPasElement; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);

  procedure RaiseConstantExprExp(ErrorEl: TPasElement);
  begin
    RaiseMsg(nConstantExpressionExpected,sConstantExpressionExpected,[],ErrorEl);
  end;

var
  DeclEl: TPasElement;
  aClass: TPasClassType;
begin
  ResolvedEl:=Default(TPasResolverResult);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ComputeElement El=',GetObjName(El),' SkipTypeAlias=',rcSkipTypeAlias in Flags);
  {$ENDIF}
  if El=nil then
    exit;
  if El.ClassType=TPrimitiveExpr then
    begin
    case TPrimitiveExpr(El).Kind of
      pekIdent,pekSelf:
        begin
        if not (El.CustomData is TResolvedReference) then
          RaiseNotYetImplemented(20160922163658,El,'Value="'+TPrimitiveExpr(El).Value+'" CustomData='+GetObjName(El.CustomData)+' '+GetElementSourcePosStr(El));
        ComputeElement(TResolvedReference(El.CustomData).Declaration,ResolvedEl,Flags-[rcReturnFuncResult]);
        if (ResolvedEl.BaseType=btProc) and (rcReturnFuncResult in Flags) then
          begin
          if rcConstant in Flags then
            RaiseConstantExprExp(El);
          if ResolvedEl.IdentEl is TPasFunction then
            // function => return result
            ComputeElement(TPasFunction(ResolvedEl.IdentEl).FuncType.ResultEl,ResolvedEl,Flags-[rcReturnFuncResult])
          else if ResolvedEl.IdentEl.ClassType=TPasConstructor then
            begin
            // constructor -> return value of type class
            aClass:=ResolvedEl.IdentEl.Parent as TPasClassType;
            SetResolverValueExpr(ResolvedEl,btContext,aClass,TPrimitiveExpr(El),[rrfReadable]);
            end;
          end;
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
  else if El.ClassType=TPasUnresolvedSymbolRef then
    begin
    // built-in type
    if El.CustomData is TResElDataBaseType then
      SetResolverIdentifier(ResolvedEl,TResElDataBaseType(El.CustomData).BaseType,
        El,TPasUnresolvedSymbolRef(El),[])
    else if El.CustomData is TResElDataBuiltInProc then
      RaiseInternalError(20161003174747) // should have been computed in El.ClassType=TParamsExpr
    else
      RaiseNotYetImplemented(20160926194756,El);
    end
  else if El.ClassType=TBinaryExpr then
    ComputeBinaryExpr(TBinaryExpr(El),ResolvedEl,Flags)
  else if El.ClassType=TPasAliasType then
    begin
    // e.g. 'type a = b' -> compute b
    ComputeElement(TPasAliasType(El).DestType,ResolvedEl,Flags-[rcReturnFuncResult]);
    ResolvedEl.IdentEl:=El;
    end
  else if (El.ClassType=TPasTypeAliasType) then
    begin
    // e.g. 'type a = type b;' -> compute b
    ComputeElement(TPasTypeAliasType(El).DestType,ResolvedEl,Flags-[rcReturnFuncResult]);
    if not (rcSkipTypeAlias in Flags) then
      ResolvedEl.IdentEl:=El;
    end
  else if (El.ClassType=TPasVariable) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if rcConstant in Flags then
      RaiseConstantExprExp(El);
    ComputeElement(TPasVariable(El).VarType,ResolvedEl,Flags-[rcReturnFuncResult]);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[rrfReadable,rrfWritable];
    end
  else if (El.ClassType=TPasConst) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    if TPasConst(El).VarType<>nil then
      begin
      // typed const -> just like a var
      if rcConstant in Flags then
        RaiseConstantExprExp(El);
      ComputeElement(TPasConst(El).VarType,ResolvedEl,Flags-[rcReturnFuncResult]);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[rrfReadable,rrfWritable];
      end
    else
      begin
      // untyped const
      ComputeElement(TPasConst(El).Expr,ResolvedEl,Flags-[rcReturnFuncResult]);
      ResolvedEl.IdentEl:=El;
      ResolvedEl.Flags:=[rrfReadable];
      end;
    end
  else if (El.ClassType=TPasEnumValue) then
    SetResolverIdentifier(ResolvedEl,btContext,El,El.Parent as TPasEnumType,[rrfReadable])
  else if (El.ClassType=TPasEnumType) then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasEnumType(El),[rrfReadable])
  else if (El.ClassType=TPasProperty) then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(El);
    ComputeElement(GetPasPropertyType(TPasProperty(El)),ResolvedEl,Flags-[rcReturnFuncResult]);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[];
    if GetPasPropertyGetter(TPasProperty(El))<>nil then
      Include(ResolvedEl.Flags,rrfReadable);
    if GetPasPropertySetter(TPasProperty(El))<>nil then
      Include(ResolvedEl.Flags,rrfWritable);
    end
  else if El.ClassType=TPasArgument then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(El);
    if TPasArgument(El).ArgType=nil then
      // untyped parameter
      SetResolverIdentifier(ResolvedEl,btUntyped,El,nil,[])
    else
      begin
      // typed parameter -> use param as IdentEl, compute type
      ComputeElement(TPasArgument(El).ArgType,ResolvedEl,Flags-[rcReturnFuncResult]);
      ResolvedEl.IdentEl:=El;
      end;
    ResolvedEl.Flags:=[rrfReadable];
    if TPasArgument(El).Access in [argDefault, argVar, argOut] then
      Include(ResolvedEl.Flags,rrfWritable);
    end
  else if El.ClassType=TPasClassType then
    begin
    if TPasClassType(El).IsForward then
      begin
      DeclEl:=(TPasClassType(El).CustomData as TResolvedReference).Declaration;
      ResolvedEl.TypeEl:=DeclEl as TPasClassType;
      end
    else
      ResolvedEl.TypeEl:=TPasClassType(El);
    SetResolverIdentifier(ResolvedEl,btContext,
                          ResolvedEl.TypeEl,ResolvedEl.TypeEl,[rrfReadable]);
    // Note: rrfReadable because a class has a vmt as value
    end
  else if El.ClassType=TPasClassOfType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasClassOfType(El),[])
  else if El.ClassType=TPasRecordType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasRecordType(El),[])
  else if El.ClassType=TPasRangeType then
    begin
    ComputeElement(TPasRangeType(El).RangeExpr,ResolvedEl,[]);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[];
    end
  else if El.ClassType=TPasSetType then
    begin
    ComputeElement(TPasSetType(El).EnumType,ResolvedEl,[]);
    if ResolvedEl.BaseType=btRange then
      ConvertRangeToFirstValue(ResolvedEl);
    ResolvedEl.SubType:=ResolvedEl.BaseType;
    ResolvedEl.BaseType:=btSet;
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[];
    end
  else if El.ClassType=TUnaryExpr then
    begin
    ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ComputeElement Unary Kind=',TUnaryExpr(El).Kind,' OpCode=',TUnaryExpr(El).OpCode,' OperandResolved=',GetResolverResultDesc(ResolvedEl),' ',GetElementSourcePosStr(El));
    {$ENDIF}
    case TUnaryExpr(El).OpCode of
      eopAdd, eopSubtract:
        if ResolvedEl.BaseType in (btAllInteger+btAllFloats) then
          exit
        else
          RaiseMsg(nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TUnaryExpr(El).OpCode]],El);
      eopNot:
        if ResolvedEl.BaseType in (btAllInteger+btAllBooleans) then
          exit
        else
          RaiseMsg(nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[TUnaryExpr(El).OpCode]],El);
    end;
    RaiseNotYetImplemented(20160926142426,El);
    end
  else if El.ClassType=TPasResultElement then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(El);
    ComputeElement(TPasResultElement(El).ResultType,ResolvedEl,Flags-[rcReturnFuncResult]);
    ResolvedEl.IdentEl:=El;
    ResolvedEl.Flags:=[rrfReadable,rrfWritable];
    end
  else if El is TPasModule then
    SetResolverIdentifier(ResolvedEl,btModule,El,nil,[])
  else if El.ClassType=TNilExpr then
    SetResolverValueExpr(ResolvedEl,btNil,FBaseTypes[btNil],TNilExpr(El),[rrfReadable])
  else if El.ClassType=TSelfExpr then
    begin
    if rcConstant in Flags then
      RaiseConstantExprExp(El);
    ComputeElement(TResolvedReference(El.CustomData).Declaration,ResolvedEl,Flags);
    end
  else if El.ClassType=TBoolConstExpr then
    SetResolverValueExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],TBoolConstExpr(El),[rrfReadable])
  else if El.ClassType=TParamsExpr then
    case TParamsExpr(El).Kind of
      pekArrayParams:
        ComputeArrayParams(TParamsExpr(El),ResolvedEl,Flags);
      pekFuncParams:
        ComputeFuncParams(TParamsExpr(El),ResolvedEl,Flags);
      pekSet:
        ComputeSetParams(TParamsExpr(El),ResolvedEl,Flags);
    else
      RaiseNotYetImplemented(20161010184559,El);
    end
  else if El is TPasProcedure then
    begin
    SetResolverIdentifier(ResolvedEl,btProc,El,TPasProcedure(El).ProcType,[]);
    if El is TPasFunction then
      Include(ResolvedEl.Flags,rrfReadable);
    end
  else if El is TPasArrayType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasArrayType(El),[])
  else
    RaiseNotYetImplemented(20160922163705,El);
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
    if not ClassScope.AncestorResolved then
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

function TPasResolver.ResolveAliasType(aType: TPasType): TPasType;
begin
  Result:=aType;
  while Result is TPasAliasType do
    Result:=TPasAliasType(Result).DestType;
end;

function TPasResolver.CheckSrcIsADstType(const ResolvedSrcType,
  ResolvedDestType: TPasResolverResult; ErrorEl: TPasElement): integer;
// finds distance between classes SrcType and DestType
var
  SrcEl, DstEl: TPasElement;
  ClassEl: TPasClassType;
begin
  Result:=cIncompatible;
  DstEl:=ResolvedDestType.TypeEl;
  if DstEl=nil then exit(cIncompatible);
  // skip Dst alias
  while (DstEl<>nil) and (DstEl.ClassType=TPasAliasType) do
    DstEl:=TPasAliasType(DstEl).DestType;

  SrcEl:=ResolvedSrcType.TypeEl;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckSrcIsADstType SrcEl=',GetObjName(SrcEl),' DstEl=',GetObjName(DstEl));
  {$ENDIF}
  Result:=cExact;
  while SrcEl<>nil do
    begin
    {$IFDEF VerbosePasResolver}
    writeln(' Step=',Result,' SrcEl=',GetObjName(SrcEl),' DstEl=',GetObjName(DstEl));
    {$ENDIF}
    if SrcEl=DstEl then exit;
    if SrcEl.ClassType=TPasAliasType then
      // alias -> skip
      SrcEl:=TPasAliasType(SrcEl).DestType
    else if SrcEl.ClassType=TPasTypeAliasType then
      begin
      // type alias -> increases distance
      SrcEl:=TPasAliasType(SrcEl).DestType;
      inc(Result);
      end
    else if SrcEl.ClassType=TPasClassType then
      begin
      ClassEl:=TPasClassType(SrcEl);
      if ClassEl.IsForward then
        // class forward -> skip
        SrcEl:=(ClassEl.CustomData as TResolvedReference).Declaration
      else
        begin
        // class ancestor -> increase distance
        SrcEl:=(ClassEl.CustomData as TPasClassScope).DirectAncestor;
        inc(Result);
        end;
      end
    else
      exit(cIncompatible);
    end;
  if ErrorEl=nil then ;
  Result:=cIncompatible;
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
  FItems.ForEachCall(@OnClearItem,nil);
  FItems.Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TPasIdentifierScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=FindLocalIdentifier(Identifier);
  {$IFDEF VerbosePasResolver}
  if (Result<>nil) and (Result.Owner<>Self) then
    begin
    writeln('TPasIdentifierScope.FindIdentifier Result.Owner<>Self Owner='+GetObjName(Result.Owner));
    raise Exception.Create('20160925184159 ');
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
    writeln('TPasIdentifierScope.IterateElements ',ClassName,' ',Item.Identifier,' ',GetObjName(Item.Element));
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

end.

