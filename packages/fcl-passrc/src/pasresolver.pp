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
  - repeat..ntil
  - if..then..else
  - binary operators
  - case..of
  - try..finally..except, on, else, raise
  - for loop
  - spot duplicates
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
    - is and as operator
    - nil
  - with..do

 ToDo:
  - classes - TPasClassType
     - property with params
     - typecast
     - class of
     - class method, property, var, const
     - visibility
     - nested var, const
     - nested types
  - procedure type
  - method type
  - records - TPasRecordType,
     - variant - TPasVariant
     - const  TRecordValues
  - enums - TPasEnumType, TPasEnumValue
     - propagate to parent scopes
  - check if types only refer types
  - check if constant is longint or int64
  - built-in functions
  - ranges TPasRangeType
  - arrays  TPasArrayType
    - const TArrayValues
  - pointer TPasPointerType
  - proc: check if forward and impl default values match
  - for..in..do
  - untyped parameters
  - sets - TPasSetType
  - forwards of ^pointer and class of - must be queued and resolved at end of type section
  - interfaces
  - properties - TPasProperty
    - index properties, implements, stored, default
  - default property
  - TPasResString
  - TPasFileType
  - generics, nested param lists
  - check const expression types, e.g. bark on "const c:string=3;"
  - dotted unitnames
  - labels
  - helpers
  - generics
  - operator overload
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
  nIncompatibleTypeGotExpected = 3028;
  nTypesAreNotRelated = 3029;
  nAbstractMethodsCannotBeCalledDirectly = 3030;

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
  sIncompatibleTypeGotExpected = 'Incompatible types: got "%s" expected "%s"';
  sTypesAreNotRelated = 'Types are not related';
  sAbstractMethodsCannotBeCalledDirectly = 'Abstract methods cannot be called directly';

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
    btNil          // nil = pointer, class, procedure, method, ...
    );
  TResolveBaseTypes = set of TResolverBaseType;
const
  btAllInteger = [btComp,btCurrency,btByte,btShortInt,btWord,btSmallInt,
    btLongWord,btCardinal,btLongint,btQWord,btInt64];
  btAllStrings = [btChar,btWideChar,btString,btAnsiString,btShortString,
    btWideString,btUnicodeString];
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
    btVariant,
    btNil
    ];

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
    'Nil'
    );

type
  TResolverBuiltInProc = (
    bfLength,    // function length(const dynarray or string): sizeint
    bfSetLength  // procedure SetLength(var dynarray or string,sizeint)
    );
  TResolverBuiltInProcs = set of TResolverBuiltInProc;
const
  ResolverBuiltInProcNames: array[TResolverBuiltInProc] of shortstring = (
    'Length',
    'SetLength'
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

  TPasWithExprScope = class;

  TResolvedReferenceFlag = (
    rrfVMT // use VMT for call
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

  TResElDataBuiltInSymbol = Class(TResolveData)
  public
  end;

  { TResElDataBaseType - CustomData for compiler built-in types, e.g. longint }

  TResElDataBaseType = Class(TResElDataBuiltInSymbol)
  public
    BaseType: TResolverBaseType;
  end;

  { TResElDataBuiltInProc - CustomData for compiler built-in procs like 'length' }

  TResElDataBuiltInProc = Class(TResElDataBuiltInSymbol)
  public
    Proc: TResElDataBuiltInProc;
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
    function InternalFindIdentifier(const Identifier: String): TPasIdentifier; inline;
    procedure InternalAdd(Item: TPasIdentifier);
    procedure OnClearItem(Item, Dummy: pointer);
    procedure OnWriteItem(Item, Dummy: pointer);
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; virtual;
    function RemoveIdentifier(El: TPasElement): boolean; virtual;
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
    function FindIdentifierInSection(const Identifier: String): TPasIdentifier;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
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
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
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

  { TPasDotRecordScope - used for aRecord.subidentifier }

  TPasDotRecordScope = Class(TPasSubScope)
  public
    RecordScope: TPasRecordScope;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasDotClassScope - used for aClass.subidentifier }

  TPasDotClassScope = Class(TPasSubScope)
  public
    ClassScope: TPasClassScope;
    InheritedExpr: boolean; // this is 'inherited <name>' instead of '.<name'
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasResolverResult }

  TPasResolverResult = record
    BaseType: TResolverBaseType;
    IdentEl: TPasElement; // if set then this specific identifier is the value, can be a type
    TypeEl: TPasType; // can be nil for const expression
    ExprEl: TPasExpr;
  end;
  PPasResolvedElement = ^TPasResolverResult;

  TPasResolverComputeFlag = (
    rcSkipTypeAlias
    );
  TPasResolverComputeFlags = set of TPasResolverComputeFlag;

  TPRFindFirstData = record
    ErrorPosEl: TPasElement;
    Found: TPasElement;
    ElScope, StartScope: TPasScope;
  end;
  PPRFindFirstData = ^TPRFindFirstData;

  { TPasResolver }

  TPasResolver = Class(TPasTreeContainer)
  private
    type
      TResolveDataListKind = (lkBuiltIn,lkModule);
    procedure ClearResolveDataList(Kind: TResolveDataListKind);
  private
    FBaseTypes: array[TResolverBaseType] of TPasUnresolvedSymbolRef;
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
    FPendingForwards: TFPList; // list of TPasElement needed to check for forwards
    function GetBaseType(bt: TResolverBaseType): TPasUnresolvedSymbolRef; inline;
    function GetScopes(Index: integer): TPasScope; inline;
  protected
    const
      cIncompatible = High(integer);
      cExact = 0;
    type
      TFindCallProcData = record
        Params: TParamsExpr;
        Found: TPasProcedure;
        ElScope, StartScope: TPasScope;
        Distance: integer; // compatibility distance
        Count: integer;
        List: TFPList; // if not nil then collect all found proc here
      end;
      PFindCallProcData = ^TFindCallProcData;

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
    procedure OnFindCallProc(El: TPasElement; ElScope, StartScope: TPasScope;
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
    Procedure AddRecordType(El: TPasRecordType);
    Procedure AddClassType(El: TPasClassType);
    procedure AddVariable(El: TPasVariable);
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
    procedure StartProcedureBody(El: TProcedureBody);
    procedure FinishModule(CurModule: TPasModule);
    procedure FinishUsesList;
    procedure FinishTypeSection(El: TPasDeclarations);
    procedure FinishTypeDef(El: TPasType);
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
    procedure CheckIsClass(El: TPasElement; const ResolvedEl: TPasResolverResult);
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
    function FindFirstElement(const AName: String; ErrorPosEl: TPasElement): TPasElement;
    function FindFirstElement(const AName: String; out Data: TPRFindFirstData;
      ErrorPosEl: TPasElement): TPasElement;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure FinishScope(ScopeType: TPasScopeType; El: TPasElement); override;
    // built in types and functions
    procedure ClearBuiltInIdentifiers;
    procedure AddObjFPCBuiltInIdentifiers(
      BaseTypes: TResolveBaseTypes = btAllStandardTypes;
      BaseProcs: TResolverBuiltInProcs = bfAllStandardProcs);
    function AddBaseType(aName: shortstring; Typ: TResolverBaseType): TResElDataBaseType;
    function IsBaseType(aType: TPasType; BaseType: TResolverBaseType): boolean;
    function AddBuiltInProc(aName: shortstring): TResElDataBuiltInProc;
    // add extra TResolveData (E.CustomData) to free list
    procedure AddResolveData(El: TPasElement; Data: TResolveData;
      Kind: TResolveDataListKind);
    function CreateReference(DeclEl, RefEl: TPasElement;
      FindData: PPRFindFirstData = nil): TResolvedReference; virtual;
    // scopes
    function CreateScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; virtual;
    procedure PopScope;
    procedure PushScope(Scope: TPasScope); overload;
    function PushScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; overload;
    function PushClassDotScope(var CurClassType: TPasClassType): TPasDotClassScope;
    function PushRecordDotScope(CurRecordType: TPasRecordType): TPasDotRecordScope;
    function PushDotScopeForType(TypeEl: TPasType; ErrorEl: TPasElement): TPasSubScope;
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
    function CheckParamCompatibility(Expr: TPasExpr; Param: TPasArgument;
      ParamNo: integer; RaiseOnError: boolean): integer;
    function CheckCustomTypeCompatibility(
      const SrcType, DestType: TPasResolverResult; ErrorEl: TPasElement): integer;
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
    // uility functions
    function GetPasPropertyType(El: TPasProperty): TPasType;
    function GetPasPropertyAncestor(El: TPasProperty): TPasProperty;
    function GetPasPropertyGetter(El: TPasProperty): TPasElement;
    function GetPasPropertySetter(El: TPasProperty): TPasElement;
    function GetPasClassAncestor(ClassEl: TPasClassType; SkipAlias: boolean): TPasType;
  public
    property BaseType[bt: TResolverBaseType]: TPasUnresolvedSymbolRef read GetBaseType;
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
procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement;
  TypeEl: TPasType); overload;
procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; TypeEl: TPasType; ExprEl: TPasExpr); overload;
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
      Result:=Result+AccessNames[Arg.Access]+' ';
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
  if (aType.ClassType=TPasUnresolvedSymbolRef)
      or (aType.ClassType=TPasUnresolvedTypeRef) then
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
    Result:=aType.ElementTypeName;
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
      Result:=Result+AccessNames[TPasArgument(El).Access]+' ';
    if TPasArgument(El).ArgType=nil then
      Result:=Result+'untyped'
    else
      Result:=Result+GetTreeDesc(TPasArgument(El).ArgType,Indent);
    end;
end;

function GetResolverResultDesc(const T: TPasResolverResult): string;
begin
  if T.IdentEl<>nil then
    begin
    // named element
    if T.IdentEl=T.TypeEl then
      Result:='type '+GetTypeDesc(T.TypeEl)
    else
      Result:=GetObjName(T.IdentEl)+':'+GetTypeDesc(T.TypeEl);
    if T.ExprEl<>nil then
      Result:=Result+':='+GetTreeDesc(T.ExprEl);
    end
  else if T.TypeEl<>nil then
    begin
    // anonymous constant expression with named type
    Result:='const '+GetTreeDesc(T.ExprEl);
    if T.ExprEl<>nil then
      Result:=Result+':='+GetTreeDesc(T.ExprEl);
    end
  else
    begin
    // anonymous const expr without explicit type, e.g. 123.4
    Result:=Result+':='+GetTreeDesc(T.ExprEl);
    end;
  Result:=Result+'='+BaseTypeNames[T.BaseType];
end;

procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement; TypeEl: TPasType);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.IdentEl:=IdentEl;
  ResolvedType.TypeEl:=TypeEl;
  ResolvedType.ExprEl:=nil;
end;

procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; TypeEl: TPasType; ExprEl: TPasExpr);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.IdentEl:=nil;
  ResolvedType.TypeEl:=TypeEl;
  ResolvedType.ExprEl:=ExprEl;
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

function TPasDotClassScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=ClassScope.FindIdentifier(Identifier);
end;

procedure TPasDotClassScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  ClassScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasDotClassScope.WriteIdentifiers(Prefix: string);
begin
  ClassScope.WriteIdentifiers(Prefix);
end;

{ TPasDotRecordScope }

function TPasDotRecordScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=RecordScope.FindIdentifier(Identifier);
end;

procedure TPasDotRecordScope.IterateElements(const aName: string;
  StartScope: TPasScope; const OnIterateElement: TIterateScopeElement;
  Data: Pointer; var Abort: boolean);
begin
  RecordScope.IterateElements(aName, StartScope, OnIterateElement, Data, Abort);
end;

procedure TPasDotRecordScope.WriteIdentifiers(Prefix: string);
begin
  RecordScope.WriteIdentifiers(Prefix);
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
    Result:=ImplementationScope.FindIdentifierInSection(Identifier);
    if (Result<>nil) and (Result.Element.ClassType<>TPasModule) then
      exit;
    end;
  if InterfaceScope<>nil then
    Result:=InterfaceScope.FindIdentifierInSection(Identifier)
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

function TPasSectionScope.FindIdentifierInSection(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=inherited FindIdentifier(Identifier);
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
  Data: PPRFindFirstData absolute FindFirstElementData;
begin
  Data^.Found:=El;
  Data^.ElScope:=ElScope;
  Data^.StartScope:=StartScope;
  Abort:=true;
end;

procedure TPasResolver.OnFindCallProc(El: TPasElement; ElScope,
  StartScope: TPasScope; FindProcsData: Pointer; var Abort: boolean);
var
  Data: PFindCallProcData absolute FindProcsData;
  Proc, PrevProc: TPasProcedure;
  Distance: integer;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindCallProc START ---------');
  {$ENDIF}
  if (El is TPasProcedure) then
    begin
    // identifier is a proc
    Proc:=TPasProcedure(El);
    Distance:=CheckCallProcCompatibility(Proc,Data^.Params,false);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.OnFindCallProc Compatible=',Distance,' Data^.Found=',Data^.Found<>nil,' Data^.Compatible=',ord(Data^.Distance));
    {$ENDIF}

    if Data^.Found<>nil then
      begin
      // there is already a previous proc
      PrevProc:=Data^.Found;

      // check if previous found proc is override of found proc
      if (PrevProc.IsOverride)
          and (TPasProcedureScope(PrevProc.CustomData).OverriddenProc=Proc) then
        begin
        // previous found proc is override of found proc -> skip
        exit;
        end;

      // check if found proc and previous found proc are 'forward' and 'body'
      if ProcNeedsImplProc(Proc) and (Proc.CustomData is TPasProcedureScope)
          and (TPasProcedureScope(Proc.CustomData).ImplProc=PrevProc)
      then
        exit;
      if ProcNeedsImplProc(PrevProc) and (PrevProc.CustomData is TPasProcedureScope)
          and (TPasProcedureScope(PrevProc.CustomData).ImplProc=Proc)
      then
        begin
        // previous proc was the forward -> replace
        if Data^.List<>nil then
          begin
          Data^.List.Remove(Data^.Found);
          Data^.List.Add(Proc);
          end;
        Data^.Found:=Proc;
        Data^.ElScope:=ElScope;
        Data^.StartScope:=StartScope;
        exit;
        end;

      end;

    if (Data^.Found=nil) or (Distance<Data^.Distance) then
      begin
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.OnFindCallProc Found a better candidate Distance=',Distance,' Data^.Distance=',Data^.Distance);
      {$ENDIF}
      Data^.Found:=Proc;
      Data^.ElScope:=ElScope;
      Data^.StartScope:=StartScope;
      Data^.Distance:=Distance;
      Data^.Count:=1;
      if Data^.List<>nil then
        begin
        Data^.List.Clear;
        Data^.List.Add(Proc);
        end;
      end
    else if Distance=Data^.Distance then
      begin
      inc(Data^.Count);
      if (Data^.List<>nil) then
        begin
        if (Data^.List.IndexOf(Proc)>=0) then
          begin
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.OnFindCallProc Found Proc twice: ',GetTreeDesc(Proc),
            ' ',GetElementSourcePosStr(Proc),
            ' PrevElScope=',GetObjName(Data^.ElScope),' ',GetTreeDesc(Data^.ElScope.Element),
            ' ElScope=',GetObjName(ElScope),' ',GetTreeDesc(ElScope.Element)
            );
          {$ENDIF}
          RaiseInternalError(20160924230805);
          end;
        Data^.List.Add(Proc);
        end;
      end;
    exit;
    end
  else if El is TPasUnresolvedSymbolRef then
    begin
    if El.CustomData.ClassType=TResElDataBuiltInProc then
      begin
      //TResElDataBuiltInProc(El.CustomData);
        RaiseNotYetImplemented(20160927203131,El);
      exit;
      end;
    end;
  // identifier is not a proc
  // ToDo: TypeCast
  Abort:=true;
  if Data^.Found=nil then
    begin
    // ToDo: use the ( as error position
    RaiseMsg(nIllegalQualifier,sIllegalQualifier,['('],Data^.Params.Value);
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
      +[po_resolvestandardtypes,po_nooverloadedprocs,po_keepclassforward];
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
  if OlderIdentifier<>nil then
    if (Identifier.Kind=pikSimple) or (OlderIdentifier.Kind=pikSimple) then
      RaiseMsg(nDuplicateIdentifier,sDuplicateIdentifier,
               [aName,GetElementSourcePosStr(OlderIdentifier.Element)],El);
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
begin
  // ToDo: resolve pending forwards
  for i:=0 to El.Declarations.Count-1 do
    begin
    Decl:=TPasElement(El.Declarations[i]);
    if Decl is TPasClassType then
      begin
      if TPasClassType(Decl).IsForward and (TPasClassType(Decl).CustomData=nil) then
        RaiseMsg(nForwardTypeNotResolved,sForwardTypeNotResolved,[Decl.Name],Decl);
      end;
    end;
end;

procedure TPasResolver.FinishTypeDef(El: TPasType);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishTypeDef El=',GetObjName(El));
  {$ENDIF}
  if TopScope.Element=El then
    begin
    if (TopScope.ClassType=TPasRecordScope)
    or (TopScope.ClassType=TPasClassScope) then
      PopScope;
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
      FoundInScope.RemoveIdentifier(DeclProc);
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

    CurClassType:=TPasClassType(FindFirstElement(aClassName,ImplProc.ProcType));
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
  //or DeclProc.IsExternal then;
  ImplProcScope:=ImplProc.CustomData as TPasProcedureScope;
  ImplProcScope.DeclarationProc:=DeclProc;
  ImplProcScope.ClassScope:=CurClassScope;
  DeclProcScope:=DeclProc.CustomData as TPasProcedureScope;
  DeclProcScope.ImplProc:=ImplProc;

  // add 'Self'
  AddIdentifier(ImplProcScope,'Self',CurClassType,pikSimple);

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
  PropType, ResultType: TPasType;
  CurClassType: TPasClassType;
  ClassScope: TPasClassScope;
  AccEl: TPasElement;
  Proc: TPasProcedure;
  ArgCount: Integer;
  Arg: TPasArgument;

  procedure GetPropType;
  var
    AncProp: TPasElement;
  begin
    if PropType<>nil then exit;
    if PropEl.VarType<>nil then
      PropType:=PropEl.VarType
      // Note: a property with a type has no ancestor property
    else
      begin
      // search property in ancestor
      AncProp:=nil;
      if ClassScope.AncestorScope<>nil then
        AncProp:=ClassScope.AncestorScope.FindElement(PropEl.Name);
      if (AncProp=nil) or (not (AncProp is TPasProperty)) then
        RaiseMsg(nNoPropertyFoundToOverride,sNoPropertyFoundToOverride,[],PropEl);
      PropType:=TPasProperty(AncProp).VarType;
      CreateReference(AncProp,PropEl);
      end;
  end;

  function GetAccessor(Expr: TPasExpr): TPasElement;
  var
    Prim: TPrimitiveExpr;
    DeclEl: TPasElement;
  begin
    repeat
      if Expr.ClassType=TBinaryExpr then
        begin
        if TBinaryExpr(Expr).left is TPrimitiveExpr then
          begin
          Prim:=TPrimitiveExpr(TBinaryExpr(Expr).left);
          if Prim.CustomData is TResolvedReference then
            begin
            DeclEl:=TResolvedReference(Prim.CustomData).Declaration;
            if DeclEl.ClassType<>TPasVariable then
              RaiseXExpectedButYFound('var',DeclEl.ElementTypeName,Prim);
            end;
          end
        else
          RaiseNotYetImplemented(20160922163430,TBinaryExpr(Expr).left);
        Expr:=TBinaryExpr(Expr).right
        end
      else if Expr.ClassType=TPrimitiveExpr then
        begin
        Prim:=TPrimitiveExpr(Expr);
        if Prim.CustomData is TResolvedReference then
          begin
          Result:=TResolvedReference(Prim.CustomData).Declaration;
          exit;
          end
        else
          RaiseNotYetImplemented(20160922163433,Expr);
        end
      else
        RaiseNotYetImplemented(20160922163436,Expr);
    until false;
  end;

begin
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
    // read accessor
    ResolveExpr(PropEl.ReadAccessor);
    // check compatibility
    AccEl:=GetAccessor(PropEl.ReadAccessor);
    if AccEl is TPasVariable then
      begin
      if TPasVariable(AccEl).VarType<>PropType then
        RaiseXExpectedButYFound(GetTypeDesc(PropType),
          GetTypeDesc(TPasVariable(AccEl).VarType),PropEl.ReadAccessor);
      end
    else if AccEl is TPasProcedure then
      begin
      // check function
      Proc:=TPasProcedure(AccEl);
      if Proc.ClassType<>TPasFunction then
        RaiseXExpectedButYFound('function',Proc.ElementTypeName,PropEl.ReadAccessor);
      // check function result type
      ResultType:=TPasFunction(Proc).FuncType.ResultEl.ResultType;
      if ResultType<>PropType then
        RaiseXExpectedButYFound('function result '+GetTypeDesc(PropType),
          GetTypeDesc(ResultType),PropEl.ReadAccessor);
      // check arg count
      ArgCount:=Proc.ProcType.Args.Count;
      if Proc.ProcType.Args.Count<>0 then
        RaiseXExpectedButYFound('function argument count '+IntToStr(0),
          IntToStr(ArgCount),PropEl.ReadAccessor);
      end
    else
      RaiseXExpectedButYFound('variable',AccEl.ElementTypeName,PropEl.ReadAccessor);
    end;
  if PropEl.WriteAccessor<>nil then
    begin
    // write accessor
    ResolveExpr(PropEl.WriteAccessor);
    // check compatibility
    AccEl:=GetAccessor(PropEl.WriteAccessor);
    if AccEl is TPasVariable then
      begin
      if TPasVariable(AccEl).VarType<>PropType then
        RaiseXExpectedButYFound(GetTypeDesc(PropType),
          GetTypeDesc(TPasVariable(AccEl).VarType),PropEl.WriteAccessor);
      end
    else if AccEl is TPasProcedure then
      begin
      // check procedure
      Proc:=TPasProcedure(AccEl);
      if Proc.ClassType<>TPasProcedure then
        RaiseXExpectedButYFound('procedure',Proc.ElementTypeName,PropEl.WriteAccessor);
      // check arg count
      ArgCount:=Proc.ProcType.Args.Count;
      if Proc.ProcType.Args.Count<>1 then
        RaiseXExpectedButYFound('procedure argument count '+IntToStr(1),
          IntToStr(ArgCount),PropEl.WriteAccessor);
      Arg:=TPasArgument(Proc.ProcType.Args[0]);
      if not (Arg.Access in [argDefault,argConst]) then
        RaiseXExpectedButYFound('procedure(const Value)',
          'procedure('+AccessNames[Arg.Access]+' '+Arg.Name+')',PropEl.WriteAccessor);
      if Arg.ArgType<>PropType then
        RaiseXExpectedButYFound('procedure('+GetTypeDesc(PropType)+')',
          'procedure('+GetTypeDesc(Arg.ArgType)+')',PropEl.WriteAccessor);
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
    // stored accessor
    ResolveExpr(PropEl.StoredAccessor);
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
      ArgCount:=Proc.ProcType.Args.Count;
      if Proc.ProcType.Args.Count<>0 then
        RaiseXExpectedButYFound('function argument count '+IntToStr(0),
          IntToStr(ArgCount),PropEl.StoredAccessor);
      end
    else
      RaiseXExpectedButYFound('function: boolean',AccEl.ElementTypeName,PropEl.StoredAccessor);
    end;
  if PropEl.DefaultExpr<>nil then
    begin
    ResolveExpr(PropEl.DefaultExpr);
    // ToDo: check compatibility
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
        AncestorEl:=TPasClassType(FindFirstElement('TObject',aClass));
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
    ClassScope.AncestorScope:=AncestorEl.CustomData as TPasClassScope;
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
begin
  ResolveExpr(CaseOf.CaseExpr);
  for i:=0 to CaseOf.Elements.Count-1 do
    begin
    El:=TPasElement(CaseOf.Elements[i]);
    if El.ClassType=TPasImplCaseStatement then
      begin
      Stat:=TPasImplCaseStatement(El);
      for j:=0 to Stat.Expressions.Count-1 do
        begin
        //writeln('TPasResolver.ResolveImplCaseOf Stat.Expr[',j,']=',GetObjName(El));
        ResolveExpr(TPasExpr(Stat.Expressions[j]));
        end;
      ResolveImplElement(Stat.Body);
      end
    else if El.ClassType=TPasImplCaseElse then
      ResolveImplBlock(TPasImplCaseElse(El))
    else
      RaiseNotYetImplemented(20160922163448,El);
    end;
  // CaseOf.ElseBranch was already resolved via Elements
end;

procedure TPasResolver.ResolveImplLabelMark(Mark: TPasImplLabelMark);
var
  DeclEl: TPasElement;
begin
  DeclEl:=FindFirstElement(Mark.LabelId,Mark);
  // ToDo: check if DeclEl is a label and check duplicate
  CreateReference(DeclEl,Mark);
end;

procedure TPasResolver.ResolveImplForLoop(Loop: TPasImplForLoop);
begin
  ResolveExpr(Loop.VariableName);
  ResolveExpr(Loop.StartExpr);
  ResolveExpr(Loop.EndExpr);
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
    ComputeElement(Expr,ExprResolved,[rcSkipTypeAlias]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ResolveImplWithDo ExprResolved=',GetResolverResultDesc(ExprResolved));
    {$ENDIF}
    ErrorEl:=Expr;
    // ToDo: use last element in Expr
    TypeEl:=ExprResolved.TypeEl;
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
  case El.Kind of
  akDefault:
    begin
    Compatible:=CheckAssignCompatibility(LeftResolved,RightResolved,El.right);
    if Compatible=cIncompatible then
      begin
      Expected:=GetResolverResultDesc(LeftResolved);
      Actual:=GetResolverResultDesc(RightResolved);
      if LeftResolved.BaseType<>RightResolved.BaseType then
        begin
        Expected:=BaseTypeNames[LeftResolved.BaseType];
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
      RaiseMsg(nIncompatibleTypeGotExpected,sIncompatibleTypeGotExpected,
        [Actual,Expected],El.right);
      end;
    end;
  else
    RaiseNotYetImplemented(20160927143649,El,'AssignKind '+IntToStr(Ord(El.Kind)));
  end;
end;

procedure TPasResolver.ResolveImplRaise(El: TPasImplRaise);
var
  ResolvedEl: TPasResolverResult;
begin
  ResolveExpr(El.ExceptObject);
  ResolveExpr(El.ExceptAddr);
  ComputeElement(El.ExceptObject,ResolvedEl,[rcSkipTypeAlias]);
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
  ComputeElement(El,ResolvedCond,[rcSkipTypeAlias]);
  if ResolvedCond.BaseType<>btBoolean then
    RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
      [BaseTypeNames[btBoolean],BaseTypeNames[ResolvedCond.BaseType]],El);
end;

procedure TPasResolver.ResolveNameExpr(El: TPasExpr; const aName: string);
var
  FindData: TPRFindFirstData;
  DeclEl: TPasElement;
  Proc: TPasProcedure;
begin
  DeclEl:=FindFirstElement(aName,FindData,El);
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
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[DeclEl.Name],El);
    end;
  CreateReference(DeclEl,El,@FindData);
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
  //eopIn,
  eopIs,
  eopAs:
  //eopSymmetricaldifference:
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
  DeclEl: TPasElement;
  ModuleScope: TPasSubModuleScope;
  aModule: TPasModule;
  VarType: TPasType;
  CurClassType: TPasClassType;
begin
  //writeln('TPasResolver.ResolveSubIdent El.left=',GetObjName(El.left));
  if El.left.ClassType=TPrimitiveExpr then
    begin
    //writeln('TPasResolver.ResolveSubIdent El.left.CustomData=',GetObjName(El.left.CustomData));
    if El.left.CustomData is TResolvedReference then
      begin
      DeclEl:=TResolvedReference(El.left.CustomData).Declaration;
      //writeln('TPasResolver.ResolveSubIdent Decl=',GetObjName(DeclEl));
      if DeclEl is TPasModule then
        begin
        // e.g. unitname.identifier
        // => search in interface and if this is our module in the implementation
        aModule:=TPasModule(DeclEl);
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
      else if DeclEl.ClassType=TPasVariable then
        begin
        VarType:=TPasVariable(DeclEl).VarType;
        if VarType=nil then
          RaiseInternalError(20160922185910);
        PushDotScopeForType(VarType,El);
        ResolveExpr(El.right);
        PopScope;
        exit;
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveSubIdent DeclEl=',GetObjName(DeclEl));
        {$ENDIF}
        end;
      end;
    end
  else if El.left.ClassType=TSelfExpr then
    begin
    if El.left.CustomData is TResolvedReference then
      begin
      DeclEl:=TResolvedReference(El.left.CustomData).Declaration;
      if DeclEl.ClassType=TPasClassType then
        begin
        CurClassType:=TPasClassType(DeclEl);
        PushClassDotScope(CurClassType);
        ResolveExpr(El.right);
        PopScope;
        exit;
        end
      else
        begin
        {$IFDEF VerbosePasResolver}
        writeln('TPasResolver.ResolveSubIdent DeclEl=',GetObjName(DeclEl));
        {$ENDIF}
        end;
      end;
    end;
  RaiseMsg(nIllegalQualifier,sIllegalQualifier,['.'],El);
end;

procedure TPasResolver.ResolveParamsExpr(Params: TParamsExpr);
var
  i, ScopeDepth: Integer;
  ProcName, Msg: String;
  FindData: TFindCallProcData;
  Abort: boolean;
  El: TPasElement;
  Ref: TResolvedReference;
begin
  // first resolve params
  ResetSubScopes(ScopeDepth);
  for i:=0 to length(Params.Params)-1 do
    ResolveExpr(Params.Params[i]);
  RestoreSubScopes(ScopeDepth);

  // then search the best fitting proc
  if Params.Value.ClassType=TPrimitiveExpr then
    begin
    ProcName:=TPrimitiveExpr(Params.Value).Value;
    FindData:=Default(TFindCallProcData);
    FindData.Params:=Params;
    Abort:=false;
    IterateElements(ProcName,@OnFindCallProc,@FindData,Abort);
    if FindData.Found=nil then
      RaiseIdentifierNotFound(ProcName,Params.Value);
    if FindData.Distance=cIncompatible then
      begin
      // found one proc, but it was incompatible => raise error
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveParamsExpr found one proc, but it was incompatible => check again to raise error');
      {$ENDIF}
      CheckCallProcCompatibility(FindData.Found,Params,true);
      end;
    if FindData.Count>1 then
      begin
      // multiple overloads fit => search again and list the candidates
      FindData:=Default(TFindCallProcData);
      FindData.Params:=Params;
      FindData.List:=TFPList.Create;
      try
        IterateElements(ProcName,@OnFindCallProc,@FindData,Abort);
        Msg:='';
        for i:=0 to FindData.List.Count-1 do
          begin
          // ToDo: create a hint for each candidate
          El:=TPasElement(FindData.List[i]);
          {$IFDEF VerbosePasResolver}
          writeln('TPasResolver.ResolveParamsExpr Overload Candidate: ',GetElementSourcePosStr(El),' ',GetTreeDesc(El));
          {$ENDIF}
          Msg:=Msg+', ';
          Msg:=Msg+GetElementSourcePosStr(El);
          end;
        RaiseMsg(nCantDetermineWhichOverloadedFunctionToCall,
          sCantDetermineWhichOverloadedFunctionToCall+Msg,[ProcName],Params.Value);
      finally
        FindData.List.Free;
      end;
      end;

    // found compatible proc -> create reference
    Ref:=CreateReference(FindData.Found,Params.Value);
    if FindData.Found.IsVirtual or FindData.Found.IsOverride then
      begin
        if (TopScope.ClassType=TPasDotClassScope)
        and TPasDotClassScope(TopScope).InheritedExpr then
          begin
          // call directly
          if FindData.Found.IsAbstract then
            RaiseMsg(nAbstractMethodsCannotBeCalledDirectly,
              sAbstractMethodsCannotBeCalledDirectly,[],Params.Value);
          end
        else
          begin
          // call via method table
          Ref.Flags:=Ref.Flags+[rrfVMT];
          end;
      end
    else
    end
  else
    RaiseNotYetImplemented(20160922163501,Params,'with parameters');
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
  else if El is TPasClassType then
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
  if El is TPasUnresolvedTypeRef then exit; // built-in type
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
    RaiseInvalidScopeForElement(20160922163515,El);
  AddIdentifier(TPasIdentifierScope(TopScope),El.Name,El,pikSimple);
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
  if not (TopScope is TPasProcedureScope) then
    RaiseInvalidScopeForElement(20160922163529,El);
  AddIdentifier(TPasProcedureScope(TopScope),El.Name,El,pikSimple);
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
var
  LeftResolved, RightResolved: TPasResolverResult;
begin
  ComputeElement(Bin.left,LeftResolved,Flags);
  ComputeElement(Bin.right,RightResolved,Flags);
  // ToDo: check operator overloading
  if LeftResolved.BaseType in btAllInteger then
    begin
    if RightResolved.BaseType in (btAllInteger+btAllFloats) then
      case Bin.OpCode of
        eopAdd, eopSubtract,
        eopMultiply, eopDiv, eopMod,
        eopPower,
        eopShl, eopShr,
        eopAnd, eopOr, eopXor:
          begin
          SetResolverTypeExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin);
          exit;
          end;
        eopEqual,
        eopNotEqual,
        eopLessThan,
        eopGreaterThan,
        eopLessthanEqual,
        eopGreaterThanEqual:
          begin
          SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
          exit;
          end;
      end;
    end
  else if LeftResolved.BaseType in btAllBooleans then
    begin
    if RightResolved.BaseType in btAllBooleans then
      case Bin.OpCode of
      eopAnd, eopOr, eopXor,
      eopEqual,
      eopNotEqual:
        begin
        SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType in btAllStrings then
    begin
    if RightResolved.BaseType in btAllStrings then
      case Bin.OpCode of
      eopAdd:
        case LeftResolved.BaseType of
        btChar:
          begin
            case RightResolved.BaseType of
            btChar: SetResolverTypeExpr(ResolvedEl,btString,nil,Bin);
            btWideChar: SetResolverTypeExpr(ResolvedEl,btUnicodeString,nil,Bin);
            else SetResolverTypeExpr(ResolvedEl,RightResolved.BaseType,nil,Bin);
            end;
            exit;
          end;
        btWideChar:
          begin
            SetResolverTypeExpr(ResolvedEl,btUnicodeString,nil,Bin);
            exit;
          end;
        btShortString:
          begin
            case RightResolved.BaseType of
            btChar,btShortString,btWideChar:
              SetResolverTypeExpr(ResolvedEl,btShortString,nil,Bin);
            else
              SetResolverTypeExpr(ResolvedEl,RightResolved.BaseType,nil,Bin);
            end;
            exit;
          end;
        btString,btAnsiString,btUnicodeString:
          begin
            SetResolverTypeExpr(ResolvedEl,LeftResolved.BaseType,nil,Bin);
            exit;
          end;
        end;
      eopEqual,
      eopNotEqual,
      eopLessThan,
      eopGreaterThan,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType in btAllFloats then
    begin
    if RightResolved.BaseType in (btAllInteger+btAllFloats) then
      case Bin.OpCode of
      eopAdd, eopSubtract,
      eopMultiply, eopDivide, eopMod,
      eopPower:
        begin
        SetResolverTypeExpr(ResolvedEl,LeftResolved.BaseType,LeftResolved.TypeEl,Bin);
        exit;
        end;
      eopEqual,
      eopNotEqual,
      eopLessThan,
      eopGreaterThan,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btPointer then
    begin
    if RightResolved.BaseType in btAllInteger then
      case Bin.OpCode of
      eopAdd,eopSubtract:
        begin
        SetResolverTypeExpr(ResolvedEl,btPointer,LeftResolved.TypeEl,Bin);
        exit;
        end;
      end
    else if RightResolved.BaseType=btPointer then
      case Bin.OpCode of
      eopEqual,
      eopNotEqual,
      eopLessThan,
      eopGreaterThan,
      eopLessthanEqual,
      eopGreaterThanEqual:
        begin
        SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
        exit;
        end;
      end;
    end
  else if LeftResolved.BaseType=btContext then
    case Bin.OpCode of
    eopEqual,eopNotEqual:
      if RightResolved.BaseType=btNil then
        begin
        if (LeftResolved.TypeEl is TPasClassType) or (LeftResolved.TypeEl is TPasPointerType) then
          begin
          SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
          exit;
          end;
        end
      else if (CheckSrcIsADstType(LeftResolved,RightResolved,Bin)<>cIncompatible)
          or (CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible) then
        begin
        SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
        exit;
        end;
    eopIs:
      if (RightResolved.TypeEl=nil)
          or (RightResolved.IdentEl=nil) then
        RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
                 ['class type',BaseTypeNames[RightResolved.BaseType]],Bin)
      else if not (RightResolved.IdentEl is TPasType) then
        RaiseMsg(nXExpectedButYFound,sXExpectedButYFound,
                 ['class type',RightResolved.IdentEl.ElementTypeName],Bin)
      else if (CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible) then
        begin
        SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
        exit;
        end
      else
        RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],Bin);
    eopAs:
      if (CheckSrcIsADstType(RightResolved,LeftResolved,Bin)<>cIncompatible) then
        begin
        SetResolverTypeExpr(ResolvedEl,btContext,RightResolved.TypeEl,Bin);
        exit;
        end
      else
        RaiseMsg(nTypesAreNotRelated,sTypesAreNotRelated,[],Bin);
    eopSubIdent:
      begin
      ResolvedEl:=RightResolved;
      exit;
      end;
    end
  else if LeftResolved.BaseType=btNil then
    case Bin.OpCode of
    eopEqual,eopNotEqual:
      case RightResolved.BaseType of
      btNil,btPointer:
        begin
        SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
        exit;
        end;
      btContext:
        if (RightResolved.TypeEl is TPasClassType)
        or (RightResolved.TypeEl is TPasPointerType) then
          begin
          SetResolverTypeExpr(ResolvedEl,btBoolean,nil,Bin);
          exit;
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
  RaiseMsg(nIllegalQualifier,sIllegalQualifier,[OpcodeStrings[Bin.OpCode]],Bin);
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

constructor TPasResolver.Create;
begin
  inherited Create;
  FDefaultScope:=TPasDefaultScope.Create;
  FPendingForwards:=TFPList.Create;
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
  if (AParent=nil) and (FRootElement<>nil)
  and (AClass<>TPasUnresolvedTypeRef) then
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
  else if AClass=TPasUnresolvedTypeRef then
  else if (AClass=TPasAliasType)
      or (AClass=TPasArrayType)
      or (AClass=TPasProcedureType)
      or (AClass=TPasFunctionType) then
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
  Result:=FindFirstElement(AName,LastElement);
end;

function TPasResolver.FindFirstElement(const AName: String;
  ErrorPosEl: TPasElement): TPasElement;
var
  Data: TPRFindFirstData;
begin
  Result:=FindFirstElement(AName,Data,ErrorPosEl);
  if (Data.StartScope<>nil) and (Data.StartScope.ClassType=TPasWithExprScope)
      and TPasWithExprScope(Data.StartScope).NeedTmpVar then
    RaiseInternalError(20160923111727); // caller forgot to handle "With", use the other FindFirstElement instead
end;

function TPasResolver.FindFirstElement(const AName: String; out
  Data: TPRFindFirstData; ErrorPosEl: TPasElement): TPasElement;
var
  Abort: boolean;
begin
  //writeln('TPasResolver.FindIdentifier Name="',AName,'"');
  Result:=Nil;
  Abort:=false;
  Data:=Default(TPRFindFirstData);
  Data.ErrorPosEl:=ErrorPosEl;
  IterateElements(AName,@OnFindFirstElement,@Data,Abort);
  Result:=Data.Found;
  if Result<>nil then exit;
  RaiseIdentifierNotFound(AName,ErrorPosEl);
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

procedure TPasResolver.FinishScope(ScopeType: TPasScopeType; El: TPasElement);
begin
  case ScopeType of
  stModule: FinishModule(El as TPasModule);
  stUsesList: FinishUsesList;
  stTypeSection: FinishTypeSection(El as TPasDeclarations);
  stTypeDef: FinishTypeDef(El as TPasType);
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
    AddBuiltInProc('Length'); // ToDo
  if bfSetLength in BaseProcs then
    AddBuiltInProc('SetLength'); // ToDo
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

function TPasResolver.AddBuiltInProc(aName: shortstring): TResElDataBuiltInProc;
var
  El: TPasUnresolvedSymbolRef;
begin
  El:=TPasUnresolvedSymbolRef.Create(aName,nil);
  Result:=TResElDataBuiltInProc.Create;
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
  FindData: PPRFindFirstData): TResolvedReference;

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
  RecScope:=TPasRecordType(CurRecordType).CustomData as TPasRecordScope;
  Result:=TPasDotRecordScope.Create;
  Result.Owner:=Self;
  Result.RecordScope:=RecScope;
  PushScope(Result);
end;

function TPasResolver.PushDotScopeForType(TypeEl: TPasType; ErrorEl: TPasElement
  ): TPasSubScope;
begin
  if TypeEl.ClassType=TPasRecordType then
    Result:=PushRecordDotScope(TPasRecordType(TypeEl))
  else if TypeEl.ClassType=TPasClassType then
    Result:=PushClassDotScope(TPasClassType(TypeEl))
  else
    RaiseMsg(nExprTypeMustBeClassOrRecordTypeGot,sExprTypeMustBeClassOrRecordTypeGot,
      [TypeEl.ElementTypeName],ErrorEl);
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
  s:='['+IntToStr(64)+'] invalid scope for "'+GetObjName(El)+'": ';
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
    ParamCompatibility:=CheckParamCompatibility(Param,TPasArgument(ProcArgs[i]),i+1,RaiseOnError);
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
  if El=nil then exit;
  if (El.ClassType=TPasVariable)
  or (El.ClassType=TPasResultElement) then
    exit(true)
  else if (El.ClassType=TPasConst) then
    begin
    if (TPasConst(El).VarType<>nil) then
      exit(true); // typed const are writable
    end
  else if El.ClassType=TPasProperty then
    begin
    if GetPasPropertySetter(TPasProperty(El))<>nil then
      exit(true);
    if ErrorOnFalse then
      RaiseMsg(nPropertyNotWritable,sPropertyNotWritable,[],ErrorEl);
    end
  else if El is TPasArgument then
    begin
    if TPasArgument(El).Access in [argDefault, argVar, argOut] then exit(true);
    end;
  if ErrorOnFalse then
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
      exit(CheckCustomTypeCompatibility(RHS,LHS,ErrorEl))
    else
      exit(cExact); // same base type, maybe not same type name (e.g. longint and integer)
    end
  else if (LHS.BaseType in btAllInteger)
      and (RHS.BaseType in btAllInteger) then
    exit(cExact+1) // ToDo: range check for Expr
  else if (LHS.BaseType in btAllBooleans)
      and (RHS.BaseType in btAllBooleans) then
    exit(cExact+1)
  else if (LHS.BaseType in btAllStrings)
      and (RHS.BaseType in btAllStrings) then
    exit(cExact+1) // ToDo: check Expr if Param=btChar/btWideChar
  else if (LHS.BaseType in btAllFloats)
      and (RHS.BaseType in btAllFloats) then
    exit(cExact+1)
  else if RHS.BaseType=btNil then
    begin
      if LHS.BaseType=btPointer then
        exit(cExact)
      else if LHS.BaseType=btContext then
        begin
        if (LHS.TypeEl is TPasClassType)
            or (LHS.TypeEl is TPasPointerType) then
          exit(cExact);
        end
      else
        exit(cIncompatible);
    end
  else
    exit(cIncompatible);
  RaiseNotYetImplemented(20160922163634,ErrorEl,'LHS='+GetResolverResultDesc(LHS)+' RHS='+GetResolverResultDesc(RHS));
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
  if El.VarType<>nil then exit; // a a property with a type has no ancestor
  if El.CustomData=nil then exit;
  Result:=TPasProperty(TResolvedReference(El.CustomData).Declaration);
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

  function ExprCanBeVarParam: boolean;
  begin
    Result:=false;
    if ExprResolved.IdentEl=nil then exit;
    if ExprResolved.IdentEl.ClassType=TPasVariable then exit(true);
    if (ExprResolved.IdentEl.ClassType=TPasConst)
        and (TPasConst(ExprResolved.IdentEl).VarType<>nil) then
      exit(true); // typed const are writable
  end;

var
  MustFitExactly: Boolean;
  ComputeFlags: TPasResolverComputeFlags;
begin
  Result:=cIncompatible;

  ComputeFlags:=[];
  MustFitExactly:=Param.Access in [argVar, argOut];

  ComputeElement(Expr,ExprResolved,ComputeFlags);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Expr=',GetTreeDesc(Expr,2),' ResolvedExpr=',GetResolverResultDesc(ExprResolved));
  {$ENDIF}

  if MustFitExactly then
    begin
    // Expr must be a variable
    if not ExprCanBeVarParam then
      begin
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
        [ParamNo,GetTypeDesc(ExprResolved.TypeEl),GetTypeDesc(ParamResolved.TypeEl)],
        Expr);
    exit(cIncompatible);
    end;

  Result:=CheckAssignCompatibility(ParamResolved,ExprResolved,Expr);
end;

function TPasResolver.CheckCustomTypeCompatibility(const SrcType,
  DestType: TPasResolverResult; ErrorEl: TPasElement): integer;
var
  SrcTypeEl, DstTypeEl: TPasType;
begin
  if (SrcType.TypeEl=nil) then
    RaiseInternalError(20160922163645);
  if (DestType.TypeEl=nil) then
    RaiseInternalError(20160922163648);
  SrcTypeEl:=SrcType.TypeEl;
  DstTypeEl:=DestType.TypeEl;

  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.CheckCustomTypeCompatibility SrcTypeEl=',GetObjName(SrcTypeEl),' DstTypeEl=',GetObjName(DstTypeEl));
  {$ENDIF}
  if SrcTypeEl.ClassType=TPasClassType then
    begin
    if DstTypeEl.ClassType=TPasClassType then
      exit(CheckSrcIsADstType(SrcType,DestType,ErrorEl))
    else
      RaiseNotYetImplemented(20160922163652,ErrorEl);
    end
  else
    RaiseNotYetImplemented(20160922163654,ErrorEl);
end;

procedure TPasResolver.ComputeElement(El: TPasElement; out
  ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags);
var
  DeclEl: TPasElement;
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
        if El.CustomData is TResolvedReference then
          ComputeElement(TResolvedReference(El.CustomData).Declaration,ResolvedEl,Flags)
        else
          RaiseNotYetImplemented(20160922163658,El);
        end;
      pekNumber:
        // ToDo: check if btByte, btSmallInt, btSingle, ...
        if Pos('.',TPrimitiveExpr(El).Value)>0 then
          SetResolverTypeExpr(ResolvedEl,btDouble,FBaseTypes[btDouble],TPrimitiveExpr(El))
        else
          SetResolverTypeExpr(ResolvedEl,btLongint,FBaseTypes[btLongint],TPrimitiveExpr(El));
      pekString:
        SetResolverTypeExpr(ResolvedEl,btString,FBaseTypes[btString],TPrimitiveExpr(El));
      pekNil:
        SetResolverTypeExpr(ResolvedEl,btNil,FBaseTypes[btNil],TPrimitiveExpr(El));
      pekBoolConst:
        SetResolverTypeExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],TPrimitiveExpr(El));
    else
      RaiseNotYetImplemented(20160922163701,El);
    end;
    end
  else if El.ClassType=TPasUnresolvedSymbolRef then
    begin
    // built-in type
    if El.CustomData is TResElDataBaseType then
      SetResolverTypeExpr(ResolvedEl,TResElDataBaseType(El.CustomData).BaseType,
        TPasUnresolvedSymbolRef(El),nil)
    else
      RaiseNotYetImplemented(20160926194756,El);
    end
  else if El.ClassType=TBinaryExpr then
    ComputeBinaryExpr(TBinaryExpr(El),ResolvedEl,Flags)
  else if El.ClassType=TPasAliasType then
    begin
    // e.g. 'var a: b' -> compute b
    ComputeElement(TPasAliasType(El).DestType,ResolvedEl,Flags);
    ResolvedEl.IdentEl:=El;
    end
  else if (El.ClassType=TPasTypeAliasType) then
    begin
    // e.g. 'type a = type b;' -> compute b
    if rcSkipTypeAlias in Flags then
      begin
      ComputeElement(TPasTypeAliasType(El).DestType,ResolvedEl,Flags);
      ResolvedEl.IdentEl:=El;
      end
    else
      SetResolverIdentifier(ResolvedEl,btContext,El,TPasAliasType(El));
    end
  else if (El.ClassType=TPasVariable) or (El.ClassType=TPasConst) then
    begin
    // e.g. 'var a:b' -> compute b, use a as IdentEl
    ComputeElement(TPasVariable(El).VarType,ResolvedEl,Flags);
    ResolvedEl.IdentEl:=El;
    end
  else if (El.ClassType=TPasProperty) then
    begin
    ComputeElement(GetPasPropertyType(TPasProperty(El)),ResolvedEl,Flags);
    ResolvedEl.IdentEl:=El;
    end
  else if El.ClassType=TPasArgument then
    begin
    if TPasArgument(El).ArgType=nil then
      // untyped parameter
      SetResolverIdentifier(ResolvedEl,btUntyped,El,nil)
    else
      begin
      // typed parameter -> use param as IdentEl, compute type
      ComputeElement(TPasArgument(El).ArgType,ResolvedEl,Flags);
      ResolvedEl.IdentEl:=El;
      end;
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
                    ResolvedEl.TypeEl,ResolvedEl.TypeEl);
    end
  else if El.ClassType=TPasRecordType then
    SetResolverIdentifier(ResolvedEl,btContext,El,TPasRecordType(El))
  else if El.ClassType=TUnaryExpr then
    begin
    ComputeElement(TUnaryExpr(El).Operand,ResolvedEl,Flags);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.ComputeElement Unary Kind=',TUnaryExpr(El).Kind,' OpCode=',TUnaryExpr(El).OpCode,' OperandResolved=',GetResolverResultDesc(ResolvedEl));
    {$ENDIF}
    case TUnaryExpr(El).OpCode of
      eopAdd, eopSubtract:
        if ResolvedEl.BaseType in btAllInteger then
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
    ComputeElement(TPasResultElement(El).ResultType,ResolvedEl,Flags);
    ResolvedEl.IdentEl:=El;
    end
  else if El is TPasModule then
    SetResolverIdentifier(ResolvedEl,btModule,El,nil)
  else if El is TNilExpr then
    SetResolverTypeExpr(ResolvedEl,btNil,FBaseTypes[btNil],TNilExpr(El))
  else if El is TSelfExpr then
    begin
    ComputeElement(TResolvedReference(El.CustomData).Declaration,ResolvedEl,Flags);
    ResolvedEl.IdentEl:=El;
    end
  else if El is TBoolConstExpr then
    SetResolverTypeExpr(ResolvedEl,btBoolean,FBaseTypes[btBoolean],TBoolConstExpr(El))
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
        writeln('TPasResolver.CheckSrcIsADstType AAA1 SrcEl=',GetObjName(SrcEl));
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
function TPasIdentifierScope.InternalFindIdentifier(const Identifier: String
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
  Result:=InternalFindIdentifier(Identifier);
  {$IFDEF VerbosePasResolver}
  if (Result<>nil) and (Result.Owner<>Self) then
    begin
    writeln('TPasIdentifierScope.FindIdentifier Result.Owner<>Self Owner='+GetObjName(Result.Owner));
    raise Exception.Create('20160925184159 ');
    end;
  {$ENDIF}
end;

function TPasIdentifierScope.RemoveIdentifier(El: TPasElement): boolean;
var
  Identifier, PrevIdentifier: TPasIdentifier;
  LoName: ShortString;
begin
  LoName:=lowercase(El.Name);
  Identifier:=TPasIdentifier(FItems.Find(LoName));
  InternalFindIdentifier(El.Name);
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
  Item:=InternalFindIdentifier(aName);
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

