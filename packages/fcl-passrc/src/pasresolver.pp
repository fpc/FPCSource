{ **********************************************************************
    This file is part of the Free Component Library
    Copyright (c) 2020  Mattias Gaertner  mattias@freepascal.org

    Pascal resolver - main unit file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ********************************************************************** }
  
{
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
  - check duplicate values
- try..finally..except, on, else, raise
- for loop
  - fail to write a loop var inside the loop
- spot duplicates
- type cast base types
- char
  - ord(), chr()
- record
  - variants
  - const param makes children const too
  - const  TRecordValues
  - function default(record type): record
  - advanced records:
    - $modeswitch AdvancedRecords
    - visibility public, private, strict private
    - sub type
    - const, var, class var
    - function/procedure/class function/class procedure
    - property, class property, default property
    - constructor
    - RTTI
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
  - events, proc type of object
  - sealed
  - $M+ / $TYPEINFO use visPublished as default visibility
  - note: constructing class with abstract method
- with..do
- enums - TPasEnumType, TPasEnumValue
  - propagate to parent scopes
  - function ord(): integer
  - function low(ordinal): ordinal
  - function high(ordinal): ordinal
  - function pred(ordinal): ordinal
  - function high(ordinal): ordinal
  - cast integer to enum, enum to integer
  - $ScopedEnums
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
  - static array range checking
  - const array of char = string
  - a:=[...]   // assignation using constant array
  - a:=[[...],[...]]
  - a:=[...]+[...]  a+[]  []+a   modeswitch arrayoperators
  - delphi: var a: dynarray = [];  // square bracket initialization
- check if var initexpr fits vartype: var a: type = expr;
- built-in functions high, low for range types
- procedure type
  - call
  - as function result
  - as parameter
  - Delphi without @
  - @@ operator
  - FPC equal and not equal
  - "is nested"
  - bark on arguments access mismatch
- function without params: mark if call or address, rrfImplicitCallWithoutParams
- procedure break, procedure continue
- built-in functions pred, succ for range type and enums
- untyped parameters
- built-in procedure str(const boolean|integer|enumvalue|classinstance,var s: string)
- built-in procedure writestr(var s: string; Args: arguments...); varargs
- pointer TPasPointerType
  - nil, assigned(), typecast, class, classref, dynarray, procvar
  - forward declaration
  - cycle detection
  - TypedPointer^, (@Some)^
  - = operator: TypedPointer, @Some, UntypedPointer
  - TypedPointer:=TypedPointer
  - TypedPointer:=@Some
  - pointer[index], (@i)[index]
  - dispose(pointerofrecord), new(pointerofrecord)
  - $PointerMath on|off
- emit hints
  - platform, deprecated, experimental, library, unimplemented
  - hiding ancestor method
  - hiding other unit identifier
- dotted unitnames
- eval:
  - nil, true, false
  - range checking:
  - integer ranges
  - boolean ranges
  - enum ranges
  - char ranges
  - +, -, *, div, mod, /, shl, shr, or, and, xor, in, ^^, ><
  - =, <>, <, <=, >, >=
  - ord(), low(), high(), pred(), succ(), length()
  - string[index]
  - call(param)
  - a:=value
  - arr[index]
- resourcestrings
- custom ranges
  - enum: low(), high(), pred(), succ(), ord(), rg(int), int(rg), enum:=rg,
    rg:=rg, rg1:=rg2, rg:=enum, =, <>, in
    array[rg], low(array), high(array)
- for..in..do :
  - type boolean, char, byte, shortint, word, smallint, longword, longint
  - type enum range, char range, integer range
  - type/var set of: enum, enum range, integer, integer range, char, char range
  - array var
  - function: enumerator
  - class
- var modifier 'absolute'
- Assert(bool[,string])
- interfaces
  - $interfaces com|corba|default
  - root interface for com: delphi: IInterface, objfpc: IUnknown
  - method resolution
  - delegation via property implements: intftype, classtype
  - IntfVar as IntfType, intfvar as classtype, ObjVar as IntfType
  - IntfVar is IntfType, intfvar is classtype, ObjVar is IntfType
  - intftype(ObjVar), classtype(IntfVar)
  - default property
  - visibility public
  - $M+
  - class interfaces, check duplicates
  - assigned()
  - IntfVar:=nil, IntfVar:=IntfVar, IntfVar:=ObjVar, ObjVar:=IntfVar
  - IntfVar=IntfVar2
- currency
  - eval type TResEvalCurrency
  - eval +, -, *, /, ^^
  - float*currency and currency*float computes to currency
- type alias type overloads
- $writeableconst off $J-
- $warn identifier ON|off|error|default
- anonymous methods:
  - assign in proc and program begin and initialization   p:=procedure begin end
  - pass as arg  doit(procedure begin end)
  - modifiers  assembler varargs cdecl
  - typecast
  - with
  - self
- built-in procedure Val(const s: string; var e: enumtype; out Code: integertype);
- intrinsic functions Lo and Hi, depending on $mode (ObjFPC or Delphi):
  - In $MODE DELPHI:
    function Lo/Hi(i: <any integer type>): Byte
  - In $MODE OBJFPC:
    function Lo/Hi(i: Byte/ShortInt/Word/SmallInt): Byte
    function Lo/Hi(i: LongWord/LongInt/UIntSingle/IntSingle): Word
    function Lo/Hi(i: QWord/Int64/UIntDouble/IntDouble): LongWord
- helpers:
  - class
  - record
  - type helper for simple type variables
  - InterfaceHelpers for fast gathering of helpers from uses sections
  - "inherited" and "inherited name" for Delphi and ObjFPC
  - for i in typehelped
  - nested: type, const, class var
  - visibility
  - property
  - helper method, Self as var argument
- generics
- array of const
- attributes

ToDo:
- operator overload
   - operator enumerator
   - binaryexpr
   - advanced records
- Include/Exclude for set of int/char/bool
- error if property method resolution is not used
- $H-hintpos$H+
- $pop, $push
- $RTTI inherited|explicit
- range checking:
  - property defaultvalue
  - IntSet:=[-1]
  - CharSet:=[#13]
- proc: check if forward and impl default values match
- call array of proc without ()
- generics, nested param lists
- object
- futures
- TPasFileType
- labels
- $zerobasedstrings on|off
- FOR_LOOP_VAR_VARPAR  passing a loop var to a var parameter gives a warning
- FOR_VARIABLE  warning if using a global var as loop var
- COMPARISON_FALSE COMPARISON_TRUE Comparison always evaluates to False
- USE_BEFORE_DEF Variable '%s' might not have been initialized
- FOR_LOOP_VAR_UNDEF FOR-Loop variable '%s' may be undefined after loop
- TYPEINFO_IMPLICITLY_ADDED Published caused RTTI ($M+) to be added to type '%s'
- IMPLICIT_STRING_CAST Implicit string cast from '%s' to '%s'
- IMPLICIT_STRING_CAST_LOSS Implicit string cast with potential data loss from '%s' to '%s'
- off by default: EXPLICIT_STRING_CAST Explicit string cast from '%s' to '%s'
- off by default: EXPLICIT_STRING_CAST_LOSS Explicit string cast with potential data loss from '%s' to '%s'
- IMPLICIT_INTEGER_CAST_LOSS Implicit integer cast with potential data loss from '%s' to '%s'
- IMPLICIT_CONVERSION_LOSS Implicit conversion may lose significant digits from '%s' to '%s'
- COMBINING_SIGNED_UNSIGNED64 Combining signed type and unsigned 64-bit type - treated as an unsigned type
-

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
{$IFNDEF FPC_DOTTEDUNITS}
unit PasResolver;
{$ENDIF FPC_DOTTEDUNITS}

{$i fcl-passrc.inc}

{$IFOPT Q+}{$DEFINE OverflowCheckOn}{$ENDIF}
{$IFOPT R+}{$DEFINE RangeCheckOn}{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  {$ifdef pas2js}
  js,
  {$IFDEF NODEJS}
  Node.FS,
  {$ENDIF}
  {$endif}
  System.Classes, System.SysUtils, System.Math, System.Types, System.Contnrs,
  Pascal.Tree, Pascal.Scanner, Pascal.Parser, Pascal.ResolveEval;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$ifdef pas2js}
  js,
  {$IFDEF NODEJS}
  Node.FS,
  {$ENDIF}
  {$endif}
  Classes, SysUtils, Math, Types, contnrs,
  PasTree, PScanner, PParser, PasResolveEval;
{$ENDIF FPC_DOTTEDUNITS}

const
  ParserMaxEmbeddedColumn = 2048;
  ParserMaxEmbeddedRow = $7fffffff div ParserMaxEmbeddedColumn;
  po_Resolver = [
    po_ResolveStandardTypes,
    po_NoOverloadedProcs,
    po_KeepClassForward,
    po_ArrayRangeExpr,
    po_CheckCondFunction,
    po_CheckDirectiveRTTI];

type
  TResolverBaseType = (
    btNone,        // undefined
    btCustom,      // provided by descendant resolver
    btContext,     // any source declared type with LoTypeEl/HiTypeEl
    btModule,
    btUntyped,     // TPasArgument without ArgType
    btChar,        // char
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar,    // ansichar
    {$endif}
    btWideChar,    // widechar
    btString,      // string
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiString,  // ansistring
    btShortString, // shortstring
    btRawByteString, // rawbytestring
    {$endif}
    btWideString,  // widestring
    btUnicodeString,// unicodestring
    btSingle,      // single  1.5E-45..3.4E38, digits 7-8, bytes 4
    btDouble,      // double  5.0E-324..1.7E308, digits 15-16, bytes 8
    btExtended,    // extended  platform, double or 1.9E-4932..1.1E4932, digits 19-20, bytes 10
    btCExtended,   // cextended
    btCurrency,    // as int64 div 10000, float, not ordinal
    btBoolean,     // boolean
    btByteBool,    // bytebool  true=not zero
    btWordBool,    // wordbool  true=not zero
    btLongBool,    // longbool  true=not zero
    {$ifdef HasInt64}
    btQWordBool,   // qwordbool true=not zero
    {$endif}
    btByte,        // byte  0..255
    btShortInt,    // shortint -128..127
    btWord,        // word  unsigned 2 bytes
    btSmallInt,    // smallint signed 2 bytes
    btUIntSingle,  // unsigned integer range of single 22bit
    btIntSingle,   // integer range of single  23bit
    btLongWord,    // longword unsigned 4 bytes
    btLongint,     // longint  signed 4 bytes
    btUIntDouble,  // unsigned integer range of double 52bit
    btIntDouble,   // integer range of double  53bit
    {$ifdef HasInt64}
    btQWord,       // qword   0..18446744073709551615, bytes 8
    btInt64,       // int64   -9223372036854775808..9223372036854775807, bytes 8
    btComp,        // as Int64, not ordinal
    {$endif}
    btPointer,     // pointer  or canonical pointer (e.g. @something)
    {$ifdef fpc}
    btFile,        // file
    btText,        // text
    btVariant,     // variant
    {$endif}
    btNil,         // nil = pointer, class, procedure, method, ...
    btProc,        // TPasProcedure
    btBuiltInProc, // TPasUnresolvedSymbolRef with CustomData is TResElDataBuiltInProc
    btArrayProperty,// IdentEl is TPasProperty with Args.Count>0, LoTypeEl=nil
    btSet,         // set of '', see SubType
    btArrayLit,    // []  array literal (TParamsExpr, TArrayValues, TBinaryExpr), see SubType
    btArrayOrSet,  // []  can be set or array literal, see SubType
    btRange        // a..b  see SubType
    );
  TResolveBaseTypes = set of TResolverBaseType;
const
  btIntMax = {$ifdef HasInt64}btInt64{$else}btIntDouble{$endif};
  btUIntMax = {$ifdef HasInt64}btQWord{$else}btUIntDouble{$endif};
  btAllInteger = [btByte,btShortInt,btWord,btSmallInt,btIntSingle,btUIntSingle,
    btLongWord,btLongint,btIntDouble,btUIntDouble
    {$ifdef HasInt64}
    ,btQWord,btInt64,btComp
    {$endif}];
  btAllIntegerNoQWord = btAllInteger{$ifdef HasInt64}-[btQWord]{$endif};
  btAllSignedInteger = [btShortInt,btSmallInt,btIntSingle,btLongint,btIntDouble
    {$ifdef HasInt64}
    ,btInt64,btComp
    {$endif}];
  btAllChars = [btChar,{$ifdef FPC_HAS_CPSTRING}btAnsiChar,{$endif}btWideChar];
  btAllStrings = [btString,
    {$ifdef FPC_HAS_CPSTRING}btAnsiString,btShortString,btRawByteString,{$endif}
    btWideString,btUnicodeString];
  btAllStringAndChars = btAllStrings+btAllChars;
  btAllStringPointer = [btString,
    {$ifdef FPC_HAS_CPSTRING}btAnsiString,btRawByteString,{$endif}
    btWideString,btUnicodeString];
  btAllFloats = [btSingle,btDouble,
    btExtended,btCExtended,btCurrency];
  btAllBooleans = [btBoolean,btByteBool,btWordBool,btLongBool
    {$ifdef HasInt64},btQWordBool{$endif}];
  btArrayRangeTypes = btAllChars+btAllBooleans+btAllInteger;
  btAllRanges = btArrayRangeTypes+[btRange];
  btAllWithSubType = [btSet, btArrayLit, btArrayOrSet, btRange];
  btAllIntrinsicTypes = btAllInteger+btAllStringAndChars+btAllFloats+btAllBooleans;
  btAllFPCTypes = [
    btChar,
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiChar,
    {$endif}
    btWideChar,
    btString,
    {$ifdef FPC_HAS_CPSTRING}
    btAnsiString,
    btShortString,
    btRawByteString,
    {$endif}
    btWideString,
    btUnicodeString,
    btSingle,
    btDouble,
    btExtended,
    btCExtended,
    btCurrency,
    btBoolean,
    btByteBool,
    btWordBool,
    btLongBool,
    {$ifdef HasInt64}
    btQWordBool,
    {$endif}
    btByte,
    btShortInt,
    btWord,
    btSmallInt,
    btLongWord,
    btLongint,
    {$ifdef HasInt64}
    btQWord,
    btInt64,
    btComp,
    {$endif}
    btPointer
    {$ifdef fpc}
    ,btFile,
    btText,
    btVariant
    {$endif}
    ];

  ResBaseTypeNames: array[TResolverBaseType] of string =(
    'None',
    'Custom',
    'Context',
    'Module',
    'Untyped',
    'Char',
    {$ifdef FPC_HAS_CPSTRING}
    'AnsiChar',
    {$endif}
    'WideChar',
    'String',
    {$ifdef FPC_HAS_CPSTRING}
    'AnsiString',
    'ShortString',
    'RawByteString',
    {$endif}
    'WideString',
    'UnicodeString',
    'Single',
    'Double',
    'Extended',
    'CExtended',
    'Currency',
    'Boolean',
    'ByteBool',
    'WordBool',
    'LongBool',
    {$ifdef HasInt64}
    'QWordBool',
    {$endif}
    'Byte',
    'ShortInt',
    'Word',
    'SmallInt',
    'UIntSingle',
    'IntSingle',
    'LongWord',
    'Longint',
    'UIntDouble',
    'IntDouble',
    {$ifdef HasInt64}
    'QWord',
    'Int64',
    'Comp',
    {$endif}
    'Pointer',
    {$ifdef fpc}
    'File',
    'Text',
    'Variant',
    {$endif}
    'Nil',
    'Procedure/Function',
    'BuiltInProc',
    'array property',
    'set',
    'array',
    'set or array literal',
    'range..'
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
    bfWriteStr,
    bfVal,
    bfLo,
    bfHi,
    bfConcatArray,
    bfConcatString,
    bfCopyArray,
    bfInsertArray,
    bfDeleteArray,
    bfTypeInfo,
    bfGetTypeKind,
    bfAssert,
    bfNew,
    bfDispose,
    bfDefault,
    bfNameOf,
    bfIsConstValue,
    // Const-eval intrinsics for the native target; registered only by
    // TPasNativeResolver, left unregistered (inert) in the base/pas2js setup.
    bfSizeOf,
    bfBitSizeOf,
    bfTrunc,
    bfRound,
    // native-target string Copy (Copy(s,start,count)); registered only by
    // TPasNativeResolver, inert in the base/pas2js setup (pas2js has no string Copy)
    bfCopyString,
    // native-target Slice(arr,count) intrinsic (open-array view of the first
    // `count` elements); registered only by TPasNativeResolver, inert elsewhere.
    bfSlice
    );
  TResolverBuiltInProcs = set of TResolverBuiltInProc;
const
  ResolverBuiltInProcNames: array[TResolverBuiltInProc] of string = (
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
    'WriteStr',
    'Val',
    'Lo',
    'Hi',
    'Concat',
    'Concat',
    'Copy',
    'Insert',
    'Delete',
    'TypeInfo',
    'GetTypeKind',
    'Assert',
    'New',
    'Dispose',
    'Default',
    'NameOf',
    'IsConstValue',
    'SizeOf',
    'BitSizeOf',
    'Trunc',
    'Round',
    'Copy',
    'Slice'
    );
  bfAllStandardProcs = [Succ(bfCustom)..high(TResolverBuiltInProc)];

const
  ResolverResultVar = 'Result';

type
  {$ifdef pas2js}
  TPasResIterate = procedure(Item, Arg: pointer) of object;

  { TPasResHashList }

  TPasResHashList = class
  private
    FItems: TJSObject;
  public
    constructor Create; reintroduce;
    procedure Add(const aName: string; Item: Pointer);
    function Find(const aName: string): Pointer;
    procedure ForEachCall(const Proc: TPasResIterate; Arg: Pointer);
    procedure Clear;
    procedure Remove(const aName: string);
  end;
  {$else}
  TPasResHashList = TFPHashList;
  {$endif}

type

  { EPasResolve }

  EPasResolve = class(Exception)
  private
    FPasElement: TPasElement;
    procedure SetPasElement(AValue: TPasElement);
  public
    Id: TMaxPrecInt;
    MsgType: TMessageType;
    MsgNumber: integer;
    MsgPattern: String;
    Args: TMessageArgs;
    SourcePos: TPasSourcePos;
    destructor Destroy; override;
    property PasElement: TPasElement read FPasElement write SetPasElement; // can be nil!
  end;

type

  { TUnresolvedPendingRef }

  TUnresolvedPendingRef = class(TPasUnresolvedSymbolRef)
  public
    Element: TPasType; // TPasClassOfType or TPasPointerType
  end;

  { TPasSpecializeTypeData - CustomData of TPasSpecializeType
    for the generic type see TPasSpecializeType(Element).DestType }

  TPasSpecializeTypeData = Class(TResolveData)
  public
    SpecializedType: TPasGenericType;
  end;

  TPRSpecializeStep = (
    prssNone,
    prssInterfaceBuilding,
    prssInterfaceFinished,
    prssImplementationBuilding,
    prssImplementationFinished
    );

  { TPRSpecializedItem }

  TPRSpecializedItem = class
  private
    FSpecializedEl: TPasElement;
  public
    GenericEl: TPasElement;
    Index: integer;
    Step: TPRSpecializeStep; // how much of the specialized element has been created
    // The method bodies are owed: the generic has an implementation to copy,
    // and it has not been copied yet. Lives on the ITEM, not on a resolver's
    // list - each unit has its own resolver and specializations are shared, so
    // the resolver that needs the code is often not the one that owed it.
    ImplOwed: boolean;
    FirstSpecialize: TPasElement;
    Params: TPasTypeArray;
    ConstExprs: array of TPasExpr; // nil for type params, expression for const params
    SyntheticConsts: TObjectList; // owns synthetic TPasConst elements
    SpecializedConstraints: TPasElementArray;
    destructor Destroy; override;
    property SpecializedEl: TPasElement read FSpecializedEl;
  end;

  { TPRSpecializedTypeItem }

  TPRSpecializedTypeItem = class(TPRSpecializedItem)
  private
    FSpecializedType: TPasGenericType;
    procedure SetSpecializedType(AValue: TPasGenericType);
  public
    HeaderScope: TObject; // TPasScope
    ImplProcs: TFPList; // list of TPasProcedure
    destructor Destroy; override;
    property SpecializedType: TPasGenericType read FSpecializedType write SetSpecializedType;
  end;

  { TPRSpecializedProcItem }

  TPRSpecializedProcItem = class(TPRSpecializedItem)
  private
    FSpecializedProc: TPasProcedure;
    procedure SetSpecializedProc(const AValue: TPasProcedure);
  public
    ImplProc: TPasProcedure; // <>SpecializedProc, can be nil
    destructor Destroy; override;
    property SpecializedProc: TPasProcedure read FSpecializedProc write SetSpecializedProc;
  end;

  TPSRefAccess = (
    psraNone,
    psraRead,
    psraWrite,
    psraReadWrite,
    psraWriteRead,
    psraTypeInfo
    );

  { TPasScopeReference }

  TPasScopeReference = class
  private
    FElement: TPasElement;
    procedure SetElement(const AValue: TPasElement);
  public
    {$IFDEF VerbosePasResolver}
    Owner: TObject;
    {$ENDIF}
    Access: TPSRefAccess;
    NextSameName: TPasScopeReference;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
  end;

  TPasScope = class;

  { TPasScopeReferences - used by TPasAnalyzer to store references of a proc or initialization section }

  TPasScopeReferences = class
  private
    FScope: TPasScope;
    procedure OnClearItem(Item, Dummy: pointer);
    procedure OnCollectItem(Item, aList: pointer);
  public
    References: TPasResHashList; // hash list of TPasScopeReference
    constructor Create(aScope: TPasScope);
    destructor Destroy; override;
    procedure Clear;
    function Add(El: TPasElement; Access: TPSRefAccess): TPasScopeReference;
    function Find(const aName: string): TPasScopeReference;
    function GetList: TFPList;
    property Scope: TPasScope read FScope;
  end;

  TIterateScopeElement = procedure(El: TPasElement; ElScope, StartScope: TPasScope;
    Data: Pointer; var Abort: boolean) of object;

  { TPasScope -
    Elements like TPasClassType use TPasScope descendants as CustomData for
    their sub identifiers.
    TPasResolver.Scopes has a stack of TPasScope for searching identifiers.
    }

  TPasScope = Class(TResolveData)
  public
    VisibilityContext: TPasElement; // used to check if the current context
                             // is allowed to access a private/protected element
    class function IsStoredInElement: boolean; virtual;
    class function FreeOnPop: boolean; virtual;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure WriteIdentifiers(Prefix: string); virtual;
  end;
  TPasScopeClass = class of TPasScope;
  TPasScopeArray = array of TPasScope;

  TPasModuleScopeFlag = (
    pmsfAssertSearched, // assert constructors searched
    pmsfRangeErrorNeeded, // somewhere is range checking on
    pmsfRangeErrorSearched // ERangeError constructor searched
    );
  TPasModuleScopeFlags = set of TPasModuleScopeFlag;

  { TPasModuleScope }

  TPasModuleScope = class(TPasScope)
  private
    FAssertClass: TPasClassType;
    FAssertDefConstructor: TPasConstructor;
    FAssertMsgConstructor: TPasConstructor;
    FRangeErrorClass: TPasClassType;
    FRangeErrorConstructor: TPasConstructor;
    FSystemTVarRec: TPasRecordType;
    procedure SetAssertClass(const AValue: TPasClassType);
    procedure SetAssertDefConstructor(const AValue: TPasConstructor);
    procedure SetAssertMsgConstructor(const AValue: TPasConstructor);
    procedure SetRangeErrorClass(const AValue: TPasClassType);
    procedure SetRangeErrorConstructor(const AValue: TPasConstructor);
    procedure SetSystemTVarRec(const AValue: TPasRecordType);
  public
    FirstName: string; // the 'unit1' in 'unit1', or 'ns' in 'ns.unit1'
    PendingResolvers: TFPList; // list of TPasResolver waiting for the unit interface
    Flags: TPasModuleScopeFlags;
    BoolSwitches: TBoolSwitches;
    constructor Create; override;
    destructor Destroy; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    property AssertClass: TPasClassType read FAssertClass write SetAssertClass;
    property AssertDefConstructor: TPasConstructor read FAssertDefConstructor write SetAssertDefConstructor;
    property AssertMsgConstructor: TPasConstructor read FAssertMsgConstructor write SetAssertMsgConstructor;
    property RangeErrorClass: TPasClassType read FRangeErrorClass write SetRangeErrorClass;
    property RangeErrorConstructor: TPasConstructor read FRangeErrorConstructor write SetRangeErrorConstructor;
    property SystemTVarRec: TPasRecordType read FSystemTVarRec write SetSystemTVarRec;
  end;
  TPasModuleScopeClass = class of TPasModuleScope;

  TPasIdentifierKind = (
    pikNone, // not yet initialized
    pikBaseType, // e.g. longint
    pikBuiltInProc,  // e.g. High(), SetLength()
    pikSimple, // simple vars, consts, types, enums
    pikProc, // may need parameter list with round brackets
    pikNamespace
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
  TPasIdentifierArray = array of TPasIdentifier;
  TPasVariableArray = array of TPasVariable;

  { TPasIdentifierScope - elements with a list of sub identifiers }

  TPasIdentifierScope = Class(TPasScope)
  private
    FItems: TPasResHashList; // hashlist of TPasIdentifier
    procedure InternalAdd(Item: TPasIdentifier);
    procedure OnClearItem(Item, Dummy: pointer);
    procedure OnCollectItem(Item, List: pointer);
  protected
    procedure OnWriteItem(Item, Dummy: pointer);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ClearIdentifiers(FreeItems: boolean);
    function FindLocalIdentifier(const Identifier: String): TPasIdentifier; inline;
    function FindIdentifier(const Identifier: String): TPasIdentifier; virtual;
    function RemoveLocalIdentifier(El: TPasElement): boolean; virtual;
    function AddIdentifier(const Identifier: String; El: TPasElement;
      const Kind: TPasIdentifierKind): TPasIdentifier; virtual;
    function FindElement(const aName: string): TPasElement;
    procedure IterateLocalElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean);
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    procedure WriteLocalIdentifiers(Prefix: string); virtual;
    function GetLocalIdentifiers: TFPList; virtual;
  end;
  TPasIdentifierScopeArray = array of TPasIdentifierScope;

  { TPasDefaultScope - root scope }

  TPasDefaultScope = class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasIterateFilterData }

  TPasIterateFilterData = record
    OnIterate: TIterateScopeElement;
    Data: Pointer;
  end;
  PPasIterateFilterData = ^TPasIterateFilterData;

  { TPRHelperEntry }

  TPRHelperEntry = class
  public
    Added: integer; // Added is bigger when it was added later to the list
    HelperForType: TPasType; // alias resolved
    Helper: TPasClassType;
  end;
  TPRHelperEntryArray = array of TPRHelperEntry;

  { TPasSectionScope - e.g. interface, implementation, program, library }

  TPasSectionScope = Class(TPasIdentifierScope)
  private
    procedure OnInternalIterate(El: TPasElement; ElScope, StartScope: TPasScope;
      Data: Pointer; var Abort: boolean);
  public
    UsesScopes: TFPList; // list of TPasSectionScope
    ImplUsesScopes: TFPList; // only on an interface scope: the UsesScopes of the implementation section, not owned
    UsesFinished: boolean;
    Finished: boolean;
    BoolSwitches: TBoolSwitches;
    ModeSwitches: TModeSwitches;
    Helpers: TPRHelperEntryArray; // only created for interface. Sorted ascending ComparePRHelperEntries
    constructor Create; override;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;
  TPasSectionScopeClass = class of TPasSectionScope;

  { TPasInitialFinalizationScope - e.g. TInitializationSection, TFinalizationSection }

  TPasInitialFinalizationScope = Class(TPasScope)
  public
    References: TPasScopeReferences; // created by TPasAnalyzer, not used by resolver
    function AddReference(El: TPasElement; Access: TPSRefAccess): TPasScopeReference;
    destructor Destroy; override;
  end;
  TPasInitialFinalizationScopeClass = class of TPasInitialFinalizationScope;

  { TPasEnumTypeScope }

  TPasEnumTypeScope = Class(TPasIdentifierScope)
  public
    CanonicalSet: TPasSetType;
    destructor Destroy; override;
  end;
  TPasEnumTypeScopeClass = class of TPasEnumTypeScope;

  { TPasGenericParamsScope - used during parsing TPasGenericTemplateType(s) }

  TPasGenericParamsScope = Class(TPasIdentifierScope)
  public
    GenericType: TPasGenericType;
  end;

  TPSGenericStep = (
    psgsNone,
    psgsInterfaceParsed,
    psgsImplementationParsed
    );

  { TPasGenericScope }

  TPasGenericScope = Class(TPasIdentifierScope)
  public
    // for generic type:
    SpecializedItems: TObjectList; // list of TPRSpecializedItem
    GenericStep: TPSGenericStep; // how much of the generic was parsed
    // for specialized type:
    SpecializedFromItem: TPRSpecializedItem;
    destructor Destroy; override;
  end;

  { TPasArrayScope }

  TPasArrayScope = Class(TPasGenericScope)
  public
  end;
  TPasArrayScopeClass = class of TPasArrayScope;

  { TPasProcTypeScope }

  TPasProcTypeScope = Class(TPasGenericScope)
  public
    BoolSwitches: TBoolSwitches; // captured at type declaration (funcref {$M+} RTTI)
  end;
  TPasProcTypeScopeClass = class of TPasProcTypeScope;

  { TPasClassOrRecordScope }


  TPasClassOrRecordScope = Class(TPasGenericScope)
  public
    DefaultProperty: TPasProperty;
    ClassConstructor: TPasClassConstructor;
    ClassDestructor: TPasClassDestructor;
  end;

  { TPasCompositionIdentifier - a member composed via record "contains" }

  TPasCompositionIdentifier = Class(TPasIdentifier)
  public
    Path: TPasVariableArray; // composition fields, from the composing record down to the record of Element
    Visibility: TPasMemberVisibility; // composed visibility, relative to the composing record
  end;

  { TPasRecordCompositionScope - the members a record composes via "contains",
    owned by the TPasRecordScope, Element is the composing record }

  TPasRecordCompositionScope = Class(TPasIdentifierScope)
  public
    Items: TFPList; // list of TPasCompositionIdentifier in order of adding, not owned
    constructor Create; override;
    destructor Destroy; override;
    class function IsStoredInElement: boolean; override;
    class function FreeOnPop: boolean; override;
    function AddComposition(const aName: string; El: TPasElement;
      const aPath: TPasVariableArray; aVisibility: TPasMemberVisibility): TPasCompositionIdentifier;
    function FindComposition(El: TPasElement): TPasCompositionIdentifier;
  end;

  { TPasRecordScope }

  TPasRecordScope = Class(TPasClassOrRecordScope)
  public
    CompositionScope: TPasRecordCompositionScope; // members composed via "contains", can be nil
    ComposedTemplate: TPasGenericTemplateType; // not nil if a generic template type is composed, members are unknown until specialization
    destructor Destroy; override;
  end;
  TPasRecordScopeClass = class of TPasRecordScope;

  TPasClassScopeFlag = (
    pcsfAncestorResolved,
    pcsfSealed,
    pcsfPublished, // default visibility is published due to $M directive
    pcsfDeferredAncestor // ancestor is a class-constrained generic template type
    );
  TPasClassScopeFlags = set of TPasClassScopeFlag;

  { TPasClassIntfMap }

  TPasClassIntfMap = class
  public
    Element: TPasElement;
    Intf: TPasClassType;
    Procs: TFPList;// maps Interface-member-index to TPasProcedure
    AncestorMap: TPasClassIntfMap;// AncestorMap.Element=Element, AncestorMap.Intf=DirectAncestor
    destructor Destroy; override;
  end;

  { TPasClassScope }

  TPasClassScope = Class(TPasClassOrRecordScope)
  public
    AncestorScope: TPasClassScope;
    CanonicalClassOf: TPasClassOfType;
    DirectAncestor: TPasType; // TPasClassType or TPasAliasType, see GetPasClassAncestor
      // Note: TPasClassType.AncestorType might be nil and DirectAncestor is "TObject"
    Flags: TPasClassScopeFlags;
    AbstractProcs: TArrayOfPasProcedure;
    Interfaces: TFPList; // list corresponds to TPasClassType(Element).Interfaces,
      // elements: TPasProperty for 'implements', or TPasClassIntfMap
    destructor Destroy; override;
  end;
  TPasClassScopeClass = class of TPasClassScope;

  { TPasGroupScope }

  TPasGroupScope = Class(TPasIdentifierScope)
  public
    Scopes: TPasIdentifierScopeArray;
    Count: integer;
    OnlyTypeMembers: boolean;
    procedure Add(Scope: TPasIdentifierScope);
    destructor Destroy; override;
    function GetFirstNonHelperScope: TPasIdentifierScope;
    class function IsStoredInElement: boolean; override;
    function FindAncestorIdentifier(const Identifier: String): TPasIdentifier;
    function FindAncestorElement(const Identifier: String): TPasElement;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  TPasProcedureScopeFlag = (
    ppsfIsGroupOverload, // mode objfpc: one overload is enough for all procs in same scope
    ppsfIsSpecialized,
    ppsfIsOverrideOverload
    );
  TPasProcedureScopeFlags = set of TPasProcedureScopeFlag;

  { TPasProcedureScope }

  TPasProcedureScope = Class(TPasGenericScope)
  public
    DeclarationProc: TPasProcedure; // the corresponding forward declaration
    ImplProc: TPasProcedure; // the corresponding proc with Body
    OverriddenProc: TPasProcedure; // the ancestor proc with same signature
    ClassRecScope: TPasClassOrRecordScope;
    GroupScope: TPasGroupScope; // set during parsing a method body
    NestedMembersScope: TPasGroupScope; // set during parsing a method body of a nested class
    SelfArg: TPasArgument;
    Flags: TPasProcedureScopeFlags;
    BoolSwitches: TBoolSwitches; // if Body<>nil then body start, otherwise when FinishProc
    ModeSwitches: TModeSwitches; // at proc start
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    function GetSelfScope: TPasProcedureScope; // get the next parent procscope with a classcope
    procedure WriteIdentifiers(Prefix: string); override;
    destructor Destroy; override;
  public
    References: TPasScopeReferences; // created by TPasAnalyzer in DeclarationProc
    function AddReference(El: TPasElement; Access: TPSRefAccess): TPasScopeReference;
    function GetReferences: TFPList;
  end;
  TPasProcedureScopeClass = class of TPasProcedureScope;

  { TPasPropertyScope }

  TPasPropertyScope = Class(TPasIdentifierScope)
  public
    AncestorProp: TPasProperty; { if TPasProperty(Element).VarType=nil this is an override
                                  otherwise it is a redeclaration }
    destructor Destroy; override;
  end;

  { TPasExceptOnScope }

  TPasExceptOnScope = Class(TPasIdentifierScope)
  end;

  TPasWithScope = class;

  TPasWithExprScopeFlag = (
    wesfNeedTmpVar,
    wesfOnlyTypeMembers,
    wesfIsClassOf,
    wesfConstParent, // not writable
    wesfDeferredTemplate // expr is a generic template type: defer body resolution
    );
  TPasWithExprScopeFlags = set of TPasWithExprScopeFlag;

  { TPasWithExprScope }

  TPasWithExprScope = Class(TPasScope)
  public
    WithScope: TPasWithScope; // owner
    Index: integer;
    Expr: TPasExpr;
    Scope: TPasGroupScope;
    ClassRecScope: TPasClassOrRecordScope;
    Flags: TPasWithExprScopeFlags;
    class function IsStoredInElement: boolean; override;
    class function FreeOnPop: boolean; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    destructor Destroy; override;
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

  { TPasForLoopScope }

  TPasForLoopScope = Class(TPasScope)
  public
    GetEnumerator: TPasFunction;
    MoveNext: TPasFunction;
    Current: TPasProperty;
    ForInFlattenDepth: Integer; // >1 when a for-in flattens leading array dimensions (multi-dim for-in)
  end;

  { TPasSubExprScope - base class for sub scopes aka dotted scopes }

  TPasSubExprScope = Class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasDotBaseScope }

  TPasDotBaseScope = Class(TPasSubExprScope)
  public
    GroupScope: TPasGroupScope;
    OnlyTypeMembers: boolean; // true=only class var/procs, false=default=all
    ConstParent: boolean;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    destructor Destroy; override;
  end;

  { TPasModuleDotScope - scope for searching unitname.<identifier> }

  TPasModuleDotScope = Class(TPasDotBaseScope)
  private
    FModule: TPasModule;
    procedure OnInternalIterate(El: TPasElement; ElScope, StartScope: TPasScope;
      Data: Pointer; var Abort: boolean);
    procedure SetModule(AValue: TPasModule);
  public
    ImplementationScope: TPasSectionScope;
    InterfaceScope: TPasSectionScope;
    SystemScope: TPasDefaultScope;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
    property Module: TPasModule read FModule write SetModule;
  end;

  { TPasDotEnumTypeScope - used for EnumType.EnumValue }

  TPasDotEnumTypeScope = Class(TPasDotBaseScope)
  public
    EnumScope: TPasEnumTypeScope;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasDotClassOrRecordScope }

  TPasDotClassOrRecordScope = Class(TPasDotBaseScope)
  public
    ClassRecScope: TPasClassOrRecordScope;
    { The type as WRITTEN, when the lookup had to borrow another type's scope -
      a specialization whose interface is still being built has none of its own.
      A constructor called through such a scope still constructs the written
      type, not the one that lent the scope. }
    WrittenType: TPasMembersType;
  end;

  { TPasDotClassScope - used for aClass.subidentifier }

  TPasDotClassScope = Class(TPasDotClassOrRecordScope)
  public
    IsClassOf: boolean; // true if aClassOf.
    {  Non-nil when this dot scope was pushed for a generic type parameter.
       a constructor called through it (T.Create) returns a T instance, 
       so member access on the result resolves via T's constraints. }
    TemplType: TPasGenericTemplateType;
  end;

  { TPasInheritedScope - used for inherited; and inherited Name() }

  TPasInheritedScope = Class(TPasDotClassOrRecordScope)
  public
    AncestorScope: TPasClassScope;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string; StartScope: TPasScope;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasDotHelperScope }

  TPasDotHelperScope = class(TPasDotBaseScope)
  end;

  TResolvedReferenceFlag = (
    rrfDotScope, // found reference via a dot scope (TPasDotBaseScope)
    rrfImplicitCallWithoutParams, // a TPrimitiveExpr is an implicit call without params
    rrfNoImplicitCallWithoutParams, // a TPrimitiveExpr is not an implicit call
    rrfNewInstance, // constructor call (without it call constructor as normal method)
    rrfFreeInstance, // destructor call (without it call destructor as normal method)
    rrfVMT, // use VMT for call (e.g. calling a virtual method)
    rrfConstInherited, // parent is const and this child is too (e.g. field of a const record argument)
    rrfUseFields, // use record fields too, flag is used by pas2js
    rrfTypeOfCast // callee of a "type of" typecast, e.g. "type of a(b)", Declaration is the type of a
    );
  TResolvedReferenceFlags = set of TResolvedReferenceFlag;

type

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
    rraParamToUnknownProc // used as param, before knowing what overloaded proc to call,
      // will later be changed to rraRead, rraVarParam, rraOutParam
    );
  TPRResolveVarAccesses = set of TResolvedRefAccess;

const
  rraAllRead = [rraRead,rraReadAndAssign,rraVarParam];
  rraAllWrite = [rraAssign,rraReadAndAssign,rraVarParam,rraOutParam];

  ResolvedToPSRefAccess: array[TResolvedRefAccess] of TPSRefAccess = (
    psraNone, // rraNone
    psraRead,  // rraRead
    psraWrite, // rraAssign
    psraReadWrite, // rraReadAndAssign
    psraReadWrite, // rraVarParam
    psraWrite, // rraOutParam
    psraNone // rraParamToUnknownProc
    );

type

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
    CompositionPath: TPasVariableArray; // if set, Declaration is a member composed via these record composition fields
    destructor Destroy; override;
    property Declaration: TPasElement read FDeclaration write SetDeclaration;
  end;

  { TResolvedRefCtxConstructor - constructed type of a newinstance reference }

  TResolvedRefCtxConstructor = Class(TResolvedRefContext)
  public
    Typ: TPasType;
  end;

  { TResolvedRefCtxAttrProc - constructor of an attribute }

  TResolvedRefCtxAttrProc = Class(TResolvedRefContext)
  public
    Proc: TPasConstructor;
  end;

  TPasResolverResultFlag = (
    rrfReadable,
    rrfWritable,
    rrfAssignable,  // not writable in general, e.g. aString[1]:=
    rrfCanBeStatement
    );
  TPasResolverResultFlags = set of TPasResolverResultFlag;

type
  { TPasResolverResult }

  TPasResolverResult = record
    BaseType: TResolverBaseType;
    SubType: TResolverBaseType; // for btSet, btArrayLit, btArrayOrSet, btRange
    IdentEl: TPasElement; // if set then this specific identifier is the value, can be a type
    LoTypeEl: TPasType; // can be nil for const expression, all alias resolved
    HiTypeEl: TPasType; // same as LoTypeEl, except alias types are not resolved
    ExprEl: TPasExpr;
    Flags: TPasResolverResultFlags;
  end;
  PPasResolverResult = ^TPasResolverResult;
  TPasResolverResultArray = array of TPasResolverResult;

type
  TPasResolverComputeFlag = (
    rcSetReferenceFlags,  // set flags of references while computing type, used by Resolve* methods
    rcNoImplicitProc,    // do not call a function without params, includes rcNoImplicitProcType
    rcNoImplicitProcType, // do not call a proc type without params
    rcConstant,  // resolve a constant expression, error if not computable
    rcType,      // resolve a type expression
    rcCall       // resolve result type of a function call
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
  TOnEvalBIFunction = procedure(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
    Flags: TResEvalFlags; out Evaluated: TResEvalValue) of object;
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
    Eval: TOnEvalBIFunction;
    FinishParamsExpression: TOnFinishParamsExpr;
    Flags: TBuiltInProcFlags;
    destructor Destroy; override;
  end;

  { TPRFindData }

  TPRFindData = record
    ErrorPosEl: TPasElement;
    Found: TPasElement;
    ElScope: TPasScope; // Where Found was found
    StartScope: TPasScope; // where the search started
    SkipGenerics: boolean;
    ViaAncestorArg: boolean; // found through FindMemberViaAncestorArgs
  end;
  PPRFindData = ^TPRFindData;

  // Selects, among the overloads of a name, the proc whose signature is
  // assignment-compatible with a target procedure type (for @overloadedProc
  // assigned to a typed procedure variable).
  TPRFindProcAddrData = record
    TargetType: TPasProcedureType;
    Found: TPasProcedure;
  end;
  PPRFindProcAddrData = ^TPRFindProcAddrData;

  TPRFindGenericData = record
    Find: TPRFindData;
    TemplateCount: integer;
    LastProc: TPasProcedure;
  end;
  PPRFindGenericData = ^TPRFindGenericData;

  TPasResolverOption = (
    proFixCaseOfOverrides,  // fix Name of overriding proc/property to the overridden proc/property
    proClassPropertyNonStatic,  // class property accessors can be non static
    proPropertyAsVarParam, // allows to pass a property as a var/out argument
    proClassOfIs, // class-of supports is and as operator
    proExtClassInstanceNoTypeMembers, // class members of external class cannot be accessed by instance
    proOpenAsDynArrays, // open arrays work like dynamic arrays
    //ToDo: proStaticArrayCopy, // copy works with static arrays, returning a dynamic array
    //ToDo: proStaticArrayConcat, // concat works with static arrays, returning a dynamic array
    proProcTypeWithoutIsNested, // proc types can use nested procs without 'is nested'
    proMethodAddrAsPointer,  // can assign @method to a pointer
    proSafecallAllowsDefault, // allow assigning a default calling convention to a SafeCall proc
    proMaximizeFPCompatibility // Forbid some things that FPC forbids
    );
  TPasResolverOptions = set of TPasResolverOption;

  { TPasResolverHub }

  TPasResolverHub = class
  private
    FOwner: TObject;
  public
    FinishedInterfaceCount: integer;
    constructor Create(TheOwner: TObject); virtual;
    procedure Reset; virtual;
    property Owner: TObject read FOwner;
  end;
  TPasResolverHubClass = class of TPasResolverHub;

  TPasResolverStep = (
    prsInit,
    prsParsing,
    prsFinishingModule,
    prsFinishedModule
    );
  TPasResolverSteps = set of TPasResolverStep;

  TPRResolveAlias = (
    prraNone, // do not resolve alias
    prraSimple, // resolve alias, but not type alias
    prraAlias // resolve alias and type alias
    );

  TPRProcTypeDescFlag = (
    prptdUseName, // add name if available
    prptdAddPaths, // add full paths to types
    prptdResolveSimpleAlias
    );
  TPRProcTypeDescFlags = set of TPRProcTypeDescFlag;

  TPRParentParams = record
    InlineSpec: TInlineSpecializeExpr;
    Params: TParamsExpr;
  end;

  TPRTemplateCompOp = (
    prtcoAssignToTempl,
    prtcoAssignFromTempl,
    prtcoEqual
    );

  { TPasResolver }

  TPasResolver = Class(TPasTreeContainer)
  private
    type
      TResolveDataListKind = (lkBuiltIn,lkModule);
    function GetBaseTypes(bt: TResolverBaseType): TPasUnresolvedSymbolRef; inline;
    function GetScopes(Index: integer): TPasScope; inline;
  private
    FActiveHelpers: TPRHelperEntryArray; // sorted ascending ComparePRHelperEntries
    FAnonymousElTypePostfix: String;
    FBaseTypeChar: TResolverBaseType;
    FBaseTypeExtended: TResolverBaseType;
    FBaseTypeLength: TResolverBaseType;
    FBaseTypes: array[TResolverBaseType] of TPasUnresolvedSymbolRef;
    FBaseTypeString: TResolverBaseType;
    FBuiltInProcs: array[TResolverBuiltInProc] of TResElDataBuiltInProc;
    FDefaultNameSpace: String;
    FDefaultScope: TPasDefaultScope;
    FDynArrayMaxIndex: TMaxPrecInt;
    FDynArrayMinIndex: TMaxPrecInt;
    FFinishedInterfaceIndex: integer;
    FHub: TPasResolverHub;
    FLastCreatedData: array[TResolveDataListKind] of TResolveData;
    FInSpecialize: Boolean; // true while resolving a specialized generic impl proc body
    // Specializations whose INTERFACE is still being built: SpecEl, GenericEl,
    // SpecEl, GenericEl, ... Their scopes do not exist yet, so a member lookup on
    // one has to fall back to the generic.
    FBuildingSpecializations: TFPList;
    { Specializations whose IMPLEMENTATION is owed until the outermost interface
      build ends; see CreateSpecializedItem. }
    FPendingSpecImpls: TFPList;
    // Specializations whose bodies wait until a forward class argument is bound;
    // shared by all resolvers, as the generic's resolver creates the item.
    class var FForwardSpecImpls: TFPList;
    // Number of specialization bodies being built right now, in all resolvers.
    class var FSpecImplDepth: integer;
    var
    FDeferSpecImpls: boolean;
    // True while an attribute's Create is looked up: private members are not skipped.
    FFindingAttributeCreate: boolean;
    FPartialSpecsAsGeneric: boolean;
    FLoadedUnitDepth: integer;
    // pairs: an `array[A, B] of T` and its array[B] of T, see GetRemainingRangesArray
    FRemainingRangeArrays: TFPList;
    { Specialized element and its item, as pairs, so a specialization that has
      no scope yet can be finished on demand. Kept on the resolver that owns the
      ELEMENT, because every specialization list is per-resolver. }
    FSpecItemsByEl: TFPList;
    FLastElement: TPasElement;
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgElement: TPasElement;
    FLastMsgId: TMaxPrecInt;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FLastSourcePos: TPasSourcePos;
    FOptions: TPasResolverOptions;
    FPendingForwardProcs: TFPList; // list of TPasElement needed to check for forward procs
    FRootElement: TPasModule;
    FScopeClass_Array: TPasArrayScopeClass;
    FScopeClass_Class: TPasClassScopeClass;
    FScopeClass_InitialFinalization: TPasInitialFinalizationScopeClass;
    FScopeClass_Module: TPasModuleScopeClass;
    FScopeClass_EnumType: TPasEnumTypeScopeClass;
    FScopeClass_Proc: TPasProcedureScopeClass;
    FScopeClass_ProcType: TPasProcTypeScopeClass;
    FScopeClass_Record: TPasRecordScopeClass;
    FScopeClass_Section: TPasSectionScopeClass;
    FScopeClass_WithExpr: TPasWithExprScopeClass;
    FScopeCount: integer;
    FScopes: TPasScopeArray; // stack of scopes
    FStep: TPasResolverStep;
    FTypeOfOperandLevel: integer; // >0 while resolving the operand of "type of"
    FStoreSrcColumns: boolean;
    FStashScopeCount: integer;
    FStashScopes: TPasScopeArray; // stack of scopes
    FTopScope: TPasScope;
    procedure ClearResolveDataList(Kind: TResolveDataListKind);
    function GetBaseTypeNames(bt: TResolverBaseType): string;
    function GetBuiltInProcs(bp: TResolverBuiltInProc): TResElDataBuiltInProc;
    function GetMaximizeFPCCompatibility: Boolean;
    procedure SetMaximizeFPCCompatibility(AValue: Boolean);
  protected
    const
      cExact = 0;
      cGenericExact = cExact+1;
      cAliasExact = cGenericExact+1;
      cCompatible = cAliasExact+1;
      cIntToIntConversion = ord(High(TResolverBaseType));
      cFloatToFloatConversion = 2*cIntToIntConversion;
      cTypeConversion = cExact+10000; // e.g. TObject to Pointer
      cLossyConversion = cExact+100000;
      cIntToFloatConversion = cExact+400000; // int to float is worse than bigint to smallint
      // An UNTYPED parameter accepts anything, so it must rank below every real
      // conversion or it wins ties it should lose: ppcx64 picks
      // `FpWrite(fd; buf: PAnsiChar; nbytes)` over `FpWrite(fd; const buf; ...)`.
      cUntypedParam = cExact+800000;
      // An INACCESSIBLE member (private/protected, reached from outside) is not
      // one FPC would pick when an accessible overload exists. It is DEMOTED
      // rather than dropped, so a call where every candidate is inaccessible
      // still reports "Can't access ..." rather than a confusing later error.
      cInaccessibleMember = cExact+900000;
      // "only decidable at specialization": one side is a PARTIAL generic
      // specialization, so the real check happens when it becomes concrete.
      // Ranked below every genuine conversion so it never wins an overload.
      cPartialSpecDefer = High(integer)-1;
      cIncompatible = High(integer);
    var
      cTGUIDToString: integer;
      cStringToTGUID: integer;
      cInterfaceToTGUID: integer;
      cInterfaceToString: integer;
      // True while ComputeBinaryExprRes asks for a user operator after no
      // built-in operator applied to the operands.
      FOperatorLastChance: boolean;
    type
      TFindCallElData = record
        Params: TParamsExpr;
        TemplCnt: integer;
        TemplParams: TFPList; // explicit specialization args (when TemplCnt>0)
        Found: TPasElement; // TPasProcedure or TPasUnresolvedSymbolRef(built in proc) or TPasType (typecast), best candidate so far
        LastProc: TPasProcedure; // last checked TPasProcedure
        ElScope, StartScope: TPasScope;
        Distance: integer; // compatibility distance
        Count: integer;
        List: TFPList; // if not nil then collect all found elements here
      end;
      PFindCallElData = ^TFindCallElData;

      TFindProcKind = (
        fpkProcDeclaration, // search declaration for a body
        fpkProc,   // check overloads for a proc
        fpkMethod  // check overloads for a method
        );
      TFindProcData = record
        Proc: TPasProcedure;
        Args: TFPList;        // List of TPasArgument objects
        Kind: TFindProcKind;
        FoundOverloadModifier: boolean;
        FoundInSameScope: integer;
        Found: TPasProcedure;
        ElScope, StartScope: TPasScope;
        FoundNonProc: TPasElement;
      end;
      PFindProcData = ^TFindProcData;

    procedure OnFindFirst_PreferNoParams(El: TPasElement; ElScope, StartScope: TPasScope;
      FindFirstElementData: Pointer; var Abort: boolean); virtual;
    procedure OnFindProcAddrForType(El: TPasElement; ElScope, StartScope: TPasScope;
      FindProcAddrData: Pointer; var Abort: boolean); virtual;
    function WithMemberIsHidden(El: TPasElement; StartScope: TPasScope): boolean;
    function NameExistsOutsideWith(const AName: String): boolean;
    function PrivateMemberIsHidden(El: TPasElement; StartScope: TPasScope): boolean;
    function NameExistsOutsideMembers(El: TPasElement): boolean;
    procedure OnFindFirst(El: TPasElement; ElScope, StartScope: TPasScope;
      FindFirstElementData: Pointer; var Abort: boolean); virtual;
    procedure OnFindFirst_GenericEl(El: TPasElement; ElScope, StartScope: TPasScope;
      FindFirstGenericData: Pointer; var Abort: boolean); virtual;
    procedure OnFindCallElements(El: TPasElement; ElScope, StartScope: TPasScope;
      FindCallElData: Pointer; var Abort: boolean); virtual; // find candidates for Name(params)
    procedure OnFindProc(El: TPasElement; ElScope, StartScope: TPasScope;
      FindProcData: Pointer; var Abort: boolean); virtual;
    procedure OnFindProcDeclaration(El: TPasElement; ElScope, StartScope: TPasScope;
      FindProcData: Pointer; var Abort: boolean); virtual;
    function IsSameProcContext(ProcParentA, ProcParentB: TPasElement): boolean;
    function IsProcOverloading(LastProc, CurProc: TPasProcedure): boolean;
    function IsVariantOverloadAmbiguous(CandA, CandB: TPasElement; Params: TParamsExpr): Boolean;
    function IsHelperMethodOverloadGroup(LastProc, CurProc: TPasProcedure): boolean;
    function FindProcSameSignature(const ProcName: string; Proc: TPasProcedure;
      Scope: TPasIdentifierScope; OnlyLocal: boolean): TPasProcedure;
    function FindSoleUnimplementedForward(const ProcName: string;
      Scope: TPasIdentifierScope; ExceptProc: TPasProcedure): TPasProcedure;
  protected
    procedure SetCurrentParser(AValue: TPasParser); override;
    procedure ScannerWarnDirective(Sender: TObject; Identifier: TPasScannerString; State: TWarnMsgState; var Handled: boolean); virtual;
    procedure SetRootElement(const AValue: TPasModule); virtual;
    procedure CheckTopScope(ExpectedClass: TPasScopeClass; AllowDescendants: boolean = false);
    function AddIdentifier(Scope: TPasIdentifierScope;
      const aName: String; El: TPasElement;
      const Kind: TPasIdentifierKind): TPasIdentifier; virtual;
    procedure AddModule(El: TPasModule); virtual;
    procedure AddSection(El: TPasSection); virtual;
    procedure AddInitialFinalizationSection(El: TPasImplBlock); virtual;
    procedure AddType(El: TPasType); virtual;
    procedure AddArrayType(El: TPasArrayType; TypeParams: TFPList); virtual;
    procedure AddRecordType(El: TPasRecordType; TypeParams: TFPList); virtual;
    procedure AddRecordVariant(El: TPasVariant); virtual;
    procedure AddClassType(El: TPasClassType; TypeParams: TFPList); virtual;
    procedure AddVariable(El: TPasVariable); virtual;
    procedure AddResourceString(El: TPasResString); virtual;
    procedure AddExportSymbol(El: TPasExportSymbol); virtual;
    procedure AddEnumType(El: TPasEnumType); virtual;
    procedure AddEnumValue(El: TPasEnumValue); virtual;
    procedure AddProperty(El: TPasProperty); virtual;
    procedure AddProcedureType(El: TPasProcedureType; TypeParams: TFPList); virtual;
    procedure AddProcedure(El: TPasProcedure; TypeParams: TFPList); virtual;
    procedure AddProcedureBody(El: TProcedureBody); virtual;
    procedure AddArgument(El: TPasArgument); virtual;
    procedure AddFunctionResult(El: TPasResultElement); virtual;
    procedure AddGenericTemplateType(El: TPasGenericTemplateType); virtual;
    procedure AddExceptOn(El: TPasImplExceptOn); virtual;
    procedure AddTryExceptExprOn(El: TTryExceptExprOn); virtual;
    procedure AddWithDo(El: TPasImplWithDo); virtual;
    procedure ResolveImplBlock(Block: TPasImplBlock); virtual;
    procedure ResolveImplElement(El: TPasImplElement); virtual;
    procedure ResolveImplCaseOf(CaseOf: TPasImplCaseOf); virtual;
    procedure ResolveCaseLabels(CaseExpr: TPasExpr; LabelLists: TFPList;
      ElseEl: TPasElement; IsExpression: boolean); virtual;
    procedure ResolveImplLabelMark(Mark: TPasImplLabelMark); virtual;
    procedure ResolveImplWithDo(El: TPasImplWithDo); virtual;
    procedure ResolveImplAsm(El: TPasImplAsmStatement); virtual;
    function RefineProcAddrForTarget(const TargetResolved: TPasResolverResult;
      AddrExpr: TPasExpr; DoChange: boolean = true): boolean;
    function FindProcOverloadFor(Proc: TPasProcedure;
      TargetType: TPasProcedureType): TPasProcedure;
    procedure ResolveImplAssign(El: TPasImplAssign); virtual;
    procedure ResolveImplSimple(El: TPasImplSimple); virtual;
    procedure ResolveImplRaise(El: TPasImplRaise); virtual;
    procedure ResolveExpr(El: TPasExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveStatementConditionExpr(El: TPasExpr); virtual;
    procedure ResolveNameExpr(El: TPasExpr; const aName: string; Access: TResolvedRefAccess); virtual;
    procedure ResolveInherited(El: TInheritedExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveInheritedName(El: TBinaryExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveBinaryExpr(El: TBinaryExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveIfExpr(El: TIfExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveCaseExpr(El: TCaseExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveTryExceptExpr(El: TTryExceptExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveSubIdent(El: TBinaryExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveParamsExprParams(Params: TParamsExpr); virtual;
    procedure ResolveFuncParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveTypeOfExpr(El: TUnaryExpr; Access: TResolvedRefAccess); virtual;
    procedure FinishTypeOfCast(Params: TParamsExpr; TypeEl: TPasType; Access: TResolvedRefAccess); virtual;
    procedure FinishTypeCastParamAccess(TypeEl: TPasType; Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveFuncParamsExprName(NameExpr: TPasExpr; TemplParams: TFPList;
      Params: TParamsExpr; Access: TResolvedRefAccess; CallName: string = ''); virtual;
    procedure ResolveArrayParamsExpr(Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveArrayParamsExprName(NameExpr: TPasExpr; Params: TParamsExpr; Access: TResolvedRefAccess); virtual;
    procedure ResolveArrayParamsArgs(Params: TParamsExpr;
      const ResolvedValue: TPasResolverResult; Access: TResolvedRefAccess); virtual;
    function ResolveBracketOperatorClassOrRec(Params: TParamsExpr;
      const ResolvedValue: TPasResolverResult;
      Access: TResolvedRefAccess): boolean; virtual;
    procedure ResolveSetParamsExpr(Params: TParamsExpr); virtual;
    procedure ResolveArrayValues(El: TArrayValues); virtual;
    procedure ResolveRecordValues(El: TRecordValues); virtual;
    procedure ResolveInlineSpecializeExpr(El: TInlineSpecializeExpr; Access: TResolvedRefAccess); virtual;
    function ResolveAccessor(Expr: TPasExpr): TPasElement;
    procedure SetResolvedRefAccess(Expr: TPasExpr; Ref: TResolvedReference;
      Access: TResolvedRefAccess); virtual;
    procedure AccessExpr(Expr: TPasExpr; Access: TResolvedRefAccess);
    function MarkArrayExpr(Expr: TParamsExpr; ArrayType: TPasArrayType): boolean; virtual;
    procedure MarkArrayExprRecursive(Expr: TPasExpr; ArrType: TPasArrayType); virtual;
    procedure DeanonymizeType(El: TPasType); virtual;
    procedure FinishModule(CurModule: TPasModule); virtual;
    procedure FinishUsesClause; virtual;
    procedure FinishSection(Section: TPasSection); virtual;
    procedure FinishInterfaceSection(Section: TPasSection); virtual;
    procedure FinishTypeSection(El: TPasElement); virtual;
    procedure FinishTypeSectionEl(El: TPasType); virtual;
    // Bind anonymous `^T` fields of Rec whose target was declared later in the section.
    procedure FinishRecordFieldPointers(Rec: TPasRecordType);
    procedure FinishTypeDef(El: TPasType); virtual;
    procedure FinishEnumType(El: TPasEnumType); virtual;
    procedure FinishSetType(El: TPasSetType); virtual;
    procedure FinishSubElementType(Parent: TPasElement; El: TPasType); virtual;
    procedure FinishRangeType(El: TPasRangeType); virtual;
    procedure FinishConstRangeExpr(RangeExpr: TBinaryExpr;
      out LeftResolved, RightResolved: TPasResolverResult);
    procedure FinishRecordType(El: TPasRecordType); virtual;
    procedure FinishRecordCompositions(El: TPasRecordType; Scope: TPasRecordScope); virtual;
    procedure FinishContainsAlias(El: TPasContainsAlias); virtual;
    procedure FinishClassType(El: TPasClassType); virtual;
    procedure FinishClassOfType(El: TPasClassOfType); virtual;
    procedure FinishPointerType(El: TPasPointerType); virtual;
    procedure FinishArrayType(El: TPasArrayType); virtual;
    procedure FinishAliasType(El: TPasAliasType); virtual;
    procedure FinishTypeOfType(El: TPasTypeOfType); virtual;
    procedure FinishGenericTemplateType(El: TPasGenericTemplateType); virtual;
    procedure FinishSpecializeType(El: TPasSpecializeType); virtual;
    procedure FinishSpecializeTypeBody(El: TPasSpecializeType);
    procedure FinishResourcestring(El: TPasResString); virtual;
    procedure FinishProcedure(Proc: TPasProcedure); virtual;
    procedure FinishProcedureType(El: TPasProcedureType); virtual;
    procedure FinishMethodDeclHeader(Proc: TPasProcedure); virtual;
    procedure FinishMethodImplHeader(ImplProc: TPasProcedure); virtual;
    procedure FinishExceptOnExpr; virtual;
    procedure FinishExceptOnStatement; virtual;
    procedure FinishParserSpecializeType(El: TPasSpecializeType); virtual;
    procedure FinishWithDo(El: TPasImplWithDo); virtual;
    procedure FinishForLoopHeader(Loop: TPasImplForLoop); virtual;
    procedure FinishDeclaration(El: TPasElement); virtual;
    procedure FinishVariable(El: TPasVariable); virtual;
    procedure FinishProperty(PropEl: TPasProperty); virtual;
    procedure FinishArgument(El: TPasArgument); virtual;
    procedure FinishAncestors(aClass: TPasClassType); virtual;
    procedure FinishMethodResolution(El: TPasMethodResolution); virtual;
    procedure FinishAttributes(El: TPasAttributes); virtual;
    procedure FinishExportSymbol(El: TPasExportSymbol); virtual;
    procedure FinishProcParamAccess(ProcType: TPasProcedureType; Params: TParamsExpr); virtual;
    procedure FinishPropertyParamAccess(Params: TParamsExpr;
      Prop: TPasProperty); virtual;
    procedure FinishCallArgAccess(Expr: TPasExpr; Access: TResolvedRefAccess); virtual;
    procedure FinishInitialFinalization(El: TPasImplBlock); virtual;
    procedure EmitTypeHints(PosEl: TPasElement; aType: TPasType); virtual;
    function EmitElementHints(PosEl, El: TPasElement): boolean; virtual;
    procedure StoreScannerFlagsInProc(ProcScope: TPasProcedureScope);
    procedure ReplaceProcScopeImplArgsWithDeclArgs(ImplProcScope: TPasProcedureScope);
    function CreateClassIntfMap(El: TPasClassType; Index: integer): TPasClassIntfMap;
    procedure CheckConditionExpr(El: TPasExpr; const ResolvedEl: TPasResolverResult); virtual;
    function SameConstParamType(DeclTempl,
      ImplTempl: TPasGenericTemplateType): boolean;
    procedure CheckProcSignatureMatch(DeclProc, ImplProc: TPasProcedure;
      IsOverride: boolean // override or class intf implementation
      );
    procedure CheckPointerCycle(El: TPasPointerType);
    procedure CheckGenericTemplateTypes(El: TPasGenericType); virtual;
    procedure ComputeUnaryNot(El: TUnaryExpr; var ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags); virtual;
    function TryResolveOperatorOverload(Bin: TBinaryExpr;
      out ResolvedEl: TPasResolverResult;
      var LeftResolved, RightResolved: TPasResolverResult): Boolean; virtual;
    function TryResolveUnaryOperator(El: TUnaryExpr;
      var ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags): Boolean; virtual;
    procedure CheckOperatorOverloadable(Op: TPasOperator); virtual;
    function IsBinaryOperatorOverloadable(OpType: TOperatorType;
      LeftBT: TResolverBaseType; LeftTypeEl: TPasType;
      RightBT: TResolverBaseType; RightTypeEl: TPasType): Boolean;
    function IsUnaryOperatorOverloadable(OpType: TOperatorType;
      LeftBT: TResolverBaseType; LeftTypeEl: TPasType): Boolean;
    procedure ComputeBinaryExpr(Bin: TBinaryExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeBinaryExprRes(Bin: TBinaryExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      var LeftResolved, RightResolved: TPasResolverResult); virtual;
    procedure ComputeIfExpr(El: TIfExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement); virtual;
    procedure ComputeCaseExpr(El: TCaseExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement); virtual;
    procedure ComputeTryExceptExpr(El: TTryExceptExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement); virtual;
    procedure CombineStatementExprBranch(El: TPasExpr;
      var CombinedResolved: TPasResolverResult; const NextResolved: TPasResolverResult;
      LastExpr, NextExpr: TPasExpr); virtual;
    function ComputeAddStringRes(
      const LeftResolved, RightResolved: TPasResolverResult; ExprEl: TPasExpr;
      out ResolvedEl: TPasResolverResult): boolean; virtual;
    procedure ComputeArgumentAndExpr(
      Arg: TPasArgument; out ArgResolved: TPasResolverResult;
      Expr: TPasExpr; out ExprResolved: TPasResolverResult;
      SetReferenceFlags: boolean);
    procedure ComputeArgumentExpr(const ArgResolved: TPasResolverResult;
      Access: TArgumentAccess; Expr: TPasExpr; out ExprResolved: TPasResolverResult;
      SetReferenceFlags: boolean); virtual;
    procedure ComputeArrayParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeArrayParams_Class(Params: TParamsExpr;
      var ResolvedEl: TPasResolverResult; ClassOrRecScope: TPasClassOrRecordScope;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement); virtual;
    procedure ComputeFuncParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeTypeCast(ToLoType, ToHiType: TPasType;
      Param: TPasExpr; const ParamResolved: TPasResolverResult;
      out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags); virtual;
    procedure ComputeSetParams(Params: TParamsExpr;
      out ResolvedEl: TPasResolverResult; Flags: TPasResolverComputeFlags;
      StartEl: TPasElement);
    procedure ComputeDereference(El: TUnaryExpr; var ResolvedEl: TPasResolverResult);
    procedure ComputeArrayValuesExpectedType(El: TArrayValues; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil);
    procedure ComputeRecordValues(El: TRecordValues; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil);
    procedure CheckIsClass(El: TPasElement; const ResolvedEl: TPasResolverResult);
    function CheckTypeCastClassInstanceToClass(
      const FromClassRes, ToClassRes: TPasResolverResult;
      ErrorEl: TPasElement): integer; virtual; // type cast not related classes
    procedure CheckSetLitElCompatible(Left, Right: TPasExpr;
      const LHS, RHS: TPasResolverResult);
    function CheckIsOrdinal(const ResolvedEl: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnError: boolean): boolean;
    procedure CombineArrayLitElTypes(Left, Right: TPasExpr;
      var LHS: TPasResolverResult; const RHS: TPasResolverResult);
    procedure ConvertRangeToElement(var ResolvedEl: TPasResolverResult);
    function IsCharLiteral(const Value: string; ErrorPos: TPasElement): TResolverBaseType; virtual;
    function HasWideCharCode(const Value: string): Boolean;
    function CheckForIn(Loop: TPasImplForLoop;
      const VarResolved, InResolved: TPasResolverResult): boolean; virtual;
    function CheckForInClassOrRec(Loop: TPasImplForLoop;
      const VarResolved, InResolved: TPasResolverResult): boolean; virtual;
    function CheckBuiltInMinParamCount(Proc: TResElDataBuiltInProc; Expr: TPasExpr;
      MinCount: integer; RaiseOnError: boolean): boolean;
    function CheckBuiltInMaxParamCount(Proc: TResElDataBuiltInProc; Params: TParamsExpr;
      MaxCount: integer; RaiseOnError: boolean; Signature: string = ''): integer;
    function CheckRaiseTypeArgNo(id: TMaxPrecInt; ArgNo: integer; Param: TPasExpr;
      const ParamResolved: TPasResolverResult; Expected: string; RaiseOnError: boolean): integer;
    function FindUsedUnitnameInSection(const aName: string; Section: TPasSection): TPasModule;
    function FindUsedUnitname(const aName: string; aMod: TPasModule): TPasModule;
    procedure FinishAssertCall(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function FindSystemIdentifier(const aUnitName, aName: string;
      ErrorEl: TPasElement): TPasElement; virtual;
    function FindSystemClassType(const aUnitName, aClassName: string;
      ErrorEl: TPasElement): TPasClassType; virtual;
    function FindSystemClassTypeAndConstructor(const aUnitName, aClassName: string;
      out aClass: TPasClassType; out aConstructor: TPasConstructor;
      ErrorEl: TPasElement): boolean; virtual;
    procedure FindAssertExceptionConstructors(ErrorEl: TPasElement); virtual;
    procedure FindRangeErrorConstructors(ErrorEl: TPasElement); virtual;
    function FindTVarRec(ErrorEl: TPasElement): TPasRecordType; virtual;
    function GetTVarRec(El: TPasArrayType): TPasRecordType; virtual;
    function FindDefaultConstructor(aClass: TPasClassType): TPasConstructor; virtual;
    function GetTypeInfoParamType(Param: TPasExpr;
      out ParamResolved: TPasResolverResult; LoType: boolean): TPasType; virtual; // returns type of param in typeinfo(param)
  protected
    // constant evaluation
    fExprEvaluator: TResExprEvaluator;
    procedure OnExprEvalLog(Sender: TResExprEvaluator; const id: TMaxPrecInt;
      MsgType: TMessageType; MsgNumber: integer; const Fmt: String;
      Args: array of const; PosEl: TPasElement); virtual;
    // Create the constant-expression evaluator. Allows a descendant to return a subclass.
    function CreateExprEvaluator: TResExprEvaluator; virtual;
    function OnExprEvalIdentifier(Sender: TResExprEvaluator;
      Expr: TPrimitiveExpr; Flags: TResEvalFlags): TResEvalValue; virtual;
    function OnExprEvalParams(Sender: TResExprEvaluator;
      Params: TParamsExpr; Flags: TResEvalFlags): TResEvalValue; virtual;
    procedure OnRangeCheckEl(Sender: TResExprEvaluator; El: TPasElement;
      var MsgType: TMessageType); virtual;
    function EvalBaseTypeCast(Params: TParamsExpr; bt: TResolverBaseType): TResEvalvalue; virtual;
    // Virtual: the base returns nil (= not folded). TPasNativeResolver overrides them to
    // fold a constant-integer cast to a pointer type and to degrade an address-of.
    function EvalNativePointerCast(Params: TParamsExpr; bt: TResolverBaseType): TResEvalValue; virtual;
    function EvalNativeNamedPointerCast(Params: TParamsExpr): TResEvalValue; virtual;
    function EvalNativeAddressOf(Expr: TPrimitiveExpr; Flags: TResEvalFlags): TResEvalValue; virtual;
    function EvalLengthOfString(ParamResolved: TPasResolverResult;
      Param: TPasExpr; Flags: TResEvalFlags): TResEvalValue; virtual;
  protected
    // generic/specialize
    type
      TScopeStashState = record
        ScopeCount: integer;
        StashCount: integer;
      end;
    procedure AddGenericTemplateIdentifiers(GenericTemplateTypes: TFPList;
      Scope: TPasIdentifierScope);
    procedure AddSpecializedTemplateIdentifiers(GenericTemplateTypes: TFPList;
      SpecializedItem: TPRSpecializedItem; Scope: TPasIdentifierScope;
      CheckConstraints: boolean);
    function CreateInferenceTypesForCall(Params: TParamsExpr;
      TargetProc: TPasProcedure): TFPList;
    function CheckGenericConstraintFitsParam(ParamType: TPasType;
      SpecializedItem: TPRSpecializedItem; // set to specialize constraints
      TemplType: TPasGenericTemplateType; ConEl: TPasElement;
      Operation: TPRTemplateCompOp;
      ErrorPos: TPasElement // can be nil to get a compatibility Result
      ): integer;
    function CheckTemplateFitsParam(ParamType: TPasType;
      GenTempl: TPasGenericTemplateType;
      SpecializedItem: TPRSpecializedItem; // set to specialize constraints
      Operation: TPRTemplateCompOp;
      ErrorPos: TPasElement // can be nil to get a compatibility Result
      ): integer;
    function CheckTemplateFitsParamRes(GenTempl: TPasGenericTemplateType;
      const ResolvedEl: TPasResolverResult;
      Operation: TPRTemplateCompOp;
      ErrorPos: TPasElement // can be nil to get a compatibility Result
      ): integer;
    procedure CheckTemplateFitsTemplate(ParamTemplType,
      GenTempl: TPasGenericTemplateType; ErrorPos: TPasElement);
    function CreateSpecializedItem(El: TPasElement; GenericEl: TPasElement;
      const ParamsResolved: TPasTypeArray;
      const AConstExprs: array of TPasExpr): TPRSpecializedItem; virtual;
    function CreateSpecializedTypeName(Item: TPRSpecializedItem): string; virtual;
    function CreateConstExprForSpecParam(OrigExpr: TPasExpr;
      AParent: TPasElement): TPasExpr; virtual;
    procedure InitSpecializeScopes(El: TPasElement; out State: TScopeStashState); virtual;
    procedure RestoreSpecializeScopes(const State: TScopeStashState); virtual;
    procedure SpecializeGenericIntf(SpecializedItem: TPRSpecializedItem); virtual;
    procedure SpecializeGenericImpl(SpecializedItem: TPRSpecializedItem); virtual;
    procedure SpecializeMembers(GenMembersType, SpecMembersType: TPasMembersType); virtual;
    procedure SpecializeMembersImpl(GenericType, SpecType: TPasMembersType;
      SpecializedItem: TPRSpecializedTypeItem); virtual;
    procedure SpecializeGenImplProc(GenDeclProc, SpecDeclProc: TPasProcedure;
      SpecializedItem: TPRSpecializedItem); virtual;
    procedure SpecializeElement(GenEl, SpecEl: TPasElement);
    procedure SpecializePasElementProperties(GenEl, SpecEl: TPasElement);
    procedure SpecializeVariable(GenEl, SpecEl: TPasVariable; Finish: boolean);
    procedure SpecializeContainsAlias(GenEl, SpecEl: TPasContainsAlias);
    procedure SpecializeConst(GenEl, SpecEl: TPasConst);
    procedure SpecializeProperty(GenEl, SpecEl: TPasProperty);
    function SpecializationRootOf(El: TPasElement): TPasElement;
    function OwnersAreRelated(A, B: TPasElement): boolean;
    function FindSpecializedMemberByOwnerRoot(StartEl: TPasElement;
      OwnerRoot: TPasElement; const aName: string): TPasType;
    function SpecializeTypeRef(GenEl, SpecEl: TPasElement; GenTypeRef: TPasType): TPasType;
    function FindMemberViaBoundTemplate(SpecEl: TPasElement;
      GenTypeRef: TPasType): TPasType;
    function SpecializedItemInProgress(El: TPasElement): TPRSpecializedItem;
    function FindGenericViaBoundTemplate(SpecEl: TPasElement;
      GenDestType: TPasType): TPasType;
    procedure SpecializeElType(GenEl, SpecEl: TPasElement;
      GenElType: TPasType; var SpecElType: TPasType);
    procedure SpecializeElExpr(GenEl, SpecEl: TPasElement;
      GenElExpr: TPasExpr; var SpecElExpr: TPasExpr);
    procedure SpecializeElImplEl(GenEl, SpecEl: TPasElement;
      GenImplEl: TPasImplElement; var SpecImplEl: TPasImplElement);
    procedure SpecializeElImplAlias(GenEl, SpecEl: TPasImplBlock;
      GenImplAlias: TPasImplElement; var SpecImplAlias: TPasImplElement);
    procedure SpecializeElList(GenEl, SpecEl: TPasElement;
      GenList, SpecList: TFPList; AllowReferences: boolean);
    procedure SpecializeElArray(GenEl, SpecEl: TPasElement;
      GenList: TPasElementArray; var SpecList: TPasElementArray; AllowReferences: boolean);
    procedure SpecializeProcedure(GenEl, SpecEl: TPasProcedure; SpecializedItem: TPRSpecializedItem); virtual;
    procedure SpecializeOperator(GenEl, SpecEl: TPasOperator);
    procedure SpecializeProcedureType(GenEl, SpecEl: TPasProcedureType; SpecializedItem: TPRSpecializedItem);
    procedure SpecializeProcedureBody(GenEl, SpecEl: TProcedureBody);
    procedure SpecializeDeclarations(GenEl, SpecEl: TPasDeclarations);
    procedure SpecializeSpecializeType(GenEl, SpecEl: TPasSpecializeType);
    procedure SpecializeGenericTemplateType(GenEl, SpecEl: TPasGenericTemplateType);
    procedure SpecializeArgument(GenEl, SpecEl: TPasArgument);
    procedure SpecializeImplBlock(GenEl, SpecEl: TPasImplBlock);
    procedure SpecializeImplAsmStatement(GenEl, SpecEl: TPasImplAsmStatement);
    procedure SpecializeImplRepeatUntil(GenEl, SpecEl: TPasImplRepeatUntil);
    procedure SpecializeImplIfElse(GenEl, SpecEl: TPasImplIfElse);
    procedure SpecializeImplWhileDo(GenEl, SpecEl: TPasImplWhileDo);
    procedure SpecializeImplWithDo(GenEl, SpecEl: TPasImplWithDo);
    procedure SpecializeImplCaseOf(GenEl, SpecEl: TPasImplCaseOf);
    procedure SpecializeImplCaseStatement(GenEl, SpecEl: TPasImplCaseStatement);
    procedure SpecializeImplAssign(GenEl, SpecEl: TPasImplAssign);
    procedure SpecializeImplSimple(GenEl, SpecEl: TPasImplSimple);
    procedure SpecializeImplForLoop(GenEl, SpecEl: TPasImplForLoop);
    procedure SpecializeImplTry(GenEl, SpecEl: TPasImplTry);
    procedure SpecializeImplExceptOn(GenEl, SpecEl: TPasImplExceptOn);
    procedure SpecializeImplRaise(GenEl, SpecEl: TPasImplRaise);
    procedure SpecializeExpr(GenEl, SpecEl: TPasExpr);
    procedure SpecializeExprArray(GenEl, SpecEl: TPasElement;
      GenArray: TPasExprArray; var SpecArray: TPasExprArray);
    procedure SpecializePrimitiveExpr(GenEl, SpecEl: TPrimitiveExpr);
    procedure SpecializeUnaryExpr(GenEl, SpecEl: TUnaryExpr);
    procedure SpecializeBinaryExpr(GenEl, SpecEl: TBinaryExpr);
    procedure SpecializeBoolConstExpr(GenEl, SpecEl: TBoolConstExpr);
    procedure SpecializeParamsExpr(GenEl, SpecEl: TParamsExpr);
    procedure SpecializeRecordValues(GenEl, SpecEl: TRecordValues);
    procedure SpecializeArrayValues(GenEl, SpecEl: TArrayValues);
    procedure SpecializeInlineSpecializeExpr(GenEl, SpecEl: TInlineSpecializeExpr);
    procedure SpecializeProcedureExpr(GenEl, SpecEl: TProcedureExpr);
    procedure SpecializeIfExpr(GenEl, SpecEl: TIfExpr);
    procedure SpecializeCaseExpr(GenEl, SpecEl: TCaseExpr);
    procedure SpecializeCaseExprBranch(GenEl, SpecEl: TCaseExprBranch);
    procedure SpecializeTryExceptExpr(GenEl, SpecEl: TTryExceptExpr);
    procedure SpecializeTryExceptExprOn(GenEl, SpecEl: TTryExceptExprOn);
    procedure SpecializeResString(GenEl, SpecEl: TPasResString);
    procedure SpecializeAliasType(GenEl, SpecEl: TPasAliasType);
    procedure SpecializeTypeOfType(GenEl, SpecEl: TPasTypeOfType);
    procedure SpecializePointerType(GenEl, SpecEl: TPasPointerType);
    procedure SpecializeRangeType(GenEl, SpecEl: TPasRangeType);
    procedure SpecializeArrayType(GenEl, SpecEl: TPasArrayType; SpecializedItem: TPRSpecializedTypeItem);
    procedure SpecializeRecordType(GenEl, SpecEl: TPasRecordType; SpecializedItem: TPRSpecializedTypeItem);
    procedure SpecializeClassType(GenEl, SpecEl: TPasClassType; SpecializedItem: TPRSpecializedTypeItem);
    procedure SpecializeEnumValue(GenEl, SpecEl: TPasEnumValue);
    procedure SpecializeEnumType(GenEl, SpecEl: TPasEnumType);
    procedure SpecializeSetType(GenEl, SpecEl: TPasSetType);
    procedure SpecializeVariant(GenEl, SpecEl: TPasVariant);
    procedure SpecializeRecordVariantPart(GenEl, SpecEl: TPasRecordType);
    procedure PublishGenericEnumValues(EnumType: TPasType);
    procedure SpecializeStringType(GenEl, SpecEl: TPasStringType);
    procedure SpecializeAttributes(GenEl, SpecEl: TPasAttributes);
    procedure SpecializeMethodResolution(GenEl, SpecEl: TPasMethodResolution);
  protected
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
    procedure BI_Length_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
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
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Assigned_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Chr_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Chr_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Chr_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_Ord_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Ord_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Ord_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_LowHigh_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_LowHigh_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_LowHigh_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_PredSucc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_PredSucc_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_PredSucc_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_Str_CheckParam(IsFunc: boolean; Param: TPasExpr;
      const ParamResolved: TPasResolverResult; ArgNo: integer;
      RaiseOnError: boolean): integer;
    function BI_StrProc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_StrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_StrFunc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_StrFunc_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_StrFunc_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_WriteStrProc_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_WriteStrProc_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Val_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Val_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_LoHi_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_LoHi_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_LoHi_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_ConcatArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_ConcatArray_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_ConcatString_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_ConcatString_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_ConcatString_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_CopyArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_CopyArray_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_Slice_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Slice_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_InsertArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_InsertArray_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_DeleteArray_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_DeleteArray_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_TypeInfo_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_TypeInfo_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    function BI_GetTypeKind_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_GetTypeKind_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_GetTypeKind_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_Assert_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Assert_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_New_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_New_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function BI_Dispose_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Dispose_OnFinishParamsExpr(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr); virtual;
    function MembersHoldFileType(aType: TPasType): boolean;
    function BI_Default_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_Default_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_Default_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_NameOf_GetIdentEl(Params: TParamsExpr; RaiseOnError: boolean): TPasElement; virtual;
    function BI_NameOf_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_NameOf_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_NameOf_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
    function BI_IsConstValue_OnGetCallCompatibility(Proc: TResElDataBuiltInProc;
      Expr: TPasExpr; RaiseOnError: boolean): integer; virtual;
    procedure BI_IsConstValue_OnGetCallResult(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; out ResolvedEl: TPasResolverResult); virtual;
    procedure BI_IsConstValue_OnEval(Proc: TResElDataBuiltInProc;
      Params: TParamsExpr; Flags: TResEvalFlags; out Evaluated: TResEvalValue); virtual;
  public
    // Defer every specialization's IMPLEMENTATION until the batch ends, so a
    // group of specializations built together sees a finished hierarchy before
    // any body of it is type-checked. The same deferral an interface build
    // already gets; a loader that builds many at once needs it explicitly.
    procedure BeginSpecializationBatch;
    procedure EndSpecializationBatch;
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
      const ASrcPos: TPasSourcePos; TypeParams: TFPList = nil): TPasElement;
      overload; override;
    function CreateOwnedElement(AClass: TPTreeElement; const AName: String; AParent: TPasElement): TPasElement; virtual;
    function FindModule(const AName: String; NameExpr, InFileExpr: TPasExpr): TPasModule; override;
    function FindUnit(const AName, InFilename: String;
      NameExpr, InFileExpr: TPasExpr): TPasModule; virtual; abstract;
    function FindElement(const aName: String): TPasElement; override;  // used by TPasParser
    function FindElementFor(const aName: String; AParent: TPasElement; TypeParamCount: integer): TPasElement; override; // used by TPasParser
    function FindElementWithoutParams(const AName: String; ErrorPosEl: TPasElement;
      NoProcsWithArgs, NoGenerics: boolean): TPasElement;
    function FindOuterTypeSkippingElement(const AName: String;
      SkipEl: TPasElement): TPasType;
    function FindElementWithoutParams(const AName: String; out Data: TPRFindData;
      ErrorPosEl: TPasElement; NoProcsWithArgs, NoGenerics: boolean): TPasElement;
    function FindMemberViaAncestorArgs(const AName: String): TPasElement;
    function BoundAncestorArgClass(T: TPasGenericTemplateType): TPasClassType;
    function FindFirstEl(const AName: String; out Data: TPRFindData;
      ErrorPosEl: TPasElement): TPasElement;
    procedure FindLongestUnitName(var El: TPasElement; Expr: TPasExpr);
    function FindGenericEl(const AName: string; TemplateCount: integer;
      out Find: TPRFindData; ErrorPosEl: TPasElement): TPasElement; virtual;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure IterateGlobalElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure CheckFoundElement(const FindData: TPRFindData;
      Ref: TResolvedReference); virtual;
    function StrictProtectedQualifierOk(const FindData: TPRFindData;
      Context: TPasType): boolean;
    procedure CheckFoundElementVisibility(const FindData: TPRFindData;
      Ref: TResolvedReference); virtual;
    function GetVisibilityContext: TPasElement;
    // Whether a property of type B may use an accessor of type A.
    function PropTypesMatch(A, B: TPasType): boolean;
    // Whether Context lies inside aClass or a descendant of it (a nested type).
    function IsNestedInClassOrDescendant(Context: TPasElement;
      aClass: TPasMembersType): boolean;
    procedure BeginScope(ScopeType: TPasScopeType; El: TPasElement); override;
    procedure FinishScope(ScopeType: TPasScopeType; El: TPasElement); override;
    procedure FinishTypeAlias(var NewType: TPasType); override;
    function IsUnitIntfFinished(AModule: TPasModule): boolean;
    procedure NotifyPendingUsedInterfaces; virtual;
    function GetPendingUsedInterface(Section: TPasSection): TPasUsesUnit;
    function CheckPendingUsedInterface(Section: TPasSection): boolean; override;
    procedure UsedInterfacesFinished(Section: TPasSection); virtual;
    function NeedArrayValues(El: TPasElement): boolean; override;
    function GetDefaultClassVisibility(AClass: TPasClassType
      ): TPasMemberVisibility; override;
    procedure ModeChanged(Sender: TObject; NewMode: TModeSwitch;
      Before: boolean; var Handled: boolean); override;
    // built in types and functions
    procedure ClearBuiltInIdentifiers; virtual;
    procedure AddObjFPCBuiltInIdentifiers(
      const TheBaseTypes: TResolveBaseTypes = btAllFPCTypes;
      const TheBaseProcs: TResolverBuiltInProcs = bfAllStandardProcs); virtual;
    function AddBaseType(const aName: string; Typ: TResolverBaseType): TResElDataBaseType;
    function AddCustomBaseType(const aName: string; aClass: TResElDataBaseTypeClass): TPasUnresolvedSymbolRef;
    function IsBaseType(aType: TPasType; BaseType: TResolverBaseType; ResolveAlias: boolean = false): boolean;
    function AddBuiltInProc(const aName: string; Signature: string;
      const GetCallCompatibility: TOnGetCallCompatibility;
      const GetCallResult: TOnGetCallResult;
      const EvalConst: TOnEvalBIFunction = nil;
      const FinishParamsExpr: TOnFinishParamsExpr = nil;
      const BuiltIn: TResolverBuiltInProc = bfCustom;
      const Flags: TBuiltInProcFlags = []): TResElDataBuiltInProc;
    // add extra TResolveData (E.CustomData) to free list
    procedure AddResolveData(El: TPasElement; Data: TResolveData;
      Kind: TResolveDataListKind);
    function CreateReference(DeclEl, RefEl: TPasElement;
      Access: TResolvedRefAccess;
      FindData: PPRFindData = nil): TResolvedReference; virtual;
    procedure SetCompositionPath(Ref: TResolvedReference; ElScope: TPasScope); virtual;
    // scopes
    function GetLocalScope: TPasScope; inline;
    function GetParentLocalScope: TPasScope; inline;
    function CreateScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; virtual;
    function CreateGroupScope(HiType: TPasType; WithTopHelpers: boolean = true): TPasGroupScope; virtual;
    function IsActiveHelperVisible(Helper: TPasClassType): boolean;
    procedure GroupScope_AddTypeAndAncestors(Scope: TPasGroupScope; HiType: TPasType; WithTopHelpers: boolean = true);
    procedure GroupScope_AddHelpersFor(Scope: TPasGroupScope; ForType: TPasType; out BestEntry: TPRHelperEntry);
    procedure PopScope;
    procedure PopWithScope(El: TPasImplWithDo);
    procedure PopGenericParamScope(El: TPasGenericType); virtual;
    procedure PushScope(Scope: TPasScope); overload;
    function PushScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; overload;
    function PushGroupScope(HiType: TPasType): TPasGroupScope;
    function PushModuleDotScope(aModule: TPasModule): TPasModuleDotScope;
    function GenericOfBuildingSpecialization(El: TPasElement): TPasElement;
    function PushClassDotScope(var CurClassType: TPasClassType; WithTopHelpers: boolean = true): TPasDotClassScope;
    function PushRecordDotScope(CurRecordType: TPasRecordType; HiType: TPasType = nil): TPasDotClassOrRecordScope;
    function PushInheritedScope(ClassOrRec: TPasMembersType;
      WithTopHelpers: boolean; AncestorScope: TPasClassScope): TPasInheritedScope;
    function PushEnumDotScope(HiType: TPasType; EnumLoType: TPasEnumType): TPasDotEnumTypeScope;
    function PushHelperDotScope(HiType: TPasType): TPasDotBaseScope;
    function PushTemplateDotScope(TemplType: TPasGenericTemplateType; ErrorEl: TPasElement): TPasDotBaseScope;
    function PushDotScope(HiType: TPasType): TPasDotBaseScope;
    function PushParserSpecializeType(SpecType: TPasSpecializeType): TPasDotBaseScope;
    function PushWithExprScope(Expr: TPasExpr): TPasWithExprScope;
    function StashScopes(NewScopeCnt: integer): integer; // returns old StashDepth
    function StashSubExprScopes: integer; // returns old StashDepth
    procedure RestoreStashedScopes(StashDepth: integer);
    procedure DeleteScope(Index: integer); virtual;
    procedure InsertScope(Scope: TPasScope; Index: integer); virtual;
    function GetCurrentProcScope(ErrorEl: TPasElement): TPasProcedureScope;
    function GetProcScope(El: TPasElement): TPasProcedureScope;
    function GetCurrentSelfScope(ErrorEl: TPasElement): TPasProcedureScope;
    function GetSelfScope(El: TPasElement): TPasProcedureScope;
    procedure AddHelper(Helper: TPasClassType; var List: TPRHelperEntryArray);
    procedure AddActiveHelper(Helper: TPasClassType); virtual;
    // True if a type helper declared for HelperForType applies to a value of type
    // HiType. Base: an exact (alias-resolved) type match. A backend may widen this,
    // e.g. to let the Double helper serve Extended values when the two share one
    // machine type.
    function HelperUsesPriority(Helper: TPasClassType): integer;
    function MatchHelperForType(HelperForType, HiType: TPasType): boolean; virtual;
    // log and messages
    class function MangleSourceLineNumber(Line, Column: integer): integer;
    class procedure UnmangleSourceLineNumber(LineNumber: integer;
      out Line, Column: integer);
    class function GetDbgSourcePosStr(El: TPasElement): string;
    function GetElementSourcePosStr(El: TPasElement): string;
    // Range/overflow-checked marking, recorded on the element's state-flag set
    // (TPasElement.States) at parse time from the {$R+}/{$Q+} scanner switches.
    // Target-agnostic base methods (not native-specific).
    procedure MarkRangeChecked(El: TPasElement);
    function IsRangeChecked(El: TPasElement): Boolean;
    procedure MarkOverflowChecked(El: TPasElement);
    function IsOverflowChecked(El: TPasElement): Boolean;
    // Native memory-layout packing directives, captured per type at its
    // declaration ({$MINENUMSIZE}/{$PACKSET}/{$PACKRECORDS}). Base defaults are
    // pas2js-safe (no packing: get 0, set no-op); TPasNativeResolver overrides
    // these to store/retrieve the value per element.
    procedure SetMinEnumSize(El: TPasElement; ASize: Integer); virtual;
    function GetMinEnumSize(El: TPasElement): Integer; virtual;
    procedure SetPackSet(El: TPasElement; ASize: Integer); virtual;
    function GetPackSet(El: TPasElement): Integer; virtual;
    procedure SetPackRecords(El: TPasElement; ASize: Integer); virtual;
    function GetPackRecords(El: TPasElement): Integer; virtual;
    // True when a pointer type permits pointer arithmetic (+/-) and indexing
    // regardless of the use-site {$POINTERMATH} switch. Base default False
    // (pas2js has no pointer arithmetic); a native resolver returns True for the
    // untyped Pointer, a pointer declared under {$POINTERMATH ON}
    // (pesfPointerMath), or a PChar-family pointer.
    // True when a class/record/type helper method may be virtual/override.
    // Base default False (fcl-passrc policy: unsupported, TestClassHelper_VirtualDelphiFail);
    // a native/FPC target overrides this to allow it in Delphi mode (tchlp10/tchlp42).
    function AllowHelperVirtualMethods: Boolean; virtual;
    // True if the implementation of a forward generic proc may repeat its
    // (matching) type constraints. Base default False (fcl-passrc policy:
    // TestGenProc_ForwardConstraintsRepeatFail); real FPC/native allows it
    // (tgenfunc20/21), so a native target overrides this to True.
    function AllowImplRepeatConstraints: Boolean; virtual;
    // True if an omitted arg's default VALUE may be used to infer a template type
    // in implicit function specialization. Base default True (fcl-passrc policy:
    // TestGenProc_Infer_DefaultValue); real FPC/native does NOT infer from
    // defaults (timpfuncspez16/17), so a native target overrides this to False.
    function InferTemplTypesFromDefaults: Boolean; virtual;
    // True if an implicit-function-specialization candidate (GenericProc(args)
    // with no explicit <...>) should be scored as a tentative match (arg count
    // only), deferring the type check to inference. Base default False (keeps the
    // full check so testpassrc multi-overload disambiguation works); a native/FPC
    // target overrides to True (timpfuncspez4/20).
    function UseTentativeImplicitSpecMatch: Boolean; virtual;
    // True if $POINTERMATH is enabled for El's pointer-arithmetic check. Base uses
    // the bool switch stored on El's enclosing scope (ElHasBoolSwitch), which can
    // be stale for a `{$MODE DELPHI}` set after the program scope was created
    // (tpointermath2). A native/FPC target overrides to the current scanner state.
    function PointerMathBoolSwitchEnabled(El: TPasElement): Boolean; virtual;
    // True if a generic record may reference its own (not-yet-finished)
    // specialization in a method signature (tgeneric76). Base default False
    // (TestGen_Record_ReferGenericSelfFail); a native/FPC target overrides to True.
    function AllowRecordGenericSelfReference: Boolean; virtual;
    // True when two distinct TPasSpecializeType nodes denote the same type
    // (same generic + same arguments), e.g. TPointEx<T> written twice in a
    // generic body (tgeneric76). Base default False (leaves upstream behaviour);
    // a native/FPC target overrides to compare them structurally.
    function SameSpecializeType(SpecA, SpecB: TPasSpecializeType;
      ResolveAlias: TPRResolveAlias): Boolean; virtual;
    // True when a named non-generic proc type should get a TPasProcTypeScope that
    // captures the current bool switches at declaration (needed for a funcref's
    // {$M+} RTTI on a native/FPC target). Base default False (pristine upstream:
    // no scope, so pas2js PCU round-trip is unaffected).
    function StoreProcTypeScopeBoolSwitches: Boolean; virtual;
    // True when reconciling two integer inferences for the same template param
    // should keep the narrower type if the wider fully contains it (timpfuncspez13).
    // Base default False (pristine upstream: widen to a common base type).
    function PreferNarrowerInferredInteger: Boolean; virtual;
    // True when a generic (parameterized) method may be declared with published
    // visibility. Base default False (pristine upstream rejects it — see
    // sXMethodsCannotHaveTypeParams). Delphi accepts it, so a native/FPC target
    // overrides to allow it (GitLab #41410).
    function AllowGenericPublishedMethod: Boolean; virtual;
    // True when a variable typecast to ToLoType keeps its operand's l-value
    // status although the two types differ. Base default False - the same-size
    // ordinal rule in ComputeTypeCast already covers the plain cases. A native
    // target knows every type's size and lets ANY same-size scalar cast stay
    // writable, which is what ppcx64 does: `IdentToColor(S,LongInt(Result))`
    // with a subrange Result, and `Longint(SingleVar)` in TInterlocked.
    function TypeCastKeepsLValue(ToLoType: TPasType;
      const FromRes: TPasResolverResult): Boolean; virtual;
    function IsPointerMathType(El: TPasType): Boolean; virtual;
    // True when Inc/Dec is permitted on a pointer of type El. Base default False;
    // a native target allows Inc/Dec on any pointer (switch-independent).
    function AllowIncDecOnPointer(El: TPasType): Boolean; virtual;
    // True when Inc/Dec is permitted on a non-integer ordinal (char, boolean,
    // enum). Base default False (fcl-passrc/pas2js accept only integers); a native
    // target allows any ordinal, like the real FPC compiler (Inc(charVar)).
    function AllowIncDecOnOrdinal(bt: TResolverBaseType; El: TPasType): Boolean; virtual;
    // True when Include/Exclude is permitted on a set whose element is a base
    // ordinal (char/int/boolean), not just an enum/subrange. Base default False;
    // a native target allows it, like the real FPC compiler (Include(charSet,c)).
    function AllowInExcludeNonEnumSet: Boolean; virtual;
    // True when Expr accesses a bit-packed ordinal array element / record field
    // whose byte address cannot be taken. Base default False (pas2js has no
    // bit-packing); a native resolver computes it from the packed bit width.
    function IsBitPackedOrdinalAccess(Expr: TPasExpr): boolean; virtual;
    // True when BtA and BtB are the WideString/UnicodeString pair AND this
    // target stores them identically, so either fills the other's var/out
    // parameter. Base default False (pas2js has no WideString of its own).
    function WideStringMatchesUnicodeString(BtA, BtB: TResolverBaseType): Boolean; virtual;
    // True when `nil^` denotes a location. Base default False; a native target
    // accepts it, like the real FPC compiler, so it can be handed to an untyped
    // var parameter meaning "no buffer" (rtl/inc/lnfodwrf.pp ReadNext(nil^,n)).
    function AllowNilDereference: Boolean; virtual;
    // True when `with Obj.RecProp do Field := X` may write into the temporary
    // copy of a record property or function result, as FPC allows. Default False.
    function AllowWriteToWithTempRecord: Boolean; virtual;
    // True when the values of an enum declared in a generic class are also
    // visible in the enclosing section, as FPC does. Default False.
    function AllowGenericEnumValuesInSection: Boolean; virtual;
    // True when `@TClass.Method` may name a method that is not visible here, as
    // FPC allows. Default False.
    function AllowTypeQualifiedMethodAddress: Boolean; virtual;
    // True when a method may declare a parameter named Self; in the body Self
    // still means the instance, as in FPC. Default False.
    function AllowArgumentNamedSelf: Boolean; virtual;
    // True when a published method may share its name with another method of
    // the same class. Base default False, because pas2js keys its published
    // RTTI by name; FPC lets published methods overload like any other.
    function AllowPublishedMethodOverload: Boolean; virtual;
    function AllowPublishedClassMethod: Boolean; virtual;
    // True when an INSTANCE field reached through its type name (TRec.field) is
    // allowed at PosEl. Base default False; FPC accepts it wherever only the
    // field's TYPE is used - High/Low/SizeOf/Default (lnfodwrf.pp).
    function AllowTypeQualifiedInstanceField(Found: TPasElement;
      PosEl: TPasElement): Boolean; virtual;
    // True when El is (part of) the argument of the built-in NameOf(), where
    // only the declared name is needed and no instance is required.
    function IsNameOfArgument(El: TPasElement): boolean; virtual;
    // Copies the {$PACKRECORDS} pack value from a generic template element to its specialized element.
    procedure SpecializePackValues(GenEl, SpecEl: TPasElement);
    procedure SetLastMsg(const id: TMaxPrecInt; MsgType: TMessageType; MsgNumber: integer;
      Const Fmt : String; Args : Array of const;
      PosEl: TPasElement);
    procedure LogMsg(const id: TMaxPrecInt; MsgType: TMessageType; MsgNumber: integer;
      const Fmt: String; Args: Array of const;
      PosEl: TPasElement); overload;
    class function GetWarnIdentifierNumbers(Identifier: string;
      out MsgNumbers: TIntegerDynArray): boolean; virtual;
    procedure GetIncompatibleTypeDesc(const GotType, ExpType: TPasResolverResult;
      out GotDesc, ExpDesc: String); overload;
    procedure GetIncompatibleTypeDesc(const GotType, ExpType: TPasType;
      out GotDesc, ExpDesc: String); overload;
    procedure GetIncompatibleProcParamsDesc(GotType, ExpType: TPasProcedureType;
      out GotDesc, ExpDesc: string);
    procedure RaiseMsg(const Id: TMaxPrecInt; MsgNumber: integer; const Fmt: String;
      Args: Array of const;
      ErrorPosEl: TPasElement); virtual;
    procedure RaiseNotYetImplemented(id: TMaxPrecInt; El: TPasElement; Msg: string = ''); virtual;
    procedure RaiseInternalError(id: TMaxPrecInt; const Msg: string = '');
    procedure RaiseInvalidScopeForElement(id: TMaxPrecInt; El: TPasElement; const Msg: string = '');
    procedure RaiseIdentifierNotFound(id: TMaxPrecInt; Identifier: string; El: TPasElement);
    procedure RaiseXExpectedButYFound(id: TMaxPrecInt; const X,Y: string; El: TPasElement);
    procedure RaiseXExpectedButTypeYFound(id: TMaxPrecInt; const X: string; Y: TPasType; El: TPasElement);
    procedure RaiseContextXExpectedButYFound(id: TMaxPrecInt; const C,X,Y: string; El: TPasElement);
    procedure RaiseContextXInvalidY(id: TMaxPrecInt; const X,Y: string; El: TPasElement);
    procedure RaiseConstantExprExp(id: TMaxPrecInt; ErrorEl: TPasElement);
    procedure RaiseVarExpected(id: TMaxPrecInt; ErrorEl: TPasElement; IdentEl: TPasElement);
    procedure RaiseRangeCheck(id: TMaxPrecInt; ErrorEl: TPasElement);
    procedure RaiseIncompatibleTypeDesc(id: TMaxPrecInt; MsgNumber: integer;
      const Args: array of const;
      const GotDesc, ExpDesc: String; ErrorEl: TPasElement);
    procedure RaiseIncompatibleType(id: TMaxPrecInt; MsgNumber: integer;
      const Args: array of const;
      GotType, ExpType: TPasType; ErrorEl: TPasElement);
    procedure RaiseIncompatibleTypeRes(id: TMaxPrecInt; MsgNumber: integer;
      const Args: array of const;
      const GotType, ExpType: TPasResolverResult;
      ErrorEl: TPasElement);
    procedure RaiseHelpersCannotBeUsedAsType(id: TMaxPrecInt; ErrorEl: TPasElement);
    procedure RaiseInvalidProcTypeModifier(id: TMaxPrecInt; ProcType: TPasProcedureType;
      ptm: TProcTypeModifier; ErrorEl: TPasElement);
    procedure RaiseInvalidProcModifier(id: TMaxPrecInt; Proc: TPasProcedure;
      pm: TProcedureModifier; ErrorEl: TPasElement);
    procedure WriteScopes;
    procedure WriteScopesShort(Title: string);
    // find value and type of an element
    procedure ComputeElement(El: TPasElement; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil); virtual;
    procedure ComputeResultElement(El: TPasResultElement; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil); virtual;
    function ComputeProcAsyncResult(El: TPasElement; var ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement = nil): boolean; virtual; // for descendants to return the promise
    function Eval(Expr: TPasExpr; Flags: TResEvalFlags; Store: boolean = true): TResEvalValue; overload;
    function Eval(const Value: TPasResolverResult; Flags: TResEvalFlags; Store: boolean = true): TResEvalValue; overload;
    // checking compatibility
    function IsSameType(TypeA, TypeB: TPasType; ResolveAlias: TPRResolveAlias): boolean; // check if it is exactly the same
    function HasExactType(const ResolvedEl: TPasResolverResult): boolean; // false if HiTypeEl was guessed, e.g. 1 guessed a btLongint
    function IndexOfGenericParam(Params: TPasExprArray): integer;
    procedure CheckUseAsType(aType: TPasElement; id: TMaxPrecInt;
      PosEl: TPasElement);
    // Inside a specialization of generic DeclEl, its bare name means that
    // specialization; returns DeclEl otherwise.
    function MapGenericSelfRef(DeclEl, PosEl: TPasElement): TPasElement;
    // Whether the bare name of GenEl may be used at PosEl (objfpc self-reference).
    function IsInsideGenericTypeAt(GenEl: TPasGenericType; PosEl: TPasElement): Boolean;
    function CheckCallProcCompatibility(ProcType: TPasProcedureType;
      Params: TParamsExpr; RaiseOnError: boolean; SetReferenceFlags: boolean = false): integer;
    function CheckExplicitSpecBareTemplateArgs(Proc: TPasProcedure;
      TemplParams: TFPList; Params: TParamsExpr): boolean;
    function CheckCallPropertyCompatibility(PropEl: TPasProperty;
      Params: TParamsExpr; RaiseOnError: boolean): integer;
    function CheckCallArrayCompatibility(ArrayEl: TPasArrayType;
      Params: TParamsExpr; RaiseOnError: boolean; EmitHints: boolean = false): integer;
    function CheckParamCompatibility(Expr: TPasExpr; Param: TPasArgument;
      ParamNo: integer; RaiseOnError: boolean; SetReferenceFlags: boolean = false): integer;
    function IsUntypedPointerDeref(Expr: TPasExpr): boolean;
    function CheckParamResCompatibility(Expr: TPasExpr; const ExprResolved,
      ParamResolved: TPasResolverResult; ParamNo: integer; RaiseOnError: boolean;
      SetReferenceFlags: boolean): integer;
    function IsInsideSpecialization: boolean;
    function SpecializedSourceModule(El: TPasElement): TPasModule;
    function InUnboundGeneric: boolean;
    function UndecidedPair(A, B: TPasType): boolean;
    function ParamIsUndecided(El: TPasElement): boolean;
    // Whether the name of TemplType, looked up from ErrorEl's scope, finds TemplType itself.
    function TemplateIsInScope(TemplType: TPasGenericTemplateType;
      ErrorEl: TPasElement): boolean;
    function IsPartiallySpecializedType(El: TPasType): boolean;
    function CheckAssignCompatibilityUserType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckAssignCompatibilityArrayType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer;
    function CheckAssignCompatibilityPointerType(LTypeEl, RTypeEl: TPasType;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
    function IsStaticCharArray(El: TPasType): boolean;
    function CheckEqualCompatibilityUserType(
      const LHS, RHS: TPasResolverResult; ErrorEl: TPasElement;
      RaiseOnIncompatible: boolean): integer; virtual; // LHS.BaseType=btContext=RHS.BaseType and both rrfReadable
    function CheckTypeCast(El: TPasType; Params: TParamsExpr; RaiseOnError: boolean): integer;
    function HasExplicitOperatorFor(aType: TPasType): boolean;
    function FindImplicitOperatorTo(aType: TPasType;
      const RHS: TPasResolverResult): TPasOperator;
    function HasImplicitOperatorTo(aType: TPasType;
      const RHS: TPasResolverResult): boolean;
    function FindGlobalConvOperator(aSourceType, aTargetType: TPasType;
      OpTypes: TOperatorTypes): TPasOperator;
    function CheckTypeCastRes(const FromResolved, ToResolved: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnError: boolean): integer; virtual;
    function CheckTypeCastArray(FromType, ToType: TPasArrayType;
      ErrorEl: TPasElement; RaiseOnError: boolean): integer;
    function CheckSrcIsADstType(
      const ResolvedSrcType, ResolvedDestType: TPasResolverResult): integer;
    function CheckClassIsClass(SrcType, DestType: TPasType): integer; virtual;
    function CheckClassesAreRelated(TypeA, TypeB: TPasType): integer;
    function CheckAssignCompatibilityClasses(LType, RType: TPasClassType): integer; virtual; // not related classes
    function GetClassImplementsIntf(ClassEl, Intf: TPasClassType): TPasClassType;
    function CheckProcOverloadCompatibility(Proc1, Proc2: TPasProcedure): boolean;
    function CheckProcTypeCompatibility(Proc1, Proc2: TPasProcedureType;
      IsAssign: boolean; ErrorEl: TPasElement; RaiseOnIncompatible: boolean): boolean;
    function CheckProcArgCompatibility(Arg1, Arg2: TPasArgument): integer;
    function ProcArgsMatchForSignature(Arg1, Arg2: TPasArgument): boolean;
    function TypesMatchAsWideVsUnicodeString(T1, T2: TPasType): boolean;
    function ResultsMatchAsAnsiStrings(T1, T2: TPasType): boolean;
    function TypesAreOneBaseType(T1, T2: TPasType): boolean;
    function CheckElTypeCompatibility(Arg1, Arg2: TPasType;
      ResolveAlias: TPRResolveAlias): integer;
    function OwnTemplateTypeOf(El: TPasElement): TPasType;
    function MemberOfUndecidedType(Expr: TPasElement): boolean;
    function CheckCanBeLHS(const ResolvedEl: TPasResolverResult;
      ErrorOnFalse: boolean; ErrorEl: TPasElement): boolean;
    function CheckAssignCompatibility(const LHS, RHS: TPasElement;
      RaiseOnIncompatible: boolean = true; ErrorEl: TPasElement = nil): integer;
    function IsConstFoldableProcAddr(RHS: TPasExpr): Boolean;
    procedure CheckAssignExprRange(const LeftResolved: TPasResolverResult; RHS: TPasExpr);
    procedure CheckAssignExprRangeToCustom(const LeftResolved: TPasResolverResult;
      RValue: TResEvalValue; RHS: TPasExpr); virtual;
    function CheckAssignResCompatibility(const LHS, RHS: TPasResolverResult;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean): integer;
    function CheckEqualElCompatibility(Left, Right: TPasElement;
      ErrorEl: TPasElement; RaiseOnIncompatible: boolean;
      SetReferenceFlags: boolean = false): integer;
    function CheckEqualResCompatibility(const LHS, RHS: TPasResolverResult;
      LErrorEl: TPasElement; RaiseOnIncompatible: boolean;
      RErrorEl: TPasElement = nil): integer;
    function IsVariableConst(El, PosEl: TPasElement; RaiseIfConst: boolean): boolean; virtual;
    function ResolvedElCanBeVarParam(const ResolvedEl: TPasResolverResult;
      PosEl: TPasElement; RaiseIfConst: boolean = true): boolean;
    // True for a writable string character-index l-value (s[i]) backed by a real
    // string variable — ComputeArrayParams marks rrfAssignable (not // rrfWritable). 
    //  Used to admit s[i] as an var/out actual (ReadBufferidiom); 
    function IsStringCharIndexLValue(const ResolvedEl: TPasResolverResult): boolean;
    function ResolvedElIsClassOrRecordInstance(const ResolvedEl: TPasResolverResult): boolean;
    // utility functions
    function GetResolver(El: TPasElement): TPasResolver;
    function ElHasModeSwitch(El: TPasElement; ms: TModeSwitch): boolean;
    function GetElModeSwitches(El: TPasElement): TModeSwitches;
    function ElHasBoolSwitch(El: TPasElement; bs: TBoolSwitch): boolean;
    function GetElBoolSwitches(El: TPasElement): TBoolSwitches;
    function GetProcTypeDescription(ProcType: TPasProcedureType;
      Flags: TPRProcTypeDescFlags = [prptdUseName,prptdResolveSimpleAlias]): string;
    function GetResolverResultDescription(const T: TPasResolverResult; OnlyType: boolean = false): string;
    function GetTypeDescription(aType: TPasType; AddPath: boolean = false): string;
    function GetTypeDescription(const R: TPasResolverResult; AddPath: boolean = false): string; virtual;
    function GetBaseDescription(const R: TPasResolverResult; AddPath: boolean = false): string; virtual;
    function GetProcFirstImplEl(Proc: TPasProcedure): TPasImplElement;
    function GetProcTemplateTypes(Proc: TPasProcedure): TFPList; // list of TPasGenericTemplateType
    function GetProcName(Proc: TPasProcedure; WithTemplates: boolean = true): string;
    function GetPasPropertyAncestor(El: TPasProperty; WithRedeclarations: boolean = false): TPasProperty;
    function GetPasPropertyType(El: TPasProperty): TPasType;
    function GetPasPropertyArgs(El: TPasProperty): TFPList;
    function GetPasPropertyGetter(El: TPasProperty): TPasElement;
    function IsFieldProperty(El: TPasProperty): boolean;
    function GetPasPropertyReadAccessor(El: TPasProperty): TPasExpr;
    function StringCodePageExprOf(T: TPasType): string;
    function TypeChainReaches(FromType, ToType: TPasType): boolean;
    function GetRemainingRangesArray(ArrType: TPasArrayType): TPasArrayType;
    function ProcTypesHaveSameSignature(T1, T2: TPasType): boolean;
    function GetPasPropertyWriteAccessor(El: TPasProperty): TPasExpr;
    function GetAccessorDeclaration(Expr: TPasExpr): TPasElement;
    function AccessorElementType(Expr: TPasExpr): TPasType;
    function GetPasPropertySetter(El: TPasProperty): TPasElement;
    function GetPropertyAccessorForParams(El: TPasProperty; Params: TParamsExpr;
      IsSetter: boolean): TPasElement;
    function GetPropertySetterForValue(El: TPasProperty;
      ValueExpr: TPasExpr): TPasElement;
    function GetPasPropertyIndex(El: TPasProperty): TPasExpr;
    function GetPasPropertyStoredExpr(El: TPasProperty): TPasExpr;
    function GetPasPropertyDefaultExpr(El: TPasProperty): TPasExpr;
    function GetPasClassAncestor(ClassEl: TPasClassType; SkipAlias: boolean): TPasType;
    function GetPasClassForward(ClassEl: TPasClassType): TPasClassType;
    function GetParentProcBody(El: TPasElement): TProcedureBody;
    function ProcHasImplElements(Proc: TPasProcedure): boolean; virtual;
    function IndexOfImplementedInterface(ClassEl: TPasClassType; aType: TPasType): integer;
    function GetLoop(El: TPasElement): TPasImplElement;
    function ResolveAliasType(aType: TPasType; SkipTypeAlias: boolean = true): TPasType;
    function ResolveAliasTypeEl(El: TPasElement): TPasType; inline;
    function ExprIsAddrTarget(El: TPasExpr): boolean;
    function IsNameExpr(El: TPasExpr): boolean; inline; // TPrimitiveExpr with Kind=pekIdent
    function GetNameExprValue(El: TPasExpr): string; // TPrimitiveExpr with Kind=pekIdent
    function GetNextDottedExpr(El: TPasExpr): TPasExpr;
    function GetLeftMostExpr(El: TPasExpr): TPasExpr;
    function GetRightMostExpr(El: TPasExpr): TPasExpr;
    procedure GetParamsOfNameExpr(El: TPasExpr; out ParentParams: TPRParentParams);
    function GetInlineSpecOfNameExpr(El: TPasExpr): TInlineSpecializeExpr;
    function GetUsesUnitInFilename(InFileExpr: TPasExpr): string;
    function GetPathStart(El: TPasExpr): TPasExpr;
    function GetPathEndIdent(El: TPasExpr; AllowCall: boolean): TPasExpr;
    function GetNewInstanceExpr(El: TPasExpr): TPasExpr;
    function ParentNeedsExprResult(El: TPasExpr): boolean;
    function GetReference_ConstructorType(Ref: TResolvedReference; Expr: TPasExpr): TPasResolverResult;
    function GetParamsValueRef(Params: TParamsExpr): TResolvedReference;
    function GetSetType(const ResolvedSet: TPasResolverResult): TPasSetType;
    function GetRecordValuesMembersType(TypeEl: TPasType): TPasMembersType;
    function ObjectHasVMT(TypeEl: TPasType): boolean;
    // True when a string element (s[i]) is a writable l-value whose address can
    // be taken - a native backend, not pas2js. See StringToCharElement.
    function StringElementIsWritable: boolean; virtual;
    { True for `@ProcVar` used as an assignment TARGET in Delphi mode. }
    function AsOperatorAllowsUpcast: boolean; virtual;
    function IsConstIntFitting(const R: TPasResolverResult;
      TargetBT: TResolverBaseType): boolean;
    function IsCharPointerRes(const R: TPasResolverResult): boolean;
    function IsDynArrayElementExpr(El: TPasElement): boolean;
    function IsDelphiProcVarAddr(El: TPasExpr): boolean;
    function OverrideRootModule(El: TPasElement): TPasModule;
    { Conservative: True only when El is a private/protected member that the
      current context definitely cannot reach. Anything unclear answers False,
      so it can only DEMOTE an overload candidate, never reject one. }
    function IsDefinitelyInaccessible(El: TPasElement): boolean;
    function IsDynArray(TypeEl: TPasType; OptionalOpenArray: boolean = true): boolean;
    function IsOpenArray(TypeEl: TPasType): boolean;
    function IsDynOrOpenArray(TypeEl: TPasType): boolean;
    function IsArrayOfConst(TypeEl: TPasType): boolean;
    function GetArrayElType(ArrType: TPasArrayType): TPasType;
    function GetPartialArrayType(ArrType: TPasArrayType; ConsumedDims: Integer): TPasArrayType;
    function SameArrayRanges(A, B: TPasArrayType): Boolean; // same dimension count and index bounds
    function IsVarInit(Expr: TPasExpr): boolean;
    function IsEmptyArrayExpr(const ResolvedEl: TPasResolverResult): boolean;
    function IsClassMethod(El: TPasElement): boolean;
    function IsClassField(El: TPasElement): boolean;
    function GetFunctionType(El: TPasElement): TPasFunctionType;
    function MethodIsStatic(El: TPasProcedure): boolean; // does not check if El is a method
    function IsMethod(El: TPasProcedure): boolean;
    function IsMethod_SelfIsClass(El: TPasElement): boolean;
    function IsHelperMethod(El: TPasElement): boolean; virtual;
    function IsHelper(El: TPasElement): boolean;
    function IsExternalClass_Name(aClass: TPasClassType; const ExtName: string): boolean;
    function IsProcedureType(const ResolvedEl: TPasResolverResult; HasValue: boolean): boolean;
    function IsArrayType(const ResolvedEl: TPasResolverResult): boolean;
    function IsArrayExpr(Expr: TParamsExpr): TPasArrayType;
    function IsArrayOperatorAdd(Expr: TPasExpr): boolean;
    function IsTypeCast(Params: TParamsExpr): boolean;
    function ExprNeedsSpecialization(Expr: TPasExpr): boolean;
    // "type of" operator
    function GetTypeOfOperandType(Operand: TPasExpr; ErrorEl: TPasElement): TPasType; virtual;
    function IsTypeOfTypeRefOperand(Operand: TPasExpr): boolean;
    procedure TypeOfOperandTypeToValue(var ResolvedEl: TPasResolverResult; Expr: TPasExpr);
    function GetTypeOfCastRoot(Params: TParamsExpr): TUnaryExpr;
    function GetTypeOfCastValueType(Value: TPasElement): TPasType; virtual;
    function IsTypeOfCastExpr(El: TUnaryExpr): boolean;
    procedure ComputeTypeOfExpr(El: TUnaryExpr; out ResolvedEl: TPasResolverResult;
      Flags: TPasResolverComputeFlags; StartEl: TPasElement);
    property TypeOfOperandLevel: integer read FTypeOfOperandLevel;
    function IsGenericTemplType(const ResolvedEl: TPasResolverResult): boolean;
    function IsDeferredTemplMember(Expr: TPasExpr): boolean;
    function IsRecordComposingTemplate(El: TPasRecordType): boolean;
    function ClassOfTemplType(const ResolvedEl: TPasResolverResult): TPasGenericTemplateType;
    function DerefPointerToArray(var R: TPasResolverResult;
      PosEl: TPasElement): boolean;
    function IsPartialSpecTypeArg(El, GenEl: TPasElement): boolean;
    function InPartialSpecWithTypeArg(GenEl: TPasElement): boolean;
    function GetTypeParameterCount(aType: TPasGenericType): integer;
    function GetGenericConstraintKeyword(El: TPasElement): TToken;
    function HasClassConstraint(TemplType: TPasGenericTemplateType): Boolean;
    function HasClassTypeConstraint(TemplType: TPasGenericTemplateType): Boolean;
    function GetClassTypeConstraint(TemplType: TPasGenericTemplateType): TPasClassType;
    function HasRecordConstraint(TemplType: TPasGenericTemplateType): Boolean;
    function GetGenericConstraintErrorEl(ConstraintEl, TemplType: TPasElement): TPasElement;
    function GetSpecializedEl(El: TPasElement; GenericEl: TPasElement;
      Params: TFPList): TPasElement; virtual;
    function ResolverOfElement(El: TPasElement): TPasResolver;
    procedure RegisterSpecializedItem(El: TPasElement; Item: TPRSpecializedItem);
    function IsSpecializedTypeEl(El: TPasElement): boolean;
    function FindSpecializedItemOfEl(El: TPasElement): TPRSpecializedItem;
    function GetSpecializeParamAsType(Param: TPasElement): TPasType;
    procedure FinishGenericClassOrRecIntf(Scope: TPasGenericScope); virtual;
    procedure FinishSpecializations(Scope: TPasGenericScope); virtual;
    procedure CheckPendingForwardTypes(El: TPasElement); virtual;
    procedure CheckPendingForwardProcs(El: TPasElement); virtual;
    function IsSpecialized(El: TPasGenericType): boolean; overload;
    function IsFullySpecialized(El: TPasGenericType): boolean; overload;
    function IsFullySpecialized(Proc: TPasProcedure): boolean; overload;
    function IsInterfaceType(const ResolvedEl: TPasResolverResult;
      IntfType: TPasClassInterfaceType): boolean; overload;
    function IsInterfaceType(TypeEl: TPasType; IntfType: TPasClassInterfaceType): boolean; overload;
    function IsTGUID(RecTypeEl: TPasRecordType): boolean; virtual;
    function IsTGUIDString(const ResolvedEl: TPasResolverResult): boolean; virtual;
    function IsCustomAttribute(El: TPasElement): boolean; virtual;
    function IsSystemUnit(El: TPasModule): boolean; virtual;
    // Owe a specialization's method bodies until something needs the CODE,
    // instead of building them when the specialization is created. Off by
    // default: a consumer that switches it on must drive the building itself
    // (EnsureSpecializedImpl), or the bodies are never made. It applies only
    // while a UNIT IS BEING LOADED (BeginLoadedUnit): a specialization the
    // code being compiled asks for is built where it was asked for, because a
    // body is resolved in the context of the request - which helpers are
    // active, above all - and that context is gone by code generation time.
    property DeferSpecializedImpls: boolean read FDeferSpecImpls
      write FDeferSpecImpls;
    // Answer a PARTIAL specialization - a generic instantiated with a type
    // PARAMETER, `TEnumerator<TAVLTreeMap.TKey>` written inside TAVLTreeMap -
    // with the GENERIC itself instead of building a substituted copy of it.
    // Such a reference is only there so the enclosing generic's own code can be
    // checked; it is specialized again, with real arguments, when that generic
    // is. fpc checks against the generic the same way. rtl-generics writes 8824
    // of them against 121 real ones, each otherwise a whole class copied member
    // by member. Off by default: it changes what such a reference resolves to.
    property PartialSpecsAsGeneric: boolean read FPartialSpecsAsGeneric
      write FPartialSpecsAsGeneric;
    // Bracket the loading of an already-compiled unit.
    procedure BeginLoadedUnit;
    procedure EndLoadedUnit;
    // Whether El is a specialization, or is declared inside one.
    function IsInsideSpecialization(El: TPasElement): boolean;
    // Build the specialization's method bodies, if they are still owed.
    procedure EnsureSpecializedImpl(El: TPasElement);
    // Every body still owed, as a backstop.
    procedure BuildOwedSpecializedImpls;
    // Whether a type argument of Item is a forward class not yet bound to its declaration.
    function HasUnboundForwardClassParam(Item: TPRSpecializedItem): boolean;
    // Build the bodies that waited for their forward class arguments to be bound.
    procedure BuildForwardWaitingSpecImpls;
    // The resolver of the module that declares Item's generic, else Self.
    function GenericModuleResolver(Item: TPRSpecializedItem): TPasResolver;
    function GetAttributeCallsEl(El: TPasElement): TPasExprArray; virtual;
    function GetAttributeCalls(Members: TFPList; Index: integer): TPasExprArray; virtual;
    function ProcNeedsParams(El: TPasProcedureType): boolean;
    function ProcHasSelf(El: TPasProcedure): boolean; // returns false for local procs
    procedure CreateProcSelfArg(Proc: TPasProcedure); virtual;
    function IsProcOverride(AncestorProc, DescendantProc: TPasProcedure): boolean;
    function IsProcOverload(Proc: TPasProcedure): boolean;
    { True when a bare ProcName inside EnclosingName's body is that routine's
      own Result - the enclosing name may be qualified and/or specialized. }
    function SameSelfRefProcName(const EnclosingName, ProcName: String): boolean;
    function RedirectSelfNameToResult(Expr: TPasExpr;
      var ExprResolved: TPasResolverResult; SetReferenceFlags: boolean): boolean;
    { False when El is the member side of "X.Name", which is never a self-ref. }
    function IsSelfRefCandidate(El: TPasExpr): boolean;
    { True when a bare Proc name at El sits inside a function of that same name. }
    function IsSelfRefResultName(El: TPasElement; Proc: TPasProcedure): boolean;
    function EnclosingFunctionNamed(El: TPasElement; const aName: string): TPasFunction;
    function GetTopLvlProc(El: TPasElement): TPasProcedure;
    function GetParentProc(El: TPasElement; GetDeclProc: boolean): TPasProcedure;
    function GetRangeLength(RangeExpr: TPasExpr): TMaxPrecInt;
    function EvalRangeLimit(RangeExpr: TPasExpr; Flags: TResEvalFlags;
      EvalLow: boolean; ErrorEl: TPasElement): TResEvalValue; virtual; // compute low() or high()
    function EvalTypeRange(Decl: TPasType; Flags: TResEvalFlags): TResEvalValue; virtual; // compute low() and high()
    function HasTypeInfo(El: TPasType): boolean; virtual;
    function IsAnonymousElType(El: TPasType): boolean; virtual;
    // Enum ordinal model. Base = INDEX (0..Count-1; pas2js). TPasNativeResolver
    // overrides these for the native ASSIGNED-value model (type e=(a,b:=8) -> Ord(b)=8).
    function EnumHasHoles(El: TPasEnumType): boolean; virtual;
    function GetEnumValueOrdinal(EnumValue: TPasEnumValue): TMaxPrecInt; virtual;
    function GetEnumValueForOrdinal(El: TPasEnumType; Ord: TMaxPrecInt): TPasEnumValue; virtual; // inverse of GetEnumValueOrdinal
    function GetEnumMinOrdinal(El: TPasEnumType): TMaxPrecInt; virtual;
    function GetEnumMaxOrdinal(El: TPasEnumType): TMaxPrecInt; virtual;
    // Funcref seam: the Invoke proc type for a call on a funcref-derived interface
    // variable. Base/pas2js has no such representation -> nil; the pas2llvm native
    // subclass (which synthesizes $FuncRef$ interfaces) overrides this.
    function GetFuncRefInvokeProcType(TypeEl: TPasType; ArgCount: Integer = -1): TPasProcedureType; virtual;
    // Native-ABI-only procedure-type constraints (cdecl-variadic must be external,
    // nostackframe needs assembler). Base/pas2js has no such target concept -> no-op.
    procedure FinishProcTypeNativeChecks(El: TPasProcedureType; Proc: TPasProcedure); virtual;
    function GetActualBaseType(bt: TResolverBaseType): TResolverBaseType; virtual;
    function GetCombinedBoolean(Bool1, Bool2: TResolverBaseType; ErrorEl: TPasElement): TResolverBaseType; virtual;
    function GetCombinedInt(const Int1, Int2: TPasResolverResult; ErrorEl: TPasElement): TResolverBaseType; virtual;
    procedure GetIntegerProps(bt: TResolverBaseType; out Precision: word; out Signed: boolean);
    function SameOrdinalWidth(bt1, bt2: TResolverBaseType): boolean;
    function GetIntegerRange(bt: TResolverBaseType; out MinVal, MaxVal: TMaxPrecInt): boolean;
    function IntegerStorageOf(const R: TPasResolverResult; out Kind: TResolverBaseType;
      out MinVal, MaxVal: TMaxPrecInt): boolean;
    function IntegerRangeFitsSameStorage(const ArgRes, ParamRes: TPasResolverResult): boolean;
    function IntegerTypeFitsSameStorage(ArgType, ParamType: TPasType): boolean;
    function GetIntegerBaseType(Precision: word; Signed: boolean; ErrorEl: TPasElement): TResolverBaseType;
    function GetSmallestIntegerBaseType(MinVal, MaxVal: TMaxPrecInt): TResolverBaseType; // returns BaseTypeExtended if too big
    function GetCombinedChar(const Char1, Char2: TPasResolverResult; ErrorEl: TPasElement): TResolverBaseType; virtual;
    function GetCombinedString(const Str1, Str2: TPasResolverResult; ErrorEl: TPasElement): TResolverBaseType; virtual;
    function GetCombinedBaseType(const A, B: TPasResolverResult; ErrorEl: TPasElement): TResolverBaseType; virtual;
    function IsElementSkipped(El: TPasElement): boolean; virtual;
    function FindLocalBuiltInSymbol(El: TPasElement): TPasElement; virtual;
    function GetFirstSection(WithUnitImpl: boolean): TPasSection;
    function GetLastSection: TPasSection;
    function GetParentSection(El: TPasElement): TPasSection;
    function FindUsedUnitInSection(aMod: TPasModule; Section: TPasSection): TPasUsesUnit;
    function FirstSectionUsesUnit(aModule: TPasModule): boolean;
    function ImplementationUsesUnit(aModule: TPasModule; NotInIntf: boolean = true): boolean;
    function GetShiftAndMaskForLoHiFunc(BaseType: TResolverBaseType;
      isLoFunc: Boolean; out Mask: LongWord): Integer;
  public
    property Hub: TPasResolverHub read FHub write FHub;
    // options
    property Options: TPasResolverOptions read FOptions write FOptions;
    property AnonymousElTypePostfix: String read FAnonymousElTypePostfix
      write FAnonymousElTypePostfix; // default empty, if set, anonymous element types are named ArrayName+Postfix and added to declarations
    property BaseTypes[bt: TResolverBaseType]: TPasUnresolvedSymbolRef read GetBaseTypes;
    property BaseTypeNames[bt: TResolverBaseType]: string read GetBaseTypeNames;
    property BaseTypeChar: TResolverBaseType read FBaseTypeChar write FBaseTypeChar;
    property BaseTypeExtended: TResolverBaseType read FBaseTypeExtended write FBaseTypeExtended;
    property BaseTypeString: TResolverBaseType read FBaseTypeString write FBaseTypeString;
    property BaseTypeLength: TResolverBaseType read FBaseTypeLength write FBaseTypeLength;
    property BuiltInProcs[bp: TResolverBuiltInProc]: TResElDataBuiltInProc read GetBuiltInProcs;
    property ExprEvaluator: TResExprEvaluator read fExprEvaluator;
    property DynArrayMinIndex: TMaxPrecInt read FDynArrayMinIndex write FDynArrayMinIndex;
    property DynArrayMaxIndex: TMaxPrecInt read FDynArrayMaxIndex write FDynArrayMaxIndex;
    property StoreSrcColumns: boolean read FStoreSrcColumns write FStoreSrcColumns; {
       If true Line and Column is mangled together in TPasElement.SourceLineNumber.
       Use method UnmangleSourceLineNumber to extract. }
    // parsed values
    property DefaultNameSpace: String read FDefaultNameSpace;
    property RootElement: TPasModule read FRootElement write SetRootElement;
    property Step: TPasResolverStep read FStep;
    property ActiveHelpers: TPRHelperEntryArray read FActiveHelpers;
    property FinishedInterfaceIndex: integer read FFinishedInterfaceIndex;
    // scopes
    property Scopes[Index: integer]: TPasScope read GetScopes;
    property ScopeCount: integer read FScopeCount;
    property TopScope: TPasScope read FTopScope;
    property MaximizeFPCCompatibility : Boolean Read GetMaximizeFPCCompatibility Write SetMaximizeFPCCompatibility;
    property DefaultScope: TPasDefaultScope read FDefaultScope write FDefaultScope;
    property ScopeClass_Array: TPasArrayScopeClass read FScopeClass_Array write FScopeClass_Array;
    property ScopeClass_Class: TPasClassScopeClass read FScopeClass_Class write FScopeClass_Class;
    property ScopeClass_InitialFinalization: TPasInitialFinalizationScopeClass read FScopeClass_InitialFinalization write FScopeClass_InitialFinalization;
    property ScopeClass_Module: TPasModuleScopeClass read FScopeClass_Module write FScopeClass_Module;
    property ScopeClass_Procedure: TPasProcedureScopeClass read FScopeClass_Proc write FScopeClass_Proc;
    property ScopeClass_ProcType: TPasProcTypeScopeClass read FScopeClass_ProcType write FScopeClass_ProcType;
    property ScopeClass_EnumType: TPasEnumTypeScopeClass read FScopeClass_EnumType write FScopeClass_EnumType;
    property ScopeClass_Record: TPasRecordScopeClass read FScopeClass_Record write FScopeClass_Record;
    property ScopeClass_Section: TPasSectionScopeClass read FScopeClass_Section write FScopeClass_Section;
    property ScopeClass_WithExpr: TPasWithExprScopeClass read FScopeClass_WithExpr write FScopeClass_WithExpr;
    // last element
    property LastElement: TPasElement read FLastElement;
    property LastMsg: string read FLastMsg write FLastMsg;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
    property LastMsgElement: TPasElement read FLastMsgElement write FLastMsgElement;
    property LastMsgId: TMaxPrecInt read FLastMsgId write FLastMsgId;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastSourcePos: TPasSourcePos read FLastSourcePos write FLastSourcePos;
  end;

function GetTreeDbg(El: TPasElement; Indent: integer = 0): string;
function GetResolverResultDbg(const T: TPasResolverResult): string;
function GetClassAncestorsDbg(El: TPasClassType): string;
function ResolverResultFlagsToStr(const Flags: TPasResolverResultFlags): string;
function GetElementTypeName(El: TPasElement): string; overload;
function GetElementTypeName(C: TPasElementBaseClass): string; overload;
function GetElementDbgPath(El: TPasElement): string; overload;
function ResolveSimpleAliasType(aType: TPasType): TPasType;

procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement;
  LoTypeEl, HiTypeEl: TPasType; Flags: TPasResolverResultFlags); overload;
procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType;
  Flags: TPasResolverResultFlags); overload;
procedure SetResolverValueExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType; ExprEl: TPasExpr;
  Flags: TPasResolverResultFlags); overload;

function ProcNeedsImplProc(Proc: TPasProcedure): boolean;
function ProcNeedsBody(Proc: TPasProcedure): boolean;
function ProcHasGroupOverload(Proc: TPasProcedure): boolean;
procedure ClearHelperList(var List: TPRHelperEntryArray);
function ChompDottedIdentifier(const Identifier: string): string;
function FirstDottedIdentifier(const Identifier: string): string; // without <>
function LastDottedIdentifier(const Identifier: string): string; // without <>
function IsDottedIdentifierPrefix(const Prefix, Identifier: string): boolean;
function GetFirstDotPos(const Identifier: string): integer;
function GetLastDotPos(const Identifier: string): integer;
{$IF FPC_FULLVERSION<30101}
function IsValidIdent(const Ident: string; AllowDots: Boolean = False; StrictDots: Boolean = False): Boolean;
{$ENDIF}
function DotExprToName(Expr: TPasExpr): string;
function NoNil(o: TObject): TObject;

function dbgs(const Flags: TPasResolverComputeFlags): string; overload;
function dbgs(const a: TResolvedRefAccess): string; overload;
function dbgs(const Flags: TResolvedReferenceFlags): string; overload;
function dbgs(const a: TPSRefAccess): string; overload;

implementation





function GetTreeDbg(El: TPasElement; Indent: integer): string;

  procedure LineBreak(SubIndent: integer);
  begin
    Inc(Indent,SubIndent);
    Result:=Result+LineEnding+StringOfChar(' ',Indent);
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
      Result:=Result+GetTreeDbg(TUnaryExpr(El).Operand,Indent)
    else if El.ClassType=TBinaryExpr then
      Result:=Result+'Left={'+GetTreeDbg(TBinaryExpr(El).Left,Indent)+'}'
         +OpcodeStrings[TPasExpr(El).OpCode]
         +'Right={'+GetTreeDbg(TBinaryExpr(El).Right,Indent)+'}'
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
      Result:=Result+GetTreeDbg(TParamsExpr(El).Value,Indent)+'(';
      l:=length(TParamsExpr(El).Params);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+GetTreeDbg(TParamsExpr(El).Params[i],Indent);
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
            +GetTreeDbg(TRecordValues(El).Fields[i].ValueExp,Indent);
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
          Result:=Result+GetTreeDbg(TArrayValues(El).Values[i],Indent);
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
    Result:=Result+GetTreeDbg(TPasProcedure(El).ProcType,Indent);
    end
  else if El is TPasProcedureType then
    begin
    if TPasProcedureType(El).IsReferenceTo then
      Result:=Result+' '+ProcTypeModifiers[ptmIsNested];
    Result:=Result+'(';
    l:=TPasProcedureType(El).Args.Count;
    if l>0 then
      begin
      inc(Indent,2);
      for i:=0 to l-1 do
        begin
        LineBreak(0);
        Result:=Result+GetTreeDbg(TPasArgument(TPasProcedureType(El).Args[i]),Indent);
        if i<l-1 then
          Result:=Result+';'
        end;
      dec(Indent,2);
      end;
    Result:=Result+')';
    if (El is TPasProcedure) and (TPasProcedure(El).ProcType is TPasFunctionType) then
      Result:=Result+':'+GetTreeDbg(TPasFunctionType(TPasProcedure(El).ProcType).ResultEl,Indent);
    if TPasProcedureType(El).IsOfObject then
      Result:=Result+' '+ProcTypeModifiers[ptmOfObject];
    if TPasProcedureType(El).IsNested then
      Result:=Result+' '+ProcTypeModifiers[ptmIsNested];
    if cCallingConventions[TPasProcedureType(El).CallingConvention]<>'' then
      Result:=Result+'; '+cCallingConventions[TPasProcedureType(El).CallingConvention];
    end
  else if El.ClassType=TPasResultElement then
    Result:=Result+GetTreeDbg(TPasResultElement(El).ResultType,Indent)
  else if El.ClassType=TPasArgument then
    begin
    if AccessNames[TPasArgument(El).Access]<>'' then
      Result:=Result+AccessNames[TPasArgument(El).Access];
    if TPasArgument(El).ArgType=nil then
      Result:=Result+'untyped'
    else
      Result:=Result+GetTreeDbg(TPasArgument(El).ArgType,Indent);
    end
  else if El.ClassType=TPasUnresolvedSymbolRef then
    begin
    if El.CustomData is TResElDataBuiltInProc then
      Result:=Result+TResElDataBuiltInProc(TPasUnresolvedSymbolRef(El).CustomData).Signature;
    end;
end;

function GetResolverResultDbg(const T: TPasResolverResult): string;
var
  HiTypeEl: TPasType;
begin
  Result:='[bt='+ResBaseTypeNames[T.BaseType];
  if T.SubType<>btNone then
    Result:=Result+' Sub='+ResBaseTypeNames[T.SubType];
  Result:=Result
         +' Ident='+GetObjName(T.IdentEl);
  HiTypeEl:=ResolveSimpleAliasType(T.HiTypeEl);
  if HiTypeEl<>T.LoTypeEl then
    Result:=Result+' LoType='+GetObjName(T.LoTypeEl)+' HiTypeEl='+GetObjName(HiTypeEl)
  else
    Result:=Result+' Type='+GetObjName(T.LoTypeEl);
  Result:=Result
         +' Expr='+GetObjName(T.ExprEl)
         +' Flags='+ResolverResultFlagsToStr(T.Flags)
         +']';
end;

function GetClassAncestorsDbg(El: TPasClassType): string;

  function GetClassDesc(C: TPasClassType): string;
  var
    Module: TPasModule;
  begin
    if C.IsExternal then
      Result:='class external '
    else
      Result:='class ';
    Module:=C.GetModule;
    if Module<>nil then
      Result:=Result+Module.Name+'.';
    Result:=Result+GetElementDbgPath(C);
  end;

var
  Scope, AncestorScope: TPasClassScope;
  AncestorEl: TPasClassType;
begin
  if El=nil then exit('nil');
  Result:=GetClassDesc(El);
  if El.CustomData is TPasClassScope then
    begin
    Scope:=TPasClassScope(El.CustomData);
    AncestorScope:=Scope.AncestorScope;
    while AncestorScope<>nil do
      begin
      Result:=Result+LineEnding+'  ';
      AncestorEl:=NoNil(AncestorScope.Element) as TPasClassType;
      Result:=Result+GetClassDesc(AncestorEl);
      AncestorScope:=AncestorScope.AncestorScope;
      end;
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

function GetElementTypeName(El: TPasElement): string;
var
  C: TClass;
begin
  if El=nil then
    exit('?');
  C:=El.ClassType;
  if C=TPrimitiveExpr then
    Result:=ExprKindNames[TPrimitiveExpr(El).Kind]
  else if C=TUnaryExpr then
    Result:='unary '+OpcodeStrings[TUnaryExpr(El).OpCode]
  else if C=TBinaryExpr then
    Result:=ExprKindNames[TBinaryExpr(El).Kind]
  else if C=TPasClassType then
    Result:=ObjKindNames[TPasClassType(El).ObjKind]
  else if C=TPasUnresolvedSymbolRef then
    Result:=El.Name
  else
    begin
    Result:=GetElementTypeName(TPasElementBaseClass(C));
    if Result='' then
      Result:=El.ElementTypeName;
    end;
end;

function GetElementTypeName(C: TPasElementBaseClass): string;
begin
  if C=nil then
    exit('nil');
  if C=TPrimitiveExpr then
    Result:='primitive expression'
  else if C=TUnaryExpr then
    Result:='unary expression'
  else if C=TBinaryExpr then
    Result:='binary expression'
  else if C=TBoolConstExpr then
    Result:='boolean const'
  else if C=TNilExpr then
    Result:='nil'
  else if C=TPasAliasType then
    Result:='alias'
  else if C=TPasTypeOfType then
    Result:='type of'
  else if C=TPasPointerType then
    Result:='pointer'
  else if C=TPasTypeAliasType then
    Result:='type alias'
  else if C=TPasClassOfType then
    Result:='class of'
  else if C=TPasSpecializeType then
    Result:='specialize'
  else if C=TInlineSpecializeExpr then
    Result:='inline-specialize'
  else if C=TIfExpr then
    Result:='if expression'
  else if C=TCaseExpr then
    Result:='case expression'
  else if C=TTryExceptExpr then
    Result:='try expression'
  else if C=TPasRangeType then
    Result:='range'
  else if C=TPasArrayType then
    Result:='array'
  else if C=TPasFileType then
    Result:='file'
  else if C=TPasEnumValue then
    Result:='enum value'
  else if C=TPasEnumType then
    Result:='enum type'
  else if C=TPasSetType then
    Result:='set'
  else if C=TPasRecordType then
    Result:='record'
  else if C=TPasClassType then
    Result:='class'
  else if C=TPasArgument then
    Result:='parameter'
  else if C=TPasProcedureType then
    Result:='procedural type'
  else if C=TPasResultElement then
    Result:='function result'
  else if C=TPasFunctionType then
    Result:='functional type'
  else if C=TPasStringType then
    Result:='string[]'
  else if C=TPasVariable then
    Result:='var'
  else if C=TPasExportSymbol then
    Result:='export'
  else if C=TPasContainsAlias then
    Result:='contains alias'
  else if C=TPasConst then
    Result:='const'
  else if C=TPasProperty then
    Result:='property'
  else if C=TPasProcedure then
    Result:='procedure'
  else if C=TPasFunction then
    Result:='function'
  else if C=TPasOperator then
    Result:='operator'
  else if C=TPasClassOperator then
    Result:='class operator'
  else if C=TPasConstructor then
    Result:='constructor'
  else if C=TPasClassConstructor then
    Result:='class constructor'
  else if C=TPasDestructor then
    Result:='destructor'
  else if C=TPasClassDestructor then
    Result:='class destructor'
  else if C=TPasClassProcedure then
    Result:='class procedure'
  else if C=TPasClassFunction then
    Result:='class function'
  else if C=TPasAnonymousProcedure then
    Result:='anonymous procedure'
  else if C=TPasAnonymousFunction then
    Result:='anonymous function'
  else if C=TPasMethodResolution then
    Result:='method resolution'
  else if C=TInterfaceSection then
    Result:='interfacesection'
  else if C=TImplementationSection then
    Result:='implementation'
  else if C=TProgramSection then
    Result:='program section'
  else if C=TLibrarySection then
    Result:='library section'
  else
    Result:=C.ClassName;
end;

function GetElementDbgPath(El: TPasElement): string;
begin
  if El=nil then exit('nil');
  Result:='';
  while El<>nil do
    begin
    if Result<>'' then Result:='.'+Result;
    if El.Name<>'' then
      Result:=El.Name+Result
    else
      Result:=GetElementTypeName(El)+Result;
    El:=El.Parent;
    end;
end;

function ResolveSimpleAliasType(aType: TPasType): TPasType;
var
  C: TClass;
begin
  while aType<>nil do
    begin
    C:=aType.ClassType;
    if (C=TPasAliasType) then
      aType:=TPasAliasType(aType).DestType
    else if (C=TPasTypeOfType) then
      aType:=TPasTypeOfType(aType).DestType
    else if (C=TPasClassType) and TPasClassType(aType).IsForward
        and (aType.CustomData is TResolvedReference) then
      aType:=NoNil(TResolvedReference(aType.CustomData).Declaration) as TPasType
    else
      exit(aType);
    end;
  Result:=nil;
end;

procedure SetResolverIdentifier(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; IdentEl: TPasElement; LoTypeEl,
  HiTypeEl: TPasType; Flags: TPasResolverResultFlags);
begin
  {$IFOPT C+}
  // Only with assertions on: this runs tens of millions of times per compilation
  // (a third of the instructions of one measured run were class-type tests, and
  // this line was 9-11% of it), and it guards against a caller mistake that the
  // test suites cover.
  if IdentEl is TPasExpr then
    raise Exception.Create('20170729101017');
  {$ENDIF}
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=IdentEl;
  ResolvedType.HiTypeEl:=HiTypeEl;
  ResolvedType.LoTypeEl:=LoTypeEl;
  ResolvedType.ExprEl:=nil;
  ResolvedType.Flags:=Flags;
end;

procedure SetResolverTypeExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType;
  Flags: TPasResolverResultFlags);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=nil;
  ResolvedType.HiTypeEl:=HiTypeEl;
  ResolvedType.LoTypeEl:=LoTypeEl;
  ResolvedType.ExprEl:=nil;
  ResolvedType.Flags:=Flags;
end;

procedure SetResolverValueExpr(out ResolvedType: TPasResolverResult;
  BaseType: TResolverBaseType; LoTypeEl, HiTypeEl: TPasType; ExprEl: TPasExpr;
  Flags: TPasResolverResultFlags);
begin
  ResolvedType.BaseType:=BaseType;
  ResolvedType.SubType:=btNone;
  ResolvedType.IdentEl:=nil;
  ResolvedType.HiTypeEl:=HiTypeEl;
  ResolvedType.LoTypeEl:=LoTypeEl;
  ResolvedType.ExprEl:=ExprEl;
  ResolvedType.Flags:=Flags;
end;

function ProcNeedsImplProc(Proc: TPasProcedure): boolean;
begin
  Result:=true;
  if Proc.IsExternal or Proc.IsInternProc then exit(false);
  if Proc.IsForward then exit;
  if Proc.Parent.ClassType=TInterfaceSection then exit;
  if Proc.Parent.ClassType=TPasClassType then
    begin
    // a method declaration
    if not Proc.IsAbstract then exit;
    end;
  Result:=false;
end;

function ProcNeedsBody(Proc: TPasProcedure): boolean;
var
  C: TClass;
begin
  if Proc.IsForward or Proc.IsExternal then exit(false);
  C:=Proc.Parent.ClassType;
  if (C=TInterfaceSection) or C.InheritsFrom(TPasClassType) then exit(false);
  Result:=true;
end;

function ProcHasGroupOverload(Proc: TPasProcedure): boolean;
var
  Data: TObject;
  ProcScope: TPasProcedureScope;
begin
  if Proc.IsOverload then
    exit(true);
  Data:=Proc.CustomData;
  if not (Data is TPasProcedureScope) then
    exit(false);
  ProcScope:=TPasProcedureScope(Data);
  if ppsfIsGroupOverload in ProcScope.Flags then
    exit(true);
  // An override inherits the overload-group status of the method it overrides:
  // TUTF7Encoding.Create (override) is part of TMBCSEncoding's overloaded Create
  // group, so "inherited Create(cp)" still finds the ancestor Create(Integer).
  if Proc.IsOverride and (ProcScope.OverriddenProc<>nil) then
    exit(ProcHasGroupOverload(ProcScope.OverriddenProc));
  Result:=false;
end;

function ProcIsUnimplementedForward(Proc: TPasProcedure): boolean;
// True while a declaration still has no body: an interface-section header, or
// one marked "forward". fpc skips its own missing-overload check in exactly
// this case - compiler/pparautl.pas guards it with "if not(fwpd.forwarddef)"
// and with fwpd.hasforward - so two interface declarations of one name without
// the directive are accepted there (openssl.pas's BioRead pair).
var
  Data: TObject;
begin
  Result:=false;
  if Proc=nil then
    exit;
  if Proc.IsForward then
    exit(true);
  if not (Proc.Parent is TInterfaceSection) then
    exit;
  Data:=Proc.CustomData;
  if not (Data is TPasProcedureScope) then
    exit;
  Result:=TPasProcedureScope(Data).ImplProc=nil;
end;

procedure ClearHelperList(var List: TPRHelperEntryArray);
var
  i: Integer;
begin
  if length(List)=0 then exit;
  for i:=0 to length(List)-1 do
    TPRHelperEntry(List[i]).Free;
  List:=nil;
end;

function ChompDottedIdentifier(const Identifier: string): string;
var
  p, Lvl: Integer;
begin
  Result:=Identifier;
  p:=length(Identifier);
  Lvl:=0;
  while (p>0) do
    begin
    case Identifier[p] of
    '.': if Lvl=0 then break;
    '>': inc(Lvl);
    '<': dec(Lvl);
    end;
    dec(p);
    end;
  Result:=LeftStr(Identifier,p-1);
end;

function FirstDottedIdentifier(const Identifier: string): string;
var
  p, l: SizeInt;
begin
  p:=1;
  l:=length(Identifier);
  repeat
    if p>l then
      exit(Identifier)
    else if Identifier[p] in ['<','.'] then
      exit(LeftStr(Identifier,p-1))
    else
      inc(p);
  until false;
end;

function LastDottedIdentifier(const Identifier: string): string;
var
  p, Lvl, EndP: Integer;
begin
  p:=length(Identifier);
  EndP:=p;
  Lvl:=0;
  while (p>0) do
    begin
    case Identifier[p] of
    '.': if Lvl=0 then break;
    '>': inc(Lvl);
    '<':
      begin
      dec(Lvl);
      EndP:=p-1;
      end;
    end;
    dec(p);
    end;
  Result:=copy(Identifier,p+1,EndP-p);
end;

function IsDottedIdentifierPrefix(const Prefix, Identifier: string): boolean;
var
  l: Integer;
begin
  l:=length(Prefix);
  if (l>length(Identifier))
      or (CompareText(Prefix,LeftStr(Identifier,l))<>0) then
    exit(false);
  Result:=(length(Identifier)=l) or (Identifier[l+1]='.');
end;

function GetFirstDotPos(const Identifier: string): integer;
var
  l: SizeInt;
  Lvl: Integer;
begin
  Result:=1;
  l:=length(Identifier);
  Lvl:=0;
  repeat
    if Result>l then
      exit(-1);
    case Identifier[Result] of
    '.': if Lvl=0 then exit;
    '<': inc(Lvl);
    '>': dec(Lvl);
    end;
    inc(Result);
  until false;
end;

function GetLastDotPos(const Identifier: string): integer;
var
  Lvl: Integer;
begin
  Result:=length(Identifier);
  Lvl:=0;
  while (Result>0) do
    begin
    case Identifier[Result] of
    '.': if Lvl=0 then exit;
    '>': inc(Lvl);
    '<': dec(Lvl);
    end;
    dec(Result);
    end;
end;

function DotExprToName(Expr: TPasExpr): string;
var
  C: TClass;
  Prim: TPrimitiveExpr;
  Bin: TBinaryExpr;
  s: String;
begin
  Result:='';
  if Expr=nil then exit;
  C:=Expr.ClassType;
  if C=TPrimitiveExpr then
    begin
    Prim:=TPrimitiveExpr(Expr);
    case Prim.Kind of
      pekIdent,pekString: Result:=Prim.Value;
      pekSelf: Result:='Self';
    else
      EPasResolve.Create('[20180309155400] DotExprToName '+GetObjName(Prim)+' '+ExprKindNames[Prim.Kind]);
    end;
    end
  else if C=TBinaryExpr then
    begin
    Bin:=TBinaryExpr(Expr);
    if Bin.OpCode=eopSubIdent then
      begin
      Result:=DotExprToName(Bin.Left);
      if Result='' then exit;
      s:=DotExprToName(Bin.Right);
      if s='' then exit('');
      Result:=Result+'.'+s;
      end;
    end;
end;

function NoNil(o: TObject): TObject;
begin
  if o=nil then
    raise Exception.Create('');
  Result:=o;
end;

{$IF FPC_FULLVERSION<30101}
function IsValidIdent(const Ident: string; AllowDots: Boolean;
  StrictDots: Boolean): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNum = Alpha + ['0'..'9'];
  Dot = '.';
var
  First: Boolean;
  I, Len: Integer;
begin
  Len := Length(Ident);
  if Len < 1 then
    Exit(False);
  First := True;
  for I := 1 to Len do
  begin
    if First then
    begin
      Result := Ident[I] in Alpha;
      First := False;
    end
    else if AllowDots and (Ident[I] = Dot) then
    begin
      if StrictDots then
      begin
        Result := I < Len;
        First := True;
      end;
    end
    else
      Result := Ident[I] in AlphaNum;
    if not Result then
      Break;
  end;
end;
{$ENDIF}

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

function dbgs(const a: TPSRefAccess): string;
begin
  str(a,Result);
end;

{$i pasres_scopes.inc}
{$i pasres_lookup.inc}
{$i pasres_finish.inc}
{$i pasres_statements.inc}
{$i pasres_expressions.inc}
{$i pasres_add.inc}
{$i pasres_operators.inc}
{$i pasres_eval.inc}
{$i pasres_generics.inc}
{$i pasres_builtins.inc}
{$i pasres_core.inc}
{$i pasres_callcompat.inc}
{$i pasres_typecompat.inc}
{$i pasres_queries.inc}

end.
