{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

Abstract:
  Write and read a precompiled module (pju).

  Store whole unit, except all
    procedure declarations, proc bodies, finalization/initialization sections are
    replaced by
    -precompiled code
    -lists of references
    -local consts
  The useanalyzer needs the references - TPas2jsUseAnalyzer.

  Due to uses cycles, ability to stop read after interface
  ReadContinueImplementation
}
unit Pas2JsFiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, SysUtils, contnrs, AVL_Tree, crc,
  fpjson, jsonparser, jsonscanner,
  PasTree, PScanner, PParser, PasResolveEval, PasResolver,
  Pas2jsFileUtils, FPPas2Js;

const
  PJUMagic = 'Pas2JSCache';
  PJUVersion = 1;

  PJUDefaultParserOptions: TPOptions = po_Pas2js;

  PJUParserOptionNames: array[TPOption] of string = (
    'delphi',
    'KeepScannerError',
    'CAssignments',
    'ResolveStandardTypes',
    'AsmWhole',
    'NoOverloadedProcs',
    'KeepClassForward',
    'ArrayRangeExpr',
    'SelfToken',
    'CheckModeSwitches',
    'CheckCondFunction',
    'StopOnErrorDirective',
    'ExtClassConstWithoutExpr');

  PJUDefaultModeSwitches: TModeSwitches = [
    msObjfpc,
    msClass,
    msResult,
    msNestedComment,
    msRepeatForward,
    msInitFinal,
    msOut,
    msDefaultPara,
    msHintDirective,
    msProperty,
    msExcept,
    msDefaultUnicodestring,
    msCBlocks];

  PJUModeSwitchNames: array[TModeSwitch] of string = (
    'None',
    'Fpc',
    'Objfpc',
    'Delphi',
    'DelphiUnicode',
    'TP7',
    'Mac',
    'Iso',
    'Extpas',
    'GPC',
    'Class',
    'Objpas',
    'Result',
    'StringPchar',
    'CVarSupport',
    'NestedComment',
    'TPProcVar',
    'MacProcVar',
    'RepeatForward',
    'Pointer2Procedure',
    'AutoDeref',
    'InitFinal',
    'DefaultAnsistring',
    'Out',
    'DefaultPara',
    'HintDirective',
    'DuplicateNames',
    'Property',
    'DefaultInline',
    'Except',
    'ObjectiveC1',
    'ObjectiveC2',
    'NestedProcVars',
    'NonLocalGoto',
    'AdvancedRecords',
    'ISOLikeUnaryMinus',
    'SystemCodePage',
    'FinalFields',
    'DefaultUnicodestring',
    'TypeHelpers',
    'CBlocks',
    'ISOLikeIO',
    'ISOLikeProgramsPara',
    'ISOLikeMod',
    'ExternalClass',
    'PrefixedAttributes',
    'IgnoreInterfaces',
    'IgnoreAttributes'
    );

  PJUDefaultBoolSwitches: TBoolSwitches = [
    bsHints,
    bsNotes,
    bsWarnings
    ];
  PJUBoolSwitchNames: array[TBoolSwitch] of string = (
    'None',
    'Align',
    'BoolEval',
    'Assertions',
    'DebugInfo',
    'Extension',
    'ImportedData',
    'LongStrings',
    'IOChecks',
    'WriteableConst',
    'LocalSymbols',
    'TypeInfo',
    'Optimization',
    'OpenStrings',
    'OverflowChecks',
    'RangeChecks',
    'TypedAddress',
    'SafeDivide',
    'VarStringChecks',
    'Stackframes',
    'ExtendedSyntax',
    'ReferenceInfo',
    'Hints',
    'Notes',
    'Warnings',
    'Macro',
    'ScopedEnums',
    'ObjectChecks'
    );

  PJUDefaultConvertOptions: TPasToJsConverterOptions = [];
  PJUConverterOptions: array[TPasToJsConverterOption] of string = (
    'LowerCase',
    'SwitchStatement',
    'EnumNumbers',
    'UseStrict',
    'NoTypeInfo',
    'EliminateDeadCode'
    );

  PJUDefaultTargetPlatform = PlatformBrowser;
  PJUTargetPlatformNames: array[TPasToJsPlatform] of string = (
    'Browser',
    'NodeJS'
    );

  PJUDefaultTargetProcessor = ProcessorECMAScript5;
  PJUTargetProcessorNames: array[TPasToJsProcessor] of string = (
    'ECMAScript5',
    'ECMAScript6'
    );

  PJUMemberVisibilityNames: array[TPasMemberVisibility] of string = (
    'Default',
    'Private',
    'Protected',
    'Public',
    'Published',
    'Automated',
    'StrictPrivate',
    'StrictProtected'
    );

  PJUMemberHintNames: array[TPasMemberHint] of string = (
    'Deprecated',
    'Library',
    'Platform',
    'Experimental',
    'Unimplemented'
    );

  PJUDefaultModuleScopeFlags = [pmsfRangeErrorSearched];
  PJUModuleScopeFlagNames: array[TPasModuleScopeFlag] of string = (
    'AssertSearched',
    'RangeErrorNeeded',
    'RangeErrorSearched'
    ) ;

  PJUDefaultIdentifierKind = pikSimple;
  PJUIdentifierKindNames: array[TPasIdentifierKind] of string = (
    'None',
    'BaseType',
    'BuiltInProc',
    'Simple',
    'Proc',
    'Namespace'
    );

  PJUVarModifierNames: array[TVariableModifier] of string = (
    'CVar',
    'External',
    'Public',
    'Export',
    'Class',
    'Static'
    );

  PJUDefaultExprKind = pekIdent;
  PJUExprKindNames: array[TPasExprKind] of string = (
    'Ident',
    'Number',
    'String',
    'Set',
    'Nil',
    'Bool',
    'Range',
    'Unary',
    'Binary',
    'Func',
    'Array',
    'List',
    'Inherited',
    'Self'
    );

  PJUExprOpCodeNames: array[TExprOpCode] of string = (
    'None',
    'Add',
    'Sub',
    'Mul',
    'DivF',
    'DivI',
    'Mod',
    'Pow',
    'Shr',
    'Shl',
    'Not',
    'And',
    'Or',
    'Xor',
    'Eq',
    'NE',
    'LT',
    'GT',
    'LTE',
    'GTE',
    'In',
    'Is',
    'As',
    'SymDif',
    'Addr',
    'Deref',
    'MemAddr',
    'SubId'
    );

type
  { TPJUInitialFlags }

  TPJUInitialFlags = class
  public
    ParserOptions: TPOptions;
    ModeSwitches: TModeSwitches;
    BoolSwitches: TBoolSwitches;
    ConverterOptions: TPasToJsConverterOptions;
    TargetPlatform: TPasToJsPlatform;
    TargetProcessor: TPasToJsProcessor;
    // ToDo: defines
    constructor Create;
    procedure Clear;
  end;

type
  TPJUSourceFileType = (
    sftUnit,
    sftInclude
  );
  TPJUSourceFileKinds = set of TPJUSourceFileType;
const
  PJUSourceFileTypeNames: array[TPJUSourceFileType] of string = (
    'Unit',
    'Include'
    );

type
  TPJUSourceFileChecksum = cardinal;
  EPas2JsFilerError = class(Exception)
  public
    Owner: TObject;
  end;
  EPas2JsWriteError = class(EPas2JsFilerError);
  EPas2JsReadError = class(EPas2JsFilerError);

  { TPJUSourceFile }

  TPJUSourceFile = class
  public
    FileType: TPJUSourceFileType;
    Filename: string;
    Checksum: TPJUSourceFileChecksum;
    Index: integer;
  end;
  TPJUSourceFileArray = array of TPJUSourceFile;

  TPJUGetSrcEvent = procedure(Sender: TObject; aFilename: string;
    out p: PChar; out Count: integer) of object;

  { TPJUFilerContext - base class TPJUWriterContext/TPJUReaderContext }

  TPJUFilerContext = class
  public
    LastElement: TPasElement;
    ModeSwitches: TModeSwitches;
    BoolSwitches: TBoolSwitches;
  end;

  { TPJUFilerPendingElRef }

  TPJUFilerPendingElRef = class
  public
    Next: TPJUFilerPendingElRef;
  end;

  { TPJUFilerElementRef }

  TPJUFilerElementRef = class
  public
    Element: TPasElement;
    Id: integer; // 0 = pending
    Pending: TPJUFilerPendingElRef;
    Obj: TJSONObject;
    procedure AddPending(Item: TPJUFilerPendingElRef);
    procedure Clear;
    destructor Destroy; override;
  end;
  TPJUFilerElementRefArray = array of TPJUFilerElementRef;

  { TPJUFiler - base class TPJUWriter/TPJUReader}

  TPJUFiler = class
  private
    FElementRefs: TAVLTree; // tree of TPJUFilerElementRef sorted for Element
    FInitialFlags: TPJUInitialFlags;
    FOnGetSrc: TPJUGetSrcEvent;
    FParser: TPasParser;
    FResolver: TPas2JSResolver;
    FScanner: TPascalScanner;
    FSourceFiles: TObjectList;
    function GetSourceFiles(Index: integer): TPJUSourceFile;
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = ''); virtual; abstract; overload;
    procedure RaiseMsg(Id: int64; El: TPasElement; const Msg: string = ''); overload;
    function GetDefaultMemberVisibility(El, LastElement: TPasElement): TPasMemberVisibility; virtual;
    procedure GetDefaultsPasIdentifierProps(El: TPasElement; out Kind: TPasIdentifierKind; out Name: string); virtual;
    function GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum; virtual;
    function GetElementReference(El: TPasElement; AutoCreate: boolean = true): TPJUFilerElementRef;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    property Resolver: TPas2JSResolver read FResolver;
    property Parser: TPasParser read FParser;
    property Scanner: TPascalScanner read FScanner;
    property InitialFlags: TPJUInitialFlags read FInitialFlags;
    property OnGetSrc: TPJUGetSrcEvent read FOnGetSrc write FOnGetSrc;
    function SourceFileCount: integer;
    property SourceFiles[Index: integer]: TPJUSourceFile read GetSourceFiles;
    property ElementRefs: TAVLTree read FElementRefs;
  end;

  { TPJUWriterContext }

  TPJUWriterContext = class(TPJUFilerContext)
  public
  end;

  { TPJUWriterPendingElRefObj }

  TPJUWriterPendingElRefObj = class(TPJUFilerPendingElRef)
  public
    Obj: TJSONObject;
    PropName: string;
  end;

  { TPJUWriterPendingElRefArray }

  TPJUWriterPendingElRefArray = class(TPJUFilerPendingElRef)
  public
    Arr: TJSONArray;
    Index: integer;
  end;

  { TPJUWriter }

  TPJUWriter = class(TPJUFiler)
  private
    FElementIdCounter: integer;
    FSourceFilesSorted: TPJUSourceFileArray;
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = ''); override; overload;
    procedure ResolvePendingElRefs(Ref: TPJUFilerElementRef);
    function CheckElScope(El: TPasElement; NotNilId: int64; ScopeClass: TPasScopeClass): TPasScope; virtual;
    procedure AddArrayFlag(Obj: TJSONObject; var Arr: TJSONArray;
      const ArrName, Flag: string; Enable: boolean);
    procedure AddReferenceToArray(Arr: TJSONArray; El: TPasElement); virtual;
    procedure AddReferenceToObj(Obj: TJSONObject; const PropName: string; El: TPasElement); virtual;
    procedure CreateElReferenceId(Ref: TPJUFilerElementRef); virtual;
    procedure WriteHeaderMagic(Obj: TJSONObject); virtual;
    procedure WriteHeaderVersion(Obj: TJSONObject); virtual;
    procedure WriteInitialFlags(Obj: TJSONObject); virtual;
    procedure WriteParserOptions(Obj: TJSONObject; const Value, DefaultValue: TPOptions); virtual;
    procedure WriteModeSwitches(Obj: TJSONObject; const Value, DefaultValue: TModeSwitches); virtual;
    procedure WriteBoolSwitches(Obj: TJSONObject; const Value, DefaultValue: TBoolSwitches); virtual;
    procedure WriteConvertOptions(Obj: TJSONObject; const Value, DefaultValue: TPasToJsConverterOptions); virtual;
    procedure WriteSrcFiles(Obj: TJSONObject); virtual;
    procedure WriteMemberHints(Obj: TJSONObject; const Value, DefaultValue: TPasMemberHints); virtual;
    procedure WritePasElement(Obj: TJSONObject; El: TPasElement; aContext: TPJUWriterContext); virtual;
    procedure WriteModuleScopeFlags(Obj: TJSONObject; const Value, DefaultValue: TPasModuleScopeFlags); virtual;
    procedure WriteModule(Obj: TJSONObject; aModule: TPasModule; aContext: TPJUWriterContext); virtual;
    procedure WriteIdentifierScope(Obj: TJSONObject; Scope: TPasIdentifierScope; aContext: TPJUWriterContext); virtual;
    procedure WriteSection(ParentJSON: TJSONObject; Section: TPasSection;
      const PropName: string; aContext: TPJUWriterContext); virtual;
    procedure WriteDeclarations(ParentJSON: TJSONObject; Decls: TPasDeclarations; aContext: TPJUWriterContext); virtual;
    procedure WriteDeclaration(Obj: TJSONObject; Decl: TPasElement; aContext: TPJUWriterContext); virtual;
    procedure WriteElType(Obj: TJSONObject; const PropName: string; El: TPasElement; aType: TPasType; aContext: TPJUWriterContext); virtual;
    procedure WriteVarModifiers(Obj: TJSONObject; const Value, DefaultValue: TVariableModifiers); virtual;
    procedure WriteVariable(Obj: TJSONObject; Decl: TPasVariable; aContext: TPJUWriterContext); virtual;
    procedure WriteConst(Obj: TJSONObject; Decl: TPasConst; aContext: TPJUWriterContext); virtual;
    procedure WriteExpr(Obj: TJSONObject; const PropName: string; Expr: TPasExpr; aContext: TPJUWriterContext); virtual;
    procedure WritePasExpr(Obj: TJSONObject; Expr: TPasExpr;
      WriteKind: boolean; DefaultOpCode: TExprOpCode; aContext: TPJUWriterContext); virtual;
    procedure WritePrimitiveExpr(Obj: TJSONObject; const PropName: string; Expr: TPrimitiveExpr; aContext: TPJUWriterContext); virtual;
    procedure WriteExternalReferences(ParentJSON: TJSONObject); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure WritePJU(aResolver: TPas2JSResolver;
      InitFlags: TPJUInitialFlags; aStream: TStream); virtual;
    function WriteJSON(aResolver: TPas2JSResolver;
      InitFlags: TPJUInitialFlags): TJSONObject; virtual;
    function IndexOfSourceFile(const Filename: string): integer;
    property SourceFilesSorted: TPJUSourceFileArray read FSourceFilesSorted;
  end;

  { TPJUReaderContext }

  TPJUReaderContext = class(TPJUFilerContext)
  end;

  TOnSetElReference = procedure(Ref: TPJUFilerElementRef; Data: Pointer) of object;

  { TPJUReaderPendingElRef }

  TPJUReaderPendingElRef = class(TPJUFilerPendingElRef)
  public
    Data: Pointer;
    Setter: TOnSetElReference;
  end;

  TPJUReaderPendingIdentifierScope = class
  public
    Scope: TPasIdentifierScope;
    Arr: TJSONArray;
  end;

  { TPJUReader }

  TPJUReader = class(TPJUFiler)
  private
    FElementRefsArray: TPJUFilerElementRefArray; // TPJUFilerElementRef by Id
    FFileVersion: longint;
    FPendingIdentifierScopes: TObjectList; // list of TPJUReaderPendingIdentifierScope
    procedure Set_Variable_VarType(Ref: TPJUFilerElementRef; Data: Pointer);
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = ''); overload; override;
    function CheckJSONArray(Data: TJSONData; El: TPasElement; const PropName: string): TJSONArray;
    function CheckJSONObject(Data: TJSONData; Id: int64): TJSONObject;
    function CheckJSONString(Data: TJSONData; Id: int64): String;
    function ReadString(Obj: TJSONObject; const PropName: string; out s: string; El: TPasElement): boolean;
    function ReadInteger(Obj: TJSONObject; const PropName: string; out i: integer; El: TPasElement): boolean;
    function ReadBoolean(Obj: TJSONObject; const PropName: string; out b: boolean; El: TPasElement): boolean;
    function ReadArray(Obj: TJSONObject; const PropName: string; out Arr: TJSONArray; El: TPasElement): boolean;
    function AddElReference(Id: integer; ErrorEl: TPasElement; El: TPasElement): TPJUFilerElementRef; virtual;
    procedure PromiseSetElReference(Id: integer; const Setter: TOnSetElReference; Data: Pointer; ErrorEl: TPasElement); virtual;
    procedure ReadHeaderMagic(Obj: TJSONObject); virtual;
    procedure ReadHeaderVersion(Obj: TJSONObject); virtual;
    procedure ReadArrayFlags(Data: TJSONData; El: TPasElement; const PropName: string; out Names: TStringDynArray; out Enable: TBooleanDynArray);
    function ReadParserOptions(Data: TJSONData; El: TPasElement; const DefaultValue: TPOptions): TPOptions; virtual;
    function ReadModeSwitches(Data: TJSONData; El: TPasElement; const DefaultValue: TModeSwitches): TModeSwitches; virtual;
    function ReadBoolSwitches(Data: TJSONData; El: TPasElement; const DefaultValue: TBoolSwitches): TBoolSwitches; virtual;
    function ReadConverterOptions(Data: TJSONData; El: TPasElement; const DefaultValue: TPasToJsConverterOptions): TPasToJsConverterOptions; virtual;
    procedure ReadTargetPlatform(Data: TJSONData); virtual;
    procedure ReadTargetProcessor(Data: TJSONData); virtual;
    procedure ReadSrcFiles(Data: TJSONData); virtual;
    function ReadMemberHints(Obj: TJSONObject; El: TPasElement; const DefaultValue: TPasMemberHints): TPasMemberHints; virtual;
    procedure ReadPasElement(Obj: TJSONObject; El: TPasElement; aContext: TPJUReaderContext); virtual;
    procedure ReadSection(Obj: TJSONObject; Section: TPasSection; aContext: TPJUReaderContext); virtual;
    procedure ReadDeclarations(Obj: TJSONObject; Section: TPasSection; aContext: TPJUReaderContext); virtual;
    procedure ReadDeclaration(Obj: TJSONObject; Section: TPasSection; aContext: TPJUReaderContext); virtual;
    procedure ReadElType(Obj: TJSONObject; const PropName: string; El: TPasElement;
      const Setter: TOnSetElReference; aContext: TPJUReaderContext); virtual;
    procedure ReadExpr(Obj: TJSONObject; const PropName: string; Parent: TPasElement; var Expr: TPasExpr; aContext: TPJUReaderContext); virtual;
    procedure ReadPasExpr(Obj: TJSONObject; Expr: TPasExpr; ReadKind: boolean; aContext: TPJUReaderContext); virtual;
    procedure ReadPrimitiveExpr(Obj: TJSONObject; Parent: TPasElement;
      Kind: TPasExprKind; var Expr: TPasExpr; aContext: TPJUReaderContext); virtual;
    function ReadVarModifiers(Obj: TJSONObject; El: TPasElement; const DefaultValue: TVariableModifiers): TVariableModifiers; virtual;
    procedure ReadVariable(Obj: TJSONObject; Decl: TPasVariable; aContext: TPJUReaderContext); virtual;
    procedure ReadConst(Obj: TJSONObject; Decl: TPasConst; aContext: TPJUReaderContext); virtual;
    procedure ReadIdentifierScope(Arr: TJSONArray; Scope: TPasIdentifierScope); virtual;
    function ReadModuleScopeFlags(Obj: TJSONObject; El: TPasElement; const DefaultValue: TPasModuleScopeFlags): TPasModuleScopeFlags; virtual;
    procedure ReadModule(Data: TJSONData; aContext: TPJUReaderContext); virtual;
    // ToDo: procedure ReadExternalReferences(ParentJSON: TJSONObject); virtual;
    procedure ResolvePending; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ReadPJU(aResolver: TPas2JSResolver; aStream: TStream); virtual;
    procedure ReadJSON(aResolver: TPas2JSResolver; Obj: TJSONObject); virtual;
    property FileVersion: longint read FFileVersion;
  end;

function ComparePointer(Data1, Data2: Pointer): integer;
function ComparePJUSrcFiles(File1, File2: Pointer): integer;
function ComparePJUFilerElementRef(Ref1, Ref2: Pointer): integer;
function CompareElWithPJUFilerElementRef(El, Ref: Pointer): integer;

function EncodeVLQ(i: MaxPrecInt): string; overload;
function EncodeVLQ(i: MaxPrecUInt): string; overload;
function DecodeVLQ(const s: string): MaxPrecInt; // base256 Variable Length Quantity
function DecodeVLQ(var p: PByte): MaxPrecInt; // base256 Variable Length Quantity

function ComputeChecksum(p: PChar; Cnt: integer): TPJUSourceFileChecksum;

function ModeSwitchToInt(ms: TModeSwitch): byte;
function StrToPasIdentifierKind(const s: string): TPasIdentifierKind;

function dbgmem(const s: string): string; overload;
function dbgmem(p: PChar; Cnt: integer): string; overload;

implementation

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function ComparePJUSrcFiles(File1, File2: Pointer): integer;
var
  Src1: TPJUSourceFile absolute File1;
  Src2: TPJUSourceFile absolute File2;
begin
  Result:=CompareStr(Src1.Filename,Src2.Filename);
end;

function ComparePJUFilerElementRef(Ref1, Ref2: Pointer): integer;
var
  Reference1: TPJUFilerElementRef absolute Ref1;
  Reference2: TPJUFilerElementRef absolute Ref2;
begin
  Result:=ComparePointer(Reference1.Element,Reference2.Element);
end;

function CompareElWithPJUFilerElementRef(El, Ref: Pointer): integer;
var
  Element: TPasElement absolute El;
  Reference: TPJUFilerElementRef absolute Ref;
begin
  Result:=ComparePointer(Element,Reference.Element);
end;

function EncodeVLQ(i: MaxPrecInt): string;
{ Convert signed number to base256-VLQ:
  Each byte has 8bit, where the least significant bit is the continuation bit
  (1=there is a next byte).
  The first byte contains the sign bit in the last bit
  and the 6 most significant bits of the number.
  For example:
  0 = %00000000 => 0
  1 = %00000001 => -0
  2 = %00000010 => 1
  130 5 = %10000010 %00000101 = 000010 0000101 = 100000101 = 133
}
var
  digits: integer;
begin
  digits:=0;
  if i<0 then
    begin
    if i=Low(MaxPrecInt) then
      begin
      Result:=EncodeVLQ(High(MaxPrecInt)+1);
      Result[1]:=chr(ord(Result[1]) or 1);
      exit;
      end;
    digits:=1;
    i:=-i;
    end;
  inc(digits,(i and %111111) shl 1);
  i:=i shr 6;
  if i>0 then
    inc(digits,%10000000); // need another byte -> set continuation bit
  Result:=chr(digits);
  while i>0 do
    begin
    digits:=i and %1111111;
    i:=i shr 7;
    if i>0 then
      inc(digits,%10000000); // need another byte -> set continuation bit
    Result:=Result+chr(digits);
    end;
end;

function EncodeVLQ(i: MaxPrecUInt): string;
var
  digits: integer;
begin
  digits:=(i and %111111) shl 1;
  if i>0 then
    inc(digits,%10000000); // need another byte -> set continuation bit
  Result:=chr(digits);
  i:=i shr 6;
  while i>0 do
    begin
    digits:=i and %1111111;
    i:=i shr 7;
    if i>0 then
      inc(digits,%10000000); // need another byte -> set continuation bit
    Result:=Result+chr(digits);
    end;
end;

function DecodeVLQ(const s: string): MaxPrecInt;
var
  p: PByte;
begin
  if s='' then
    raise EConvertError.Create('DecodeVLQ empty');
  p:=PByte(s);
  Result:=DecodeVLQ(p);
  if p-PByte(s)<>length(s) then
    raise EConvertError.Create('DecodeVLQ waste');
end;

function DecodeVLQ(var p: PByte): MaxPrecInt;
{ Convert base256-VLQ to signed number,
  For the fomat see EncodeVLQ
}

  procedure RaiseInvalid;
  begin
    raise ERangeError.Create('DecodeVLQ');
  end;

const
  MaxShift = 63; // actually log2(High(MaxPrecInt))
var
  digit, Shift: Integer;
  Negated: Boolean;
begin
  digit:=p^;
  inc(p);
  Negated:=(digit and 1)>0;
  Result:=(digit shr 1) and %111111;
  Shift:=6;
  while digit>=%10000000 do
    begin
    digit:=p^;
    inc(p);
    if Shift>MaxShift then
      RaiseInvalid;
    inc(Result,MaxPrecInt(digit and %1111111) shl Shift);
    inc(Shift,7);
    end;
  if Negated then
    Result:=-Result;
end;

function ComputeChecksum(p: PChar; Cnt: integer): TPJUSourceFileChecksum;
var
  SrcP, SrcEndP, SrcLineEndP, SrcLineStartP: PChar;
  l: PtrInt;
  CheckSum, CurLen: Cardinal;
begin
  if Cnt=0 then exit(0);

  // ignore trailing spaces and unify line endings
  SrcP:=p;
  SrcEndP:=p+Cnt;
  while (SrcEndP>SrcP) and (SrcEndP[-1] in [#9,#10,#13,' ']) do
    dec(SrcEndP);
  CheckSum:=crc32(0,nil,0);
  while SrcP<SrcEndP do
    begin
    SrcLineStartP:=SrcP;
    while (SrcP<SrcEndP) and not (SrcP^ in [#10,#13]) do
      inc(SrcP);
    SrcLineEndP:=SrcP;
    while (SrcLineEndP>SrcLineStartP) and (SrcLineEndP[-1] in [#9,' ']) do
      dec(SrcLineEndP);
    l:=SrcLineEndP-SrcLineStartP;
    while l>0 do
      begin
      if l<$8000 then
        CurLen:=l
      else
        CurLen:=$8000;
      CheckSum:=crc32(CheckSum, PByte(SrcLineStartP), CurLen);
      inc(SrcLineStartP,CurLen);
      dec(l,CurLen);
      end;
    while (SrcP<SrcEndP) and (SrcP^ in [#10,#13]) do
      inc(SrcP);
    end;
  Result:=CheckSum;
end;

function ModeSwitchToInt(ms: TModeSwitch): byte;
begin
  case ms of
    msNone: Result:=0;
    msFpc: Result:=1;
    msObjfpc: Result:=2;
    msDelphi: Result:=3;
    msDelphiUnicode: Result:=4;
    msTP7: Result:=5;
    msMac: Result:=6;
    msIso: Result:=7;
    msExtpas: Result:=8;
    msGPC: Result:=9;
    msClass: Result:=10;
    msObjpas: Result:=11;
    msResult: Result:=12;
    msStringPchar: Result:=13;
    msCVarSupport: Result:=14;
    msNestedComment: Result:=15;
    msTPProcVar: Result:=16;
    msMacProcVar: Result:=17;
    msRepeatForward: Result:=18;
    msPointer2Procedure: Result:=19;
    msAutoDeref: Result:=20;
    msInitFinal: Result:=21;
    msDefaultAnsistring: Result:=22;
    msOut: Result:=23;
    msDefaultPara: Result:=24;
    msHintDirective: Result:=25;
    msDuplicateNames: Result:=26;
    msProperty: Result:=27;
    msDefaultInline: Result:=28;
    msExcept: Result:=29;
    msObjectiveC1: Result:=30;
    msObjectiveC2: Result:=31;
    msNestedProcVars: Result:=32;
    msNonLocalGoto: Result:=33;
    msAdvancedRecords: Result:=34;
    msISOLikeUnaryMinus: Result:=35;
    msSystemCodePage: Result:=36;
    msFinalFields: Result:=37;
    msDefaultUnicodestring: Result:=38;
    msTypeHelpers: Result:=39;
    msCBlocks: Result:=40;
    msISOLikeIO: Result:=41;
    msISOLikeProgramsPara: Result:=42;
    msISOLikeMod: Result:=43;
    msExternalClass: Result:=44;
    msPrefixedAttributes: Result:=45;
    msIgnoreInterfaces: Result:=46;
    msIgnoreAttributes: Result:=47;
  end;
end;

function StrToPasIdentifierKind(const s: string): TPasIdentifierKind;
var
  Kind: TPasIdentifierKind;
begin
  for Kind in TPasIdentifierKind do
    if s=PJUIdentifierKindNames[Kind] then
      exit(Kind);
  Result:=pikNone;
end;

function dbgmem(const s: string): string;
begin
  if s='' then exit('');
  Result:=dbgmem(PChar(s),length(s));
end;

function dbgmem(p: PChar; Cnt: integer): string;

  procedure AddLine(const Line: string);
  begin
    if Result<>'' then
      Result:=Result+LineEnding;
    Result:=Result+Line;
  end;

var
  c: Char;
  IsTxt: boolean;
  Line: String;
  i: Integer;
begin
  Result:='';
  if (p=nil) or (Cnt<=0) then exit;
  Line:='';
  IsTxt:=false;
  for i:=0 to Cnt-1 do
    begin
    c:=p[i];
    if c in ['a'..'z','A'..'Z','_','/','0'..'9'] then
      begin
      if not IsTxt then
        begin
        Line:=Line+'''';
        IsTxt:=true;
        end;
      Line:=Line+c;
      end
    else
      begin
      if IsTxt then
        begin
        Line:=Line+'''';
        IsTxt:=false;
        end;
      Line:=Line+'#'+HexStr(ord(c),2);
      end;
    if length(Line)>78 then
      begin
      AddLine(Line);
      Line:='';
      end;
    end;
  if Line<>'' then
    AddLine(Line);
end;

{ TPJUFilerElementRef }

procedure TPJUFilerElementRef.AddPending(Item: TPJUFilerPendingElRef);
begin
  Item.Next:=Pending;
  Pending:=Item;
end;

procedure TPJUFilerElementRef.Clear;
var
  Ref, NextRef: TPJUFilerPendingElRef;
begin
  Ref:=Pending;
  while Ref<>nil do
    begin
    NextRef:=Ref.Next;
    Ref.Next:=nil;
    Ref.Free;
    Ref:=NextRef;
    end;
end;

destructor TPJUFilerElementRef.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ TPJUFiler }

function TPJUFiler.GetSourceFiles(Index: integer): TPJUSourceFile;
begin
  Result:=TPJUSourceFile(FSourceFiles[Index]);
end;

procedure TPJUFiler.RaiseMsg(Id: int64; El: TPasElement; const Msg: string);
var
  Path, s: String;
begin
  Path:='';
  while El<>nil do
    begin
    if Path<>'' then Path:='.'+Path;
    s:=El.Name;
    if s='' then
      s:=El.ClassName;
    Path:=s+Path;
    El:=El.Parent;
    end;
  RaiseMsg(Id,Path+': '+Msg);
end;

function TPJUFiler.GetDefaultMemberVisibility(El, LastElement: TPasElement
  ): TPasMemberVisibility;
begin
  if El=nil then ;
  if LastElement<>nil then
    Result:=LastElement.Visibility
  else
    Result:=visDefault;
end;

procedure TPJUFiler.GetDefaultsPasIdentifierProps(El: TPasElement; out
  Kind: TPasIdentifierKind; out Name: string);
begin
  Kind:=PJUDefaultIdentifierKind;
  if El is TPasProcedure then
    Kind:=pikProc;
  Name:=El.Name;
end;

function TPJUFiler.GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum;
var
  p: PChar;
  Cnt: integer;
begin
  OnGetSrc(Self,aFilename,p,Cnt);
  Result:=ComputeChecksum(p,Cnt);
end;

function TPJUFiler.GetElementReference(El: TPasElement; AutoCreate: boolean
  ): TPJUFilerElementRef;
var
  Node: TAVLTreeNode;
  Data: TObject;
begin
  if El.CustomData is TResElDataBuiltInSymbol then
    begin
    // built-in symbol -> redirect to symbol of this module
    Data:=El.CustomData;
    if Data is TResElDataBaseType then
      El:=Resolver.BaseTypes[TResElDataBaseType(Data).BaseType]
    else if Data is TResElDataBuiltInProc then
      El:=TResElDataBuiltInProc(Data).Proc
    else
      RaiseMsg(20180207121004,El,Data.ClassName);
    end;
  Node:=FElementRefs.FindKey(El,@CompareElWithPJUFilerElementRef);
  if Node<>nil then
    Result:=TPJUFilerElementRef(Node.Data)
  else if AutoCreate then
    begin
    Result:=TPJUFilerElementRef.Create;
    Result.Element:=El;
    FElementRefs.Add(Result);
    end
  else
    Result:=nil;
end;

constructor TPJUFiler.Create;
begin
  FSourceFiles:=TObjectList.Create(true);
  FElementRefs:=TAVLTree.Create(@ComparePJUFilerElementRef);
  FElementRefs.SetNodeManager(TAVLTreeNodeMemManager.Create,true); // no shared manager, needed for multithreading
end;

destructor TPJUFiler.Destroy;
begin
  Clear;
  FreeAndNil(FSourceFiles);
  FreeAndNil(FElementRefs);
  inherited Destroy;
end;

procedure TPJUFiler.Clear;
begin
  FElementRefs.FreeAndClear;
  FSourceFiles.Clear;
  FResolver:=nil;
  FParser:=nil;
  FScanner:=nil;
end;

function TPJUFiler.SourceFileCount: integer;
begin
  Result:=FSourceFiles.Count;
end;

{ TPJUInitialFlags }

constructor TPJUInitialFlags.Create;
begin
  Clear;
end;

procedure TPJUInitialFlags.Clear;
begin
  ParserOptions:=PJUDefaultParserOptions;
  ModeSwitches:=PJUDefaultModeSwitches;
  BoolSwitches:=PJUDefaultBoolSwitches;
  ConverterOptions:=PJUDefaultConvertOptions;
  TargetPlatform:=PJUDefaultTargetPlatform;
  TargetProcessor:=PJUDefaultTargetProcessor;
end;

{ TPJUWriter }

procedure TPJUWriter.ResolvePendingElRefs(Ref: TPJUFilerElementRef);
var
  RefItem: TPJUFilerPendingElRef;
  RefObj: TPJUWriterPendingElRefObj;
  RefArr: TPJUWriterPendingElRefArray;
begin
  if Ref.Pending=nil then exit;
  // this element is referenced
  if Ref.Id=0 then
    CreateElReferenceId(Ref);
  // resolve all pending references
  while Ref.Pending<>nil do
    begin
    RefItem:=Ref.Pending;
    if RefItem is TPJUWriterPendingElRefObj then
      begin
      RefObj:=TPJUWriterPendingElRefObj(RefItem);
      RefObj.Obj.Add(RefObj.PropName,Ref.Id);
      end
    else if RefItem is TPJUWriterPendingElRefArray then
      begin
      RefArr:=TPJUWriterPendingElRefArray(RefItem);
      RefArr.Arr.Integers[RefArr.Index]:=Ref.Id;
      end
    else
      RaiseMsg(20180207113335,RefItem.ClassName);
    Ref.Pending:=RefItem.Next;
    RefItem.Next:=nil;
    RefItem.Free;
    end;
end;

procedure TPJUWriter.RaiseMsg(Id: int64; const Msg: string);
var
  E: EPas2JsWriteError;
begin
  E:=EPas2JsWriteError.Create('['+IntToStr(Id)+'] '+Msg);
  E.Owner:=Self;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUWriter.RaiseMsg ',E.Message);
  {$ENDIF}
  raise E;
end;

function TPJUWriter.CheckElScope(El: TPasElement; NotNilId: int64;
  ScopeClass: TPasScopeClass): TPasScope;
var
  Data: TObject;
begin
  Data:=El.CustomData;
  if Data=nil then
    begin
    if NotNilId>0 then
      RaiseMsg(NotNilId);
    exit(nil);
    end;
  if Data.ClassType<>ScopeClass then
    RaiseMsg(20180206113601,'expected '+ScopeClass.ClassName+', but found '+Data.ClassName);
  Result:=TPasScope(Data);
  if Result.Element<>El then
    RaiseMsg(20180206113723,'El='+GetObjName(El)+' Scope.Element='+GetObjName(Result.Element));
  if Result.Owner<>Resolver then
    RaiseMsg(20180206113750,El,GetObjName(Result));
end;

procedure TPJUWriter.AddArrayFlag(Obj: TJSONObject; var Arr: TJSONArray;
  const ArrName, Flag: string; Enable: boolean);
begin
  if Arr=nil then
    begin
    Arr:=TJSONArray.Create;
    Obj.Add(ArrName,Arr);
    end;
  if Enable then
    Arr.Add(Flag)
  else
    Arr.Add('-'+Flag);
end;

procedure TPJUWriter.AddReferenceToArray(Arr: TJSONArray; El: TPasElement);
var
  Ref: TPJUFilerElementRef;
  Item: TPJUWriterPendingElRefArray;
begin
  Ref:=GetElementReference(El);
  if (Ref.Obj<>nil) and (Ref.Id=0) then
    CreateElReferenceId(Ref);
  Arr.Add(Ref.Id);
  if Ref.Id<>0 then
    exit;
  // Element was not yet written -> add a pending item to the queue
  Item:=TPJUWriterPendingElRefArray.Create;
  Item.Arr:=Arr;
  Item.Index:=Arr.Count-1;
  Ref.AddPending(Item);
end;

procedure TPJUWriter.AddReferenceToObj(Obj: TJSONObject;
  const PropName: string; El: TPasElement);
var
  Ref: TPJUFilerElementRef;
  Item: TPJUWriterPendingElRefObj;
begin
  Ref:=GetElementReference(El);
  if (Ref.Obj<>nil) and (Ref.Id=0) then
    CreateElReferenceId(Ref);
  if Ref.Id<>0 then
    Obj.Add(PropName,Ref.Id)
  else
    begin
    // Element was not yet written -> add a pending item to the queue
    Item:=TPJUWriterPendingElRefObj.Create;
    Item.Obj:=Obj;
    Item.PropName:=PropName;
    Ref.AddPending(Item);
    end;
end;

procedure TPJUWriter.CreateElReferenceId(Ref: TPJUFilerElementRef);
begin
  if Ref.Id<>0 then
    RaiseMsg(20180207114300,Ref.Element,IntToStr(Ref.Id));
  inc(FElementIdCounter);
  Ref.Id:=FElementIdCounter;
  Ref.Obj.Add('Id',Ref.Id);
end;

procedure TPJUWriter.WriteHeaderMagic(Obj: TJSONObject);
begin
  Obj.Add('FileType',PJUMagic);
end;

procedure TPJUWriter.WriteHeaderVersion(Obj: TJSONObject);
begin
  Obj.Add('Version',PJUVersion);
end;

procedure TPJUWriter.WriteInitialFlags(Obj: TJSONObject);
begin
  WriteParserOptions(Obj,InitialFlags.ParserOptions,PJUDefaultParserOptions);
  WriteModeSwitches(Obj,InitialFlags.Modeswitches,PJUDefaultModeSwitches);
  WriteBoolSwitches(Obj,InitialFlags.BoolSwitches,PJUDefaultBoolSwitches);
  WriteConvertOptions(Obj,InitialFlags.ConverterOptions,PJUDefaultConvertOptions);
  if InitialFlags.TargetPlatform<>PJUDefaultTargetPlatform then
    Obj.Add('TargetPlatform',PJUTargetPlatformNames[InitialFlags.TargetPlatform]);
  if InitialFlags.TargetProcessor<>PJUDefaultTargetProcessor then
    Obj.Add('TargetProcessor',PJUTargetProcessorNames[InitialFlags.TargetProcessor]);
  // ToDo: write initial flags: used defines, used macros
end;

procedure TPJUWriter.WriteParserOptions(Obj: TJSONObject; const Value,
  DefaultValue: TPOptions);
var
  Arr: TJSONArray;
  f: TPOption;
begin
  Arr:=nil;
  for f in TPOptions do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ParserOptions',PJUParserOptionNames[f],f in Value);
end;

procedure TPJUWriter.WriteModeSwitches(Obj: TJSONObject; const Value,
  DefaultValue: TModeSwitches);
var
  Arr: TJSONArray;
  f: TModeSwitch;
begin
  Arr:=nil;
  for f in TModeSwitch do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ModeSwitches',PJUModeSwitchNames[f],f in Value);
end;

procedure TPJUWriter.WriteBoolSwitches(Obj: TJSONObject; const Value,
  DefaultValue: TBoolSwitches);
var
  Arr: TJSONArray;
  f: TBoolSwitch;
begin
  Arr:=nil;
  for f in TBoolSwitch do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'BoolSwitches',PJUBoolSwitchNames[f],f in Value);
end;

procedure TPJUWriter.WriteConvertOptions(Obj: TJSONObject; const Value,
  DefaultValue: TPasToJsConverterOptions);
var
  Arr: TJSONArray;
  f: TPasToJsConverterOption;
begin
  Arr:=nil;
  for f in TPasToJsConverterOption do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ConverterOptions',PJUConverterOptions[f],f in Value);
end;

procedure TPJUWriter.WriteSrcFiles(Obj: TJSONObject);
var
  CurFile: TPJUSourceFile;
  List: TFPList;
  i: Integer;
  SourcesArr: TJSONArray;
  Src: TJSONObject;
begin
  List:=TFPList.Create;
  try
    // get files from scanner
    for i:=0 to Scanner.Files.Count-1 do
      begin
      CurFile:=TPJUSourceFile.Create;
      CurFile.Index:=i;
      CurFile.Filename:=Scanner.Files[i];
      if i=0 then
        CurFile.FileType:=sftUnit
      else
        CurFile.FileType:=sftInclude;
      FSourceFiles.Add(CurFile);
      CurFile.Checksum:=GetSrcCheckSum(CurFile.Filename);
      List.Add(CurFile);
      end;

    // create FSourceFilesSorted
    List.Sort(@ComparePJUSrcFiles);
    SetLength(FSourceFilesSorted,List.Count);
    for i:=0 to List.Count-1 do
      FSourceFilesSorted[i]:=TPJUSourceFile(List[i]);

    // write
    SourcesArr:=TJSONArray.Create;
    Obj.Add('Sources',SourcesArr);
    for i:=0 to FSourceFiles.Count-1 do
      begin
      CurFile:=TPJUSourceFile(FSourceFiles[i]);
      Src:=TJSONObject.Create;
      SourcesArr.Add(Src);
      if (i=0) then
        // the first file is the unit source, no need to write Kind
      else if (CurFile.FileType=sftInclude) then
        // the default file type is include, no need to write Kind
      else
        Src.Add('Type',PJUSourceFileTypeNames[CurFile.FileType]);
      Src.Add('File',CurFile.Filename);
      Src.Add('CheckSum',CurFile.Checksum);
      end;
  finally
    List.Free;
  end;
end;

procedure TPJUWriter.WriteMemberHints(Obj: TJSONObject; const Value,
  DefaultValue: TPasMemberHints);
var
  Arr: TJSONArray;
  f: TPasMemberHint;
begin
  Arr:=nil;
  for f in TPasMemberHints do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'Hints',PJUMemberHintNames[f],f in Value);
end;

procedure TPJUWriter.WritePasElement(Obj: TJSONObject; El: TPasElement;
  aContext: TPJUWriterContext);
var
  i: Integer;
  LastElement: TPasElement;
  DefHints: TPasMemberHints;
  DefVisibility: TPasMemberVisibility;
  Ref: TPJUFilerElementRef;
begin
  if El.Name<>'' then
    Obj.Add('Name',El.Name);
  LastElement:=aContext.LastElement;

  // Id
  Ref:=GetElementReference(El);
  Ref.Obj:=Obj;
  ResolvePendingElRefs(Ref);

  if (LastElement=nil) or (LastElement.SourceFilename<>El.SourceFilename) then
    begin
    i:=IndexOfSourceFile(El.SourceFilename);
    if i<0 then
      RaiseMsg(20180205110259,El,El.SourceFilename);
    Obj.Add('File',i);
    end;

  if (LastElement=nil) or (LastElement.SourceLinenumber<>El.SourceLinenumber) then
    Obj.Add('Pos',El.SourceLinenumber);
  // not needed: El.SourceEndLinenumber

  DefVisibility:=GetDefaultMemberVisibility(El,LastElement);
  if El.Visibility<>DefVisibility then
    Obj.Add('Visibility',PJUMemberVisibilityNames[El.Visibility]);

  DefHints:=[];
  if LastElement<>nil then
    DefHints:=LastElement.Hints;
  WriteMemberHints(Obj,El.Hints,DefHints);

  if El.HintMessage<>'' then
    Obj.Add('HintMessage',El.HintMessage);

  // not needed El.DocComment

  // ToDo: El.CustomData
end;

procedure TPJUWriter.WriteModuleScopeFlags(Obj: TJSONObject; const Value,
  DefaultValue: TPasModuleScopeFlags);
var
  Arr: TJSONArray;
  f: TPasModuleScopeFlag;
begin
  Arr:=nil;
  for f in TPasModuleScopeFlags do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'ScopeFlags',PJUModuleScopeFlagNames[f],f in Value);
end;

procedure TPJUWriter.WriteModule(Obj: TJSONObject; aModule: TPasModule;
  aContext: TPJUWriterContext);

  procedure WSection(Section: TPasSection; const PropName: string);
  begin
    if Section=nil then exit;
    if Section.Parent<>aModule then
      RaiseMsg(20180205153912,aModule,PropName);
    WriteSection(Obj,Section,PropName,aContext);
    aContext.LastElement:=Section;
  end;

var
  ModScope: TPasModuleScope;
begin
  WritePasElement(Obj,aModule,aContext);

  if aModule.ClassType=TPasModule then
    Obj.Add('Type','Unit')
  else if aModule.ClassType=TPasProgram then
    Obj.Add('Type','Program')
  else if aModule.ClassType=TPasLibrary then
    Obj.Add('Type','Library')
  else
    RaiseMsg(20180203163923);

  // module scope
  ModScope:=TPasModuleScope(CheckElScope(aModule,20180206113855,TPasModuleScope));
  if ModScope.FirstName<>FirstDottedIdentifier(aModule.Name) then
    RaiseMsg(20180206114233,aModule);
  // write not needed: ModScope.FirstName
  WriteModuleScopeFlags(Obj,ModScope.Flags,PJUDefaultModuleScopeFlags);
  WriteBoolSwitches(Obj,ModScope.BoolSwitches,aContext.BoolSwitches);
  // ToDo: AssertClass: TPasClassType
  // ToDo: AssertDefConstructor: TPasConstructor
  // ToDo: AssertMsgConstructor: TPasConstructor
  // ToDo: RangeErrorClass: TPasClassType
  // ToDo: RangeErrorConstructor: TPasConstructor

  // write sections
  aContext.LastElement:=aModule;
  WSection(aModule.InterfaceSection,'Interface');
  WSection(aModule.ImplementationSection,'Implementation');
  if aModule.ClassType=TPasProgram then
    WSection(TPasProgram(aModule).ProgramSection,'Program')
  else if aModule.ClassType=TPasLibrary then
    WSection(TPasLibrary(aModule).LibrarySection,'Library');
  // ToDo: write precompiled aModule.InitializationSection
  // ToDo: write precompiled aModule.FinalizationSection

  WriteExternalReferences(Obj);
end;

procedure TPJUWriter.WriteIdentifierScope(Obj: TJSONObject;
  Scope: TPasIdentifierScope; aContext: TPJUWriterContext);
var
  Arr: TJSONArray;

  procedure WriteItem(Item: TPasIdentifier);
  var
    DefKind: TPasIdentifierKind;
    DefName: string;
    Sub: TJSONObject;
  begin
    GetDefaultsPasIdentifierProps(Item.Element,DefKind,DefName);
    if (Item.Kind=DefKind) and (Item.Identifier=DefName) then
    begin
      // add simply the element Id
      AddReferenceToArray(Arr,Item.Element);
    end
    else begin
      // add a json object
      Sub:=TJSONObject.Create;
      Arr.Add(Sub);
      if Item.Kind<>DefKind then
        Sub.Add('Kind',PJUIdentifierKindNames[Item.Kind]);
      if Item.Identifier<>DefName then
        Sub.Add('Name',Item.Identifier);
      AddReferenceToObj(Sub,'El',Item.Element);
    end;
  end;

var
  Locals: TFPList;
  i, p: Integer;
  Item: TPasIdentifier;
  Ordered: TPasIdentifierArray;
begin
  Arr:=nil;
  Locals:=Scope.GetLocalIdentifiers;
  try
    p:=0;
    Ordered:=nil;
    for i:=0 to Locals.Count-1 do
      begin
      if Arr=nil then
        begin
        Arr:=TJSONArray.Create;
        Obj.Add('SItems',Arr);
        end;
      Item:=TPasIdentifier(Locals[i]);
      if Item.NextSameIdentifier=nil then
        WriteItem(Item)
      else
        begin
        // write in declaration order (i.e. reverse)
        p:=0;
        while Item<>nil do
          begin
          if length(Ordered)<=p then
            SetLength(Ordered,length(Ordered)+4);
          Ordered[p]:=Item;
          inc(p);
          Item:=Item.NextSameIdentifier;
          end;
        while p>0 do
          begin
          dec(p);
          WriteItem(Ordered[p]);
          end;
        end;
      end;
  finally
    Locals.Free;
  end;
end;

procedure TPJUWriter.WriteSection(ParentJSON: TJSONObject;
  Section: TPasSection; const PropName: string; aContext: TPJUWriterContext);
var
  Obj: TJSONObject;
  Scope, UsesScope: TPasSectionScope;
  i: Integer;
  Arr: TJSONArray;
  UsesUnit: TPasUsesUnit;
begin
  if Section=nil then exit;
  Obj:=TJSONObject.Create;
  ParentJSON.Add(PropName,Obj);
  WritePasElement(Obj,Section,aContext);

  Scope:=TPasSectionScope(CheckElScope(Section,20180206121825,TPasSectionScope));
  if not Scope.Finished then
    RaiseMsg(20180206130333,Section);
  if Scope.UsesScopes.Count<>length(Section.UsesClause) then
    RaiseMsg(20180206122222,Section);
  if length(Section.UsesClause)>0 then
    begin
    Arr:=TJSONArray.Create;
    ParentJSON.Add('Uses',Arr);
    for i:=0 to Scope.UsesScopes.Count-1 do
      begin
      UsesUnit:=Section.UsesClause[i];
      UsesScope:=TPasSectionScope(Scope.UsesScopes[i]);
      if UsesScope.Element<>UsesUnit.Module then
        RaiseMsg(20180206122459,Section,'usesscope '+IntToStr(i)+' UsesScope.Element='+GetObjName(UsesScope.Element)+' Module='+GetObjName(Section.UsesClause[i].Module));
      // ToDo
      RaiseMsg(20180206124005,'ToDo');
      end;
    end;
  WriteIdentifierScope(Obj,Scope,aContext);

  WriteDeclarations(Obj,Section,aContext);
end;

procedure TPJUWriter.WriteDeclarations(ParentJSON: TJSONObject;
  Decls: TPasDeclarations; aContext: TPJUWriterContext);
var
  i: Integer;
  Decl: TPasElement;
  Arr: TJSONArray;
  DeclObj: TJSONObject;
begin
  Arr:=nil;
  for i:=0 to Decls.Declarations.Count-1 do
    begin
    Decl:=TPasElement(Decls.Declarations[i]);
    if Arr=nil then
      begin
      Arr:=TJSONArray.Create;
      ParentJSON.Add('Declarations',Arr);
      end;
    DeclObj:=TJSONObject.Create;
    Arr.Add(DeclObj);
    WriteDeclaration(DeclObj,Decl,aContext);
    end;
end;

procedure TPJUWriter.WriteDeclaration(Obj: TJSONObject;
  Decl: TPasElement; aContext: TPJUWriterContext);
var
  C: TClass;
begin
  C:=Decl.ClassType;
  if C=TPasConst then
  begin
    Obj.Add('Type','Const');
    WriteConst(Obj,TPasConst(Decl),aContext);
  end else if C=TPasVariable then
  begin
    Obj.Add('Type','Var');
    WriteVariable(Obj,TPasVariable(Decl),aContext)
  end else
    RaiseMsg(20180205154041,Decl);
end;

procedure TPJUWriter.WriteElType(Obj: TJSONObject; const PropName: string;
  El: TPasElement; aType: TPasType; aContext: TPJUWriterContext);
begin
  if aType=nil then exit;
  if (aType.Name='') or (aType.Parent=El) then
  begin
    // anonymous type

  end
  else begin
    // reference

  end;
  RaiseMsg(20180206183542,El);
end;

procedure TPJUWriter.WriteVarModifiers(Obj: TJSONObject; const Value,
  DefaultValue: TVariableModifiers);
var
  Arr: TJSONArray;
  f: TVariableModifier;
begin
  Arr:=nil;
  for f in TVariableModifier do
    if (f in Value)<>(f in DefaultValue) then
      AddArrayFlag(Obj,Arr,'VarMod',PJUVarModifierNames[f],f in Value);
end;

procedure TPJUWriter.WriteVariable(Obj: TJSONObject; Decl: TPasVariable;
  aContext: TPJUWriterContext);
begin
  WriteElType(Obj,'VarType',Decl,Decl.VarType,aContext);
  WriteVarModifiers(Obj,Decl.VarModifiers,[]);
  WriteExpr(Obj,'Library',Decl.LibraryName,aContext);
  WriteExpr(Obj,'Export',Decl.ExportName,aContext);
  WriteExpr(Obj,'Absolute',Decl.AbsoluteExpr,aContext);
  WriteExpr(Obj,'Expr',Decl.Expr,aContext);

  WritePasElement(Obj,Decl,aContext);
end;

procedure TPJUWriter.WriteConst(Obj: TJSONObject; Decl: TPasConst;
  aContext: TPJUWriterContext);
begin
  if Decl.IsConst<>(Decl.VarType=nil) then
    Obj.Add('IsConst',Decl.IsConst);
  WriteVariable(Obj,Decl,aContext);
end;

procedure TPJUWriter.WriteExpr(Obj: TJSONObject; const PropName: string;
  Expr: TPasExpr; aContext: TPJUWriterContext);
var
  C: TClass;
begin
  if Expr=nil then exit;
  C:=Expr.ClassType;
  if C=TPrimitiveExpr then
    WritePrimitiveExpr(Obj,PropName,TPrimitiveExpr(Expr),aContext)
  else
    RaiseMsg(20180206185146,Expr);
end;

procedure TPJUWriter.WritePasExpr(Obj: TJSONObject; Expr: TPasExpr;
  WriteKind: boolean; DefaultOpCode: TExprOpCode; aContext: TPJUWriterContext);
begin
  if WriteKind then
    Obj.Add('Kind',PJUExprKindNames[Expr.Kind]);
  if (Expr.OpCode<>DefaultOpCode) then
    Obj.Add('Op',PJUExprOpCodeNames[Expr.OpCode]);
  WriteExpr(Obj,'Format1',Expr.format1,aContext);
  WriteExpr(Obj,'Format2',Expr.format2,aContext);
  WritePasElement(Obj,Expr,aContext);
end;

procedure TPJUWriter.WritePrimitiveExpr(Obj: TJSONObject;
  const PropName: string; Expr: TPrimitiveExpr; aContext: TPJUWriterContext);
var
  SubObj: TJSONObject;
begin
  SubObj:=TJSONObject.Create;
  Obj.Add(PropName,SubObj);
  SubObj.Add('Type',PJUExprKindNames[Expr.Kind]);
  if Expr.Value<>'' then
    SubObj.Add('Value',Expr.Value);
  WritePasExpr(SubObj,Expr,false,eopNone,aContext);
end;

procedure TPJUWriter.WriteExternalReferences(ParentJSON: TJSONObject);
var
  Node: TAVLTreeNode;
  Ref: TPJUFilerElementRef;
  El: TPasElement;
  Data: TObject;
  SystemArr, ExtArr: TJSONArray;
  Obj: TJSONObject;
begin
  ExtArr:=nil;
  SystemArr:=nil;
  Node:=FElementRefs.FindLowest;
  while Node<>nil do
    begin
    Ref:=TPJUFilerElementRef(Node.Data);
    Node:=FElementRefs.FindSuccessor(Node);
    if Ref.Pending=nil then continue;
    El:=Ref.Element;
    Data:=El.CustomData;
    if Data is TResElDataBuiltInSymbol then
      begin
      // add built-in symbol to System array
      if El.GetModule<>Resolver.RootElement then
        RaiseMsg(20180207124914,El);
      if SystemArr=nil then
        begin
        SystemArr:=TJSONArray.Create;
        ParentJSON.Add('System');
        end;
      Obj:=TJSONObject.Create;
      SystemArr.Add(Obj);
      Obj.Add('Name',El.Name);
      if Data is TResElDataBuiltInProc then
        begin
        case TResElDataBuiltInProc(Data).BuiltIn of
        bfStrFunc: Obj.Add('Type','Func');
        end;
        end;
      Ref.Obj:=Obj;
      ResolvePendingElRefs(Ref);
      continue;
      end;
    if Ref.Element.GetModule=Resolver.RootElement then
      RaiseMsg(20180207115645,Ref.Element); // an element of this module was not written
    // external element
    if ExtArr=nil then
      begin
      ExtArr:=TJSONArray.Create;
      ParentJSON.Add('External');
      end;
    Obj:=TJSONObject.Create;
    ExtArr.Add(Obj);
    Obj.Add('Name',El.Name);

    // ToDo
    RaiseMsg(20180207115730,Ref.Element);
    Ref.Obj:=Obj;
    ResolvePendingElRefs(Ref);
    end;
end;

constructor TPJUWriter.Create;
begin
  inherited Create;
end;

destructor TPJUWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TPJUWriter.Clear;
begin
  FInitialFlags:=nil;
  FElementIdCounter:=0;
  FSourceFilesSorted:=nil;
  inherited Clear;
end;

procedure TPJUWriter.WritePJU(aResolver: TPas2JSResolver;
  InitFlags: TPJUInitialFlags; aStream: TStream);
var
  aJSON: TJSONObject;
begin
  aJSON:=WriteJSON(aResolver,InitFlags);
  try
    aJSON.DumpJSON(aStream);
  finally
    aJSON.Free;
  end;
end;

function TPJUWriter.WriteJSON(aResolver: TPas2JSResolver;
  InitFlags: TPJUInitialFlags): TJSONObject;
var
  Obj, JSMod: TJSONObject;
  aContext: TPJUWriterContext;
begin
  Result:=nil;
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;
  FInitialFlags:=InitFlags;

  aContext:=nil;
  Obj:=TJSONObject.Create;
  try
    WriteHeaderMagic(Obj);
    WriteHeaderVersion(Obj);
    WriteInitialFlags(Obj);
    WriteSrcFiles(Obj);
    // ToDo: WriteUsedModulesPrecompiledChecksums
    aContext:=TPJUWriterContext.Create;
    aContext.ModeSwitches:=InitialFlags.ModeSwitches;
    aContext.BoolSwitches:=InitialFlags.BoolSwitches;
    JSMod:=TJSONObject.Create;
    Obj.Add('Module',JSMod);
    WriteModule(JSMod,aResolver.RootElement,aContext);
    // ToDo: write final flags: modeswitches, boolswitches, used defines

    Result:=Obj;
  finally
    aContext.Free;
    if Result=nil then
      Obj.Free;
  end;
end;

function TPJUWriter.IndexOfSourceFile(const Filename: string): integer;
var
  l, r, m, cmp: Integer;
begin
  l:=0;
  r:=length(FSourceFilesSorted)-1;
  while l<=r do
    begin
    m:=(l+r) div 2;
    cmp:=CompareStr(Filename,FSourceFilesSorted[m].Filename);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else
      exit(FSourceFilesSorted[m].Index);
    end;
  Result:=-1;
end;

{ TPJUReader }

procedure TPJUReader.Set_Variable_VarType(Ref: TPJUFilerElementRef;
  Data: Pointer);
var
  V: TPasVariable absolute Data;
begin
  V.VarType:=Ref.Element as TPasType;
end;

procedure TPJUReader.RaiseMsg(Id: int64; const Msg: string);
var
  E: EPas2JsReadError;
begin
  E:=EPas2JsReadError.Create('['+IntToStr(Id)+'] '+Msg);
  E.Owner:=Self;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.RaiseMsg ',E.Message);
  {$ENDIF}
  raise E;
end;

function TPJUReader.CheckJSONArray(Data: TJSONData; El: TPasElement;
  const PropName: string): TJSONArray;
begin
  if Data is TJSONArray then exit(TJSONArray(Data));
  if Data=nil then
    RaiseMsg(20180205140943,El,PropName+': nil')
  else
    RaiseMsg(20180205140358,El,PropName+': '+Data.ClassName);
  Result:=nil;
end;

function TPJUReader.CheckJSONObject(Data: TJSONData; Id: int64): TJSONObject;
begin
  if Data is TJSONObject then exit(TJSONObject(Data));
  RaiseMsg(Id);
  Result:=nil;
end;

function TPJUReader.CheckJSONString(Data: TJSONData; Id: int64): String;
begin
  if Data is TJSONString then
    exit(String(Data.AsString));
  RaiseMsg(Id);
  Result:='';
end;

function TPJUReader.ReadString(Obj: TJSONObject; const PropName: string; out
  s: string; El: TPasElement): boolean;
var
  C: TClass;
  Data: TJSONData;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  C:=Data.ClassType;
  if C=TJSONString then
    begin
    s:=String(Data.AsString);
    exit(true);
    end;
  RaiseMsg(20180205133227,El,PropName+':'+Data.ClassName);
  Result:=false;
end;

function TPJUReader.ReadInteger(Obj: TJSONObject; const PropName: string; out
  i: integer; El: TPasElement): boolean;
var
  C: TClass;
  Data: TJSONData;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  C:=Data.ClassType;
  if C=TJSONIntegerNumber then
    begin
    i:=Data.AsInteger;
    exit(true);
    end;
  RaiseMsg(20180205133132,El,PropName+':'+Data.ClassName);
  Result:=false;
end;

function TPJUReader.ReadBoolean(Obj: TJSONObject; const PropName: string; out
  b: boolean; El: TPasElement): boolean;
var
  C: TClass;
  Data: TJSONData;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  C:=Data.ClassType;
  if C=TJSONBoolean then
    begin
    b:=Data.AsBoolean;
    exit(true);
    end;
  RaiseMsg(20180207183730,El,PropName+':'+Data.ClassName);
  Result:=false;
end;

function TPJUReader.ReadArray(Obj: TJSONObject; const PropName: string; out
  Arr: TJSONArray; El: TPasElement): boolean;
var
  Data: TJSONData;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit(false);
  if not (Data is TJSONArray) then
    RaiseMsg(20180207144507,El,PropName+':'+Data.ClassName);
  Arr:=TJSONArray(Data);
  Result:=true;
end;

function TPJUReader.AddElReference(Id: integer; ErrorEl: TPasElement;
  El: TPasElement): TPJUFilerElementRef;
var
  NewCapacity, OldCapacity: Integer;
  Ref: TPJUFilerElementRef;
  RefItem: TPJUFilerPendingElRef;
  PendingRef: TPJUReaderPendingElRef;
begin
  if Id<=0 then
    RaiseMsg(20180207151233,ErrorEl);
  OldCapacity:=length(FElementRefsArray);
  if Id>=OldCapacity then
    begin
    // grow
    NewCapacity:=OldCapacity;
    if NewCapacity=0 then NewCapacity:=16;
    while NewCapacity<Id+1 do NewCapacity:=NewCapacity*2;
    SetLength(FElementRefsArray,NewCapacity);
    FillByte(FElementRefsArray[OldCapacity],SizeOf(Pointer)*(NewCapacity-OldCapacity),0);
    end;

  Ref:=FElementRefsArray[Id];
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.AddElReference Id=',Id,' El=',GetObjName(El),' ErrorEl=',GetObjName(ErrorEl),' OldRef=',GetObjName(Ref));
  {$ENDIF}
  if Ref=nil then
    begin
    // new target element
    if El<>nil then
      begin
      Ref:=GetElementReference(El,true);
      if Ref.Id=0 then
        Ref.Id:=Id
      else if Ref.Id<>Id then
        RaiseMsg(20180207152251,ErrorEl,IntToStr(Ref.Id)+'<>'+IntToStr(Id));
      end
    else
      begin
      Ref:=TPJUFilerElementRef.Create;
      Ref.Id:=Id;
      end;
    FElementRefsArray[Id]:=Ref;
    end;
  Result:=Ref;

  if El=nil then exit;

  if Ref.Element=nil then
    begin
    Ref.Element:=El;
    if Ref.Pending<>nil then
      begin
      // resolve pending references
      while Ref.Pending<>nil do
        begin
        RefItem:=Ref.Pending;
        if RefItem is TPJUReaderPendingElRef then
          begin
          PendingRef:=TPJUReaderPendingElRef(RefItem);
          PendingRef.Setter(Ref,PendingRef.Data);
          end
        else
          RaiseMsg(20180207153056,ErrorEl,RefItem.ClassName);
        Ref.Pending:=RefItem.Next;
        RefItem.Next:=nil;
        RefItem.Free;
        end;
      end;
    end
  else if El<>Ref.Element then
    RaiseMsg(20180207194919,ErrorEl,'Duplicate Id='+IntToStr(Id)+' El='+GetObjName(El)+' Ref.Element='+GetObjName(Ref.Element));
end;

procedure TPJUReader.PromiseSetElReference(Id: integer;
  const Setter: TOnSetElReference; Data: Pointer; ErrorEl: TPasElement);
var
  Ref: TPJUFilerElementRef;
  PendingItem: TPJUReaderPendingElRef;
begin
  Ref:=AddElReference(Id,ErrorEl,nil);
  if Ref.Element<>nil then
    begin
    // element was already created -> execute Setter immediately
    Setter(Ref,Data);
    end
  else
    begin
    // element was not yet created -> store Setter
    PendingItem:=TPJUReaderPendingElRef.Create;
    PendingItem.Setter:=Setter;
    PendingItem.Data:=Data;
    Ref.AddPending(PendingItem);
    end;
end;

procedure TPJUReader.ReadHeaderMagic(Obj: TJSONObject);
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadHeaderMagic ',Obj.Get('FileType',''));
  {$ENDIF}
  if Obj.Get('FileType','')<>PJUMagic then
    RaiseMsg(20180130201710,'not a pju file');
end;

procedure TPJUReader.ReadHeaderVersion(Obj: TJSONObject);
begin
  FFileVersion:=Obj.Get('Version',0);
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadHeaderVersion ',FFileVersion);
  {$ENDIF}
  if FFileVersion<1 then
    RaiseMsg(20180130201801,'invalid pju file version');
  if FFileVersion>PJUVersion then
    RaiseMsg(20180130201822,'pju file was created by a newer compiler.');
end;

procedure TPJUReader.ReadArrayFlags(Data: TJSONData; El: TPasElement;
  const PropName: string; out Names: TStringDynArray; out
  Enable: TBooleanDynArray);
const
  IdentStart = ['a'..'z','A'..'Z','_'];
var
  Arr: TJSONArray;
  Cnt, i: Integer;
  s: String;
begin
  Names:=nil;
  Enable:=nil;
  if Data=nil then exit;
  Arr:=CheckJSONArray(Data,El,PropName);
  Cnt:=Arr.Count;
  if Cnt=0 then exit;
  SetLength(Names,Cnt);
  SetLength(Enable,Cnt);
  for i:=0 to Cnt-1 do
    begin
    Data:=Arr[i];
    if not (Data is TJSONString) then
      RaiseMsg(20180202132350,El,PropName+' elements must be string');
    s:=String(TJSONString(Data).AsString);
    if s='' then
      RaiseMsg(20180202133605,El,PropName+' elements must be string');
    if s[1]='-' then
      begin
      Enable[i]:=false;
      system.Delete(s,1,1);
      end
    else
      Enable[i]:=true;
    if not (s[1] in IdentStart) then
      RaiseMsg(20180202133605,El,PropName+' elements must be identifiers');
    Names[i]:=s;
    end;
end;

function TPJUReader.ReadParserOptions(Data: TJSONData; El: TPasElement;
  const DefaultValue: TPOptions): TPOptions;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPOption;
  Found: Boolean;
  i: Integer;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadParserOptions START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'ParserOptions',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPOption do
      if s=PJUParserOptionNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144009,'unknown ParserOption "'+s+'"');
    end;
end;

function TPJUReader.ReadModeSwitches(Data: TJSONData; El: TPasElement;
  const DefaultValue: TModeSwitches): TModeSwitches;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TModeSwitch;
  Found: Boolean;
  i: Integer;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModeSwitches START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'ModeSwitches',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TModeSwitch do
      if s=PJUModeSwitchNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144054,'unknown ModeSwitch "'+s+'"');
    end;
end;

function TPJUReader.ReadBoolSwitches(Data: TJSONData; El: TPasElement;
  const DefaultValue: TBoolSwitches): TBoolSwitches;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TBoolSwitch;
  i: Integer;
  Found: Boolean;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadBoolSwitches START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'BoolSwitches',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TBoolSwitch do
      if s=PJUBoolSwitchNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144116,'unknown BoolSwitch "'+s+'"');
    end;
end;

function TPJUReader.ReadConverterOptions(Data: TJSONData; El: TPasElement;
  const DefaultValue: TPasToJsConverterOptions): TPasToJsConverterOptions;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasToJsConverterOption;
  i: Integer;
  Found: Boolean;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadConverterOptions START');
  {$ENDIF}
  ReadArrayFlags(Data,El,'ConverterOptions',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasToJsConverterOption do
      if s=PJUConverterOptions[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180202144136,'unknown ConvertOptions "'+s+'"');
    end;
end;

procedure TPJUReader.ReadTargetPlatform(Data: TJSONData);
var
  p: TPasToJsPlatform;
  s: String;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadTargetPlatform START');
  {$ENDIF}
  s:=CheckJSONString(Data,20180203100215);
  for p in TPasToJsPlatform do
    if s=PJUTargetPlatformNames[p] then
      begin
      InitialFlags.TargetPlatform:=p;
      exit;
      end;
  RaiseMsg(20180202145542,'invalid TargetPlatform');
end;

procedure TPJUReader.ReadTargetProcessor(Data: TJSONData);
var
  p: TPasToJsProcessor;
  s: String;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadTargetProcessor START');
  {$ENDIF}
  s:=CheckJSONString(Data,20180203100235);
  for p in TPasToJsProcessor do
    if s=PJUTargetProcessorNames[p] then
      begin
      InitialFlags.TargetProcessor:=p;
      exit;
      end;
  RaiseMsg(20180202145623,'invalid TargetProcessor');
end;

procedure TPJUReader.ReadSrcFiles(Data: TJSONData);
var
  SourcesArr: TJSONArray;
  i, j: Integer;
  Src: TJSONObject;
  CurFile: TPJUSourceFile;
  Found: Boolean;
  ft: TPJUSourceFileType;
  s: TJSONStringType;
  CurFilename, PropName: string;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadSrcFiles START ');
  {$ENDIF}
  SourcesArr:=CheckJSONArray(Data,nil,'Sources');
  for i:=0 to SourcesArr.Count-1 do
    begin
    Src:=CheckJSONObject(SourcesArr[i],20180203100307);
    CurFile:=TPJUSourceFile.Create;
    FSourceFiles.Add(CurFile);
    if i=0 then
      CurFile.FileType:=sftUnit
    else
      CurFile.FileType:=sftInclude;

    for j:=0 to Src.Count-1 do
      begin
      PropName:=Src.Names[j];
      Data:=Src.Elements[PropName];
      case PropName of
      'Type':
        begin
        s:=CheckJSONString(Data,20180203101322);
        Found:=false;
        for ft in TPJUSourceFileType do
          if s=PJUSourceFileTypeNames[ft] then
            begin
            Found:=true;
            CurFile.FileType:=ft;
            break;
            end;
        if not Found then
          RaiseMsg(20180202144347,'unknown filetype "'+s+'"');
        end;
      'File':
        begin
        CurFilename:=CheckJSONString(Data,20180203100410);
        if CurFilename='' then
          RaiseMsg(20180130203605);
        if length(CurFilename)>MAX_PATH then
          RaiseMsg(20180130203624);
        DoDirSeparators(CurFilename);
        if CurFilename<>ResolveDots(CurFilename) then
          RaiseMsg(20180130203841);
        if ExtractFilenameOnly(CurFilename)='' then
          RaiseMsg(20180130203924);
        CurFile.Filename:=CurFilename;
        end;
      'CheckSum':
        CurFile.Checksum:=Data.AsInt64;
      else
        RaiseMsg(20180202152628,'unknown file property "'+PropName+'"');
      end;
      end;
    end;
end;

function TPJUReader.ReadMemberHints(Obj: TJSONObject; El: TPasElement;
  const DefaultValue: TPasMemberHints): TPasMemberHints;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasMemberHint;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadMemberHints START');
  {$ENDIF}
  Data:=Obj.Find('Hints');
  if Data=nil then exit;
  ReadArrayFlags(Data,El,'Hints',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasMemberHint do
      if s=PJUMemberHintNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180205134551,'unknown element Hints "'+s+'"');
    end;
end;

procedure TPJUReader.ReadPasElement(Obj: TJSONObject; El: TPasElement;
  aContext: TPJUReaderContext);

  function StrToMemberVisibility(const s: string): TPasMemberVisibility;
  var
    vis: TPasMemberVisibility;
  begin
    for vis in TPasMemberVisibility do
      if PJUMemberVisibilityNames[vis]=s then
        exit(vis);
    RaiseMsg(20180205134334,El,s);
  end;

var
  i, Id: integer;
  s: string;
  LastElement: TPasElement;
  DefHints: TPasMemberHints;
begin
  LastElement:=aContext.LastElement;

  if ReadInteger(Obj,'Id',Id,El) then
    AddElReference(Id,El,El);

  if ReadInteger(Obj,'File',i,El) then
    El.SourceFilename:=SourceFiles[i].Filename
  else
    El.SourceFilename:=LastElement.SourceFilename;

  if ReadInteger(Obj,'Pos',i,El) then
    El.SourceLinenumber:=i
  else
    El.SourceLinenumber:=LastElement.SourceLinenumber;

  if ReadString(Obj,'Visibility',s,El) then
    El.Visibility:=StrToMemberVisibility(s)
  else
    El.Visibility:=GetDefaultMemberVisibility(El,LastElement);

  DefHints:=[];
  if LastElement<>nil then
    DefHints:=LastElement.Hints;
  El.Hints:=ReadMemberHints(Obj,El,DefHints);

  if ReadString(Obj,'HintMessage',s,El) then
    El.HintMessage:=s;
end;

procedure TPJUReader.ReadSection(Obj: TJSONObject; Section: TPasSection;
  aContext: TPJUReaderContext);
var
  Scope: TPasSectionScope;
  UsesArr, Arr: TJSONArray;
  Data: TJSONData;
  i: Integer;
  Pending: TPJUReaderPendingIdentifierScope;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadSection ',GetObjName(Section));
  {$ENDIF}
  ReadPasElement(Obj,Section,aContext);

  Scope:=TPasSectionScope(Resolver.CreateScope(Section,TPasSectionScope));
  Scope.Finished:=true;
  Data:=Obj.Find('Uses');
  if Data<>nil then
    begin
    UsesArr:=CheckJSONArray(Data,Section,'Uses');
    // ToDo UsesClause
    RaiseMsg(20180206124604,'ToDo');
    for i:=0 to UsesArr.Count-1 do ;
    end;

  if ReadArray(Obj,'SItems',Arr,Scope.Element) then
    begin
    Pending:=TPJUReaderPendingIdentifierScope.Create;
    Pending.Scope:=Scope;
    Pending.Arr:=Arr;
    FPendingIdentifierScopes.Add(Pending);
    end;

  ReadDeclarations(Obj,Section,aContext);
end;

procedure TPJUReader.ReadDeclarations(Obj: TJSONObject; Section: TPasSection;
  aContext: TPJUReaderContext);
var
  Arr: TJSONArray;
  i: Integer;
  Data: TJSONData;
begin
  if not ReadArray(Obj,'Declarations',Arr,Section) then exit;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadDeclarations ',GetObjName(Section),' ',Arr.Count);
  {$ENDIF}
  for i:=0 to Arr.Count-1 do
    begin
    Data:=Arr[i];
    if not (Data is TJSONObject) then
      RaiseMsg(20180207182304,Section,IntToStr(i)+' '+GetObjName(Data));
    ReadDeclaration(TJSONObject(Data),Section,aContext);
    end;
end;

procedure TPJUReader.ReadDeclaration(Obj: TJSONObject; Section: TPasSection;
  aContext: TPJUReaderContext);
var
  aType, Name: string;
  El: TPasConst;
begin
  if not ReadString(Obj,'Type',aType,Section) then
    RaiseMsg(20180207183050,Section);
  if not ReadString(Obj,'Name',Name,Section) then
    RaiseMsg(20180207183415,Section);
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadDeclaration ',GetObjName(Section),' Type="',aType,'" Name="',Name,'"');
  {$ENDIF}
  case aType of
  'Const':
    begin
    El:=TPasConst.Create(Name,Section);
    Section.Declarations.Add(El);
    ReadConst(Obj,TPasConst(El),aContext);
    end
  else
    RaiseMsg(20180207183141,Section,'unknown type "'+LeftStr(aType,100)+'"');
  end;
end;

procedure TPJUReader.ReadElType(Obj: TJSONObject; const PropName: string;
  El: TPasElement; const Setter: TOnSetElReference; aContext: TPJUReaderContext
  );
var
  Data: TJSONData;
  Id: Integer;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  if Data is TJSONIntegerNumber then
    begin
    // reference
    Id:=Data.AsInteger;
    PromiseSetElReference(Id,Setter,El,El);
    end
  else
    begin
    // anonymous type
    RaiseMsg(20180207185313,El,PropName+':'+GetObjName(Data));
    end;
end;

procedure TPJUReader.ReadExpr(Obj: TJSONObject; const PropName: string;
  Parent: TPasElement; var Expr: TPasExpr; aContext: TPJUReaderContext);
var
  Data: TJSONData;
  Prim: TPrimitiveExpr;
  aType: string;
  SubObj: TJSONObject;
begin
  Data:=Obj.Find(PropName);
  if Data=nil then exit;
  if Data is TJSONObject then
    begin
    SubObj:=TJSONObject(Data);
    if not ReadString(SubObj,'Type',aType,Parent) then
      RaiseMsg(20180208072727,Parent,PropName);
    case aType of
    'Ident': ReadPrimitiveExpr(SubObj,Parent,pekIdent,Expr,aContext);
    'Number': ReadPrimitiveExpr(SubObj,Parent,pekNumber,Expr,aContext);
    'String': ReadPrimitiveExpr(SubObj,Parent,pekString,Expr,aContext);
    'Nil': ReadPrimitiveExpr(SubObj,Parent,pekNil,Expr,aContext);
    'Bool': ReadPrimitiveExpr(SubObj,Parent,pekBoolConst,Expr,aContext);
    else
      RaiseMsg(20180208073421,Parent,aType);
    end;
    end
  else if Data is TJSONBoolean then
    begin
    Prim:=TPrimitiveExpr.Create('',Parent);
    Expr:=Prim;
    Prim.Kind:=pekBoolConst;
    Prim.Value:=BoolToStr(Data.AsBoolean,'True','False');
    end
  else if Data is TJSONNumber then
    begin
    if Data is TJSONIntegerNumber then
      begin
      Prim:=TPrimitiveExpr.Create('',Parent);
      Expr:=Prim;
      Prim.Kind:=pekNumber;
      Prim.Value:=IntToStr(Data.AsInteger);
      end
    else if Data is TJSONInt64Number then
      begin
      Prim:=TPrimitiveExpr.Create('',Parent);
      Expr:=Prim;
      Prim.Kind:=pekNumber;
      Prim.Value:=IntToStr(Data.AsInt64);
      end
    else if Data is TJSONQWordNumber then
      begin
      Prim:=TPrimitiveExpr.Create('',Parent);
      Expr:=Prim;
      Prim.Kind:=pekNumber;
      Prim.Value:=IntToStr(Data.AsQWord);
      end
    else
      RaiseMsg(20180207190752,Parent,PropName+':'+GetObjName(Data));
    end
  else
    RaiseMsg(20180207190200,Parent,PropName+':'+GetObjName(Data));
end;

procedure TPJUReader.ReadPasExpr(Obj: TJSONObject; Expr: TPasExpr;
  ReadKind: boolean; aContext: TPJUReaderContext);
var
  Kind: TPasExprKind;
  s: string;
  Op: TExprOpCode;
  Found: Boolean;
begin
  if ReadKind and ReadString(Obj,'Kind',s,Expr) then
    begin
    Found:=false;
    for Kind in TPasExprKind do
      if s=PJUExprKindNames[Kind] then
        begin
        Expr.Kind:=Kind;
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180208074859,Expr,s);
    end;
  if ReadString(Obj,'Op',s,Expr) then
    begin
    Found:=false;
    for Op in TExprOpCode do
      if s=PJUExprOpCodeNames[Op] then
        begin
        Expr.OpCode:=Op;
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180208074950,Expr,s);
    end;
  ReadExpr(Obj,'format1',Expr,Expr.format1,aContext);
  ReadExpr(Obj,'format2',Expr,Expr.format2,aContext);
  ReadPasElement(Obj,Expr,aContext);
end;

procedure TPJUReader.ReadPrimitiveExpr(Obj: TJSONObject; Parent: TPasElement;
  Kind: TPasExprKind; var Expr: TPasExpr; aContext: TPJUReaderContext);
var
  Prim: TPrimitiveExpr;
  Value: string;
begin
  ReadString(Obj,'Value',Value,Parent);
  Prim:=TPrimitiveExpr.Create(Parent,Kind,Value);
  Expr:=Prim;
  Prim.Name:='';
  ReadPasExpr(Obj,Expr,false,aContext);
end;

function TPJUReader.ReadVarModifiers(Obj: TJSONObject; El: TPasElement;
  const DefaultValue: TVariableModifiers): TVariableModifiers;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TVariableModifier;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadVarModifiers START');
  {$ENDIF}
  Data:=Obj.Find('Hints');
  if Data=nil then exit;
  ReadArrayFlags(Data,El,'VarMod',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TVariableModifier do
      if s=PJUVarModifierNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180207184723,'unknown var modifier "'+s+'"');
    end;
end;

procedure TPJUReader.ReadVariable(Obj: TJSONObject; Decl: TPasVariable;
  aContext: TPJUReaderContext);
begin
  ReadPasElement(Obj,Decl,aContext);

  ReadElType(Obj,'VarType',Decl,@Set_Variable_VarType,aContext);
  Decl.VarModifiers:=ReadVarModifiers(Obj,Decl,[]);
  ReadExpr(Obj,'Library',Decl,Decl.LibraryName,aContext);
  ReadExpr(Obj,'Export',Decl,Decl.ExportName,aContext);
  ReadExpr(Obj,'Absolute',Decl,Decl.AbsoluteExpr,aContext);
  ReadExpr(Obj,'Expr',Decl,Decl.Expr,aContext);
end;

procedure TPJUReader.ReadConst(Obj: TJSONObject; Decl: TPasConst;
  aContext: TPJUReaderContext);
begin
  ReadVariable(Obj,Decl,aContext);
  if not ReadBoolean(Obj,'IsConst',Decl.IsConst,Decl) then
    Decl.IsConst:=Obj.Find('VarType')=nil;
end;

procedure TPJUReader.ReadIdentifierScope(Arr: TJSONArray;
  Scope: TPasIdentifierScope);
// called after reading module, i.e. all elements are created

  function GetElRef(Id: integer; out DefKind: TPasIdentifierKind;
    out DefName: string): TPJUFilerElementRef;
  begin
    Result:=AddElReference(Id,Scope.Element,nil);
    if Result.Element=nil then
      RaiseMsg(20180207161358,Scope.Element,'Id not found: '+IntToStr(Id));
    GetDefaultsPasIdentifierProps(Result.Element,DefKind,DefName);
  end;

var
  i, Id: Integer;
  Data: TJSONData;
  ItemObj: TJSONObject;
  s, Name, DefName: string;
  Kind, DefKind: TPasIdentifierKind;
  Ref: TPJUFilerElementRef;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadIdentifierScope ',Arr.Count);
  {$ENDIF}
  for i:=0 to Arr.Count-1 do
    begin
    Data:=Arr[i];
    if Data is TJSONIntegerNumber then
      begin
      Id:=Data.AsInteger;
      Ref:=GetElRef(Id,DefKind,DefName);
      {$IFDEF VerbosePJUFiler}
      writeln('TPJUReader.ReadIdentifierScope Id=',Id,' ',DefName,' ',DefKind,' ',GetObjName(Ref.Element));
      {$ENDIF}
      Scope.AddIdentifier(DefName,Ref.Element,DefKind);
      end
    else if Data is TJSONObject then
      begin
      ItemObj:=TJSONObject(Data);
      if not ReadInteger(ItemObj,'El',Id,Scope.Element) then
        RaiseMsg(20180207162015,Scope.Element,'missing El:integer');
      Ref:=GetElRef(Id,DefKind,DefName);
      if ReadString(ItemObj,'Kind',s,Scope.Element) then
        Kind:=StrToPasIdentifierKind(s)
      else
        Kind:=DefKind;
      if not ReadString(ItemObj,'Name',Name,Scope.Element) then
        Name:=DefName;
      if Name='' then
        RaiseMsg(20180207162358,Scope.Element,IntToStr(Id));
      Scope.AddIdentifier(Name,Ref.Element,Kind);
      end
    else
      RaiseMsg(20180207154839,Scope.Element,GetObjName(Data));
    end;
end;

function TPJUReader.ReadModuleScopeFlags(Obj: TJSONObject; El: TPasElement;
  const DefaultValue: TPasModuleScopeFlags): TPasModuleScopeFlags;
var
  Names: TStringDynArray;
  Enable: TBooleanDynArray;
  s: String;
  f: TPasModuleScopeFlag;
  i: Integer;
  Found: Boolean;
  Data: TJSONData;
begin
  Result:=DefaultValue;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModuleScopeFlags START');
  {$ENDIF}
  Data:=Obj.Find('ScopeFlags');
  if Data=nil then exit;
  ReadArrayFlags(Data,El,'ScopeFlags',Names,Enable);
  for i:=0 to length(Names)-1 do
    begin
    s:=Names[i];
    Found:=false;
    for f in TPasModuleScopeFlag do
      if s=PJUModuleScopeFlagNames[f] then
        begin
        if Enable[i] then
          Include(Result,f)
        else
          Exclude(Result,f);
        Found:=true;
        break;
        end;
    if not Found then
      RaiseMsg(20180206114404,'unknown ModuleScopeFlag "'+s+'"');
    end;
end;

procedure TPJUReader.ReadModule(Data: TJSONData; aContext: TPJUReaderContext);

  function PreReadSection(ParentJSON: TJSONObject; const PropName: string): TJSONObject;
  var
    PropData: TJSONData;
  begin
    PropData:=ParentJSON.Find(PropName);
    if PropData=nil then exit(nil);
    Result:=CheckJSONObject(PropData,20180205121719);
  end;

var
  Obj, SubObj: TJSONObject;
  aType, aName: String;
  aModule: TPasModule;
  ModScope: TPasModuleScope;
  OldBoolSwitches: TBoolSwitches;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModule START ');
  {$ENDIF}
  Obj:=CheckJSONObject(Data,20180203100422);
  aName:=String(Obj.Get('Name',''));
  aType:=String(Obj.Get('Type',''));
  case aType of
  'Unit': aModule:=TPasModule.Create(aName,nil);
  'Program': aModule:=TPasProgram.Create(aName,nil);
  'Library': aModule:=TPasLibrary.Create(aName,nil);
  else
    {$IFDEF VerbosePJUFiler}
    writeln('TPJUReader.ReadModule Type="',aType,'"');
    {$ENDIF}
    RaiseMsg(20180203100748);
  end;
  Resolver.RootElement:=aModule;

  ReadPasElement(Obj,aModule,aContext);

  // modscope
  ModScope:=TPasModuleScope(Resolver.CreateScope(aModule,TPasModuleScope));
  ModScope.FirstName:=FirstDottedIdentifier(aModule.Name);
  ModScope.Flags:=ReadModuleScopeFlags(Obj,aModule,PJUDefaultModuleScopeFlags);
  ModScope.BoolSwitches:=ReadBoolSwitches(Obj.Find('BoolSwitches'),aModule,aContext.BoolSwitches);
  OldBoolSwitches:=aContext.BoolSwitches;
  aContext.BoolSwitches:=ModScope.BoolSwitches;
  // ToDo: AssertClass: TPasClassType
  // ToDo: AssertDefConstructor: TPasConstructor
  // ToDo: AssertMsgConstructor: TPasConstructor
  // ToDo: RangeErrorClass: TPasClassType
  // ToDo: RangeErrorConstructor: TPasConstructor

  // read sections
  aContext.LastElement:=aModule;
  SubObj:=PreReadSection(Obj,'Interface');
  if SubObj<>nil then
    begin
    aModule.InterfaceSection:=TInterfaceSection.Create('',aModule);
    ReadSection(SubObj,aModule.InterfaceSection,aContext);
    aContext.LastElement:=aModule.InterfaceSection;
    end;
  SubObj:=PreReadSection(Obj,'Implementation');
  if SubObj<>nil then
    begin
    aModule.ImplementationSection:=TImplementationSection.Create('',aModule);
    ReadSection(SubObj,aModule.ImplementationSection,aContext);
    aContext.LastElement:=aModule.InterfaceSection;
    end;
  if aModule.ClassType=TPasProgram then
    begin
    SubObj:=PreReadSection(Obj,'Program');
    if SubObj<>nil then
      begin
      TPasProgram(aModule).ProgramSection:=TProgramSection.Create('',aModule);
      ReadSection(SubObj,TPasProgram(aModule).ProgramSection,aContext);
      aContext.LastElement:=TPasProgram(aModule).ProgramSection;
      end;
    end
  else if aModule.ClassType=TPasLibrary then
    begin
    SubObj:=PreReadSection(Obj,'Library');
    if SubObj<>nil then
      begin
      TPasLibrary(aModule).LibrarySection:=TLibrarySection.Create('',aModule);
      ReadSection(SubObj,TPasLibrary(aModule).LibrarySection,aContext);
      aContext.LastElement:=TPasLibrary(aModule).LibrarySection;
      end;
    end;
  // ToDo: read precompiled aModule.InitializationSection
  // ToDo: read precompiled aModule.FinalizationSection

  aContext.BoolSwitches:=OldBoolSwitches;

  ResolvePending;
end;

procedure TPJUReader.ResolvePending;
var
  i: Integer;
  PendingIdentifierScope: TPJUReaderPendingIdentifierScope;
  Node: TAVLTreeNode;
  Ref: TPJUFilerElementRef;
begin
  for i:=0 to FPendingIdentifierScopes.Count-1 do
    begin
    PendingIdentifierScope:=TPJUReaderPendingIdentifierScope(FPendingIdentifierScopes[i]);
    ReadIdentifierScope(PendingIdentifierScope.Arr,PendingIdentifierScope.Scope);
    end;

  Node:=FElementRefs.FindLowest;
  while Node<>nil do
    begin
    Ref:=TPJUFilerElementRef(Node.Data);
    {$IFDEF VerbosePJUFiler}
    write('TPJUReader.ResolvePending Ref.Id=',Ref.Id,' Ref.Element=',GetObjName(Ref.Element));
    {$ENDIF}
    Node:=FElementRefs.FindSuccessor(Node);
    if Ref.Pending<>nil then
      RaiseMsg(20180207194340,Ref.Element,IntToStr(Ref.Id))
    end;
end;

constructor TPJUReader.Create;
begin
  inherited Create;
  FInitialFlags:=TPJUInitialFlags.Create;
  FPendingIdentifierScopes:=TObjectList.Create(true);
end;

destructor TPJUReader.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPendingIdentifierScopes);
  FreeAndNil(FInitialFlags);
end;

procedure TPJUReader.Clear;
begin
  FElementRefsArray:=nil;
  FPendingIdentifierScopes.Clear;
  inherited Clear;
  FInitialFlags.Clear;
end;

procedure TPJUReader.ReadPJU(aResolver: TPas2JSResolver; aStream: TStream);
var
  JParser: TJSONParser;
  Data: TJSONData;
begin
  JParser:=TJSONParser.Create(aStream,[joUTF8,joStrict]);
  try
    Data:=JParser.Parse;
    if not (Data is TJSONObject) then
      RaiseMsg(20180202130727,'expected JSON object, but found '+JSONTypeName(Data.JSONType));
  finally
    JParser.Free;
  end;
  ReadJSON(aResolver,TJSONObject(Data));
end;

procedure TPJUReader.ReadJSON(aResolver: TPas2JSResolver;
  Obj: TJSONObject);
var
  aName: String;
  Data: TJSONData;
  i: Integer;
  aContext: TPJUReaderContext;
  aModule: TPasModule;
begin
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModuleAsJSON START ');
  {$ENDIF}
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;

  ReadHeaderMagic(Obj);
  ReadHeaderVersion(Obj);

  aModule:=nil;
  for i:=0 to Obj.Count-1 do
    begin
    aName:=Obj.Names[i];
    {$IFDEF VerbosePJUFiler}
    writeln('TPJUReader.ReadModuleAsJSON ',aName);
    {$ENDIF}
    Data:=Obj.Elements[aName];
    case Obj.Names[i] of
    'FileType': ;
    'Version': ;
    'ParserOptions': InitialFlags.ParserOptions:=ReadParserOptions(Data,aModule,PJUDefaultParserOptions);
    'ModeSwitches': InitialFlags.ModeSwitches:=ReadModeSwitches(Data,aModule,PJUDefaultModeSwitches);
    'BoolSwitches': InitialFlags.BoolSwitches:=ReadBoolSwitches(Data,aModule,PJUDefaultBoolSwitches);
    'ConverterOptions': InitialFlags.ConverterOptions:=ReadConverterOptions(Data,aModule,PJUDefaultConvertOptions);
    'TargetPlatform': ReadTargetPlatform(Data);
    'TargetProcessor': ReadTargetProcessor(Data);
    'Sources': ReadSrcFiles(Data);
    'Module':
      begin
      aContext:=TPJUReaderContext.Create;
      try
        aContext.ModeSwitches:=InitialFlags.ModeSwitches;
        aContext.BoolSwitches:=InitialFlags.BoolSwitches;
        ReadModule(Data,aContext);
        aModule:=aResolver.RootElement;
      finally
        aContext.Free;
      end;
      end
    else
      RaiseMsg(20180202151706,'unknown property "'+aName+'"');
    end;
    end;
  {$IFDEF VerbosePJUFiler}
  writeln('TPJUReader.ReadModuleAsJSON END');
  {$ENDIF}
end;

end.

