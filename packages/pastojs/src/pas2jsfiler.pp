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
  Classes, Types, SysUtils, contnrs, crc,
  fpjson, jsonparser, jsonscanner,
  PasTree, PScanner, PParser, PasResolveEval, PasResolver,
  Pas2jsFileUtils, FPPas2Js;

const
  PJUMagic = 'Pas2JSCache';
  PJUVersion = 1;

  PJUDefaultParserOptions: TPOptions = [
    po_KeepScannerError,
    po_ResolveStandardTypes,
    po_AsmWhole,
    po_NoOverloadedProcs,
    po_KeepClassForward,
    po_ArrayRangeExpr,
    po_CheckModeSwitches,
    po_CheckCondFunction,
    po_ExtClassConstWithoutExpr];

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

  TPJUGetSrcEvent = procedure(Sender: TObject; aFilename: string;
    out p: PChar; out Count: integer) of object;

  { TPJUWriterContext }

  TPJUWriterContext = class
  public
    ModeSwitches: TModeSwitches;
    BoolSwitches: TBoolSwitches;
  end;

  { TPJUWriter }

  TPJUWriter = class
  private
    FInitialFlags: TPJUInitialFlags;
    FOnGetSrc: TPJUGetSrcEvent;
    FParser: TPasParser;
    FResolver: TPas2JSResolver;
    FScanner: TPascalScanner;
    FSourceFiles: TObjectList;
    FSourceFilesSorted: array of TPJUSourceFile;
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = '');
    procedure AddArrayFlag(Obj: TJSONObject; var Arr: TJSONArray;
      const ArrName, Flag: string; Enable: boolean);
    function GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum;
    procedure WriteHeaderMagic(Obj: TJSONObject); virtual;
    procedure WriteHeaderVersion(Obj: TJSONObject); virtual;
    procedure WriteInitialFlags(Obj: TJSONObject); virtual;
    procedure WriteParserOptions(Obj: TJSONObject; const Value, DefaultValue: TPOptions); virtual;
    procedure WriteModeSwitches(Obj: TJSONObject; const Value, DefaultValue: TModeSwitches); virtual;
    procedure WriteBoolSwitches(Obj: TJSONObject; const Value, DefaultValue: TBoolSwitches); virtual;
    procedure WriteConvertOptions(Obj: TJSONObject; const Value, DefaultValue: TPasToJsConverterOptions); virtual;
    procedure WriteSrcFiles(Obj: TJSONObject); virtual;
    procedure WriteModule(ParentJSON: TJSONObject; Module: TPasModule;
      aContext: TPJUWriterContext); virtual;
    procedure WriteModuleScope(ParentJSON: TJSONObject; ModScope: TPasModuleScope;
      aContext: TPJUWriterContext); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure WritePJU(aResolver: TPas2JSResolver;
      InitFlags: TPJUInitialFlags; aStream: TStream); virtual;
    function WriteJSON(aResolver: TPas2JSResolver;
      InitFlags: TPJUInitialFlags): TJSONObject; virtual;
    property Resolver: TPas2JSResolver read FResolver;
    property Parser: TPasParser read FParser;
    property Scanner: TPascalScanner read FScanner;
    property InitialFlags: TPJUInitialFlags read FInitialFlags;
    property OnGetSrc: TPJUGetSrcEvent read FOnGetSrc write FOnGetSrc;
  end;

  { TPJUReader }

  TPJUReader = class
  private
    FFileVersion: longint;
    FInitialFlags: TPJUInitialFlags;
    FParser: TPasParser;
    FResolver: TPas2JSResolver;
    FScanner: TPascalScanner;
    FSourceFiles: TObjectList;
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = '');
    function CheckJSONArray(Data: TJSONData; Id: int64): TJSONArray;
    function CheckJSONObject(Data: TJSONData; Id: int64): TJSONObject;
    function CheckJSONString(Data: TJSONData; Id: int64): String;
    procedure ReadHeaderMagic(Obj: TJSONObject); virtual;
    procedure ReadHeaderVersion(Obj: TJSONObject); virtual;
    procedure ReadArrayFlags(Data: TJSONData; const PropName: string; out Names: TStringDynArray; out Enable: TBooleanDynArray);
    function ReadParserOptions(Data: TJSONData; const DefaultValue: TPOptions): TPOptions; virtual;
    function ReadModeSwitches(Data: TJSONData; const DefaultValue: TModeSwitches): TModeSwitches; virtual;
    function ReadBoolSwitches(Data: TJSONData; const DefaultValue: TBoolSwitches): TBoolSwitches; virtual;
    function ReadConverterOptions(Data: TJSONData; const DefaultValue: TPasToJsConverterOptions): TPasToJsConverterOptions; virtual;
    procedure ReadTargetPlatform(Data: TJSONData); virtual;
    procedure ReadTargetProcessor(Data: TJSONData); virtual;
    procedure ReadSrcFiles(Data: TJSONData); virtual;
    procedure ReadModule(Data: TJSONData); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure ReadPJU(aResolver: TPas2JSResolver; aStream: TStream); virtual;
    procedure ReadJSON(aResolver: TPas2JSResolver; Obj: TJSONObject); virtual;
    property Resolver: TPas2JSResolver read FResolver;
    property Parser: TPasParser read FParser;
    property Scanner: TPascalScanner read FScanner;
    property FileVersion: longint read FFileVersion;
    property InitialFlags: TPJUInitialFlags read FInitialFlags;
  end;

function ComparePJUSrcFiles(File1, File2: Pointer): integer;
function EncodeVLQ(i: MaxPrecInt): string; overload;
function EncodeVLQ(i: MaxPrecUInt): string; overload;
function DecodeVLQ(const s: string): MaxPrecInt; // base256 Variable Length Quantity
function DecodeVLQ(var p: PByte): MaxPrecInt; // base256 Variable Length Quantity

function ComputeChecksum(p: PChar; Cnt: integer): TPJUSourceFileChecksum;

function ModeSwitchToInt(ms: TModeSwitch): byte;

function dbgmem(const s: string): string; overload;
function dbgmem(p: PChar; Cnt: integer): string; overload;

implementation

function ComparePJUSrcFiles(File1, File2: Pointer): integer;
var
  Src1: TPJUSourceFile absolute File1;
  Src2: TPJUSourceFile absolute File2;
begin
  Result:=CompareStr(Src1.Filename,Src2.Filename);
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

procedure TPJUWriter.RaiseMsg(Id: int64; const Msg: string);
var
  E: EPas2JsWriteError;
begin
  E:=EPas2JsWriteError.Create('['+IntToStr(Id)+'] '+Msg);
  E.Owner:=Self;
  raise E;
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

function TPJUWriter.GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum;
var
  p: PChar;
  Cnt: integer;
begin
  OnGetSrc(Self,aFilename,p,Cnt);
  Result:=ComputeChecksum(p,Cnt);
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

procedure TPJUWriter.WriteModule(ParentJSON: TJSONObject; Module: TPasModule;
  aContext: TPJUWriterContext);
var
  Obj: TJSONObject;
  ModScope: TPasModuleScope;
begin
  Obj:=TJSONObject.Create;
  ParentJSON.Add('Module',Obj);
  Obj.Add('Name',Module.Name);

  if Module.ClassType=TPasModule then
    Obj.Add('Type','Unit')
  else if Module.ClassType=TPasProgram then
    Obj.Add('Type','Program')
  else if Module.ClassType=TPasLibrary then
    Obj.Add('Type','Library')
  else
    RaiseMsg(20180203163923);

  ModScope:=Module.CustomData as TPasModuleScope;
  WriteModuleScope(Obj,ModScope,aContext);

  // ToDo: write sections
end;

procedure TPJUWriter.WriteModuleScope(ParentJSON: TJSONObject;
  ModScope: TPasModuleScope; aContext: TPJUWriterContext);
begin
  // FirstName not needed
  // ToDo: Flags: TPasModuleScopeFlags;
  WriteBoolSwitches(ParentJSON,ModScope.ScannerBoolSwitches,aContext.BoolSwitches);
  // ToDo: AssertClass: TPasClassType
  // ToDo: AssertDefConstructor: TPasConstructor
  // ToDo: AssertMsgConstructor: TPasConstructor
  // ToDo: RangeErrorClass: TPasClassType
  // ToDo: RangeErrorConstructor: TPasConstructor
end;

constructor TPJUWriter.Create;
begin
  FSourceFiles:=TObjectList.Create(true);
end;

destructor TPJUWriter.Destroy;
begin
  Clear;
  FreeAndNil(FSourceFiles);
  inherited Destroy;
end;

procedure TPJUWriter.Clear;
begin
  FSourceFiles.Clear;
  FResolver:=nil;
  FParser:=nil;
  FScanner:=nil;
  FInitialFlags:=nil;
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
  Obj: TJSONObject;
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
    // ToDo: WriteUsedModulesPrecompiledChecksums;
    aContext:=TPJUWriterContext.Create;
    aContext.ModeSwitches:=InitialFlags.ModeSwitches;
    aContext.BoolSwitches:=InitialFlags.BoolSwitches;
    WriteModule(Obj,aResolver.RootElement,aContext);
    // ToDo: write final flags: modeswitches, boolswitches, used defines

    Result:=Obj;
  finally
    aContext.Free;
    if Result=nil then
      Obj.Free;
  end;
end;

{ TPJUReader }

procedure TPJUReader.RaiseMsg(Id: int64; const Msg: string);
var
  E: EPas2JsReadError;
begin
  E:=EPas2JsReadError.Create('['+IntToStr(Id)+'] '+Msg);
  E.Owner:=Self;
  raise E;
end;

function TPJUReader.CheckJSONArray(Data: TJSONData; Id: int64): TJSONArray;
begin
  if Data is TJSONArray then exit(TJSONArray(Data));
  RaiseMsg(Id);
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

procedure TPJUReader.ReadHeaderMagic(Obj: TJSONObject);
begin
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadHeaderMagic ',Obj.Get('FileType',''));
  {$ENDIF}
  if Obj.Get('FileType','')<>PJUMagic then
    RaiseMsg(20180130201710,'not a pju file');
end;

procedure TPJUReader.ReadHeaderVersion(Obj: TJSONObject);
begin
  FFileVersion:=Obj.Get('Version',0);
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadHeaderVersion ',FFileVersion);
  {$ENDIF}
  if FFileVersion<1 then
    RaiseMsg(20180130201801,'invalid pju file version');
  if FFileVersion>PJUVersion then
    RaiseMsg(20180130201822,'pju file was created by a newer compiler.');
end;

procedure TPJUReader.ReadArrayFlags(Data: TJSONData; const PropName: string;
  out Names: TStringDynArray; out Enable: TBooleanDynArray);
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
  Arr:=CheckJSONArray(Data,20180203100055);
  Cnt:=Arr.Count;
  if Cnt=0 then exit;
  SetLength(Names,Cnt);
  SetLength(Enable,Cnt);
  for i:=0 to Cnt-1 do
    begin
    Data:=Arr[i];
    if not (Data is TJSONString) then
      RaiseMsg(20180202132350,PropName+' elements must be string');
    s:=String(TJSONString(Data).AsString);
    if s='' then
      RaiseMsg(20180202133605,PropName+' elements must be string');
    if s[1]='-' then
      begin
      Enable[i]:=false;
      system.Delete(s,1,1);
      end
    else
      Enable[i]:=true;
    if not (s[1] in IdentStart) then
      RaiseMsg(20180202133605,PropName+' elements must be identifiers');
    Names[i]:=s;
    end;
end;

function TPJUReader.ReadParserOptions(Data: TJSONData;
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
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadParserOptions START');
  {$ENDIF}
  ReadArrayFlags(Data,'ParserOptions',Names,Enable);
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

function TPJUReader.ReadModeSwitches(Data: TJSONData;
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
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadModeSwitches START');
  {$ENDIF}
  ReadArrayFlags(Data,'ModeSwitches',Names,Enable);
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

function TPJUReader.ReadBoolSwitches(Data: TJSONData;
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
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadBoolSwitches START');
  {$ENDIF}
  ReadArrayFlags(Data,'BoolSwitches',Names,Enable);
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

function TPJUReader.ReadConverterOptions(Data: TJSONData;
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
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadConverterOptions START');
  {$ENDIF}
  ReadArrayFlags(Data,'ConverterOptions',Names,Enable);
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
  {$IFDEF VerbosePJUReader}
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
  {$IFDEF VerbosePJUReader}
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
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadSrcFiles START ');
  {$ENDIF}
  SourcesArr:=CheckJSONArray(Data,20180203100250);
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

procedure TPJUReader.ReadModule(Data: TJSONData);
var
  Obj: TJSONObject;
  aType, aName: String;
  aModule: TPasModule;
begin
  {$IFDEF VerbosePJUReader}
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
    {$IFDEF VerbosePJUReader}
    writeln('TPJUReader.ReadModule Type="',aType,'"');
    {$ENDIF}
    RaiseMsg(20180203100748);
  end;
  Resolver.RootElement:=aModule;
  // ToDo: modscope
end;

constructor TPJUReader.Create;
begin
  FSourceFiles:=TObjectList.Create(true);
  FInitialFlags:=TPJUInitialFlags.Create;
end;

destructor TPJUReader.Destroy;
begin
  FreeAndNil(FInitialFlags);
  FreeAndNil(FSourceFiles);
  inherited Destroy;
end;

procedure TPJUReader.Clear;
begin
  FSourceFiles.Clear;
  FResolver:=nil;
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
begin
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadModuleAsJSON START ');
  {$ENDIF}
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;

  ReadHeaderMagic(Obj);
  ReadHeaderVersion(Obj);

  for i:=0 to Obj.Count-1 do
    begin
    aName:=Obj.Names[i];
    writeln('TPJUReader.ReadModuleAsJSON ',aName);
    Data:=Obj.Elements[aName];
    case Obj.Names[i] of
    'FileType': ;
    'Version': ;
    'ParserOptions': InitialFlags.ParserOptions:=ReadParserOptions(Data,PJUDefaultParserOptions);
    'ModeSwitches': InitialFlags.ModeSwitches:=ReadModeSwitches(Data,PJUDefaultModeSwitches);
    'BoolSwitches': InitialFlags.BoolSwitches:=ReadBoolSwitches(Data,PJUDefaultBoolSwitches);
    'ConverterOptions': InitialFlags.ConverterOptions:=ReadConverterOptions(Data,PJUDefaultConvertOptions);
    'TargetPlatform': ReadTargetPlatform(Data);
    'TargetProcessor': ReadTargetProcessor(Data);
    'Sources': ReadSrcFiles(Data);
    'Module': ReadModule(Data);
    else
      RaiseMsg(20180202151706,'unknown property "'+aName+'"');
    end;
    end;
  {$IFDEF VerbosePJUReader}
  writeln('TPJUReader.ReadModuleAsJSON END');
  {$ENDIF}
end;

end.

