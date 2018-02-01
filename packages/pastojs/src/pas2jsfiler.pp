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
  Classes, SysUtils, contnrs, crc,
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
  PJUDefaultBoolSwitches: TBoolSwitches = [
    bsHints,
    bsNotes,
    bsWarnings
    ];
  PJUDefaultConvertOptions: TPasToJsConverterOptions = [];
  PJUDefaultTargetPlatform = PlatformBrowser;
  PJUDefaultTargetProcessor = ProcessorECMAScript5;

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
  TPJUSourceFileKind = (
    sfkUnit,   // 0
    sfkInclude // 1
  );
  TPJUSourceFileKinds = set of TPJUSourceFileKind;
  TPJUSourceFileChecksum = cardinal;

  { TPJUSourceFile }

  TPJUSourceFile = class
  public
    Kind: TPJUSourceFileKind;
    Filename: string;
    Checksum: TPJUSourceFileChecksum;
    Index: integer;
  end;

  TPJUGetSrcEvent = procedure(Sender: TObject; aFilename: string;
    out p: PChar; out Count: integer) of object;

  { TPJUWriter }

  TPJUWriter = class
  private
    FInitialFlags: TPJUInitialFlags;
    FOnGetSrc: TPJUGetSrcEvent;
    FParser: TPasParser;
    FResolver: TPasResolver;
    FScanner: TPascalScanner;
    FSourceFiles: TObjectList;
    FSourceFilesSorted: array of TPJUSourceFile;
    FStream: TStream;
  protected
    function GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum;
    procedure WriteStr(const s: string);
    procedure WriteInt(const i: MaxPrecInt);
    procedure WriteText(const s: string);
    procedure WriteHeaderMagic; virtual;
    procedure WriteHeaderVersion; virtual;
    procedure WriteInitialFlags; virtual;
    procedure WriteParserOptions(const Value, DefaultValue: TPOptions); virtual;
    procedure WriteModeSwitches(const Value, DefaultValue: TModeSwitches); virtual;
    procedure WriteBoolSwitches(const Value, DefaultValue: TBoolSwitches); virtual;
    procedure WriteConvertOptions(const Value, DefaultValue: TPasToJsConverterOptions); virtual;
    procedure WriteSrcFiles; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure WriteModule(aResolver: TPasResolver; aStream: TStream;
      InitFlags: TPJUInitialFlags); virtual;
    property Resolver: TPasResolver read FResolver;
    property Stream: TStream read FStream;
    property Parser: TPasParser read FParser;
    property Scanner: TPascalScanner read FScanner;
    property InitialFlags: TPJUInitialFlags read FInitialFlags;
    property OnGetSrc: TPJUGetSrcEvent read FOnGetSrc write FOnGetSrc;
  end;

  EPas2JsReadError = class(Exception)
  public
    Owner: TObject;
  end;

  { TPJUReader }

  TPJUReader = class
  private
    FCur: PByte;
    FEnd: PByte;
    FFileVersion: longint;
    FInitialFlags: TPJUInitialFlags;
    FParser: TPasParser;
    FPJU: String;
    FResolver: TPasResolver;
    FScanner: TPascalScanner;
    FSourceFiles: TObjectList;
  protected
    procedure RaiseMsg(Id: int64; const Msg: string = '');
    procedure RaiseEOF(Id: int64);
    function ReadStr(Cnt: integer): string;
    function CheckStr(const s: string): boolean;
    function ReadInt: MaxPrecInt; overload;
    function ReadInt(LowBound, UpBound: MaxPrecInt): MaxPrecInt; overload;
    function ReadText: string;
    procedure ReadHeaderMagic; virtual;
    procedure ReadHeaderVersion; virtual;
    procedure ReadInitialFlags; virtual;
    function ReadParserOptions(const DefaultValue: TPOptions): TPOptions; virtual;
    function ReadModeSwitches(const DefaultValue: TModeSwitches): TModeSwitches; virtual;
    function ReadBoolSwitches(const DefaultValue: TBoolSwitches): TBoolSwitches; virtual;
    function ReadConverterOptions(const DefaultValue: TPasToJsConverterOptions): TPasToJsConverterOptions; virtual;
    procedure ReadSrcFiles; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure ReadModule(aResolver: TPasResolver; aPJU: String); virtual;
    property Resolver: TPasResolver read FResolver;
    property Parser: TPasParser read FParser;
    property Scanner: TPascalScanner read FScanner;
    property PJU: String read FPJU;
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

{ TPJUReader }

procedure TPJUReader.RaiseMsg(Id: int64; const Msg: string);
begin
  raise EPas2JsReadError.Create('['+IntToStr(Id)+'] '+Msg);
end;

procedure TPJUReader.RaiseEOF(Id: int64);
begin
  RaiseMsg(Id,'unexpected EOF');
end;

function TPJUReader.ReadStr(Cnt: integer): string;
begin
  if Cnt=0 then exit('');
  if Cnt>0 then
    begin
    if FEnd-FCur<Cnt then
      RaiseEOF(20180130192845);
    SetLength(Result,Cnt);
    System.Move(FCur^,Result[1],Cnt);
    inc(FCur,Cnt);
    end
  else
    RaiseMsg(20180130193811);
end;

function TPJUReader.CheckStr(const s: string): boolean;
var
  l: Integer;
begin
  l:=length(s);
  if l=0 then exit(true);
  if FEnd-FCur<l then
    RaiseEOF(20180130201602);
  if not CompareMem(FCur,@s[1],l) then exit(false);
  inc(FCur,l);
  Result:=true;
end;

function TPJUReader.ReadInt: MaxPrecInt;
begin
  if FCur=FEnd then
    RaiseMsg(20180130201047);
  Result:=DecodeVLQ(FCur); // Note: safe because FEnd^=#0
  if FCur>FEnd then
    begin
    FCur:=FEnd;
    RaiseMsg(20180130200819);
    end;
end;

function TPJUReader.ReadInt(LowBound, UpBound: MaxPrecInt): MaxPrecInt;
begin
  Result:=ReadInt();
  if Result<LowBound then
    RaiseMsg(20180130203354)
  else if Result>UpBound then
    RaiseMsg(20180130203413);
end;

function TPJUReader.ReadText: string;
var
  l: MaxPrecInt;
begin
  l:=ReadInt;
  if l=0 then
    exit('')
  else if l<0 then
    RaiseMsg(20180130200936)
  else if FEnd-FCur<l then
    RaiseMsg(20180130200946);
  SetLength(Result,l);
  System.Move(FCur^,Result[1],l);
  inc(FCur,l);
end;

procedure TPJUReader.ReadHeaderMagic;
begin
  if not CheckStr(PJUMagic) then
    RaiseMsg(20180130201710,'not a pju file');
end;

procedure TPJUReader.ReadHeaderVersion;
begin
  FFileVersion:=ReadInt;
  if FFileVersion<1 then
    RaiseMsg(20180130201801,'invalid pju file version');
  if FFileVersion>PJUVersion then
    RaiseMsg(20180130201822,'pju file was created by a newer compiler.');
end;

procedure TPJUReader.ReadInitialFlags;
begin
  InitialFlags.ParserOptions:=ReadParserOptions(PJUDefaultParserOptions);
  InitialFlags.ModeSwitches:=ReadModeSwitches(PJUDefaultModeSwitches);
  InitialFlags.BoolSwitches:=ReadBoolSwitches(PJUDefaultBoolSwitches);
  InitialFlags.ConverterOptions:=ReadConverterOptions(PJUDefaultConvertOptions);
  case ReadInt of
    1: InitialFlags.TargetPlatform:=PlatformBrowser;
    2: InitialFlags.TargetPlatform:=PlatformNodeJS;
  else
    RaiseMsg(20180131170539,'invalid target platform');
  end;
  case ReadInt of
    1: InitialFlags.TargetProcessor:=ProcessorECMAScript5;
    2: InitialFlags.TargetProcessor:=ProcessorECMAScript6;
  else
    RaiseMsg(20180131170622,'invalid target processor');
  end;
  // ToDo: write initial flags: BoolSwitches, used defines, used macros
end;

function TPJUReader.ReadParserOptions(const DefaultValue: TPOptions): TPOptions;
var
  i: integer;
begin
  Result:=DefaultValue;
  repeat
    i:=ReadInt(-100,100);
    case i of
    0: exit;
    +1: Include(Result,po_KeepScannerError);
    -1: Exclude(Result,po_KeepScannerError);
    +2: Include(Result,po_CAssignments);
    -2: Exclude(Result,po_CAssignments);
    +3: Include(Result,po_ResolveStandardTypes);
    -3: Exclude(Result,po_ResolveStandardTypes);
    +4: Include(Result,po_AsmWhole);
    -4: Exclude(Result,po_AsmWhole);
    +5: Include(Result,po_NoOverloadedProcs);
    -5: Exclude(Result,po_NoOverloadedProcs);
    +6: Include(Result,po_KeepClassForward);
    -6: Exclude(Result,po_KeepClassForward);
    +7: Include(Result,po_ArrayRangeExpr);
    -7: Exclude(Result,po_ArrayRangeExpr);
    +8: Include(Result,po_SelfToken);
    -8: Exclude(Result,po_SelfToken);
    +9: Include(Result,po_CheckModeSwitches);
    -9: Exclude(Result,po_CheckModeSwitches);
    +10: Include(Result,po_CheckCondFunction);
    -10: Exclude(Result,po_CheckCondFunction);
    +11: Include(Result,po_StopOnErrorDirective);
    -11: Exclude(Result,po_StopOnErrorDirective);
    +12: Include(Result,po_ExtClassConstWithoutExpr);
    -12: Exclude(Result,po_ExtClassConstWithoutExpr);
    else
      RaiseMsg(20180131163751,'po='+IntToStr(i));
    end;
  until false;
end;

function TPJUReader.ReadModeSwitches(const DefaultValue: TModeSwitches
  ): TModeSwitches;
var
  i: integer;
begin
  Result:=DefaultValue;
  repeat
    i:=ReadInt(-100,100);
    case i of
    0: exit;
    -1: Exclude(Result,msNone);
    +1: Include(Result,msNone);
    // mode
    -2: Exclude(Result,msFpc);
    +2: Include(Result,msFpc);
    -3: Exclude(Result,msObjfpc);
    +3: Include(Result,msObjfpc);
    -4: Exclude(Result,msDelphi);
    +4: Include(Result,msDelphi);
    -5: Exclude(Result,msDelphiUnicode);
    +5: Include(Result,msDelphiUnicode);
    -6: Exclude(Result,msTP7);
    +6: Include(Result,msTP7);
    -7: Exclude(Result,msMac);
    +7: Include(Result,msMac);
    -8: Exclude(Result,msIso);
    +8: Include(Result,msIso);
    -9: Exclude(Result,msExtpas);
    +9: Include(Result,msExtpas);
    -10: Exclude(Result,msGPC);
    +10: Include(Result,msGPC);
    // switches
    -31: Exclude(Result,msClass);
    +31: Include(Result,msClass);
    -32: Exclude(Result,msObjpas);
    +32: Include(Result,msObjpas);
    -33: Exclude(Result,msResult);
    +33: Include(Result,msResult);
    -34: Exclude(Result,msStringPchar);
    +34: Include(Result,msStringPchar);
    -35: Exclude(Result,msCVarSupport);
    +35: Include(Result,msCVarSupport);
    -36: Exclude(Result,msNestedComment);
    +36: Include(Result,msNestedComment);
    -37: Exclude(Result,msTPProcVar);
    +37: Include(Result,msTPProcVar);
    -38: Exclude(Result,msMacProcVar);
    +38: Include(Result,msMacProcVar);
    -39: Exclude(Result,msRepeatForward);
    +39: Include(Result,msRepeatForward);
    -40: Exclude(Result,msPointer2Procedure);
    +40: Include(Result,msPointer2Procedure);
    -41: Exclude(Result,msAutoDeref);
    +41: Include(Result,msAutoDeref);
    -42: Exclude(Result,msInitFinal);
    +42: Include(Result,msInitFinal);
    -43: Exclude(Result,msDefaultAnsistring);
    +43: Include(Result,msDefaultAnsistring);
    -44: Exclude(Result,msOut);
    +44: Include(Result,msOut);
    -45: Exclude(Result,msDefaultPara);
    +45: Include(Result,msDefaultPara);
    -46: Exclude(Result,msHintDirective);
    +46: Include(Result,msHintDirective);
    -47: Exclude(Result,msDuplicateNames);
    +47: Include(Result,msDuplicateNames);
    -48: Exclude(Result,msProperty);
    +48: Include(Result,msProperty);
    -49: Exclude(Result,msDefaultInline);
    +49: Include(Result,msDefaultInline);
    -50: Exclude(Result,msExcept);
    +50: Include(Result,msExcept);
    -51: Exclude(Result,msObjectiveC1);
    +51: Include(Result,msObjectiveC1);
    -52: Exclude(Result,msObjectiveC2);
    +52: Include(Result,msObjectiveC2);
    -53: Exclude(Result,msNestedProcVars);
    +53: Include(Result,msNestedProcVars);
    -54: Exclude(Result,msNonLocalGoto);
    +54: Include(Result,msNonLocalGoto);
    -55: Exclude(Result,msAdvancedRecords);
    +55: Include(Result,msAdvancedRecords);
    -56: Exclude(Result,msISOLikeUnaryMinus);
    +56: Include(Result,msISOLikeUnaryMinus);
    -57: Exclude(Result,msSystemCodePage);
    +57: Include(Result,msSystemCodePage);
    -58: Exclude(Result,msFinalFields);
    +58: Include(Result,msFinalFields);
    -59: Exclude(Result,msDefaultUnicodestring);
    +59: Include(Result,msDefaultUnicodestring);
    -60: Exclude(Result,msTypeHelpers);
    +60: Include(Result,msTypeHelpers);
    -61: Exclude(Result,msCBlocks);
    +61: Include(Result,msCBlocks);
    -62: Exclude(Result,msISOLikeIO);
    +62: Include(Result,msISOLikeIO);
    -63: Exclude(Result,msISOLikeProgramsPara);
    +63: Include(Result,msISOLikeProgramsPara);
    -64: Exclude(Result,msISOLikeMod);
    +64: Include(Result,msISOLikeMod);
    -65: Exclude(Result,msExternalClass);
    +65: Include(Result,msExternalClass);
    -66: Exclude(Result,msPrefixedAttributes);
    +66: Include(Result,msPrefixedAttributes);
    -67: Exclude(Result,msIgnoreInterfaces);
    +67: Include(Result,msIgnoreInterfaces);
    -68: Exclude(Result,msIgnoreAttributes);
    +68: Include(Result,msIgnoreAttributes);
    else
      RaiseMsg(20180131152915,'ms='+IntToStr(i));
    end;
  until false;
end;

function TPJUReader.ReadBoolSwitches(const DefaultValue: TBoolSwitches
  ): TBoolSwitches;
var
  i: integer;
begin
  Result:=DefaultValue;
  repeat
    i:=ReadInt(-100,100);
    case i of
    0: exit;
    +1: Include(Result,bsNone);
    -1: Exclude(Result,bsNone);
    +2: Include(Result,bsAlign);
    -2: Exclude(Result,bsAlign);
    +3: Include(Result,bsBoolEval);
    -3: Exclude(Result,bsBoolEval);
    +4: Include(Result,bsAssertions);
    -4: Exclude(Result,bsAssertions);
    +5: Include(Result,bsDebugInfo);
    -5: Exclude(Result,bsDebugInfo);
    +6: Include(Result,bsExtension);
    -6: Exclude(Result,bsExtension);
    +7: Include(Result,bsImportedData);
    -7: Exclude(Result,bsImportedData);
    +8: Include(Result,bsLongStrings);
    -8: Exclude(Result,bsLongStrings);
    +9: Include(Result,bsIOChecks);
    -9: Exclude(Result,bsIOChecks);
    +10: Include(Result,bsWriteableConst);
    -10: Exclude(Result,bsWriteableConst);
    +11: Include(Result,bsLocalSymbols);
    -11: Exclude(Result,bsLocalSymbols);
    +12: Include(Result,bsTypeInfo);
    -12: Exclude(Result,bsTypeInfo);
    +13: Include(Result,bsOptimization);
    -13: Exclude(Result,bsOptimization);
    +14: Include(Result,bsOpenStrings);
    -14: Exclude(Result,bsOpenStrings);
    +15: Include(Result,bsOverflowChecks);
    -15: Exclude(Result,bsOverflowChecks);
    +16: Include(Result,bsRangeChecks);
    -16: Exclude(Result,bsRangeChecks);
    +17: Include(Result,bsTypedAddress);
    -17: Exclude(Result,bsTypedAddress);
    +18: Include(Result,bsSafeDivide);
    -18: Exclude(Result,bsSafeDivide);
    +19: Include(Result,bsVarStringChecks);
    -19: Exclude(Result,bsVarStringChecks);
    +20: Include(Result,bsStackframes);
    -20: Exclude(Result,bsStackframes);
    +21: Include(Result,bsExtendedSyntax);
    -21: Exclude(Result,bsExtendedSyntax);
    +22: Include(Result,bsReferenceInfo);
    -22: Exclude(Result,bsReferenceInfo);
    +23: Include(Result,bsHints);
    -23: Exclude(Result,bsHints);
    +24: Include(Result,bsNotes);
    -24: Exclude(Result,bsNotes);
    +25: Include(Result,bsWarnings);
    -25: Exclude(Result,bsWarnings);
    +26: Include(Result,bsMacro);
    -26: Exclude(Result,bsMacro);
    +27: Include(Result,bsScopedEnums);
    -27: Exclude(Result,bsScopedEnums);
    +28: Include(Result,bsObjectChecks);
    -28: Exclude(Result,bsObjectChecks);
    else
      RaiseMsg(20180131170303,'bs='+IntToStr(i));
    end;
  until false;
end;

function TPJUReader.ReadConverterOptions(
  const DefaultValue: TPasToJsConverterOptions): TPasToJsConverterOptions;
var
  i: integer;
begin
  Result:=DefaultValue;
  repeat
    i:=ReadInt(-100,100);
    case i of
    0: exit;
    +1: Exclude(Result,coLowerCase);
    -1: Include(Result,coLowerCase);
    +2: Exclude(Result,coSwitchStatement);
    -2: Include(Result,coSwitchStatement);
    +3: Exclude(Result,coEnumNumbers);
    -3: Include(Result,coEnumNumbers);
    +4: Exclude(Result,coUseStrict);
    -4: Include(Result,coUseStrict);
    +5: Exclude(Result,coNoTypeInfo);
    -5: Include(Result,coNoTypeInfo);
    +6: Exclude(Result,coEliminateDeadCode);
    -6: Include(Result,coEliminateDeadCode);
    else
      RaiseMsg(20180131170301,'co='+IntToStr(i));
    end;
  until false;
end;

procedure TPJUReader.ReadSrcFiles;
var
  Cnt: MaxPrecInt;
  i: Integer;
  CurFile: TPJUSourceFile;
  CurFilename: String;
begin
  if not CheckStr('Files') then
    RaiseMsg(20180130202024);
  Cnt:=ReadInt;
  for i:=0 to Cnt-1 do
    begin
    CurFile:=TPJUSourceFile.Create;
    FSourceFiles.Add(CurFile);
    CurFile.Kind:=TPJUSourceFileKind(ReadInt(ord(low(TPJUSourceFileKind)),ord(high(TPJUSourceFileKind))));
    CurFilename:=ReadText;
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
    CurFile.Checksum:=ReadInt(low(TPJUSourceFileChecksum),high(TPJUSourceFileChecksum));
    end;
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
  FPJU:='';
  FInitialFlags.Clear;
end;

procedure TPJUReader.ReadModule(aResolver: TPasResolver; aPJU: String);
begin
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;
  FPJU:=aPJU;
  FCur:=PByte(PChar(FPJU));
  FEnd:=FCur+length(FPJU);

  ReadHeaderMagic;
  ReadHeaderVersion;
  ReadInitialFlags;
  ReadSrcFiles;
end;

{ TPJUWriter }

function TPJUWriter.GetSrcCheckSum(aFilename: string): TPJUSourceFileChecksum;
var
  p: PChar;
  Cnt: integer;
begin
  OnGetSrc(Self,aFilename,p,Cnt);
  Result:=ComputeChecksum(p,Cnt);
end;

procedure TPJUWriter.WriteStr(const s: string);
begin
  if s='' then exit;
  FStream.Write(s[1],length(s));
end;

procedure TPJUWriter.WriteInt(const i: MaxPrecInt);
begin
  WriteStr(EncodeVLQ(i));
  //writeln('TPasToJsWriter.WriteInt ',i,' ',dbgmem(EncodeVLQ(i)));
end;

procedure TPJUWriter.WriteText(const s: string);
begin
  WriteInt(length(s));
  if s<>'' then
    WriteStr(s);
end;

procedure TPJUWriter.WriteHeaderMagic;
begin
  WriteStr(PJUMagic);
end;

procedure TPJUWriter.WriteHeaderVersion;
begin
  WriteInt(PJUVersion);
end;

procedure TPJUWriter.WriteInitialFlags;
begin
  WriteParserOptions(InitialFlags.ParserOptions,PJUDefaultParserOptions);
  WriteModeSwitches(InitialFlags.Modeswitches,PJUDefaultModeSwitches);
  WriteBoolSwitches(InitialFlags.BoolSwitches,PJUDefaultBoolSwitches);
  WriteConvertOptions(InitialFlags.ConverterOptions,PJUDefaultConvertOptions);
  case InitialFlags.TargetPlatform of
    PlatformBrowser: WriteInt(1);
    PlatformNodeJS: WriteInt(2);
  end;
  case InitialFlags.TargetProcessor of
    ProcessorECMAScript5: WriteInt(1);
    ProcessorECMAScript6: WriteInt(2);
  end;
  // ToDo: write initial flags: used defines, used macros
end;

procedure TPJUWriter.WriteParserOptions(const Value, DefaultValue: TPOptions);

  procedure AddDiff(s: TPOption; Int: MaxPrecInt);
  begin
    if s in Value then
      begin
      if not (s in DefaultValue) then
        begin
        WriteInt(Int);
        //writeln('TPJUWriter.WriteParserOptions.AddDiff ',s);
        end;
      end
    else
      begin
      if s in DefaultValue then
        WriteInt(-Int);
      end;
  end;

begin
  AddDiff(po_KeepScannerError,1);
  AddDiff(po_CAssignments,2);
  AddDiff(po_ResolveStandardTypes,3);
  AddDiff(po_AsmWhole,4);
  AddDiff(po_NoOverloadedProcs,5);
  AddDiff(po_KeepClassForward,6);
  AddDiff(po_ArrayRangeExpr,7);
  AddDiff(po_SelfToken,8);
  AddDiff(po_CheckModeSwitches,9);
  AddDiff(po_CheckCondFunction,10);
  AddDiff(po_StopOnErrorDirective,11);
  AddDiff(po_ExtClassConstWithoutExpr,12);
  WriteInt(0);
end;

procedure TPJUWriter.WriteModeSwitches(const Value,
  DefaultValue: TModeSwitches);

  procedure AddDiff(s: TModeSwitch; Int: MaxPrecInt);
  begin
    if s in Value then
      begin
      if not (s in DefaultValue) then
        begin
        WriteInt(Int);
        //writeln('TPJUWriter.WriteModeSwitches.AddDiff ',s);
        end;
      end
    else
      begin
      if s in DefaultValue then
        WriteInt(-Int);
      end;
  end;

begin
  AddDiff(msNone,1);
  // mode
  AddDiff(msFpc,2);
  AddDiff(msObjfpc,3);
  AddDiff(msDelphi,4);
  AddDiff(msDelphiUnicode,5);
  AddDiff(msTP7,6);
  AddDiff(msMac,7);
  AddDiff(msIso,8);
  AddDiff(msExtpas,9);
  AddDiff(msGPC,10);
  // switches
  AddDiff(msClass,31);
  AddDiff(msObjpas,32);
  AddDiff(msResult,33);
  AddDiff(msStringPchar,34);
  AddDiff(msCVarSupport,35);
  AddDiff(msNestedComment,36);
  AddDiff(msTPProcVar,37);
  AddDiff(msMacProcVar,38);
  AddDiff(msRepeatForward,39);
  AddDiff(msPointer2Procedure,40);
  AddDiff(msAutoDeref,41);
  AddDiff(msInitFinal,42);
  AddDiff(msDefaultAnsistring,43);
  AddDiff(msOut,44);
  AddDiff(msDefaultPara,45);
  AddDiff(msHintDirective,46);
  AddDiff(msDuplicateNames,47);
  AddDiff(msProperty,48);
  AddDiff(msDefaultInline,49);
  AddDiff(msExcept,50);
  AddDiff(msObjectiveC1,51);
  AddDiff(msObjectiveC2,52);
  AddDiff(msNestedProcVars,53);
  AddDiff(msNonLocalGoto,54);
  AddDiff(msAdvancedRecords,55);
  AddDiff(msISOLikeUnaryMinus,56);
  AddDiff(msSystemCodePage,57);
  AddDiff(msFinalFields,58);
  AddDiff(msDefaultUnicodestring,59);
  AddDiff(msTypeHelpers,60);
  AddDiff(msCBlocks,61);
  AddDiff(msISOLikeIO,62);
  AddDiff(msISOLikeProgramsPara,63);
  AddDiff(msISOLikeMod,64);
  AddDiff(msExternalClass,65);
  AddDiff(msPrefixedAttributes,66);
  AddDiff(msIgnoreInterfaces,67);
  AddDiff(msIgnoreAttributes,68);
  // stop byte
  WriteInt(0);
end;

procedure TPJUWriter.WriteBoolSwitches(const Value,
  DefaultValue: TBoolSwitches);

  procedure AddDiff(s: TBoolSwitch; Int: MaxPrecInt);
  begin
    if s in Value then
      begin
      if not (s in DefaultValue) then
        begin
        WriteInt(Int);
        //writeln('TPJUWriter.WriteBoolSwitches.AddDiff ',s);
        end;
      end
    else
      begin
      if s in DefaultValue then
        WriteInt(-Int);
      end;
  end;

begin
  AddDiff(bsNone,1);
  AddDiff(bsAlign,2);
  AddDiff(bsBoolEval,3);
  AddDiff(bsAssertions,4);
  AddDiff(bsDebugInfo,5);
  AddDiff(bsExtension,6);
  AddDiff(bsImportedData,7);
  AddDiff(bsLongStrings,8);
  AddDiff(bsIOChecks,9);
  AddDiff(bsWriteableConst,10);
  AddDiff(bsLocalSymbols,11);
  AddDiff(bsTypeInfo,12);
  AddDiff(bsOptimization,13);
  AddDiff(bsOpenStrings,14);
  AddDiff(bsOverflowChecks,15);
  AddDiff(bsRangeChecks,16);
  AddDiff(bsTypedAddress,17);
  AddDiff(bsSafeDivide,18);
  AddDiff(bsVarStringChecks,19);
  AddDiff(bsStackframes,20);
  AddDiff(bsExtendedSyntax,21);
  AddDiff(bsReferenceInfo,22);
  AddDiff(bsHints,23);
  AddDiff(bsNotes,24);
  AddDiff(bsWarnings,25);
  AddDiff(bsMacro,26);
  AddDiff(bsScopedEnums,27);
  AddDiff(bsObjectChecks,28);
  WriteInt(0);
end;

procedure TPJUWriter.WriteConvertOptions(const Value,
  DefaultValue: TPasToJsConverterOptions);

  procedure AddDiff(s: TPasToJsConverterOption; Int: MaxPrecInt);
  begin
    if s in Value then
      begin
      if not (s in DefaultValue) then
        begin
        WriteInt(Int);
        //writeln('TPJUWriter.WriteConvertOptions.AddDiff ',s);
        end;
      end
    else
      begin
      if s in DefaultValue then
        WriteInt(-Int);
      end;
  end;

begin
  AddDiff(coLowerCase,1);
  AddDiff(coSwitchStatement,2);
  AddDiff(coEnumNumbers,3);
  AddDiff(coUseStrict,4);
  AddDiff(coNoTypeInfo,5);
  AddDiff(coEliminateDeadCode,6);
  WriteInt(0);
end;

procedure TPJUWriter.WriteSrcFiles;
var
  CurFile: TPJUSourceFile;
  List: TFPList;
  i: Integer;
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
        CurFile.Kind:=sfkUnit
      else
        CurFile.Kind:=sfkInclude;
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
    WriteStr('Files');
    WriteInt(FSourceFiles.Count);
    for i:=0 to FSourceFiles.Count-1 do
      begin
      CurFile:=TPJUSourceFile(FSourceFiles[i]);
      WriteInt(ord(CurFile.Kind));
      WriteText(CurFile.Filename);
      WriteInt(CurFile.Checksum);
      end;
  finally
    List.Free;
  end;
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
  FStream:=nil;
  FInitialFlags:=nil;
end;

procedure TPJUWriter.WriteModule(aResolver: TPasResolver; aStream: TStream;
  InitFlags: TPJUInitialFlags);
begin
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;
  FStream:=aStream;
  FInitialFlags:=InitFlags;
  try
    WriteHeaderMagic;
    WriteHeaderVersion;
    WriteInitialFlags;
    WriteSrcFiles;
    // ToDo: WriteUsedModulesPrecompiledChecksums;
    // ToDo: WriteModule;
    // ToDo: write final flags: modeswitches, boolswitches, used defines
  finally
    FStream:=nil;
  end;
end;

end.

