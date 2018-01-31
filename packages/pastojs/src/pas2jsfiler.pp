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
  Write and read a precompiled module.

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
  Classes, SysUtils, contnrs, FPPas2Js, Pas2jsFileUtils, PasTree,
  PScanner, PParser, PasResolveEval, PasResolver;

const
  PJUMagic = 'Pas2JSCache';
  PJUVersion = 1;

type
  TPJUSourceFileKind = (
    sfkUnit,   // 0
    sfkInclude // 1
  );
  TPJUSourceFileKinds = set of TPJUSourceFileKind;
  TPJUSourceFileChecksum = longint;

  { TPJUSourceFile }

  TPJUSourceFile = class
  public
    Kind: TPJUSourceFileKind;
    Filename: string;
    Checksum: TPJUSourceFileChecksum;
    Index: integer;
  end;

  TPJUInitialFlags = class
  public
    ParserOptions: TPOptions;
    ModeSwitches: TModeSwitches;
    BoolSwitches: TBoolSwitches;
    ResolverOptions: TPasResolverOptions;
    PasTojsOptions: TPasToJsConverterOptions;
    TargetPlatform: TPasToJsPlatform;
    TargetProcessor: TPasToJsProcessor;
    // ToDo: defines
  end;

  { TPasToJsWriter }

  TPasToJsWriter = class
  private
    FInitialFlags: TPJUInitialFlags;
    FParser: TPasParser;
    FResolver: TPasResolver;
    FScanner: TPascalScanner;
    FSourceFiles: TObjectList;
    FSourceFilesSorted: array of TPJUSourceFile;
    FStream: TStream;
  protected
    procedure WriteStr(const s: string);
    procedure WriteInt(const i: MaxPrecInt);
    procedure WriteText(const s: string);
    procedure WriteHeaderMagic; virtual;
    procedure WriteHeaderVersion; virtual;
    procedure WriteInitialFlags; virtual;
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
  end;

  EPas2JsReadError = class(Exception)
  public
    Owner: TObject;
  end;

  { TPasToJsReader }

  TPasToJsReader = class
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

{ TPasToJsReader }

procedure TPasToJsReader.RaiseMsg(Id: int64; const Msg: string);
begin
  raise EPas2JsReadError.Create('['+IntToStr(Id)+'] '+Msg);
end;

procedure TPasToJsReader.RaiseEOF(Id: int64);
begin
  RaiseMsg(Id,'unexpected EOF');
end;

function TPasToJsReader.ReadStr(Cnt: integer): string;
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

function TPasToJsReader.CheckStr(const s: string): boolean;
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

function TPasToJsReader.ReadInt: MaxPrecInt;
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

function TPasToJsReader.ReadInt(LowBound, UpBound: MaxPrecInt): MaxPrecInt;
begin
  Result:=ReadInt();
  if Result<LowBound then
    RaiseMsg(20180130203354)
  else if Result>UpBound then
    RaiseMsg(20180130203413);
end;

function TPasToJsReader.ReadText: string;
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

procedure TPasToJsReader.ReadHeaderMagic;
begin
  if not CheckStr(PJUMagic) then
    RaiseMsg(20180130201710,'not a pju file');
end;

procedure TPasToJsReader.ReadHeaderVersion;
begin
  FFileVersion:=ReadInt;
  if FFileVersion<1 then
    RaiseMsg(20180130201801,'invalid pju file version');
  if FFileVersion>PJUVersion then
    RaiseMsg(20180130201822,'pju file was created by a newer compiler.');
end;

procedure TPasToJsReader.ReadInitialFlags;
begin
  // Write modeswitches
  //for ms in InitialFlags.Modeswitches do
  //  WriteInt(ModeSwitchToInt(ms));
  //WriteInt(-1);
  // ToDo: write initial flags: BoolSwitches, used defines, used macros
end;

procedure TPasToJsReader.ReadSrcFiles;
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

constructor TPasToJsReader.Create;
begin
  FSourceFiles:=TObjectList.Create(true);
  FInitialFlags:=TPJUInitialFlags.Create;
end;

destructor TPasToJsReader.Destroy;
begin
  FreeAndNil(FInitialFlags);
  FreeAndNil(FSourceFiles);
  inherited Destroy;
end;

procedure TPasToJsReader.Clear;
begin
  FSourceFiles.Clear;
  FResolver:=nil;
  FPJU:='';
  FInitialFlags.Free;
  FInitialFlags:=TPJUInitialFlags.Create;
end;

procedure TPasToJsReader.ReadModule(aResolver: TPasResolver; aPJU: String);
begin
  FResolver:=aResolver;
  FParser:=Resolver.CurrentParser;
  FScanner:=FParser.Scanner;
  FPJU:=aPJU;
  FCur:=PByte(PChar(FPJU));
  FEnd:=FCur+length(FPJU);

  ReadHeaderMagic;
  ReadHeaderVersion;
  ReadSrcFiles;
  ReadInitialFlags;
end;

{ TPasToJsWriter }

procedure TPasToJsWriter.WriteStr(const s: string);
begin
  if s='' then exit;
  FStream.Write(s[1],length(s));
end;

procedure TPasToJsWriter.WriteInt(const i: MaxPrecInt);
begin
  WriteStr(EncodeVLQ(i));
  //writeln('TPasToJsWriter.WriteInt ',i,' ',dbgmem(EncodeVLQ(i)));
end;

procedure TPasToJsWriter.WriteText(const s: string);
begin
  WriteInt(length(s));
  if s<>'' then
    WriteStr(s);
end;

procedure TPasToJsWriter.WriteHeaderMagic;
begin
  WriteStr(PJUMagic);
end;

procedure TPasToJsWriter.WriteHeaderVersion;
begin
  WriteInt(PJUVersion);
end;

procedure TPasToJsWriter.WriteInitialFlags;
begin
  // Write modeswitches
  //for ms in InitialFlags.Modeswitches do
  //  WriteInt(ModeSwitchToInt(ms));
  //WriteInt(-1);
  // ToDo: write initial flags: BoolSwitches, used defines, used macros
end;

procedure TPasToJsWriter.WriteSrcFiles;
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
      // ToDo: checksum
      List.Add(CurFile);
      end;

    // create FSourceFilesSorted;
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

constructor TPasToJsWriter.Create;
begin
  FSourceFiles:=TObjectList.Create(true);
end;

destructor TPasToJsWriter.Destroy;
begin
  Clear;
  FreeAndNil(FSourceFiles);
  inherited Destroy;
end;

procedure TPasToJsWriter.Clear;
begin
  FSourceFiles.Clear;
  FResolver:=nil;
  FParser:=nil;
  FScanner:=nil;
  FStream:=nil;
  FInitialFlags:=nil;
end;

procedure TPasToJsWriter.WriteModule(aResolver: TPasResolver; aStream: TStream;
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

