{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    In-process ppudump interface-stub generator for resolver RTL provisioning

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.PpuStub;

{ In-process ppudump interface-stub generator:
  drives ppudump over a version-matched FPC .ppu (from the installed FPC)
  and the cross-unit refs it needs, synthesizing compact interface stubs
  the resolver binds against, cached by InterfaceCRC with a per-dependency
  synthetic fallback. You can opt out by specifying --synthetic-only. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes;
{$ELSE}
  SysUtils, Classes;
{$ENDIF}

type
  { In-process ppudump interface-stub generator.
    A thin wrapper: all fpjson/JSON machinery lives in the implementation,
    so this unit's INTERFACE is fpjson-free. }
  TFpSonarPpuStubGen = class
  private
    FImpl: TObject;             // TPpuGenImpl (implementation-only)
  public
    constructor Create;
    destructor Destroy; override;
    // Adds a directory tree to the .ppu name->path index (recursive scan).
    procedure AddSearchDir(const aDir: string);
    // True iff a usable 'ppudump' executable was found (cached after first call).
    function PpudumpAvailable: boolean;
    // The resolved .ppu path for aName, or '' if not in the indexed dirs.
    function LocatePpu(const aName: string): string;
    // The .ppu InterfaceCRC for aName without emitting, or '' if unavailable.
    function InterfaceCrc(const aName: string): string;
    { Generates the flattened stub SOURCE for aName. Returns True with aCanonical
      (real cased unit name), aCrc (InterfaceCRC) and aSource filled; False (all
      out params '') when the .ppu is absent / ppudump fails / JSON unparseable. }
    function GenerateUnit(const aName: string; out aCanonical, aCrc,
      aSource: string): boolean;
    // The accumulated faithful-degrade omit report (reason=count lines).
    function Omitted: TStringList;
  end;

// Runs 'ppudump -Fj -VA aPath', returning stdout in aOut. False on spawn error.
function RunPpudump(const aPpudump, aPath: string; out aOut: string): boolean;

// Repairs the ppudump bare NaN/Inf float-const JSON bug into valid JSON.
function RepairPpudumpJson(const aText: string): string;

// Locates a 'ppudump' executable on PATH; '' if none.
function FindPpudump: string;

{ The superset-hybrid RTL source for SysUtils/Classes }
function HybridRtlSource(const aName, aSyntheticSource: string): string;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.StrUtils, System.Process, System.Pipes, FpJson.Parser, FpJson.Data;
{$ELSE}
  StrUtils, Process, Pipes, jsonparser, fpjson;
{$ENDIF}

type
  // One dumped .ppu interface (owns its parsed JSON tree + the deref/byid maps).
  TFpSonarPpu = class
  private
    FName: string;              // canonical unit name (from self.Name)
    FPath: string;
    FData: TJSONData;           // owns the parsed tree
    FUnitObj: TJSONObject;      // the unit object (data[0])
    FUnits: TStringList;        // deref map: index -> lowercase unit name (0-based)
    FIface: TJSONArray;         // the Interface[] array (may be nil)
    FById: TStringList;         // 'Id' -> TJSONObject (sorted)
    FCrc: string;
    FTarget: string;
  public
    constructor Create(const aName, aPath: string; aData: TJSONData);
    destructor Destroy; override;
    function ById(aId: integer): TJSONObject;
    function UnitAt(aIdx: integer): string;
    property Name: string read FName;
    property Iface: TJSONArray read FIface;
    property Crc: string read FCrc;
    property Target: string read FTarget;
  end;

  // The real generator (pimpl body for TFpSonarPpuStubGen). All fpjson-touching
  // logic is confined here so the public wrapper's interface stays fpjson-free.
  TPpuGenImpl = class
  private
    FIndex: TStringList;        // lowercase unit name -> .ppu path (first wins)
    FScanned: TStringList;      // dir trees already indexed (skip re-scan)
    FCache: TStringList;        // lowercase name -> TFpSonarPpu (Objects; may be nil)
    FOmitted: TStringList;      // reason=count (report)
    FPpudump: string;           // resolved ppudump path ('' = unavailable)
    FDetected: boolean;
    // Fixpoint of local type names allowed to be emitted this pass (nil = all).
    FCurAllowed: TStringList;
    FLocalTypes: TStringList;   // param-scoped inline defs: 'Id' -> TJSONObject
    FNeededAliases: TStringList;// RTL_LOCAL_ALIAS names referenced this unit
    FAnonEnums: TStringList;    // anon-enum ppu Id -> synthesized decl line
    FClsMethods: TStringList;   // per-class overload-key dedup
    procedure BumpOmit(const aReason: string);
    procedure IndexDir(const aDir: string);
    function GetPpu(const aName: string): TFpSonarPpu;
    procedure RefName(aRef: TJSONObject; aUnit: TFpSonarPpu;
      out aName, aProv: string; out aAnonDecl: TJSONObject);
    function Deref(aRef: TJSONObject; aUnit: TFpSonarPpu;
      out aHome: TFpSonarPpu): TJSONObject;
    function NearestAncestor(aRef: TJSONObject; aUnit: TFpSonarPpu;
      aUses: TStringList; aDepth: integer): string;
    function Resolve(aRef: TJSONObject; aUnit: TFpSonarPpu;
      aUses: TStringList; aDepth: integer): string;
    function InlineAnon(aDecl: TJSONObject; aUnit: TFpSonarPpu;
      aUses: TStringList; aDepth: integer): string;
    function ProcSig(aDecl: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList;
      aDepth: integer; aAnon: boolean): string;
    function RetOf(aDecl: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList;
      aDepth: integer): string;
    function Params(aPlist: TJSONArray; aUnit: TFpSonarPpu; aUses: TStringList;
      aDepth: integer; out aOk: boolean): string;
    function RenderDefault(aDflt: TJSONObject; const aTy: string): string;
    function LocalTypeById(aId: integer): TJSONObject;
    function EmitEnum(aE: TJSONObject): string;
    function EmitAlias(aE: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList): string;
    function EmitSet(aE: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList): string;
    function EmitConst(aE: TJSONObject): string;
    function EmitProctype(aE: TJSONObject; aUnit: TFpSonarPpu;
      aUses: TStringList): string;
    function EmitAnonAlias(aE: TJSONObject; aUnit: TFpSonarPpu;
      aUses: TStringList): string;
    function EmitRecord(aE: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList): string;
    function EmitClass(aE: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList;
      aIndexedProps: TStringList): string;
    function EmitHelper(aE: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList;
      aIndexedProps: TStringList): string;
    function HelperKind(aHpRef: TJSONObject; aUnit: TFpSonarPpu): string;
    procedure EmitMember(aF: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList;
      aPub, aPriv, aSeen, aIndexedProps: TStringList; const aCls: string;
      aInHelper: boolean);
    function MethodSig(aF: TJSONObject; aUnit: TFpSonarPpu; aUses: TStringList): string;
    function EmitUnitBody(aPpu: TFpSonarPpu; const aCanonical, aVersion: string;
      aEmittedNames: TStringList): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSearchDir(const aDir: string);
    function PpudumpAvailable: boolean;
    function LocatePpu(const aName: string): string;
    function InterfaceCrc(const aName: string): string;
    function GenerateUnit(const aName: string; out aCanonical, aCrc,
      aSource: string): boolean;
    property Omitted: TStringList read FOmitted;
  end;

const
  // The flatten boundary — units resolved through committed synthetic stubs.
  cSyntheticRtl: array[0..7] of string = ('system', 'sysutils', 'classes',
    'contnrs', 'math', 'types', 'regexpr', 'objpas');
  // Units whose types resolve but are never 'used' (cascade + RTL-internal).
  cCascadeStop: array[0..14] of string = ('variants', 'typinfo', 'objpas',
    'baseunix', 'unix', 'linux', 'unixtype', 'unixutil', 'syscall', 'errors',
    'sysconst', 'rtlconsts', 'sortbase', 'dtdmodel', 'ctypes');
  // Compiler builtin scalar/string/pointer names the resolver knows verbatim.
  cBuiltins: array[0..47] of string = ('longint', 'integer', 'int64', 'qword',
    'word', 'byte', 'smallint', 'shortint', 'cardinal', 'longword', 'nativeint',
    'nativeuint', 'ptrint', 'ptruint', 'sizeint', 'boolean', 'bytebool',
    'wordbool', 'longbool', 'qwordbool', 'double', 'single', 'extended', 'comp',
    'currency', 'real', 'char', 'ansichar', 'widechar', 'pchar', 'pansichar',
    'pwidechar', 'string', 'ansistring', 'shortstring', 'utf8string',
    'rawbytestring', 'unicodestring', 'widestring', 'pointer', 'variant',
    'olevariant', 'dword', 'sizeuint', 'valreal', 'text', 'file', 'tdatetime');

function InArr(const aName: string; const aArr: array of string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(aArr) to High(aArr) do
    if SameText(aName, aArr[i]) then
      Exit(True);
end;


function IsSyntheticRtl(const aName: string): boolean;
begin
  Result := InArr(aName, cSyntheticRtl);
end;


function IsCascadeStop(const aName: string): boolean;
begin
  Result := InArr(aName, cCascadeStop);
end;


function IsBuiltin(const aName: string): boolean;
begin
  Result := InArr(aName, cBuiltins);
end;


// An RTL type the synthetic stubs declare -> the providing synthetic unit, or ''.
function RtlKnownUnit(const aName: string): string;
var
  lLow: string;
begin
  lLow := LowerCase(aName);
  if (lLow = 'tobject') or (lLow = 'tclass') or (lLow = 'tinterfacedobject') then
    Result := 'System'
  else if (lLow = 'exception') or (lLow = 'econverterror') then
    Result := 'SysUtils'
  else if (lLow = 'tstream') or (lLow = 'tstrings') or (lLow = 'tstringlist')
    or (lLow = 'tlist') or (lLow = 'tfplist') or (lLow = 'tpersistent')
    or (lLow = 'tcomponent') or (lLow = 'tfilestream') or (lLow = 'thandlestream') then
    Result := 'Classes'
  else if (lLow = 'tfphashlist') or (lLow = 'tfpobjectlist')
    or (lLow = 'tobjectlist') then
    Result := 'Contnrs'
  else
    Result := '';
end;


// A faithful System alias the resolver lacks: name -> (Cased, Base). True if so.
function RtlLocalAlias(const aName: string; out aCased, aBase: string): boolean;
var
  lLow: string;
begin
  Result := True;
  lLow := LowerCase(aName);
  if lLow = 'tdatetime' then
  begin
    aCased := 'TDateTime';
    aBase := 'Double';
  end
  else if lLow = 'tdate' then
  begin
    aCased := 'TDate';
    aBase := 'Double';
  end
  else if lLow = 'ttime' then
  begin
    aCased := 'TTime';
    aBase := 'Double';
  end
  else
    Result := False;
end;


function IsSpecial(const aName: string): boolean;
begin
  Result := (aName = '$void') or (aName = '$formal')
    or (aName = '$char_pointer') or (aName = '$open_char_array');
end;


// True for a compiler-internal generic / specialization name (a '$' inside the
// name, not a leading-'$' pseudo-type) — unrepresentable, omit.
function IsGenericName(const aName: string): boolean;
begin
  Result := (aName <> '') and (aName[1] <> '$') and (Pos('$', aName) > 0);
end;


function IsWordChar(aCh: char): boolean;
begin
  Result := aCh in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;


// Renders a Pascal string constant, encoding control chars as #N concatenations.
function PasStringLiteral(const aStr: string): string;
var
  i, lOrd: integer;
  lBuf: string;
  lHaveOut: boolean;
begin
  Result := '';
  lBuf := '';
  lHaveOut := False;
  for i := 1 to Length(aStr) do
  begin
    lOrd := Ord(aStr[i]);
    if aStr[i] = '''' then
      lBuf := lBuf + ''''''
    else if (lOrd >= 32) and (lOrd < 127) then
      lBuf := lBuf + aStr[i]
    else
    begin
      if lBuf <> '' then
      begin
        Result := Result + '''' + lBuf + '''';
        lBuf := '';
        lHaveOut := True;
      end;
      Result := Result + '#' + IntToStr(lOrd);
      lHaveOut := True;
    end;
  end;
  if (lBuf <> '') or (not lHaveOut) then
    Result := Result + '''' + lBuf + '''';
end;


// Joins the entries of aList with aSep (no quoting).
function JoinSL(aList: TStringList; const aSep: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to aList.Count - 1 do
  begin
    if i > 0 then
      Result := Result + aSep;
    Result := Result + aList[i];
  end;
end;


function RepairPpudumpJson(const aText: string): string;
var
  i, lLen: integer;
  lTok, lPrevSig: string;

  function WordAt(const aWord: string; aPos: integer): boolean;
  var
    lAfter: integer;
  begin
    Result := (aPos + Length(aWord) - 1 <= lLen)
      and (Copy(aText, aPos, Length(aWord)) = aWord);
    if not Result then
      Exit;
    // Whole-token only: the char AFTER must not continue an identifier/number
    // (so 'Infinity' is not matched as 'Inf', and 'Information' is never matched).
    lAfter := aPos + Length(aWord);
    if (lAfter <= lLen) and IsWordChar(aText[lAfter]) then
      Result := False;
  end;

begin
  Result := '';
  lLen := Length(aText);
  i := 1;
  lPrevSig := '';   // last non-whitespace significant char emitted
  while i <= lLen do
  begin
    lTok := '';
      { Only quote a bare NaN/Inf/Infinity that sits in a JSON VALUE position —
        i.e. immediately after a ':' (possibly signed). This mirrors the Python's
        `:\s*-?(NaN|Inf|Infinity)\b` anchor and never corrupts string contents
        (inside a JSON string the preceding significant char is never a bare ':'). }
    if (lPrevSig = ':') and (aText[i] in ['N', 'I', '-']) then
    begin
      if WordAt('-Infinity', i) then lTok := '-Infinity'
      else if WordAt('-Inf', i) then lTok := '-Inf'
      else if WordAt('Infinity', i) then lTok := 'Infinity'
      else if WordAt('Inf', i) then lTok := 'Inf'
      else if WordAt('NaN', i) then lTok := 'NaN';
    end;
    if lTok <> '' then
    begin
      Result := Result + '"' + lTok + '"';
      Inc(i, Length(lTok));
      lPrevSig := '"';
    end
    else
    begin
      Result := Result + aText[i];
      if aText[i] > ' ' then
        lPrevSig := aText[i];
      Inc(i);
    end;
  end;
end;


// Reads the bytes currently available on aPipe (non-blocking) into aSink
// (nil = discard). Returns True if any bytes were read.
function DrainAvailable(aPipe: TInputPipeStream; aSink: TMemoryStream): boolean;
var
  lBuf: array[0..65535] of byte;
  lRead: longint;
begin
  Result := False;
  while aPipe.NumBytesAvailable > 0 do
  begin
    lRead := aPipe.Read(lBuf, SizeOf(lBuf));
    if lRead <= 0 then
      Break;
    if aSink <> nil then
      aSink.Write(lBuf, lRead);
    Result := True;
  end;
end;


{ Blocking-reads aPipe to EOF into aSink. Safe ONLY after the child has exited
  (its write-end is closed) — then Read returns the remaining bytes then 0, with
  no deadlock — so it guarantees the full stdout is captured post-exit. }
procedure DrainToEof(aPipe: TInputPipeStream; aSink: TMemoryStream);
var
  lBuf: array[0..65535] of byte;
  lRead: longint;
begin
  repeat
    lRead := aPipe.Read(lBuf, SizeOf(lBuf));
    if lRead > 0 then
      aSink.Write(lBuf, lRead);
  until lRead <= 0;
end;


function RunPpudump(const aPpudump, aPath: string; out aOut: string): boolean;
var
  lProc: TProcess;
  lStream: TMemoryStream;
begin
  Result := False;
  aOut := '';
  if (aPpudump = '') or (not FileExists(aPath)) then
    Exit;
  lProc := TProcess.Create(nil);
  lStream := TMemoryStream.Create;
  try
    lProc.Executable := aPpudump;
    lProc.Parameters.Add('-Fj');
    lProc.Parameters.Add('-VA');
    lProc.Parameters.Add(aPath);
    lProc.Options := [poUsePipes, poNoConsole];
    try
      lProc.Execute;
      { Drain BOTH pipes while the child runs. poUsePipes also pipes stderr, and
        ppudump can write to it; leaving stderr unread risks a DEADLOCK (the child
        blocks writing a full stderr pipe while we block reading stdout). We keep
        stdout (the JSON) and discard stderr; polling NumBytesAvailable keeps
        either read from stalling the other pipe. }
      while lProc.Running do
        if not (DrainAvailable(lProc.Output, lStream)
          or DrainAvailable(lProc.Stderr, nil)) then
          Sleep(1);
      // Child exited: write-ends are closed, so blocking-read the buffered rest.
      DrainToEof(lProc.Output, lStream);
      DrainAvailable(lProc.Stderr, nil);
      if lStream.Size > 0 then
        SetString(aOut, PChar(lStream.Memory), lStream.Size);
      Result := aOut <> '';
    except
      Result := False;
      aOut := '';
    end;
  finally
    lStream.Free;
    lProc.Free;
  end;
end;


function FindPpudump: string;
var
  lExe: string;
begin
  {$IFDEF WINDOWS}
  lExe := 'ppudump.exe';
  {$ELSE}
  lExe := 'ppudump';
  {$ENDIF}
  Result := FileSearch(lExe, GetEnvironmentVariable('PATH'));
end;


// Joins the lines with #10 line-endings, each terminated by #10 (matches the
// make_rtl_stubs.py '\n'-joined literal blocks on this target).
function LinesToStr(const aLines: array of string): string;
var
  i: integer;
begin
  Result := '';
  for i := Low(aLines) to High(aLines) do
    Result := Result + aLines[i] + #10;
end;


// The 1-based position of the LAST occurrence of aSub in aStr, or 0.
function LastPosOf(const aSub, aStr: string): integer;
var
  p, lNext: integer;
begin
  Result := 0;
  p := Pos(aSub, aStr);
  while p > 0 do
  begin
    Result := p;
    lNext := PosEx(aSub, aStr, p + 1);
    if lNext = 0 then
      Break;
    p := lNext;
  end;
end;


// Replaces the FIRST occurrence of aOld with aNew. aFound reports whether aOld
// was present (a missing anchor => the caller degrades rather than approximate).
function ReplaceFirst(const aStr, aOld, aNew: string; out aFound: boolean): string;
var
  p: integer;
begin
  p := Pos(aOld, aStr);
  aFound := p > 0;
  if not aFound then
    Exit(aStr);
  Result := Copy(aStr, 1, p - 1) + aNew + Copy(aStr, p + Length(aOld), MaxInt);
end;


function HybridRtlSource(const aName, aSyntheticSource: string): string;
var
  lSrc: string;
  lOk1, lOk2, lOk3: boolean;
  lIdx: integer;

  procedure SpliceBeforeImpl(const aAdd: string);
  var
    q: integer;
  begin
    q := Pos('implementation', lSrc);
    if q > 0 then
      lSrc := Copy(lSrc, 1, q - 1) + aAdd + Copy(lSrc, q, MaxInt);
  end;

begin
  Result := '';
  if SameText(aName, 'SysUtils') then
  begin
    lSrc := StringReplace(aSyntheticSource, '{$mode objfpc}{$H+}',
      '{$mode objfpc}{$H+}{$modeswitch typehelpers}', []);
    lSrc := ReplaceFirst(lSrc,
      LinesToStr(['  TFormatSettings = record',
      '    DecimalSeparator: AnsiChar;',
      '  end;']),
      LinesToStr(['  TFormatSettings = record',
      '    CurrencyFormat: Byte;',
      '    NegCurrFormat: Byte;',
      '    ThousandSeparator: Char;',
      '    DecimalSeparator: Char;',
      '    CurrencyDecimals: Byte;',
      '    DateSeparator: Char;',
      '    TimeSeparator: Char;',
      '    ListSeparator: Char;',
      '    CurrencyString: string;',
      '    ShortDateFormat: string;',
      '    LongDateFormat: string;',
      '    TimeAMString: string;',
      '    TimePMString: string;',
      '    ShortTimeFormat: string;',
      '    LongTimeFormat: string;',
      '  end;']), lOk1);
    if not lOk1 then
      Exit;   // synthetic TFormatSettings shape changed — degrade to synthetic
    SpliceBeforeImpl(LinesToStr([
      'type',
      '  TBytes = array of Byte;',
      '  RTLString = AnsiString;',
      '  TRTLStringDynArray = array of RTLString;',
      '  DWord = Cardinal;',
      '  TStringArray = array of string;',
      '  TStringSplitOptions = (None, ExcludeEmpty, ExcludeLastEmpty);',
      '  TStringHelper = type helper for AnsiString',
      '    function Split(const aSeparators: array of Char): TStringArray; overload;',
      '    function Split(const aSeparators: array of Char; aOptions: TStringSplitOptions): TStringArray; overload;',
      '    function Split(const aSeparators: array of AnsiString): TStringArray; overload;',
      '    function Split(const aSeparators: array of AnsiString; aOptions: TStringSplitOptions): TStringArray; overload;',
      '  end;',
      'function FileExists(const aFileName: RawByteString; aFollowLink: Boolean = True): Boolean;',
      'function ExpandFileName(const aFileName: AnsiString): AnsiString;',
      'function ChangeFileExt(const aFileName, aExtension: AnsiString): AnsiString;',
      'function ExtractFileExt(const aFileName: AnsiString): AnsiString;',
      'function GetEnvironmentVariable(const aEnvVar: AnsiString): AnsiString;',
      'function DirectoryExists(const aDirectory: RawByteString; aFollowLink: Boolean = True): Boolean;',
      'procedure Move(const aSource; var aDest; aCount: SizeInt);',
      'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte); overload;',
      'procedure FillChar(var aDest; aCount: SizeInt; aValue: Char); overload;',
      'procedure FreeMem(aPtr: Pointer); overload;',
      'procedure FreeMem(aPtr: Pointer; aSize: PtrUInt); overload;',
      'procedure GetMem(out aPtr: Pointer; aSize: PtrUInt);',
      'procedure ReallocMem(var aPtr: Pointer; aSize: PtrUInt);',
      'function SwapEndian(aValue: Word): Word; overload;',
      'function SwapEndian(aValue: DWord): DWord; overload;',
      'function SwapEndian(aValue: QWord): QWord; overload;',
      'function Odd(aValue: Int64): Boolean;',
      'const',
      '  sLineBreak = #10;',
      '  MaxInt = 2147483647;',
      '  MinInt = -2147483648;',
      'var',
      '  DefaultFormatSettings: TFormatSettings;',
      '  TimeSeparator: Char;',
      '  DateSeparator: Char;']));
    lIdx := LastPosOf('end.', lSrc);
    if lIdx = 0 then
      Exit;
    lSrc := Copy(lSrc, 1, lIdx - 1) + LinesToStr([
      'function FileExists(const aFileName: RawByteString; aFollowLink: Boolean = True): Boolean;',
      'begin', 'end;',
      'function TStringHelper.Split(const aSeparators: array of Char): TStringArray;',
      'begin', 'end;',
      'function TStringHelper.Split(const aSeparators: array of Char; aOptions: TStringSplitOptions): TStringArray;',
      'begin', 'end;',
      'function TStringHelper.Split(const aSeparators: array of AnsiString): TStringArray;',
      'begin', 'end;',
      'function TStringHelper.Split(const aSeparators: array of AnsiString; aOptions: TStringSplitOptions): TStringArray;',
      'begin', 'end;',
      'function ExpandFileName(const aFileName: AnsiString): AnsiString;',
      'begin', 'end;',
      'function ChangeFileExt(const aFileName, aExtension: AnsiString): AnsiString;',
      'begin', 'end;',
      'function ExtractFileExt(const aFileName: AnsiString): AnsiString;',
      'begin', 'end;',
      'function GetEnvironmentVariable(const aEnvVar: AnsiString): AnsiString;',
      'begin', 'end;',
      'function DirectoryExists(const aDirectory: RawByteString; aFollowLink: Boolean = True): Boolean;',
      'begin', 'end;',
      'procedure Move(const aSource; var aDest; aCount: SizeInt);',
      'begin', 'end;',
      'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte);',
      'begin', 'end;',
      'procedure FillChar(var aDest; aCount: SizeInt; aValue: Char);',
      'begin', 'end;',
      'procedure FreeMem(aPtr: Pointer);',
      'begin', 'end;',
      'procedure FreeMem(aPtr: Pointer; aSize: PtrUInt);',
      'begin', 'end;',
      'procedure GetMem(out aPtr: Pointer; aSize: PtrUInt);',
      'begin', 'end;',
      'procedure ReallocMem(var aPtr: Pointer; aSize: PtrUInt);',
      'begin', 'end;',
      'function SwapEndian(aValue: Word): Word;',
      'begin', 'end;',
      'function SwapEndian(aValue: DWord): DWord;',
      'begin', 'end;',
      'function SwapEndian(aValue: QWord): QWord;',
      'begin', 'end;',
      'function Odd(aValue: Int64): Boolean;',
      'begin', 'end;']) + Copy(lSrc, lIdx, MaxInt);
    Result := lSrc;
  end
  else if SameText(aName, 'Classes') then
  begin
    lSrc := ReplaceFirst(aSyntheticSource,
      LinesToStr(['  TStream = class(TObject)', '  end;']),
      LinesToStr([
      '  TSeekOrigin = (soBeginning, soCurrent, soEnd);',
      '  TStream = class(TObject)',
      '  protected',
      '    function GetPosition: Int64; virtual;',
      '    procedure SetPosition(const aPos: Int64); virtual;',
      '    function GetSize: Int64; virtual;',
      '    procedure SetSize(aNewSize: LongInt); virtual; overload;',
      '    procedure SetSize(const aNewSize: Int64); virtual; overload;',
      '    procedure SetSize64(const aNewSize: Int64); virtual;',
      '    procedure InvalidSeek; virtual;',
      '  public',
      '    function Read(var aBuffer; aCount: LongInt): LongInt; virtual; abstract;',
      '    function Write(const aBuffer; aCount: LongInt): LongInt; virtual; abstract;',
      '    function Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64; virtual; overload;',
      '    function Seek(aOffset: LongInt; aOrigin: Word): LongInt; virtual; overload;',
      '    procedure ReadBuffer(var aBuffer; aCount: LongInt);',
      '    function ReadByte: Byte;',
      '    procedure WriteByte(aByte: Byte);',
      '    property Position: Int64 read GetPosition write SetPosition;',
      '    property Size: Int64 read GetSize write SetSize;',
      '  end;']), lOk1);
    lSrc := ReplaceFirst(lSrc,
      '  fmOpenRead = 0;'#10,
      LinesToStr([
      '  fmOpenRead = 0;',
      '  fmOpenWrite = 1;',
      '  fmOpenReadWrite = 2;',
      '  fmShareExclusive = $0010;',
      '  fmShareDenyWrite = $0020;',
      '  fmShareDenyRead = $0030;',
      '  fmShareDenyNone = $0040;']), lOk2);
    lSrc := ReplaceFirst(lSrc,
      LinesToStr(['  TStrings = class(TPersistent)', '  public']),
      LinesToStr([
      '  TStrings = class(TPersistent)',
      '  private',
      '    function GetPpuStubCount: Integer;',
      '    function GetPpuStubString(aIndex: Integer): AnsiString;',
      '  public',
      '    property Count: Integer read GetPpuStubCount;',
      '    property Strings[aIndex: Integer]: AnsiString read GetPpuStubString; default;']), lOk3);
    if not (lOk1 and lOk2 and lOk3) then
      Exit;   // a synthetic anchor changed — degrade to synthetic
    SpliceBeforeImpl(LinesToStr([
      'type',
      '  TStreamOwnership = (soReference, soOwned);',
      '  EStreamError = class(Exception)',
      '  end;',
      '  TListSortCompare = function(Item1, Item2: Pointer): Integer;',
      '  TOwnerStream = class(TStream)',
      '  protected',
      '    FSource: TStream;',
      '    FOwner: Boolean;',
      '  public',
      '    constructor Create(aSource: TStream);',
      '    property Source: TStream read FSource;',
      '    property SourceOwner: Boolean read FOwner write FOwner;',
      '  end;',
      '  TBinaryObjectReader = class(TObject)',
      '  end;']));
    lIdx := LastPosOf('end.', lSrc);
    if lIdx = 0 then
      Exit;
    lSrc := Copy(lSrc, 1, lIdx - 1) + LinesToStr([
      'constructor TOwnerStream.Create(aSource: TStream);',
      'begin', 'end;',
      'procedure TStream.ReadBuffer(var aBuffer; aCount: LongInt);',
      'begin', 'end;',
      'function TStream.ReadByte: Byte;',
      'begin', 'end;',
      'procedure TStream.WriteByte(aByte: Byte);',
      'begin', 'end;',
      'function TStream.GetPosition: Int64;',
      'begin', 'end;',
      'procedure TStream.SetPosition(const aPos: Int64);',
      'begin', 'end;',
      'function TStream.GetSize: Int64;',
      'begin', 'end;',
      'procedure TStream.SetSize(aNewSize: LongInt);',
      'begin', 'end;',
      'procedure TStream.SetSize(const aNewSize: Int64);',
      'begin', 'end;',
      'function TStream.Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64;',
      'begin', 'end;',
      'function TStream.Seek(aOffset: LongInt; aOrigin: Word): LongInt;',
      'begin', 'end;',
      'procedure TStream.SetSize64(const aNewSize: Int64);',
      'begin', 'end;',
      'procedure TStream.InvalidSeek;',
      'begin', 'end;',
      'function TStrings.GetPpuStubCount: Integer;',
      'begin', 'end;',
      'function TStrings.GetPpuStubString(aIndex: Integer): AnsiString;',
      'begin', 'end;']) + Copy(lSrc, lIdx, MaxInt);
    Result := lSrc;
  end;
end;


{ TFpSonarPpu }

constructor TFpSonarPpu.Create(const aName, aPath: string; aData: TJSONData);
var
  lUnitsArr: TJSONArray;
  lIface, lEntry: TJSONData;
  i, lId: integer;
begin
  inherited Create;
  FName := aName;
  FPath := aPath;
  FData := aData;
  FUnits := TStringList.Create;
  FById := TStringList.Create;
  FById.Sorted := True;
  FById.Duplicates := dupIgnore;
  if (aData.JSONType = jtArray) and (TJSONArray(aData).Count > 0) then
    FUnitObj := TJSONArray(aData).Items[0] as TJSONObject
  else if aData.JSONType = jtObject then
    FUnitObj := TJSONObject(aData)
  else
    FUnitObj := nil;
  if FUnitObj = nil then
    Exit;
  FName := FUnitObj.Get('Name', aName);
  FCrc := FUnitObj.Get('InterfaceCRC', '');
  FTarget := FUnitObj.Get('TargetCPU', '') + '/' + FUnitObj.Get('TargetOS', '');
  lUnitsArr := FUnitObj.Find('Units') as TJSONArray;
  if lUnitsArr <> nil then
    for i := 0 to lUnitsArr.Count - 1 do
      FUnits.Add(LowerCase(lUnitsArr.Items[i].AsString));
  lIface := FUnitObj.Find('Interface');
  if (lIface <> nil) and (lIface.JSONType = jtArray) then
  begin
    FIface := TJSONArray(lIface);
    for i := 0 to FIface.Count - 1 do
    begin
      lEntry := FIface.Items[i];
      if lEntry.JSONType <> jtObject then
        Continue;
      if TJSONObject(lEntry).Find('Id') <> nil then
      begin
        lId := TJSONObject(lEntry).Get('Id', -1);
        if lId >= 0 then
          FById.AddObject(IntToStr(lId), lEntry);
      end;
    end;
  end;
end;


destructor TFpSonarPpu.Destroy;
begin
  FUnits.Free;
  FById.Free;
  FData.Free;
  inherited Destroy;
end;


function TFpSonarPpu.ById(aId: integer): TJSONObject;
var
  lIdx: integer;
begin
  Result := nil;
  if FById.Find(IntToStr(aId), lIdx) then
    Result := TJSONObject(FById.Objects[lIdx]);
end;


function TFpSonarPpu.UnitAt(aIdx: integer): string;
begin
  if (aIdx >= 0) and (aIdx < FUnits.Count) then
    Result := FUnits[aIdx]
  else
    Result := '';
end;


{ TFpSonarPpuStubGen }

constructor TPpuGenImpl.Create;
begin
  inherited Create;
  FIndex := TStringList.Create;
  FIndex.Sorted := True;
  FIndex.Duplicates := dupIgnore;
  FScanned := TStringList.Create;
  FScanned.Sorted := True;
  FScanned.Duplicates := dupIgnore;
  FCache := TStringList.Create;
  FCache.Sorted := True;
  FCache.Duplicates := dupIgnore;
  FOmitted := TStringList.Create;
  FLocalTypes := TStringList.Create;
  FNeededAliases := TStringList.Create;
  FNeededAliases.Sorted := True;
  FNeededAliases.Duplicates := dupIgnore;
  FAnonEnums := TStringList.Create;
  FAnonEnums.Sorted := True;
  FAnonEnums.Duplicates := dupIgnore;
  FClsMethods := TStringList.Create;
  FDetected := False;
  FPpudump := '';
end;


destructor TPpuGenImpl.Destroy;
var
  i: integer;
begin
  for i := 0 to FCache.Count - 1 do
    FCache.Objects[i].Free;
  FCache.Free;
  FIndex.Free;
  FScanned.Free;
  FOmitted.Free;
  FLocalTypes.Free;
  FNeededAliases.Free;
  FAnonEnums.Free;
  FClsMethods.Free;
  inherited Destroy;
end;


procedure TPpuGenImpl.BumpOmit(const aReason: string);
var
  lIdx: integer;
begin
  lIdx := FOmitted.IndexOfName(aReason);
  if lIdx >= 0 then
    FOmitted.ValueFromIndex[lIdx] :=
      IntToStr(StrToIntDef(FOmitted.ValueFromIndex[lIdx], 0) + 1)
  else
    FOmitted.Add(aReason + '=1');
end;


procedure TPpuGenImpl.IndexDir(const aDir: string);
var
  lRec: TSearchRec;
  lSub: TStringList;
  i: integer;
  lName: string;
begin
  if (aDir = '') or (not DirectoryExists(aDir)) then
    Exit;
  lSub := TStringList.Create;
  try
    if FindFirst(IncludeTrailingPathDelimiter(aDir) + '*', faAnyFile, lRec) = 0 then
    begin
      repeat
        if (lRec.Name = '.') or (lRec.Name = '..') then
          Continue;
        if (lRec.Attr and faDirectory) <> 0 then
          lSub.Add(IncludeTrailingPathDelimiter(aDir) + lRec.Name)
        else if SameText(ExtractFileExt(lRec.Name), '.ppu') then
        begin
          lName := LowerCase(ChangeFileExt(lRec.Name, ''));
          if FIndex.IndexOfName(lName) < 0 then
            FIndex.Add(lName + '=' + IncludeTrailingPathDelimiter(aDir) + lRec.Name);
        end;
      until FindNext(lRec) <> 0;
      FindClose(lRec);
    end;
    for i := 0 to lSub.Count - 1 do
      IndexDir(lSub[i]);
  finally
    lSub.Free;
  end;
end;


procedure TPpuGenImpl.AddSearchDir(const aDir: string);
var
  lKey: string;
begin
  if aDir = '' then
    Exit;
  lKey := ExpandFileName(aDir);
  if FScanned.IndexOf(lKey) >= 0 then
    Exit;   // already indexed this tree — skip the recursive re-scan
  FScanned.Add(lKey);
  IndexDir(aDir);
end;


function TPpuGenImpl.PpudumpAvailable: boolean;
begin
  if not FDetected then
  begin
    FPpudump := FindPpudump;
    FDetected := True;
  end;
  Result := FPpudump <> '';
end;


function TPpuGenImpl.LocatePpu(const aName: string): string;
var
  lIdx: integer;
begin
  Result := '';
  lIdx := FIndex.IndexOfName(LowerCase(aName));
  if lIdx >= 0 then
    Result := FIndex.ValueFromIndex[lIdx];
end;


function TPpuGenImpl.GetPpu(const aName: string): TFpSonarPpu;
var
  lKey, lPath, lOut: string;
  lIdx: integer;
  lData: TJSONData;
begin
  lKey := LowerCase(aName);
  lIdx := FCache.IndexOf(lKey);
  if lIdx >= 0 then
    Exit(TFpSonarPpu(FCache.Objects[lIdx]));
  Result := nil;
  lPath := LocatePpu(aName);
  if (lPath = '') or (not PpudumpAvailable) then
  begin
    FCache.AddObject(lKey, nil);
    Exit;
  end;
  if not RunPpudump(FPpudump, lPath, lOut) then
  begin
    FCache.AddObject(lKey, nil);
    Exit;
  end;
  lData := nil;
  try
    lData := GetJSON(lOut);
  except
    lData := nil;
  end;
  if lData = nil then
  try
    lData := GetJSON(RepairPpudumpJson(lOut));
  except
    lData := nil;
  end;
  if lData = nil then
  begin
    FCache.AddObject(lKey, nil);
    Exit;
  end;
  Result := TFpSonarPpu.Create(aName, lPath, lData);
  FCache.AddObject(lKey, Result);
end;


function TPpuGenImpl.InterfaceCrc(const aName: string): string;
var
  lPpu: TFpSonarPpu;
begin
  Result := '';
  lPpu := GetPpu(aName);
  if lPpu <> nil then
    Result := lPpu.Crc;
end;


function TPpuGenImpl.LocalTypeById(aId: integer): TJSONObject;
var
  i: integer;
begin
  Result := nil;
  for i := FLocalTypes.Count - 1 downto 0 do
    if FLocalTypes[i] = IntToStr(aId) then
      Exit(TJSONObject(FLocalTypes.Objects[i]));
end;


procedure TPpuGenImpl.RefName(aRef: TJSONObject; aUnit: TFpSonarPpu;
  out aName, aProv: string; out aAnonDecl: TJSONObject);
var
  lId, lUidx: integer;
  lDecl: TJSONObject;
  lHomeName, lNm, lLow, lCased, lBase: string;
  lHomePpu: TFpSonarPpu;
begin
  aName := '';
  aProv := '';
  aAnonDecl := nil;
  if (aRef = nil) or (aRef.Find('Id') = nil) then
    Exit;
  lId := aRef.Get('Id', -1);
  if aRef.Find('Unit') = nil then
  begin
    lDecl := aUnit.ById(lId);
    if lDecl = nil then
      lDecl := LocalTypeById(lId);
    if lDecl = nil then
      Exit;
    lNm := lDecl.Get('Name', '');
    if IsGenericName(lNm) then
      Exit;
    if (lNm <> '') and (lNm[1] <> '$') then
    begin
      if (FCurAllowed <> nil) and (FCurAllowed.IndexOf(lNm) < 0) then
        Exit;
      aName := lNm;
      Exit;
    end;
    aName := '#anon#';
    aAnonDecl := lDecl;
    Exit;
  end;
  lUidx := aRef.Get('Unit', -1);
  lHomeName := aUnit.UnitAt(lUidx);
  if lHomeName = '' then
    Exit;
  lHomePpu := GetPpu(lHomeName);
  lNm := '';
  if lHomePpu <> nil then
  begin
    lDecl := lHomePpu.ById(lId);
    if lDecl <> nil then
      lNm := lDecl.Get('Name', '');
  end;
  if lNm = '' then
    Exit;
  if IsGenericName(lNm) then
    Exit;
  lLow := LowerCase(lNm);
  if IsSpecial(lNm) or (lNm[1] = '$') then
  begin
    aName := lNm;
    aProv := '$';
    Exit;
  end;
  if IsBuiltin(lNm) then
  begin
    aName := lNm;
    Exit;
  end;
  if RtlLocalAlias(lNm, lCased, lBase) then
  begin
    FNeededAliases.Add(lLow);
    aName := lCased;
    Exit;
  end;
  aProv := RtlKnownUnit(lNm);
  if aProv <> '' then
  begin
    aName := lNm;
    Exit;
  end;
  if (not IsSyntheticRtl(lHomeName)) and (not IsCascadeStop(lHomeName)) then
  begin
    aName := lNm;
    aProv := lHomeName;
    Exit;
  end;
  aName := '';
  aProv := '';
end;


function TPpuGenImpl.Deref(aRef: TJSONObject; aUnit: TFpSonarPpu;
  out aHome: TFpSonarPpu): TJSONObject;
var
  lId, lUidx: integer;
begin
  Result := nil;
  aHome := nil;
  if (aRef = nil) or (aRef.Find('Id') = nil) then
    Exit;
  lId := aRef.Get('Id', -1);
  if aRef.Find('Unit') = nil then
  begin
    aHome := aUnit;
    Result := aUnit.ById(lId);
    Exit;
  end;
  lUidx := aRef.Get('Unit', -1);
  aHome := GetPpu(aUnit.UnitAt(lUidx));
  if aHome = nil then
    Exit;
  Result := aHome.ById(lId);
end;


// UTF8String IS AnsiString; distinct string overloads tie for a string literal.
function NormString(const aName: string): string;
var
  lLow: string;
begin
  lLow := LowerCase(aName);
  if (lLow = 'utf8string') or (lLow = 'rawbytestring') or (lLow = 'shortstring') then
    Result := 'AnsiString'
  else if lLow = 'widestring' then
    Result := 'UnicodeString'
  else
    Result := aName;
end;


function TPpuGenImpl.NearestAncestor(aRef: TJSONObject;
  aUnit: TFpSonarPpu; aUses: TStringList; aDepth: integer): string;
var
  lDirect, lNm, lProv: string;
  lDecl, lParent, lAnon: TJSONObject;
  lHome: TFpSonarPpu;
begin
  Result := '';
  if (aDepth > 12) or (aRef = nil) then
    Exit;
  lDirect := Resolve(aRef, aUnit, aUses, aDepth);
  if lDirect <> '' then
    Exit(lDirect);
  lDecl := Deref(aRef, aUnit, lHome);
  if (lDecl = nil) or (lDecl.Get('Type', '') <> 'obj') then
    Exit;
  lParent := lDecl.Find('Ancestor') as TJSONObject;
  if lParent = nil then
  begin
    RefName(aRef, aUnit, lNm, lProv, lAnon);
    if lNm <> '' then
      Result := 'TObject';
    Exit;
  end;
  Result := NearestAncestor(lParent, lHome, aUses, aDepth + 1);
end;


function TPpuGenImpl.Resolve(aRef: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList; aDepth: integer): string;
var
  lNm, lProv: string;
  lAnon: TJSONObject;
begin
  Result := '';
  if aDepth > 6 then
    Exit;
  RefName(aRef, aUnit, lNm, lProv, lAnon);
  if lNm = '' then
    Exit;
  if lNm = '#anon#' then
    Exit(InlineAnon(lAnon, aUnit, aUses, aDepth));
  if IsSpecial(lNm) then
  begin
    if lNm = '$char_pointer' then
      Exit('PChar');
    Exit('');
  end;
  if lNm[1] = '$' then
    Exit('');
  if (lProv <> '') and (lProv <> '$') then
    if aUses.IndexOf(lProv) < 0 then
      aUses.Add(lProv);
  Result := NormString(lNm);
end;


function TPpuGenImpl.InlineAnon(aDecl: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList; aDepth: integer): string;
var
  lT, lInner, lEl, lName: string;
  lEls: TStringList;
  lElsArr: TJSONArray;
  lEid, i, lLo, lHi: integer;
  lElRef, lElDecl: TJSONObject;
  lHome: TFpSonarPpu;
begin
  Result := '';
  if aDecl = nil then
    Exit;
  lT := aDecl.Get('Type', '');
  if lT = 'classref' then
  begin
    lInner := Resolve(aDecl.Find('Ref') as TJSONObject, aUnit, aUses, aDepth + 1);
    if lInner <> '' then
      Result := 'class of ' + lInner;
  end
  else if lT = 'ptr' then
  begin
    lInner := Resolve(aDecl.Find('Ptr') as TJSONObject, aUnit, aUses, aDepth + 1);
    if lInner <> '' then
      Result := '^' + lInner;
  end
  else if lT = 'enum' then
  begin
    lElsArr := aDecl.Find('Elements') as TJSONArray;
    if lElsArr = nil then
      Exit;
    lEls := TStringList.Create;
    try
      for i := 0 to lElsArr.Count - 1 do
      begin
        lName := TJSONObject(lElsArr.Items[i]).Get('Name', '');
        if lName <> '' then
          lEls.Add(lName);
      end;
      if lEls.Count = 0 then
        Exit;
      if aDecl.Find('Id') = nil then
        Exit('(' + JoinSL(lEls, ', ') + ')');
      lEid := aDecl.Get('Id', -1);
      lName := 'TPpuStubEnum' + IntToStr(lEid);
      FAnonEnums.Values[IntToStr(lEid)] :=
        '  ' + lName + ' = (' + JoinSL(lEls, ', ') + ');';
      Result := lName;
    finally
      lEls.Free;
    end;
  end
  else if lT = 'set' then
  begin
    lEl := Resolve(aDecl.Find('ElType') as TJSONObject, aUnit, aUses, aDepth + 1);
    if lEl <> '' then
      Result := 'set of ' + lEl;
  end
  else if lT = 'array' then
  begin
    lElRef := aDecl.Find('ElType') as TJSONObject;
    lElDecl := Deref(lElRef, aUnit, lHome);
    if (lElDecl <> nil) and (lElDecl.Get('Name', '') = 'TVarRec') then
      Exit('array of const');
    lEl := Resolve(lElRef, aUnit, aUses, aDepth + 1);
    if lEl = '' then
      Exit;
    if (aDecl.Find('Low') <> nil) and (aDecl.Find('High') <> nil)
      and (aDecl.Find('RangeType') = nil) then
    begin
      lLo := aDecl.Get('Low', 0);
      lHi := aDecl.Get('High', -1);
      Exit(Format('array[%d..%d] of %s', [lLo, lHi, lEl]));
    end;
    Result := 'array of ' + lEl;
  end
  else if lT = 'proctype' then
    Result := ProcSig(aDecl, aUnit, aUses, aDepth + 1, True);
end;


function TPpuGenImpl.RetOf(aDecl: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList; aDepth: integer): string;
var
  lRt, lAnon: TJSONObject;
  lNm, lProv: string;
begin
  lRt := aDecl.Find('RetType') as TJSONObject;
  if lRt = nil then
    Exit('$void');
  RefName(lRt, aUnit, lNm, lProv, lAnon);
  if lNm = '$void' then
    Exit('$void');
  Result := Resolve(lRt, aUnit, aUses, aDepth);
end;


function TPpuGenImpl.ProcSig(aDecl: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList; aDepth: integer; aAnon: boolean): string;
var
  lOpts: TJSONArray;
  lParamsStr, lRet: string;
  lOk, lIsFunc: boolean;

  function HasOpt(const aOpt: string): boolean;
  var
    k: integer;
  begin
    Result := False;
    if lOpts = nil then Exit;
    for k := 0 to lOpts.Count - 1 do
      if SameText(lOpts.Items[k].AsString, aOpt) then Exit(True);
  end;

begin
  Result := '';
  lOpts := aDecl.Find('Options') as TJSONArray;
  lParamsStr := Params(aDecl.Find('Params') as TJSONArray, aUnit, aUses, aDepth, lOk);
  if not lOk then
    Exit;
  lRet := RetOf(aDecl, aUnit, aUses, aDepth);
  lIsFunc := HasOpt('function');
  if (lRet = '$void') or ((lRet = '') and (not lIsFunc)) then
    Result := 'procedure' + lParamsStr
  else
  begin
    if (lRet = '') or (lRet = '$void') then
      Exit('');
    Result := 'function' + lParamsStr + ': ' + lRet;
  end;
  if aAnon and (aDecl.Find('MethodPtr') <> nil) then
    Result := Result + ' of object';
end;


function TPpuGenImpl.RenderDefault(aDflt: TJSONObject;
  const aTy: string): string;
var
  lVt, lTyLow: string;
  lVal: TJSONData;
begin
  Result := '';
  if aDflt = nil then
    Exit;
  lVt := aDflt.Get('ValType', '');
  lVal := aDflt.Find('Value');
  lTyLow := LowerCase(aTy);
  if lTyLow = 'boolean' then
  begin
    if ((lVt = 'int') or (lVt = 'bool')) and (lVal <> nil)
      and (lVal.JSONType = jtNumber) then
    begin
      if lVal.AsInteger <> 0 then Result := 'True'
      else
        Result := 'False';
    end;
    Exit;
  end;
  if (lTyLow = 'char') or (lTyLow = 'ansichar') or (lTyLow = 'widechar') then
    Exit;
  if (lVt = 'int') and (lVal <> nil) and (lVal.JSONType = jtNumber) then
  begin
    if IsBuiltin(lTyLow) and (lTyLow <> 'boolean') then
      Result := IntToStr(lVal.AsInt64);
    Exit;
  end;
  if lVt = 'bool' then
  begin
    if (lVal <> nil) and lVal.AsBoolean then Result := 'True'
    else
      Result := 'False';
    Exit;
  end;
  if ((lVt = 'string') or (lVt = 'str')) and (lVal <> nil)
    and (lVal.JSONType = jtString) then
    Exit(PasStringLiteral(lVal.AsString));
  if (lVt = 'set') and (lVal <> nil) and (lVal.JSONType = jtString) then
  begin
    if StringReplace(lVal.AsString, '0', '', [rfReplaceAll]) = '' then
      Result := '[]';
    Exit;
  end;
  if (lVt = 'pointer') or (lVt = 'nil') then
    Result := 'nil';
end;


function TPpuGenImpl.Params(aPlist: TJSONArray; aUnit: TFpSonarPpu;
  aUses: TStringList; aDepth: integer; out aOk: boolean): string;
var
  i, lId, lSavedCount, lKeepFrom: integer;
  lP, lVtRef, lAnon: TJSONObject;
  lSpez, lPrefix, lPname, lNm, lProv, lTy, lDflt, lOut: string;
  lPrefixes, lPnames, lTypes, lDflts: TStringList;
begin
  aOk := True;
  Result := '';
  if (aPlist = nil) or (aPlist.Count = 0) then
    Exit;
  lSavedCount := FLocalTypes.Count;
  lPrefixes := TStringList.Create;
  lPnames := TStringList.Create;
  lTypes := TStringList.Create;   // '' marks a formal (untyped) param
  lDflts := TStringList.Create;
  try
    for i := 0 to aPlist.Count - 1 do
    begin
      lP := aPlist.Items[i] as TJSONObject;
      if (lP.Get('Type', '') <> 'param') and (lP.Find('Id') <> nil) then
      begin
        lId := lP.Get('Id', -1);
        if lId >= 0 then
          FLocalTypes.AddObject(IntToStr(lId), lP);
      end;
    end;
    for i := 0 to aPlist.Count - 1 do
    begin
      lP := aPlist.Items[i] as TJSONObject;
      if lP.Get('Type', '') <> 'param' then
        Continue;
      lSpez := LowerCase(lP.Get('Spez', ''));
      if lSpez = 'const' then lPrefix := 'const '
      else if lSpez = 'var' then lPrefix := 'var '
      else if lSpez = 'out' then lPrefix := 'out '
      else if lSpez = 'constref' then lPrefix := 'constref '
      else
        lPrefix := '';
      lPname := lP.Get('Name', '');
      if lPname = '' then lPname := 'A';
      lVtRef := lP.Find('VarType') as TJSONObject;
      lNm := '';
      if lVtRef <> nil then
        RefName(lVtRef, aUnit, lNm, lProv, lAnon);
      if lNm = '$formal' then
      begin
        if lPrefix = '' then lPrefix := 'const ';
        lPrefixes.Add(lPrefix);
        lPnames.Add(lPname);
        lTypes.Add('');
        lDflts.Add('');
        Continue;
      end;
      if lVtRef <> nil then
        lTy := Resolve(lVtRef, aUnit, aUses, aDepth)
      else
        lTy := '';
      if lTy = '' then
      begin
        aOk := False;
        Exit('');
      end;
      lDflt := RenderDefault(lP.Find('Default') as TJSONObject, lTy);
      if (lPrefix <> '') and (lPrefix <> 'const ') then
        lDflt := '';
      lPrefixes.Add(lPrefix);
      lPnames.Add(lPname);
      lTypes.Add(lTy);
      lDflts.Add(lDflt);
    end;
    lKeepFrom := lPrefixes.Count;
    for i := lPrefixes.Count - 1 downto 0 do
    begin
      if lDflts[i] = '' then
        Break;
      lKeepFrom := i;
    end;
    lOut := '';
    for i := 0 to lPrefixes.Count - 1 do
    begin
      if lOut <> '' then
        lOut := lOut + '; ';
      if lTypes[i] = '' then
        lOut := lOut + lPrefixes[i] + lPnames[i]
      else
      begin
        lOut := lOut + lPrefixes[i] + lPnames[i] + ': ' + lTypes[i];
        if (i >= lKeepFrom) and (lDflts[i] <> '') then
          lOut := lOut + ' = ' + lDflts[i];
      end;
    end;
    Result := '(' + lOut + ')';
  finally
    while FLocalTypes.Count > lSavedCount do
      FLocalTypes.Delete(FLocalTypes.Count - 1);
    lPrefixes.Free;
    lPnames.Free;
    lTypes.Free;
    lDflts.Free;
  end;
end;


function TPpuGenImpl.EmitEnum(aE: TJSONObject): string;
var
  lElsArr: TJSONArray;
  lEls: TStringList;
  i: integer;
  lName: string;
begin
  Result := '';
  lElsArr := aE.Find('Elements') as TJSONArray;
  if lElsArr = nil then
    Exit;
  lEls := TStringList.Create;
  try
    for i := 0 to lElsArr.Count - 1 do
    begin
      lName := TJSONObject(lElsArr.Items[i]).Get('Name', '');
      if lName <> '' then
        lEls.Add(lName);
    end;
    if lEls.Count = 0 then
      Exit;
    for i := 0 to lEls.Count - 1 do
      if IsBuiltin(lEls[i]) then
      begin
        BumpOmit('enum-shadows-builtin:' + aE.Get('Name', ''));
        Exit;
      end;
    Result := '  ' + aE.Get('Name', '') + ' = (' + JoinSL(lEls, ', ') + ');';
  finally
    lEls.Free;
  end;
end;


function TPpuGenImpl.EmitAlias(aE: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList): string;
var
  lBase: string;
begin
  lBase := Resolve(aE.Find('Ref') as TJSONObject, aUnit, aUses, 0);
  if lBase = '' then
  begin
    BumpOmit('alias:' + aE.Get('Name', ''));
    Exit('');
  end;
  Result := '  ' + aE.Get('Name', '') + ' = ' + lBase + ';';
end;


function TPpuGenImpl.EmitSet(aE: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList): string;
var
  lEl: string;
begin
  lEl := Resolve(aE.Find('ElType') as TJSONObject, aUnit, aUses, 0);
  if lEl = '' then
    Exit('');
  Result := '  ' + aE.Get('Name', '') + ' = set of ' + lEl + ';';
end;


function TPpuGenImpl.EmitConst(aE: TJSONObject): string;
var
  lVt: string;
  lVal: TJSONData;
begin
  Result := '';
  lVt := aE.Get('ValType', '');
  lVal := aE.Find('Value');
  if (lVt = 'int') and (lVal <> nil) and (lVal.JSONType = jtNumber) then
    Result := '  ' + aE.Get('Name', '') + ' = ' + IntToStr(lVal.AsInt64) + ';'
  else if ((lVt = 'string') or (lVt = 'str')) and (lVal <> nil)
    and (lVal.JSONType = jtString) then
    Result := '  ' + aE.Get('Name', '') + ' = ' + PasStringLiteral(lVal.AsString) + ';'
  else if lVt = 'bool' then
  begin
    if (lVal <> nil) and lVal.AsBoolean then
      Result := '  ' + aE.Get('Name', '') + ' = True;'
    else
      Result := '  ' + aE.Get('Name', '') + ' = False;';
  end;
end;


function TPpuGenImpl.EmitProctype(aE: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList): string;
var
  lSig: string;
begin
  lSig := ProcSig(aE, aUnit, aUses, 0, True);
  if lSig <> '' then
    Result := '  ' + aE.Get('Name', '') + ' = ' + lSig + ';'
  else
    Result := '';
end;


function TPpuGenImpl.EmitAnonAlias(aE: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList): string;
var
  lExpr: string;
begin
  lExpr := InlineAnon(aE, aUnit, aUses, 0);
  if lExpr <> '' then
    Result := '  ' + aE.Get('Name', '') + ' = ' + lExpr + ';'
  else
    Result := '';
end;


function TPpuGenImpl.EmitRecord(aE: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList): string;
var
  lFields: TJSONArray;
  i: integer;
  lF: TJSONObject;
  lNm, lTy: string;
  lLines: TStringList;
begin
  lLines := TStringList.Create;
  try
    lFields := aE.Find('Fields') as TJSONArray;
    if lFields <> nil then
      for i := 0 to lFields.Count - 1 do
      begin
        lF := lFields.Items[i] as TJSONObject;
        if lF.Get('Type', '') <> 'field' then
          Continue;
        lNm := lF.Get('Name', '');
        if (lNm = '') or (lNm[1] = '$') or IsGenericName(lNm) then
          Continue;
        lTy := Resolve(lF.Find('VarType') as TJSONObject, aUnit, aUses, 0);
        if lTy = '' then
          Continue;
        if lTy = aE.Get('Name', '') then
        begin
          BumpOmit('record-self-field:' + aE.Get('Name', '') + '.' + lNm);
          Continue;
        end;
        lLines.Add('    ' + lNm + ': ' + lTy + ';');
      end;
    if lLines.Count > 0 then
      Result := '  ' + aE.Get('Name', '') + ' = record'#10 +
        TrimRight(lLines.Text) + #10'  end;'
    else
      Result := '  ' + aE.Get('Name', '') + ' = record end;';
  finally
    lLines.Free;
  end;
end;


// A normalized signature key for overload de-duplication (drops param names +
// modifiers; folds UnicodeString->AnsiString and OleVariant->Variant).
function OverloadKey(const aSig: string): string;
var
  lS, lOut: string;
  i, lLen, lStart, lJ: integer;
  lPrevWord: boolean;
begin
  lS := LowerCase(aSig);
  lOut := '';
  lLen := Length(lS);
  i := 1;
  lPrevWord := False;
  while i <= lLen do
  begin
    if (not lPrevWord) and (lS[i] in ['a'..'z', '_']) then
    begin
      lStart := i;
      while (i <= lLen) and IsWordChar(lS[i]) do
        Inc(i);
      lJ := i;
      while (lJ <= lLen) and (lS[lJ] = ' ') do
        Inc(lJ);
      if (lJ <= lLen) and (lS[lJ] = ':') then
      begin
        lOut := lOut + ':';
        i := lJ + 1;
        lPrevWord := False;
      end
      else
      begin
        lOut := lOut + Copy(lS, lStart, i - lStart);
        lPrevWord := True;
      end;
    end
    else
    begin
      lOut := lOut + lS[i];
      lPrevWord := IsWordChar(lS[i]);
      Inc(i);
    end;
  end;
  lOut := StringReplace(lOut, 'const ', '', [rfReplaceAll]);
  lOut := StringReplace(lOut, 'constref ', '', [rfReplaceAll]);
  lOut := StringReplace(lOut, 'var ', '', [rfReplaceAll]);
  lOut := StringReplace(lOut, 'out ', '', [rfReplaceAll]);
  lOut := StringReplace(lOut, 'unicodestring', 'ansistring', [rfReplaceAll]);
  lOut := StringReplace(lOut, 'olevariant', 'variant', [rfReplaceAll]);
  lOut := StringReplace(lOut, ' ', '', [rfReplaceAll]);
  lOut := StringReplace(lOut, #9, '', [rfReplaceAll]);
  Result := lOut;
end;


function SanitizeIdent(const aStr: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(aStr) do
    if IsWordChar(aStr[i]) then
      Result := Result + aStr[i];
end;


function TPpuGenImpl.MethodSig(aF: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList): string;
var
  lOpts: TJSONArray;
  lNm, lPrefix, lParamsStr, lRet: string;
  lOk, lIsClass: boolean;

  function HasOpt(const aOpt: string): boolean;
  var
    k: integer;
  begin
    Result := False;
    if lOpts = nil then Exit;
    for k := 0 to lOpts.Count - 1 do
      if SameText(lOpts.Items[k].AsString, aOpt) then Exit(True);
  end;

begin
  Result := '';
  lOpts := aF.Find('Options') as TJSONArray;
  lNm := aF.Get('Name', '');
  lIsClass := HasOpt('classmethod') or HasOpt('static');
  lParamsStr := Params(aF.Find('Params') as TJSONArray, aUnit, aUses, 0, lOk);
  if not lOk then
    Exit('');
  if lIsClass then lPrefix := 'class '
  else
    lPrefix := '';
  if HasOpt('constructor') then
    Exit(lPrefix + 'constructor ' + lNm + lParamsStr + ';');
  lRet := RetOf(aF, aUnit, aUses, 0);
  if (lRet = '$void') or ((lRet = '') and (not HasOpt('function'))) then
    Exit(lPrefix + 'procedure ' + lNm + lParamsStr + ';');
  if (lRet = '') or (lRet = '$void') then
    Exit('');
  Result := lPrefix + 'function ' + lNm + lParamsStr + ': ' + lRet + ';';
end;


// Strips " = <default>" segments and param modifiers from a "(...)" param string.
function IndexBrackets(const aPsig: string): string;
var
  lS: string;
  lEq, lStop: integer;
begin
  lS := aPsig;
  lS := StringReplace(lS, 'const ', '', [rfReplaceAll]);
  lS := StringReplace(lS, 'constref ', '', [rfReplaceAll]);
  lS := StringReplace(lS, 'var ', '', [rfReplaceAll]);
  lS := StringReplace(lS, 'out ', '', [rfReplaceAll]);
  lEq := Pos(' = ', lS);
  while lEq > 0 do
  begin
    lStop := lEq + 3;
    while (lStop <= Length(lS)) and (not (lS[lStop] in [';', ')'])) do
      Inc(lStop);
    Delete(lS, lEq, lStop - lEq);
    lEq := Pos(' = ', lS);
  end;
  if (Length(lS) >= 2) and (lS[1] = '(') then
    lS := '[' + Copy(lS, 2, Length(lS) - 2) + ']';
  Result := lS;
end;


procedure TPpuGenImpl.EmitMember(aF: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList; aPub, aPriv, aSeen, aIndexedProps: TStringList;
  const aCls: string; aInHelper: boolean);
var
  lVis, lNm, lT, lPt, lBack, lFld, lPsig, lPidx, lDfltProp, lSig, lKey: string;
  lIdxParams, lOpts: TJSONArray;
  lOk: boolean;
  lUsesBefore: TStringList;

  function HasOpt(const aOpt: string): boolean;
  var
    k: integer;
  begin
    Result := False;
    if lOpts = nil then Exit;
    for k := 0 to lOpts.Count - 1 do
      if SameText(lOpts.Items[k].AsString, aOpt) then Exit(True);
  end;

begin
  lVis := LowerCase(aF.Get('Visibility', 'public'));
  if (lVis = 'private') or (lVis = 'strictprivate') then
    Exit;
  lNm := aF.Get('Name', '');
  if IsGenericName(lNm) then
  begin
    BumpOmit('generic-member:' + aCls + '.' + lNm);
    Exit;
  end;
  lT := aF.Get('Type', '');
  lOpts := aF.Find('Options') as TJSONArray;
  if lT = 'prop' then
  begin
    if (lNm = '') or (lNm[1] = '$') then
      Exit;
    if aSeen.IndexOf(LowerCase(lNm)) >= 0 then
      Exit;
    lPt := Resolve(aF.Find('PropType') as TJSONObject, aUnit, aUses, 0);
    if lPt = '' then
    begin
      BumpOmit('prop:' + aCls + '.' + lNm);
      Exit;
    end;
    lBack := 'GetPpuStub_' + SanitizeIdent(aCls) + '_' + SanitizeIdent(lNm);
    lIdxParams := aF.Find('Params') as TJSONArray;
    if (lIdxParams <> nil) and (lIdxParams.Count > 0) then
    begin
      lPsig := Params(lIdxParams, aUnit, aUses, 0, lOk);
      if not lOk then
      begin
        BumpOmit('prop-index:' + aCls + '.' + lNm);
        Exit;
      end;
      aSeen.Add(LowerCase(lNm));
      aIndexedProps.Add(LowerCase(lNm));
      aPriv.Add('    function ' + lBack + lPsig + ': ' + lPt + ';');
      lPidx := IndexBrackets(lPsig);
      if HasOpt('default') then
        lDfltProp := ' default;'
      else
        lDfltProp := '';
      aPub.Add('    property ' + lNm + lPidx + ': ' + lPt + ' read ' +
        lBack + ';' + lDfltProp);
      Exit;
    end;
    if aIndexedProps.IndexOf(LowerCase(lNm)) >= 0 then
      Exit;
    aSeen.Add(LowerCase(lNm));
    if aInHelper then
    begin
      aPriv.Add('    function ' + lBack + ': ' + lPt + ';');
      aPub.Add('    property ' + lNm + ': ' + lPt + ' read ' + lBack + ';');
      Exit;
    end;
    lFld := 'FPpuStub_' + SanitizeIdent(aCls) + '_' + SanitizeIdent(lNm);
    aPriv.Add('    ' + lFld + ': ' + lPt + ';');
    aPub.Add('    property ' + lNm + ': ' + lPt + ' read ' + lFld +
      ' write ' + lFld + ';');
    Exit;
  end;
  if lT = 'proc' then
  begin
    if (lNm = '') or (lNm[1] = '$') then
      Exit;
    if HasOpt('destructor') or SameText(lNm, 'destroy') then
      Exit;
    lUsesBefore := TStringList.Create;
    try
      lUsesBefore.Assign(aUses);
      lSig := MethodSig(aF, aUnit, aUses);
      if lSig = '' then
      begin
        aUses.Assign(lUsesBefore);
        BumpOmit('method:' + aCls + '.' + lNm);
        Exit;
      end;
    finally
      lUsesBefore.Free;
    end;
    lKey := OverloadKey(lSig);
    if FClsMethods.IndexOf(lKey) >= 0 then
      Exit;
    FClsMethods.Add(lKey);
    if aInHelper and (Copy(TrimLeft(lSig), 1, 6) = 'class ') then
      lSig := Copy(lSig, 1, Length(lSig) - 1) + ' static;';
    aPub.Add('    ' + lSig + ' overload;');
    Exit;
  end;
  // data fields / vars: skipped (not API surface).
end;


function TPpuGenImpl.HelperKind(aHpRef: TJSONObject;
  aUnit: TFpSonarPpu): string;
var
  lDecl: TJSONObject;
  lHome: TFpSonarPpu;
  lT: string;
begin
  Result := 'type';
  lDecl := Deref(aHpRef, aUnit, lHome);
  if lDecl = nil then
    Exit;
  lT := lDecl.Get('Type', '');
  if (lT = 'obj') and ((lDecl.Find('ObjType') = nil)
    or (lDecl.Get('ObjType', '') = 'class')) then
    Result := 'class'
  else if lT = 'rec' then
    Result := 'record';
end;


function TPpuGenImpl.EmitClass(aE: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList; aIndexedProps: TStringList): string;
var
  lObjType, lAnc, lAn, lPrivS, lPubS: string;
  lAref: TJSONObject;
  lFields: TJSONArray;
  i: integer;
  lSeen, lPub, lPriv: TStringList;
begin
  Result := '';
  lObjType := aE.Get('ObjType', '');
  if (lObjType <> '') and (lObjType <> 'class') then
  begin
    BumpOmit('non-class-obj:' + aE.Get('Name', ''));
    Exit;
  end;
  lAnc := 'TObject';
  lAref := aE.Find('Ancestor') as TJSONObject;
  if lAref <> nil then
  begin
    lAn := NearestAncestor(lAref, aUnit, aUses, 0);
    if lAn <> '' then
      lAnc := lAn
    else
    begin
      BumpOmit('class-ancestor:' + aE.Get('Name', ''));
      Exit;
    end;
  end;
  lSeen := TStringList.Create;
  lPub := TStringList.Create;
  lPriv := TStringList.Create;
  FClsMethods.Clear;
  try
    lFields := aE.Find('Fields') as TJSONArray;
    if lFields <> nil then
      for i := 0 to lFields.Count - 1 do
        EmitMember(lFields.Items[i] as TJSONObject, aUnit, aUses, lPub, lPriv,
          lSeen, aIndexedProps, aE.Get('Name', ''), False);
    if lPriv.Count > 0 then
      lPrivS := '  private'#10 + TrimRight(lPriv.Text) + #10
    else
      lPrivS := '';
    if lPub.Count > 0 then
      lPubS := '  public'#10 + TrimRight(lPub.Text) + #10
    else
      lPubS := '';
    Result := '  ' + aE.Get('Name', '') + ' = class(' + lAnc + ')'#10 +
      lPrivS + lPubS + '  end;';
  finally
    lSeen.Free;
    lPub.Free;
    lPriv.Free;
  end;
end;


function TPpuGenImpl.EmitHelper(aE: TJSONObject; aUnit: TFpSonarPpu;
  aUses: TStringList; aIndexedProps: TStringList): string;
var
  lHp, lAref: TJSONObject;
  lFortype, lKw, lAnc, lAn, lPrivS, lPubS: string;
  lFields: TJSONArray;
  i: integer;
  lSeen, lPub, lPriv: TStringList;
begin
  Result := '';
  lHp := aE.Find('HelperParent') as TJSONObject;
  if lHp = nil then
  begin
    BumpOmit('helper-fortype:' + aE.Get('Name', ''));
    Exit;
  end;
  lFortype := Resolve(lHp, aUnit, aUses, 0);
  if lFortype = '' then
  begin
    BumpOmit('helper-fortype:' + aE.Get('Name', ''));
    Exit;
  end;
  lKw := HelperKind(lHp, aUnit);
  lAnc := '';
  lAref := aE.Find('Ancestor') as TJSONObject;
  if lAref <> nil then
  begin
    lAn := Resolve(lAref, aUnit, aUses, 0);
    if lAn <> '' then
      lAnc := '(' + lAn + ')';
  end;
  lSeen := TStringList.Create;
  lPub := TStringList.Create;
  lPriv := TStringList.Create;
  FClsMethods.Clear;
  try
    lFields := aE.Find('Fields') as TJSONArray;
    if lFields <> nil then
      for i := 0 to lFields.Count - 1 do
        EmitMember(lFields.Items[i] as TJSONObject, aUnit, aUses, lPub, lPriv,
          lSeen, aIndexedProps, aE.Get('Name', ''), True);
    if lPriv.Count > 0 then
      lPrivS := '  private'#10 + TrimRight(lPriv.Text) + #10
    else
      lPrivS := '';
    if lPub.Count > 0 then
      lPubS := '  public'#10 + TrimRight(lPub.Text) + #10
    else
      lPubS := '';
    Result := '  ' + aE.Get('Name', '') + ' = ' + lKw + ' helper' + lAnc +
      ' for ' + lFortype + #10 + lPrivS + lPubS + '  end;';
  finally
    lSeen.Free;
    lPub.Free;
    lPriv.Free;
  end;
end;


// Parses a "TFoo = ...(TBar)..." emitted body into name + local ancestor.
procedure ParseClassNameAnc(const aBody: string; out aName, aAnc: string);
var
  lEq, lOpen, lClose: integer;
begin
  aName := '';
  aAnc := '';
  lEq := Pos('=', aBody);
  if lEq = 0 then
  begin
    aName := Trim(aBody);
    Exit;
  end;
  aName := Trim(Copy(aBody, 1, lEq - 1));
  lOpen := PosEx('(', aBody, lEq);
  if lOpen > 0 then
  begin
    lClose := PosEx(')', aBody, lOpen);
    if lClose > lOpen then
      aAnc := Trim(Copy(aBody, lOpen + 1, lClose - lOpen - 1));
  end;
end;


// Orders class/helper bodies so a local ancestor precedes its subclasses.
procedure TopoClasses(aBodies: TStringList);
var
  lNames, lAncs, lDone, lOut: TStringList;
  lStack: TStringList;
  i: integer;
  lName, lAnc: string;

  procedure Visit(const aNodeName: string);
  var
    lIdx, lAncIdx: integer;
  begin
    lIdx := lNames.IndexOf(aNodeName);
    if lIdx < 0 then Exit;
    if lDone.IndexOf(aNodeName) >= 0 then Exit;
    if lStack.IndexOf(aNodeName) >= 0 then Exit;
    lStack.Add(aNodeName);
    lAncIdx := lNames.IndexOf(lAncs[lIdx]);
    if (lAncIdx >= 0) and (lDone.IndexOf(lAncs[lIdx]) < 0) then
      Visit(lAncs[lIdx]);
    lStack.Delete(lStack.IndexOf(aNodeName));
    lDone.Add(aNodeName);
    lOut.Add(aBodies[PtrInt(lNames.Objects[lIdx])]);
  end;

begin
  lNames := TStringList.Create;
  lAncs := TStringList.Create;
  lDone := TStringList.Create;
  lOut := TStringList.Create;
  lStack := TStringList.Create;
  try
    for i := 0 to aBodies.Count - 1 do
    begin
      ParseClassNameAnc(aBodies[i], lName, lAnc);
      lNames.AddObject(lName, TObject(PtrInt(i)));
      lAncs.Add(lAnc);
    end;
    for i := 0 to lNames.Count - 1 do
      Visit(lNames[i]);
    if lOut.Count = aBodies.Count then
      aBodies.Assign(lOut);
  finally
    lNames.Free;
    lAncs.Free;
    lDone.Free;
    lOut.Free;
    lStack.Free;
  end;
end;


function TPpuGenImpl.EmitUnitBody(aPpu: TFpSonarPpu;
  const aCanonical, aVersion: string; aEmittedNames: TStringList): string;
var
  lUses, lPre, lClasses, lHelpers, lConsts, lTail, lTailSeen: TStringList;
  lForwards, lUsesClean, lAliasLines, lAnonEnumLines, lIndexedProps: TStringList;
  lBody: TStringList;
  i: integer;
  lE: TJSONObject;
  lT, lEn, lS, lSig, lKey, lU, lMode, lUsesLine, lHeader, lCased, lBase: string;
  lNm, lAnc: string;

  procedure Keep(const aEn, aStr: string; aList: TStringList);
  begin
    if aStr <> '' then
    begin
      aEmittedNames.Add(aEn);
      aList.Add(aStr);
    end;
  end;

begin
  lUses := TStringList.Create;
  lPre := TStringList.Create;
  lClasses := TStringList.Create;
  lHelpers := TStringList.Create;
  lConsts := TStringList.Create;
  lTail := TStringList.Create;
  lTailSeen := TStringList.Create;
  lForwards := TStringList.Create;
  lUsesClean := TStringList.Create;
  lAliasLines := TStringList.Create;
  lAnonEnumLines := TStringList.Create;
  lIndexedProps := TStringList.Create;
  lBody := TStringList.Create;
  FNeededAliases.Clear;
  FAnonEnums.Clear;
  try
    for i := 0 to aPpu.Iface.Count - 1 do
    begin
      if aPpu.Iface.Items[i].JSONType <> jtObject then
        Continue;
      lE := TJSONObject(aPpu.Iface.Items[i]);
      lT := lE.Get('Type', '');
      lEn := lE.Get('Name', '');
      if (lEn = '') or (lEn[1] = '$') then
        Continue;
      if IsGenericName(lEn) then
      begin
        BumpOmit('generic:' + lEn);
        Continue;
      end;
      if lT = 'enum' then
        Keep(lEn, EmitEnum(lE), lPre)
      else if lT = 'type' then
        Keep(lEn, EmitAlias(lE, aPpu, lUses), lPre)
      else if lT = 'set' then
        Keep(lEn, EmitSet(lE, aPpu, lUses), lPre)
      else if lT = 'proctype' then
        Keep(lEn, EmitProctype(lE, aPpu, lUses), lPre)
      else if lT = 'rec' then
        Keep(lEn, EmitRecord(lE, aPpu, lUses), lPre)
      else if (lT = 'ptr') or (lT = 'classref') or (lT = 'array') then
        Keep(lEn, EmitAnonAlias(lE, aPpu, lUses), lPre)
      else if lT = 'obj' then
      begin
        if lE.Get('ObjType', '') = 'helper' then
          Keep(lEn, EmitHelper(lE, aPpu, lUses, lIndexedProps), lHelpers)
        else
          Keep(lEn, EmitClass(lE, aPpu, lUses, lIndexedProps), lClasses);
      end;
    end;

    for i := 0 to aPpu.Iface.Count - 1 do
    begin
      if aPpu.Iface.Items[i].JSONType <> jtObject then
        Continue;
      lE := TJSONObject(aPpu.Iface.Items[i]);
      lEn := lE.Get('Name', '');
      if (lEn = '') or (lEn[1] = '$') or IsGenericName(lEn) then
        Continue;
      lT := lE.Get('Type', '');
      if lT = 'const' then
      begin
        lS := EmitConst(lE);
        if lS <> '' then
          lConsts.Add(lS);
      end
      else if lT = 'proc' then
      begin
        lSig := MethodSig(lE, aPpu, lUses);
        if lSig <> '' then
        begin
          lKey := OverloadKey(lSig);
          if lTailSeen.IndexOf(lKey) >= 0 then
            Continue;
          lTailSeen.Add(lKey);
          lTail.Add(Copy(lSig, 1, Length(lSig) - 1) + ' overload;');
        end
        else
          BumpOmit('proc:' + lEn);
      end;
    end;

    for i := 0 to FNeededAliases.Count - 1 do
      if RtlLocalAlias(FNeededAliases[i], lCased, lBase) then
        if aEmittedNames.IndexOf(lCased) < 0 then
          lAliasLines.Add('  ' + lCased + ' = ' + lBase + ';');
    for i := 0 to FAnonEnums.Count - 1 do
      lAnonEnumLines.Add(FAnonEnums.ValueFromIndex[i]);

    TopoClasses(lClasses);
    TopoClasses(lHelpers);

    for i := 0 to lClasses.Count - 1 do
    begin
      ParseClassNameAnc(lClasses[i], lNm, lAnc);
      lForwards.Add('  ' + lNm + ' = class;');
    end;

    for i := 0 to lUses.Count - 1 do
    begin
      lU := lUses[i];
      if SameText(lU, 'system') or SameText(lU, 'objpas') then
        Continue;
      if SameText(lU, aCanonical) then
        Continue;
      if lUsesClean.IndexOf(lU) < 0 then
        lUsesClean.Add(lU);
    end;

    if lUsesClean.Count > 0 then
      lUsesLine := 'uses ' + JoinSL(lUsesClean, ', ') + ';'#10
    else
      lUsesLine := '';

    if (lForwards.Count > 0) or (lAnonEnumLines.Count > 0)
      or (lAliasLines.Count > 0) or (lPre.Count > 0)
      or (lClasses.Count > 0) or (lHelpers.Count > 0) then
    begin
      lBody.Add('type');
      lBody.AddStrings(lForwards);
      lBody.AddStrings(lAnonEnumLines);
      lBody.AddStrings(lAliasLines);
      lBody.AddStrings(lPre);
      lBody.AddStrings(lClasses);
      lBody.AddStrings(lHelpers);
    end;
    if lConsts.Count > 0 then
    begin
      lBody.Add('const');
      lBody.AddStrings(lConsts);
    end;

    lMode := '{$mode objfpc}{$H+}';
    if lHelpers.Count > 0 then
      lMode := lMode + '{$modeswitch typehelpers}';

    lHeader :=
      '// AUTO-GENERATED in-process ppudump interface stub for unit ' +
      aCanonical + '.'#10 +
      '// DO NOT EDIT — generated on demand by FpSonar.PpuStub.'#10 +
      '// FPC ' + aVersion + '  target ' + aPpu.Target + '  InterfaceCRC ' +
      aPpu.Crc + #10 +
      '// Faithful-or-omit: unresolvable shapes are omitted, not approximated.'#10#10;

    Result := lHeader + 'unit ' + aCanonical + ';'#10#10 + lMode + #10#10 +
      'interface'#10#10 + lUsesLine + #10 + TrimRight(lBody.Text) + #10 +
      TrimRight(lTail.Text) + #10#10'implementation'#10#10'end.'#10;
  finally
    lUses.Free;
    lPre.Free;
    lClasses.Free;
    lHelpers.Free;
    lConsts.Free;
    lTail.Free;
    lTailSeen.Free;
    lForwards.Free;
    lUsesClean.Free;
    lAliasLines.Free;
    lAnonEnumLines.Free;
    lIndexedProps.Free;
    lBody.Free;
  end;
end;


function TPpuGenImpl.GenerateUnit(const aName: string; out aCanonical,
  aCrc, aSource: string): boolean;
var
  lPpu: TFpSonarPpu;
  lAllowed, lEmitted: TStringList;
  lPass: integer;
  lStable: boolean;
begin
  Result := False;
  aCanonical := '';
  aCrc := '';
  aSource := '';
  lPpu := GetPpu(aName);
  if lPpu = nil then
    Exit;
  aCanonical := lPpu.Name;
  aCrc := lPpu.Crc;
  lAllowed := nil;
  try
    for lPass := 0 to 5 do
    begin
      FCurAllowed := lAllowed;
      lEmitted := TStringList.Create;
      lEmitted.Sorted := True;
      lEmitted.Duplicates := dupIgnore;
      // The header stamp is the host toolchain version (the ppudump whose
      // dump we parsed is this same toolchain — DiscoverPpuUnitDirs picks the
      // {$I %FPCVERSION%} unit dir); NOT a hardcoded '3.3.1' that would misstate
      // the version in a stub generated on any other host (faithful header).
      aSource := EmitUnitBody(lPpu, aCanonical, {$I %FPCVERSION%}, lEmitted);
      lStable := (lAllowed <> nil) and (lEmitted.CommaText = lAllowed.CommaText);
      if lStable then
      begin
        lEmitted.Free;
        Break;
      end;
      lAllowed.Free;
      lAllowed := lEmitted;
    end;
  finally
    lAllowed.Free;
    FCurAllowed := nil;
  end;
  Result := aSource <> '';
end;


{ TFpSonarPpuStubGen — pimpl wrapper (interface stays fpjson-free) }

constructor TFpSonarPpuStubGen.Create;
begin
  inherited Create;
  FImpl := TPpuGenImpl.Create;
end;


destructor TFpSonarPpuStubGen.Destroy;
begin
  FImpl.Free;
  inherited Destroy;
end;


procedure TFpSonarPpuStubGen.AddSearchDir(const aDir: string);
begin
  TPpuGenImpl(FImpl).AddSearchDir(aDir);
end;


function TFpSonarPpuStubGen.PpudumpAvailable: boolean;
begin
  Result := TPpuGenImpl(FImpl).PpudumpAvailable;
end;


function TFpSonarPpuStubGen.LocatePpu(const aName: string): string;
begin
  Result := TPpuGenImpl(FImpl).LocatePpu(aName);
end;


function TFpSonarPpuStubGen.InterfaceCrc(const aName: string): string;
begin
  Result := TPpuGenImpl(FImpl).InterfaceCrc(aName);
end;


function TFpSonarPpuStubGen.GenerateUnit(const aName: string; out aCanonical,
  aCrc, aSource: string): boolean;
begin
  Result := TPpuGenImpl(FImpl).GenerateUnit(aName, aCanonical, aCrc, aSource);
end;


function TFpSonarPpuStubGen.Omitted: TStringList;
begin
  Result := TPpuGenImpl(FImpl).Omitted;
end;


end.
