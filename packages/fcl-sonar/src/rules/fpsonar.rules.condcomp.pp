{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Rules for conditional-compilation shape and hardcoded platform literals

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.CondComp;


{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework;

type
  { Flags an {$ifdef} or {$ifndef} branch holding no source text.
    Polarity: positive. }
  TRuleEmptyConditionalBranch = class(TRuleBase)
  public
    // Emits one issue per empty branch, at that branch's own delimiter.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags an {$ifndef} with a populated then branch and an empty else branch.
    Polarity: positive. }
  TRuleNegatedConditionalWithEmptyElse = class(TRuleBase)
  public
    // Emits one issue at the {$ifndef} delimiter.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a lone directory separator literal that is an operand of a
    concatenation. Polarity: positive. }
  TRuleHardcodedPathSeparator = class(TRuleBase)
  public
    // Emits one issue per offending literal, at that literal's position.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a CR+LF pair written as character escapes rather than as LineEnding.
    Polarity: positive. }
  TRuleHardcodedLineEnding = class(TRuleBase)
  public
    // Emits one issue per literal carrying the pair, at that literal's position.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a SizeOf of a non-packed record used as the byte count of an I/O
    routine. Polarity: positive. }
  TRulePackedRecordFieldAlignmentAssumption = class(TRuleBase)
  public
    // Emits one issue per offending SizeOf call, at that call's row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a variable declared absolute over a variable of a different declared
    byte size. Polarity: positive. }
  TRuleAbsoluteVariableOverlay = class(TRuleBase)
  public
    // Emits one issue per offending overlay, at that declaration's row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a fixed-width SizeOf used as the byte count of a pointer-sized
    buffer. Polarity: positive. }
  TRulePointerSizedDatumTruncatedByByteCount = class(TRuleBase)
  public
    // Emits one issue per offending byte count, at that SizeOf call's row.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags an {$ifdef}/{$ifndef} on a symbol the analysis neither defined nor
    knows as FPC/target vocabulary. Polarity: absence. }
  TRuleUnknownConditionalSymbol = class(TRuleBase)
  public
    // Emits one issue per offending conditional, at its opening delimiter.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a populated {$ifdef}/{$ifndef} then branch whose guard is false under
    the analysis define set. Polarity: absence. }
  TRuleConditionalBranchNeverCompiled = class(TRuleBase)
  public
    // Emits one issue per dead branch, at its opening delimiter.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, Pascal.Tree,
{$ELSE}
  SysUtils, PasTree,
{$ENDIF}
  FpSonar.Ingest, FpSonar.Resolver, FpSonar.Traversal, FpSonar.Rules.Consts;

const
  cEmptyBranchId = 'EmptyConditionalBranch';
  cNegatedElseId = 'NegatedConditionalWithEmptyElse';
  cPathSeparatorId = 'HardcodedPathSeparator';
  cLineEndingId = 'HardcodedLineEnding';
  cRecordLayoutId = 'PackedRecordFieldAlignmentAssumption';
  cOverlayId = 'AbsoluteVariableOverlay';
  cByteCountWidthId = 'PointerSizedDatumTruncatedByByteCount';
  cUnknownSymbolId = 'UnknownConditionalSymbol';
  cNeverCompiledId = 'ConditionalBranchNeverCompiled';
  cKeyEmptyBranch = 'rule.EmptyConditionalBranch.message';
  cKeyNegatedElse = 'rule.NegatedConditionalWithEmptyElse.message';
  cKeyPathSeparator = 'rule.HardcodedPathSeparator.message';
  cKeyLineEnding = 'rule.HardcodedLineEnding.message';
  cKeyRecordLayout = 'rule.PackedRecordFieldAlignmentAssumption.message';
  cKeyOverlay = 'rule.AbsoluteVariableOverlay.message';
  cKeyByteCountWidth = 'rule.PointerSizedDatumTruncatedByByteCount.message';
  cKeyUnknownSymbol = 'rule.UnknownConditionalSymbol.message';
  cKeyNeverCompiled = 'rule.ConditionalBranchNeverCompiled.message';

  cKnownSymbolsParam = 'knownSymbols';
  // Compiler symbol families as globs, then the target, OS and dialect names.
  cDefaultKnownSymbols =
    'FPC,FPC_*,VER?,VER?_*,CPU*,FPU*,ENDIAN_*,'
    + 'UNIX,LINUX,ANDROID,BSD,FREEBSD,NETBSD,OPENBSD,DARWIN,MACOS,SOLARIS,'
    + 'SUNOS,AIX,HAIKU,BEOS,WINDOWS,MSWINDOWS,WIN32,WIN64,WINCE,OS2,EMX,'
    + 'MSDOS,GO32V1,GO32V2,NETWARE,NETWLIBC,AMIGA,AMIGAOS4,AROS,MORPHOS,'
    + 'HASAMIGA,ATARI,SYMBIAN,GBA,NDS,WASI,JVM,PAS2JS,UNICODE,UNICODERTL';

  // The I/O routines that take a byte count, by written name.
  cIOByteCountRoutines: array[0..3] of string = ('BlockRead', 'BlockWrite',
    'ReadBuffer', 'WriteBuffer');
  // The byte-count argument index of each cIOByteCountRoutines entry, and the
  // highest argument count that still puts the byte count at that index.
  cIOCountArgIndex: array[0..3] of integer = (2, 2, 1, 1);
  cIOMaxArgCount: array[0..3] of integer = (4, 4, 2, 2);
  cSizeOfName = 'SizeOf';

type
  // The spelling a conditional was opened with.
  TCondKind = (ckIfDef, ckIfNDef, ckOther);

  { One directive occurrence: its span and its text as written. }
  TCondDelimiter = record
    Row: integer;
    Col: integer;
    EndRow: integer;
    EndCol: integer;
    Text: string;
  end;

  { One conditional of the file: its opener, its {$else} and what each branch
    was seen to hold. }
  TCondEntry = record
    Kind: TCondKind;
    Symbol: string;
    Opener: TCondDelimiter;
    ElseDelim: TCondDelimiter;
    HasElse: boolean;
    ThenHasContent: boolean;
    ElseHasContent: boolean;
    Opaque: boolean;
    Closed: boolean;
  end;

  // What one pass over a file's physical lines yields.
  TCondCompScan = array of TCondEntry;

{ ---- name and directive primitives }

function IsIdentStart(aChar: char): boolean;
begin
  Result := aChar in ['A'..'Z', 'a'..'z', '_'];
end;


function IsIdentChar(aChar: char): boolean;
begin
  Result := aChar in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;


// The aIndex-th (1-based) identifier-shaped word of a directive body, or ''.
function DirectiveWord(const aBody: string; aIndex: integer): string;
var
  i, lStart, lSeen: integer;
begin
  Result := '';
  lSeen := 0;
  i := 1;
  while i <= Length(aBody) do
    if IsIdentStart(aBody[i]) then
    begin
      lStart := i;
      while (i <= Length(aBody)) and IsIdentChar(aBody[i]) do
        Inc(i);
      Inc(lSeen);
      if lSeen = aIndex then
      begin
        Result := Copy(aBody, lStart, i - lStart);
        Exit;
      end;
    end
    else
      Inc(i);
end;


function MakeDelimiter(aRow, aCol, aEndRow, aEndCol: integer;
  const aText: string): TCondDelimiter;
begin
  Result.Row := aRow;
  Result.Col := aCol;
  Result.EndRow := aEndRow;
  Result.EndCol := aEndCol;
  Result.Text := aText;
end;


{ ---- the line-text pass }

// Yields one record per conditional of aLines: opener kind and symbol, the
// spans of the opening and {$else} delimiters, and per-branch content.
function ScanCondComp(const aLines: TFpSonarStringArray): TCondCompScan;
type
  TScanState = (ssNormal, ssBrace, ssParen);
var
  lState: TScanState;
  lIsDirective, lIsParen, lBroken: boolean;
  lBody, lLine: string;
  lRow, lCol, lLen, lStart, lDepth: integer;
  lDirRow, lDirCol, lTop: integer;
  lStack: array of integer;

  procedure MarkContent;
  begin
    if lTop < 0 then
      Exit;
    if Result[lStack[lTop]].HasElse then
      Result[lStack[lTop]].ElseHasContent := True
    else
      Result[lStack[lTop]].ThenHasContent := True;
  end;

  procedure Push(aKind: TCondKind; const aSymbol: string;
    const aDelim: TCondDelimiter);
  var
    lNew: integer;
  begin
    // A nested conditional is content for the branch that encloses it.
    MarkContent;
    SetLength(Result, Length(Result) + 1);
    lNew := High(Result);
    Result[lNew].Kind := aKind;
    Result[lNew].Symbol := aSymbol;
    Result[lNew].Opener := aDelim;
    Result[lNew].ElseDelim := MakeDelimiter(0, 0, 0, 0, '');
    Result[lNew].HasElse := False;
    Result[lNew].ThenHasContent := False;
    Result[lNew].ElseHasContent := False;
    Result[lNew].Opaque := False;
    Result[lNew].Closed := False;
    Inc(lTop);
    if lTop > High(lStack) then
      SetLength(lStack, lTop + 1);
    lStack[lTop] := lNew;
  end;

  procedure SwitchBranch(const aDelim: TCondDelimiter);
  begin
    if lTop < 0 then
    begin
      lBroken := True;
      Exit;
    end;
    if Result[lStack[lTop]].HasElse then
      Result[lStack[lTop]].Opaque := True
    else
    begin
      Result[lStack[lTop]].HasElse := True;
      Result[lStack[lTop]].ElseDelim := aDelim;
    end;
  end;

  procedure CloseEntry;
  begin
    if lTop < 0 then
    begin
      lBroken := True;
      Exit;
    end;
    Result[lStack[lTop]].Closed := True;
    Dec(lTop);
  end;

  procedure MarkOpaque;
  begin
    if lTop < 0 then
      lBroken := True
    else
      Result[lStack[lTop]].Opaque := True;
  end;

  procedure HandleDirective(const aBody: string; aEndRow, aEndCol: integer);
  var
    lFirst: string;
    lDelim: TCondDelimiter;
  begin
    lFirst := LowerCase(DirectiveWord(aBody, 1));
    if lIsParen then
      lDelim := MakeDelimiter(lDirRow, lDirCol, aEndRow, aEndCol,
        '(*$' + aBody + '*)')
    else
      lDelim := MakeDelimiter(lDirRow, lDirCol, aEndRow, aEndCol,
        '{$' + aBody + '}');
    if lFirst = 'ifdef' then
      Push(ckIfDef, DirectiveWord(aBody, 2), lDelim)
    else if lFirst = 'ifndef' then
      Push(ckIfNDef, DirectiveWord(aBody, 2), lDelim)
    else if (lFirst = 'if') or (lFirst = 'ifc') or (lFirst = 'ifopt') then
      Push(ckOther, '', lDelim)
    else if (lFirst = 'else') or (lFirst = 'elsec') then
      SwitchBranch(lDelim)
    else if (lFirst = 'endif') or (lFirst = 'endc') or (lFirst = 'ifend') then
      CloseEntry
    else if (lFirst = 'elseif') or (lFirst = 'elifc') then
      MarkOpaque
    else
      MarkContent;
  end;

begin
  Result := nil;
  lStack := nil;
  lTop := -1;
  lBroken := False;
  lState := ssNormal;
  lIsDirective := False;
  lIsParen := False;
  lBody := '';
  lDirRow := 0;
  lDirCol := 0;
  lDepth := 0;
  for lRow := 0 to High(aLines) do
  begin
    lLine := aLines[lRow];
    lLen := Length(lLine);
    lCol := 1;
    while lCol <= lLen do
      case lState of
        ssBrace:
          begin
            lStart := lCol;
            // {$mode objfpc} enables nested comments, so depth ends this one.
            while (lCol <= lLen) and not ((lLine[lCol] = '}') and (lDepth = 1)) do
            begin
              if lLine[lCol] = '{' then
                Inc(lDepth)
              else if lLine[lCol] = '}' then
                Dec(lDepth);
              Inc(lCol);
            end;
            if lIsDirective then
              lBody := lBody + Copy(lLine, lStart, lCol - lStart);
            if lCol <= lLen then
            begin
              lDepth := 0;
              lState := ssNormal;
              Inc(lCol);
              if lIsDirective then
              begin
                lIsDirective := False;
                HandleDirective(lBody, lRow + 1, lCol - 1);
              end;
            end;
          end;
        ssParen:
          begin
            lStart := lCol;
            while lCol <= lLen do
              if (lLine[lCol] = '(') and (lCol < lLen)
                and (lLine[lCol + 1] = '*') then
              begin
                Inc(lDepth);
                Inc(lCol, 2);
              end
              else if (lLine[lCol] = '*') and (lCol < lLen)
                and (lLine[lCol + 1] = ')') then
              begin
                if lDepth = 1 then
                  Break;
                Dec(lDepth);
                Inc(lCol, 2);
              end
              else
                Inc(lCol);
            if lIsDirective then
              lBody := lBody + Copy(lLine, lStart, lCol - lStart);
            if lCol <= lLen then
            begin
              lDepth := 0;
              lState := ssNormal;
              Inc(lCol, 2);
              if lIsDirective then
              begin
                lIsDirective := False;
                HandleDirective(lBody, lRow + 1, lCol - 1);
              end;
            end;
          end;
        else
          begin
            if (lLine[lCol] = '/') and (lCol < lLen) and (lLine[lCol + 1] = '/') then
              lCol := lLen + 1
            else if lLine[lCol] = '{' then
            begin
              lIsDirective := (lCol < lLen) and (lLine[lCol + 1] = '$');
              lIsParen := False;
              lBody := '';
              lDirRow := lRow + 1;
              lDirCol := lCol;
              lDepth := 1;
              if lIsDirective then
                Inc(lCol, 2)
              else
                Inc(lCol);
              lState := ssBrace;
            end
            else if (lLine[lCol] = '(') and (lCol < lLen)
              and (lLine[lCol + 1] = '*') then
            begin
              lIsDirective := (lCol + 2 <= lLen) and (lLine[lCol + 2] = '$');
              lIsParen := True;
              lBody := '';
              lDirRow := lRow + 1;
              lDirCol := lCol;
              lDepth := 1;
              if lIsDirective then
                Inc(lCol, 3)
              else
                Inc(lCol, 2);
              lState := ssParen;
            end
            else if lLine[lCol] = '''' then
            begin
              MarkContent;
              Inc(lCol);
              while lCol <= lLen do
                if lLine[lCol] <> '''' then
                  Inc(lCol)
                else if (lCol < lLen) and (lLine[lCol + 1] = '''') then
                  Inc(lCol, 2)
                else
                begin
                  Inc(lCol);
                  Break;
                end;
            end
            else
            begin
              if not (lLine[lCol] in [' ', #9]) then
                MarkContent;
              Inc(lCol);
            end;
          end;
      end;
    if (lState <> ssNormal) and lIsDirective then
      lBody := lBody + ' ';
  end;
  // A delimiter with no conditional open puts the whole nesting model in doubt.
  if lBroken then
    SetLength(Result, 0);
end;


// True iff aEntry is a closed, unchained {$ifdef}/{$ifndef} naming a symbol.
function EntryMayReport(const aEntry: TCondEntry): boolean;
begin
  Result := aEntry.Closed and not aEntry.Opaque and (aEntry.Symbol <> '')
    and ((aEntry.Kind = ckIfDef) or (aEntry.Kind = ckIfNDef));
end;


{ TRuleEmptyConditionalBranch }

procedure TRuleEmptyConditionalBranch.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TCondCompScan;
  lSiblingEnabled: boolean;
  i: integer;

  procedure Emit(const aDelim: TCondDelimiter; const aSymbol: string);
  begin
    aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
      aDelim.Row, aDelim.Col, aDelim.EndRow, aDelim.EndCol,
      FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
      FMetadata.MessageKey, [aSymbol], aDelim.Text);
  end;

begin
  lScan := ScanCondComp(aContext.Lines);
  // The negated/empty-else shape is the sibling's only when the sibling runs.
  lSiblingEnabled := aContext.Config.RuleEnabled(cNegatedElseId, False);
  for i := 0 to High(lScan) do
  begin
    if not EntryMayReport(lScan[i]) then
      Continue;
    if not lScan[i].ThenHasContent then
      Emit(lScan[i].Opener, lScan[i].Symbol);
    if lScan[i].HasElse and not lScan[i].ElseHasContent then
    begin
      if lSiblingEnabled and (lScan[i].Kind = ckIfNDef)
        and lScan[i].ThenHasContent then
        Continue;
      Emit(lScan[i].ElseDelim, lScan[i].Symbol);
    end;
  end;
end;


{ TRuleNegatedConditionalWithEmptyElse }

procedure TRuleNegatedConditionalWithEmptyElse.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TCondCompScan;
  i: integer;
begin
  lScan := ScanCondComp(aContext.Lines);
  for i := 0 to High(lScan) do
  begin
    if not EntryMayReport(lScan[i]) then
      Continue;
    if (lScan[i].Kind = ckIfNDef) and lScan[i].HasElse
      and lScan[i].ThenHasContent and not lScan[i].ElseHasContent then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lScan[i].Opener.Row, lScan[i].Opener.Col,
        lScan[i].Opener.EndRow, lScan[i].Opener.EndCol,
        FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
        FMetadata.MessageKey, [lScan[i].Symbol], lScan[i].Opener.Text);
  end;
end;


{ ---- the define-dependent pass }

// True iff aEntry is an {$ifdef}/{$ifndef} naming a symbol, whatever its
// closure and chaining.
function EntryNamesSymbol(const aEntry: TCondEntry): boolean;
begin
  Result := (aEntry.Symbol <> '')
    and ((aEntry.Kind = ckIfDef) or (aEntry.Kind = ckIfNDef));
end;


// Splits a comma-separated glob list, trimming whitespace, dropping empties and
// upper-casing: GlobMatch compares literally and {$ifdef} does not.
function SplitSymbolGlobs(const aValue: string): TStringArray;
var
  lParts: TStringArray;
  lTrimmed: string;
  i: integer;
begin
  SetLength(Result, 0);
  lParts := aValue.Split([',']);
  for i := 0 to High(lParts) do
  begin
    lTrimmed := Trim(lParts[i]);
    if lTrimmed <> '' then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := UpperCase(lTrimmed);
    end;
  end;
end;


// True iff any glob of aGlobs matches aSymbol. aGlobs arrives upper-cased from
// SplitSymbolGlobs.
function SymbolIsKnown(const aSymbol: string;
  const aGlobs: TStringArray): boolean;
var
  lSymbol: string;
  i: integer;
begin
  Result := False;
  lSymbol := UpperCase(aSymbol);
  for i := 0 to High(aGlobs) do
    if GlobMatch(aGlobs[i], lSymbol) then
      Exit(True);
end;


// Emits a point-span issue over aDelim carrying aSymbol as its sole argument.
procedure EmitAtDelimiter(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  const aDelim: TCondDelimiter; const aSymbol: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aDelim.Row, aDelim.Col,
    aDelim.EndRow, aDelim.EndCol, aMeta.Severity, aMeta.Category,
    aMeta.DefaultConfidence, aMeta.MessageKey, [aSymbol], aDelim.Text);
end;


{ TRuleUnknownConditionalSymbol }

procedure TRuleUnknownConditionalSymbol.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TCondCompScan;
  lGlobs: TStringArray;
  lDefined: boolean;
  i: integer;
begin
  lGlobs := SplitSymbolGlobs(aContext.Config.RuleParamStr(FMetadata.RuleId,
    cKnownSymbolsParam, cDefaultKnownSymbols));
  lScan := ScanCondComp(aContext.Lines);
  for i := 0 to High(lScan) do
  begin
    if not EntryNamesSymbol(lScan[i]) then
      Continue;
    // No define set at all is silence, per file rather than per symbol.
    if not aContext.Defines.TryIsDefined(lScan[i].Symbol, lDefined) then
      Exit;
    if lDefined or SymbolIsKnown(lScan[i].Symbol, lGlobs) then
      Continue;
    EmitAtDelimiter(FMetadata, aContext, aCollector, lScan[i].Opener,
      lScan[i].Symbol);
  end;
end;


{ TRuleConditionalBranchNeverCompiled }

procedure TRuleConditionalBranchNeverCompiled.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lScan: TCondCompScan;
  lDefined: boolean;
  i: integer;
begin
  lScan := ScanCondComp(aContext.Lines);
  for i := 0 to High(lScan) do
  begin
    if not (EntryMayReport(lScan[i]) and lScan[i].ThenHasContent) then
      Continue;
    if not aContext.Defines.TryIsDefined(lScan[i].Symbol, lDefined) then
      Exit;
    // The guard holds iff a positive spelling meets a defined symbol.
    if lDefined = (lScan[i].Kind = ckIfDef) then
      Continue;
    EmitAtDelimiter(FMetadata, aContext, aCollector, lScan[i].Opener,
      lScan[i].Symbol);
  end;
end;


{ ---- the token-stream pass }

// The nearest non-trivia token index from aIndex, aStep -1 back or +1 on, or -1.
function NeighbourIndex(const aTokens: TFpSonarTokenArray;
  aIndex, aStep: integer): integer;
begin
  Result := aIndex + aStep;
  while (Result >= 0) and (Result <= High(aTokens)) do
  begin
    if not aTokens[Result].IsTrivia then
      Exit;
    Inc(Result, aStep);
  end;
  Result := -1;
end;


{ TRuleHardcodedPathSeparator }

procedure TRuleHardcodedPathSeparator.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i, lLeft, lRight: integer;
  lTok: TFpSonarToken;
begin
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsString then
      Continue;
    if (Length(lTok.Text) <> 3) or (lTok.Text[1] <> '''')
      or (lTok.Text[3] <> '''') or not (lTok.Text[2] in ['\', '/']) then
      Continue;
    lLeft := NeighbourIndex(aContext.Tokens, i, -1);
    lRight := NeighbourIndex(aContext.Tokens, i, 1);
    if ((lLeft < 0) or (aContext.Tokens[lLeft].Punct <> '+'))
      and ((lRight < 0) or (aContext.Tokens[lRight].Punct <> '+')) then
      Continue;
    aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
      lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
      FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
      [lTok.Text], lTok.Text);
  end;
end;


{ TRuleHardcodedLineEnding }

procedure TRuleHardcodedLineEnding.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i: integer;
  lTok: TFpSonarToken;
  lPair: string;

  // The source text of the first #13#10 escape pair of aText, or ''.
  function CrLfPair(const aText: string): string;
  var
    lPos, lLen, lStart, lDigits, lValue, lPrev, lPrevStart: integer;
    lRadix: string;
    lQuoted: boolean;
  begin
    Result := '';
    lLen := Length(aText);
    lQuoted := False;
    lPrev := -1;
    lPrevStart := 0;
    lPos := 1;
    while lPos <= lLen do
      if lQuoted then
      begin
        if aText[lPos] <> '''' then
          Inc(lPos)
        else if (lPos < lLen) and (aText[lPos + 1] = '''') then
          Inc(lPos, 2)
        else
        begin
          lQuoted := False;
          Inc(lPos);
        end;
      end
      else if aText[lPos] = '''' then
      begin
        lQuoted := True;
        lPrev := -1;
        Inc(lPos);
      end
      else if aText[lPos] = '#' then
      begin
        lStart := lPos;
        Inc(lPos);
        lRadix := '';
        if (lPos <= lLen) and (aText[lPos] = '$') then
        begin
          lRadix := '$';
          Inc(lPos);
        end;
        lDigits := lPos;
        while (lPos <= lLen) and (((lRadix = '') and (aText[lPos] in ['0'..'9']))
          or ((lRadix = '$') and (aText[lPos] in ['0'..'9', 'A'..'F', 'a'..'f']))) do
          Inc(lPos);
        lValue := StrToIntDef(lRadix + Copy(aText, lDigits, lPos - lDigits), -1);
        if (lPrev = 13) and (lValue = 10) then
          Exit(Copy(aText, lPrevStart, lPos - lPrevStart));
        lPrev := lValue;
        lPrevStart := lStart;
      end
      else
      begin
        lPrev := -1;
        Inc(lPos);
      end;
  end;

begin
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsString then
      Continue;
    lPair := CrLfPair(lTok.Text);
    if lPair <> '' then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
        FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
        [lPair], lTok.Text);
  end;
end;


{ ---- the declared-storage-layout pass }

// The written name a callee expression ends in: a bare identifier, or the
// right-hand identifier of a qualified access; '' for any other shape.
function CalleeName(aExpr: TPasExpr): string;
begin
  Result := '';
  if aExpr = nil then
    Exit;
  if (aExpr is TPrimitiveExpr) and (aExpr.Kind = pekIdent) then
    Result := TPrimitiveExpr(aExpr).Value
  else if (aExpr is TBinaryExpr) and (TBinaryExpr(aExpr).OpCode = eopSubIdent) then
    Result := CalleeName(TBinaryExpr(aExpr).right);
end;


// True iff aName is one of the byte-counting I/O routines.
function IsIOByteCountRoutine(const aName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(cIOByteCountRoutines) to High(cIOByteCountRoutines) do
    if SameText(aName, cIOByteCountRoutines[i]) then
      Exit(True);
end;


// The byte-count argument index of a call to aName carrying aArgCount
// arguments, or -1 when aName is not a byte-counting routine or carries an
// argument count that puts something else at that index.
function IOCountArgIndex(const aName: string; aArgCount: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := Low(cIOByteCountRoutines) to High(cIOByteCountRoutines) do
    if SameText(aName, cIOByteCountRoutines[i]) then
    begin
      if (aArgCount > cIOCountArgIndex[i])
        and (aArgCount <= cIOMaxArgCount[i]) then
        Result := cIOCountArgIndex[i];
      Exit;
    end;
end;


// Appends every statement below aRoot to aList (recurses ChildStatements).
procedure CollectStatements(aRoot: TPasImplElement;
  var aList: TPasImplElementArray);
var
  lChildren: TPasImplElementArray;
  i: integer;
begin
  lChildren := ChildStatements(aRoot);
  for i := 0 to High(lChildren) do
  begin
    SetLength(aList, Length(aList) + 1);
    aList[High(aList)] := lChildren[i];
    CollectStatements(lChildren[i], aList);
  end;
end;


// Emits a point issue at aRow, column 1.
procedure EmitLayout(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aRow: integer;
  const aArgs: array of string; const aSnippet: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aRow, 1, aRow, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    aArgs, aSnippet);
end;


{ TRulePackedRecordFieldAlignmentAssumption }

procedure TRulePackedRecordFieldAlignmentAssumption.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lRoots, lStmts: TPasImplElementArray;
  lExpr: TPasExpr;
  lCall, lArg: TParamsExpr;
  lType: TFpSonarResolvedType;
  i, j: integer;
begin
  lRoots := EnumerateStatementRoots(aContext.Resolver.ResolvedModule);
  SetLength(lStmts, 0);
  for i := 0 to High(lRoots) do
    CollectStatements(lRoots[i], lStmts);
  for i := 0 to High(lStmts) do
  begin
    if not (lStmts[i] is TPasImplSimple) then
      Continue;
    lExpr := TPasImplSimple(lStmts[i]).Expr;
    if not ((lExpr is TParamsExpr) and (lExpr.Kind = pekFuncParams)) then
      Continue;
    lCall := TParamsExpr(lExpr);
    if not IsIOByteCountRoutine(CalleeName(lCall.Value)) then
      Continue;
    for j := 0 to High(lCall.Params) do
    begin
      if not ((lCall.Params[j] is TParamsExpr)
        and (lCall.Params[j].Kind = pekFuncParams)) then
        Continue;
      lArg := TParamsExpr(lCall.Params[j]);
      if (Length(lArg.Params) <> 1)
        or not SameText(CalleeName(lArg.Value), cSizeOfName) then
        Continue;
      if not aContext.Resolver.TryResolvedType(lArg.Params[0], lType) then
        Continue;
      if (lType.Kind <> ltkRecord) or not (lType.TypeEl is TPasRecordType) then
        Continue;
      if (TPasRecordType(lType.TypeEl).PackMode <> pmNone)
        or (TPasRecordType(lType.TypeEl).Align > 0) then
        Continue;
      if lType.NamedTypeName = '' then
        Continue;
      EmitLayout(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lArg), [lType.NamedTypeName],
        lType.NamedTypeName);
    end;
  end;
end;


{ TRuleAbsoluteVariableOverlay }

procedure TRuleAbsoluteVariableOverlay.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lValues: TPasValueDeclArray;
  lRoutines: TAstRoutineArray;
  i, j: integer;

  procedure CheckVar(aVar: TPasVariable);
  var
    lOverlay, lTarget: TFpSonarResolvedType;
    lDecl: TPasElement;
    lOverlaySize, lTargetSize: integer;
  begin
    if (aVar.AbsoluteExpr = nil) or (aVar.Name = '') then
      Exit;
    if not aContext.Resolver.TryResolvedType(aVar, lOverlay) then
      Exit;
    if not aContext.Resolver.TryTypeByteSize(lOverlay.TypeEl, lOverlaySize) then
      Exit;
    lDecl := aContext.Resolver.ReferencedDecl(aVar.AbsoluteExpr);
    if (lDecl = nil) or (lDecl.Name = '') then
      Exit;
    if not aContext.Resolver.TryResolvedType(lDecl, lTarget) then
      Exit;
    if not aContext.Resolver.TryTypeByteSize(lTarget.TypeEl, lTargetSize) then
      Exit;
    if lOverlaySize = lTargetSize then
      Exit;
    EmitLayout(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(aVar), [aVar.Name, lDecl.Name], aVar.Name);
  end;

begin
  lValues := EnumerateValueDecls(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lValues) do
    CheckVar(lValues[i].Decl);
  lRoutines := EnumerateRoutines(aContext.Resolver.ResolvedModule);
  for i := 0 to High(lRoutines) do
    for j := 0 to lRoutines[i].Decl.Body.Declarations.Count - 1 do
      if TObject(lRoutines[i].Decl.Body.Declarations[j]) is TPasVariable then
        CheckVar(TPasVariable(lRoutines[i].Decl.Body.Declarations[j]));
end;


{ TRulePointerSizedDatumTruncatedByByteCount }

procedure TRulePointerSizedDatumTruncatedByByteCount.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lRoots, lStmts: TPasImplElementArray;
  lExpr: TPasExpr;
  lCall, lSizeOf: TParamsExpr;
  lCount, lBuffer: TFpSonarResolvedType;
  i, lIndex: integer;
begin
  lRoots := EnumerateStatementRoots(aContext.Resolver.ResolvedModule);
  SetLength(lStmts, 0);
  for i := 0 to High(lRoots) do
    CollectStatements(lRoots[i], lStmts);
  for i := 0 to High(lStmts) do
  begin
    if not (lStmts[i] is TPasImplSimple) then
      Continue;
    lExpr := TPasImplSimple(lStmts[i]).Expr;
    if not ((lExpr is TParamsExpr) and (lExpr.Kind = pekFuncParams)) then
      Continue;
    lCall := TParamsExpr(lExpr);
    lIndex := IOCountArgIndex(CalleeName(lCall.Value), Length(lCall.Params));
    if lIndex < 1 then
      Continue;
    if not ((lCall.Params[lIndex] is TParamsExpr)
      and (lCall.Params[lIndex].Kind = pekFuncParams)) then
      Continue;
    lSizeOf := TParamsExpr(lCall.Params[lIndex]);
    if (Length(lSizeOf.Params) <> 1)
      or not SameText(CalleeName(lSizeOf.Value), cSizeOfName) then
      Continue;
    if not aContext.Resolver.TryResolvedType(lSizeOf.Params[0], lCount) then
      Continue;
    if (lCount.IntWidth <> liwFixed) or (lCount.NamedTypeName = '') then
      Continue;
    if not aContext.Resolver.TryResolvedType(lCall.Params[lIndex - 1],
      lBuffer) then
      Continue;
    if (lBuffer.Kind <> ltkPointer)
      and (lBuffer.IntWidth <> liwPointerSized) then
      Continue;
    EmitLayout(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lSizeOf), [lCount.NamedTypeName],
      lCount.NamedTypeName);
  end;
end;


var
  lMeta: TRuleMetadata;

initialization
  RegisterRule(TRuleEmptyConditionalBranch.Create(TRuleMetadata.Make(
    cEmptyBranchId, rtTok, rfLineText, sevMinor, itCodeSmell,
    cfMedium, False, cKeyEmptyBranch).WithDescription(
    'Flags an empty branch of a closed {$ifdef} or {$ifndef} conditional that '
    + 'names a symbol and carries no {$elseif}.')));
  RegisterMessage(cKeyEmptyBranch, SEmptyConditionalBranch);


  RegisterRule(TRuleNegatedConditionalWithEmptyElse.Create(TRuleMetadata.Make(
    cNegatedElseId, rtTok, rfLineText, sevMinor, itCodeSmell,
    cfMedium, False, cKeyNegatedElse).WithDescription(
    'Flags an empty {$else} branch of a closed {$ifndef} conditional that '
    + 'names a symbol, carries no {$elseif} and has a populated then branch.')));
  RegisterMessage(cKeyNegatedElse, SNegatedConditionalWithEmptyElse);


  RegisterRule(TRuleHardcodedPathSeparator.Create(TRuleMetadata.Make(
    cPathSeparatorId, rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfMedium, False, cKeyPathSeparator).WithDescription(
    'Flags a string literal holding exactly a directory separator that is an '
    + 'operand of a + concatenation.')));
  RegisterMessage(cKeyPathSeparator, SHardcodedPathSeparator);


  RegisterRule(TRuleHardcodedLineEnding.Create(TRuleMetadata.Make(
    cLineEndingId, rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfMedium, False, cKeyLineEnding).WithDescription(
    'Flags a literal carrying a CR+LF pair written as character escapes '
    + 'outside its quoted text.')));
  RegisterMessage(cKeyLineEnding, SHardcodedLineEnding);


  RegisterRule(TRulePackedRecordFieldAlignmentAssumption.Create(
    TRuleMetadata.Make(cRecordLayoutId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyRecordLayout).WithDescription(
    'Flags a SizeOf of a record declared with no packing and no explicit align '
    + 'used as a direct byte-count argument of a byte-counting I/O routine.')));
  RegisterMessage(cKeyRecordLayout, SPackedRecordFieldAlignmentAssumption);


  RegisterRule(TRuleAbsoluteVariableOverlay.Create(TRuleMetadata.Make(
    cOverlayId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyOverlay).WithDescription(
    'Flags a variable declared absolute over a variable whose declared byte '
    + 'size differs from its own.')));
  RegisterMessage(cKeyOverlay, SAbsoluteVariableOverlay);


  RegisterRule(TRulePointerSizedDatumTruncatedByByteCount.Create(
    TRuleMetadata.Make(cByteCountWidthId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyByteCountWidth).WithDescription(
    'Flags a SizeOf of a fixed-width type used as the byte-count argument of a '
    + 'byte-counting I/O routine whose buffer is a pointer or a pointer-sized '
    + 'integer.')));
  RegisterMessage(cKeyByteCountWidth, SPointerSizedDatumTruncatedByByteCount);


  // knownSymbols declared AFTER Make, which resets ParamSpecs.
  lMeta := TRuleMetadata.Make(cUnknownSymbolId, rtTok, rfLineText, sevMajor,
    itBug, cfLow, False, cKeyUnknownSymbol).WithDescription(
    'Flags an {$ifdef} or {$ifndef} on a symbol neither defined for this '
    + 'analysis nor matched by the knownSymbols list, inverting the silence '
    + 'contract: a gap in that list reports rather than stays silent.');
  lMeta.AddParam(cKnownSymbolsParam, rpkString, cDefaultKnownSymbols);
  RegisterRule(TRuleUnknownConditionalSymbol.Create(lMeta));
  RegisterMessage(cKeyUnknownSymbol, SUnknownConditionalSymbol);


  RegisterRule(TRuleConditionalBranchNeverCompiled.Create(TRuleMetadata.Make(
    cNeverCompiledId, rtTok, rfLineText, sevMinor, itCodeSmell,
    cfLow, False, cKeyNeverCompiled).WithDescription(
    'Flags a populated then branch of a closed {$ifdef} or {$ifndef} whose '
    + 'guard is false under the analysis define set, inverting the silence '
    + 'contract: a define set narrower than the real target matrix reports '
    + 'rather than stays silent.')));
  RegisterMessage(cKeyNeverCompiled, SConditionalBranchNeverCompiled);

end.
