{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Rules for FPC/FCL source-base conventions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.FpcStyle;


{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework;

type
  { Flags a unit listed in only one branch of an FPC_DOTTEDUNITS guard.
    Polarity: positive. }
  TRuleDottedUnitsBranchesInconsistent = class(TRuleBase)
  public
    // Emits one issue per unpaired unit of a guarded uses clause the scan trusts.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a uses clause in a file whose directives never name FPC_DOTTEDUNITS.
    Polarity: absence. }
  TRuleMissingDottedUnitsGuard = class(TRuleBase)
  public
    // Emits one issue at the first uses keyword of an unguarded file.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a dotted unit name that is not the recorded alias of its counterpart.
    Polarity: positive. }
  TRuleDottedUnitAliasMismatch = class(TRuleBase)
  public
    // Emits one issue per guarded pair whose dotted name contradicts the table.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a unit file whose name is neither the lowercase form of the unit name
    nor that name verbatim. Polarity: positive. }
  TRuleUnitFileNameCaseMismatch = class(TRuleBase)
  public
    // Emits one issue at the unit-name identifier of the first unit clause.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a module whose file declares no {$mode} directive.
    Polarity: absence. }
  TRuleMissingModeDirective = class(TRuleBase)
  public
    // Emits one issue at the module keyword of the first named clause.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a module with no COPYING.FPC reference in its leading comment.
    Polarity: absence. }
  TRuleMissingCopyrightHeader = class(TRuleBase)
  public
    // Emits one issue at the module keyword of the first named clause.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a use of a symbol declared deprecated. Polarity: positive. }
  TRuleDeprecatedSymbolUsed = class(TRuleBase)
  public
    // Emits one issue per resolved use of a deprecated declaration.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a use of a symbol declared platform in a unit not marked platform.
    Polarity: positive. }
  TRulePlatformSymbolUsedInPortableUnit = class(TRuleBase)
  public
    // Emits one issue per resolved use of a platform declaration.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a use of a symbol declared experimental. Polarity: positive. }
  TRuleExperimentalSymbolUsed = class(TRuleBase)
  public
    // Emits one issue per resolved use of an experimental declaration.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a public or protected method with no documentation comment on the
    line above it. Polarity: absence. }
  TRulePublicMethodUndocumented = class(TRuleBase)
  public
    // Emits one issue at the declaring keyword of each undocumented method.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags a public or published property with no documentation comment on the
    line above it. Polarity: absence. }
  TRulePublicPropertyUndocumented = class(TRuleBase)
  public
    // Emits one issue at the property keyword of each undocumented property.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags an interface uses entry naming a unit no interface declaration
    references. Polarity: absence. }
  TRuleInterfaceUsesTooBroad = class(TRuleBase)
  public
    // Emits one issue per interface uses entry the interface never names.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


  { Flags an I/O call under {$I-} whose IOResult nothing reads before the next
    I/O call. Polarity: absence. }
  TRuleIOResultNotChecked = class(TRuleBase)
  public
    // Emits one issue per unchecked I/O call inside an {$I-} region.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Pascal.Tree,
{$ELSE}
  Classes, SysUtils, PasTree,
{$ENDIF}
  FpSonar.Ingest, FpSonar.Traversal, FpSonar.Config, FpSonar.DataFlow,
  FpSonar.NamespaceMap, FpSonar.Resolver, FpSonar.Rules.Consts;

const
  cGuardSymbol = 'FPC_DOTTEDUNITS';
  cCopyrightMarker = 'COPYING.FPC';

  // Message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyBranches = 'rule.DottedUnitsBranchesInconsistent.message';
  cKeyMissingGuard = 'rule.MissingDottedUnitsGuard.message';
  cKeyAlias = 'rule.DottedUnitAliasMismatch.message';
  cKeyCaseMismatch = 'rule.UnitFileNameCaseMismatch.message';
  cKeyMissingMode = 'rule.MissingModeDirective.message';
  cKeyMissingCopyright = 'rule.MissingCopyrightHeader.message';
  cKeyDeprecatedSymbol = 'rule.DeprecatedSymbolUsed.message';
  cKeyPlatformSymbol = 'rule.PlatformSymbolUsedInPortableUnit.message';
  cKeyExperimentalSymbol = 'rule.ExperimentalSymbolUsed.message';
  cKeyMethodUndocumented = 'rule.PublicMethodUndocumented.message';
  cKeyPropertyUndocumented = 'rule.PublicPropertyUndocumented.message';
  cKeyUsesTooBroad = 'rule.InterfaceUsesTooBroad.message';
  cKeyIOResultNotChecked = 'rule.IOResultNotChecked.message';

type
  { One unit name of a guarded branch, with its 1-based byte position. }
  TDottedUnitRef = record
    Name: string;
    Row: integer;
    Col: integer;
  end;

  TDottedUnitRefArray = array of TDottedUnitRef;

  { One FPC_DOTTEDUNITS guard and both its branch unit lists. }
  TDottedGuard = record
    Dotted: TDottedUnitRefArray;
    Plain: TDottedUnitRefArray;
    HasElse: boolean;
    Closed: boolean;
    // A construct inside the guard leaves a branch list untrustworthy.
    Opaque: boolean;
    // Whether a uses clause was open when the guard opened.
    ClauseAtOpen: boolean;
    // A ';' inside a branch terminates the clause when the guard closes.
    EndsClause: boolean;
  end;

  TDottedGuardArray = array of TDottedGuard;

  TModuleKind = (mkUnit, mkProgram, mkLibrary);

  { One module clause, with its keyword as written and both 1-based positions. }
  TModuleClause = record
    Kind: TModuleKind;
    Name: string;
    Keyword: string;
    NameRow: integer;
    NameCol: integer;
    KeywordRow: integer;
    KeywordCol: integer;
  end;

  TModuleClauseArray = array of TModuleClause;

  { What one pass over a file's physical lines yields for all six rules. }
  TFpcStyleScan = record
    Guards: TDottedGuardArray;
    GuardMentioned: boolean;
    UsesRow: integer;
    UsesCol: integer;
    Modules: TModuleClauseArray;
    // Any {$mode} and any real include, wherever in the file they sit.
    HasMode: boolean;
    HasInclude: boolean;
    // Row of the first real include, of the first interface word of a unit,
    // and of the first begin word; 0 when the file holds none.
    HeaderIncludeRow: integer;
    InterfaceRow: integer;
    BeginRow: integer;
  end;

{ ---- name and directive primitives }

function IsIdentStart(aChar: char): boolean;
begin
  Result := aChar in ['A'..'Z', 'a'..'z', '_'];
end;


function IsIdentChar(aChar: char): boolean;
begin
  Result := aChar in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;


// The part of aName after its last dot, or aName when it carries no dot.
function LastComponent(const aName: string): string;
var
  i: integer;
begin
  Result := aName;
  for i := Length(aName) downto 1 do
    if aName[i] = '.' then
    begin
      Result := Copy(aName, i + 1, Length(aName) - i);
      Exit;
    end;
end;


// True iff aDotted is a dotted spelling of aPlain: equal, or equal once the
// namespace prefix is dropped.
function Corresponds(const aDotted, aPlain: string): boolean;
begin
  Result := SameText(aDotted, aPlain) or SameText(LastComponent(aDotted), aPlain);
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


// True iff aWord opens a module clause, and of which kind.
function ModuleKeyword(const aWord: string; out aKind: TModuleKind): boolean;
begin
  Result := True;
  if SameText(aWord, 'unit') then
    aKind := mkUnit
  else if SameText(aWord, 'program') then
    aKind := mkProgram
  else if SameText(aWord, 'library') then
    aKind := mkLibrary
  else
  begin
    aKind := mkUnit;
    Result := False;
  end;
end;


// True iff aWord opens a module body, which no module name may be.
function IsBodyWord(const aWord: string): boolean;
begin
  Result := SameText(aWord, 'interface') or SameText(aWord, 'implementation')
    or SameText(aWord, 'begin');
end;


// True iff an {$i}-family directive body names an include target rather than
// the IO-check switch or a compile-time macro.
function IsIncludeBody(const aBody: string): boolean;
var
  i: integer;
begin
  Result := False;
  i := 1;
  // The word is located as DirectiveWord locates it, past any lead-in the
  // scanner put in front of it.
  while (i <= Length(aBody)) and not IsIdentStart(aBody[i]) do
    Inc(i);
  while (i <= Length(aBody)) and IsIdentChar(aBody[i]) do
    Inc(i);
  while (i <= Length(aBody)) and (aBody[i] in [' ', #9]) do
    Inc(i);
  if i > Length(aBody) then
    Exit;
  Result := not (aBody[i] in ['-', '+', '%']);
end;


// True iff aBody carries FPC_DOTTEDUNITS as a whole word.
function MentionsGuardSymbol(const aBody: string): boolean;
var
  i: integer;
  lUpper: string;
begin
  Result := False;
  lUpper := UpperCase(aBody);
  i := Pos(cGuardSymbol, lUpper);
  while i > 0 do
  begin
    if ((i = 1) or not IsIdentChar(lUpper[i - 1]))
      and ((i + Length(cGuardSymbol) > Length(lUpper))
      or not IsIdentChar(lUpper[i + Length(cGuardSymbol)])) then
    begin
      Result := True;
      Exit;
    end;
    i := Pos(cGuardSymbol, lUpper, i + 1);
  end;
end;


{ ---- the single line-text pass }

// Yields every FPC_DOTTEDUNITS-guarded uses clause of aLines, whether any
// directive names the guard, the position of the first uses keyword, every
// module clause, and the mode, include and interface facts.
function ScanFpcStyle(const aLines: TFpSonarStringArray): TFpcStyleScan;
type
  TScanState = (ssNormal, ssBrace, ssParen);
var
  lState: TScanState;
  lIsDirective, lInClause, lInverted, lBranchDone: boolean;
  lHeaderIntact, lHasUnit: boolean;
  lBody, lLine: string;
  lRow, lCol, lLen, lEnd, lStart: integer;
  lGuard, lNest, lBranch, lClauseBase, lPending, lDirRow: integer;

  procedure AddRef(const aName: string; aRow, aCol: integer);
  var
    lRef: TDottedUnitRef;
  begin
    lRef.Name := aName;
    lRef.Row := aRow;
    lRef.Col := aCol;
    if (lBranch = 0) <> lInverted then
    begin
      SetLength(Result.Guards[lGuard].Dotted,
        Length(Result.Guards[lGuard].Dotted) + 1);
      Result.Guards[lGuard].Dotted[High(Result.Guards[lGuard].Dotted)] := lRef;
    end
    else
    begin
      SetLength(Result.Guards[lGuard].Plain,
        Length(Result.Guards[lGuard].Plain) + 1);
      Result.Guards[lGuard].Plain[High(Result.Guards[lGuard].Plain)] := lRef;
    end;
  end;

  procedure OpenGuard(aInverted: boolean);
  begin
    SetLength(Result.Guards, Length(Result.Guards) + 1);
    lGuard := High(Result.Guards);
    Result.Guards[lGuard].ClauseAtOpen := lInClause;
    lInverted := aInverted;
    lBranch := 0;
    lNest := 0;
    lBranchDone := False;
  end;

  procedure MarkOpaque;
  begin
    if lGuard >= 0 then
      Result.Guards[lGuard].Opaque := True;
  end;

  procedure SwitchBranch;
  begin
    if (lGuard < 0) or (lNest > 0) then
      Exit;
    if lBranch = 0 then
    begin
      lBranch := 1;
      Result.Guards[lGuard].HasElse := True;
      lBranchDone := False;
      // A branch carrying its own uses keyword reopens the clause for itself.
      lInClause := Result.Guards[lGuard].ClauseAtOpen;
    end
    else
      Result.Guards[lGuard].Opaque := True;
  end;

  procedure CloseGuard;
  begin
    if lGuard < 0 then
      Exit;
    if lNest > 0 then
    begin
      Dec(lNest);
      Exit;
    end;
    Result.Guards[lGuard].Closed := True;
    if Result.Guards[lGuard].EndsClause then
      lInClause := False;
    lGuard := -1;
    lBranchDone := False;
  end;

  procedure HandleDirective(const aBody: string);
  var
    lFirst: string;
  begin
    if MentionsGuardSymbol(aBody) then
      Result.GuardMentioned := True;
    lFirst := LowerCase(DirectiveWord(aBody, 1));
    if Copy(lFirst, 1, 2) = 'if' then
    begin
      if ((lFirst = 'ifdef') or (lFirst = 'ifndef')) and (lGuard < 0)
        and SameText(DirectiveWord(aBody, 2), cGuardSymbol) then
        OpenGuard(lFirst = 'ifndef')
      else if lGuard >= 0 then
      begin
        Inc(lNest);
        Result.Guards[lGuard].Opaque := True;
      end;
    end
    else if lFirst = 'else' then
      SwitchBranch
    else if lFirst = 'endif' then
      CloseGuard
    else if (lFirst = 'elseif') or (lFirst = 'elsec') or (lFirst = 'elifc') then
      MarkOpaque
    else if (lFirst = 'i') or (lFirst = 'include') then
    begin
      MarkOpaque;
      if IsIncludeBody(aBody) then
      begin
        Result.HasInclude := True;
        if Result.HeaderIncludeRow = 0 then
          Result.HeaderIncludeRow := lDirRow;
      end;
    end
    else if lFirst = 'mode' then
      Result.HasMode := True;
  end;

  procedure OpenModule(const aWord: string; aKind: TModuleKind;
    aRow, aCol: integer);
  begin
    SetLength(Result.Modules, Length(Result.Modules) + 1);
    lPending := High(Result.Modules);
    Result.Modules[lPending].Kind := aKind;
    Result.Modules[lPending].Name := '';
    Result.Modules[lPending].Keyword := aWord;
    Result.Modules[lPending].NameRow := 0;
    Result.Modules[lPending].NameCol := 0;
    Result.Modules[lPending].KeywordRow := aRow;
    Result.Modules[lPending].KeywordCol := aCol;
    lHasUnit := lHasUnit or (aKind = mkUnit);
  end;

  procedure HandleModuleHeader(const aWord: string; aRow, aCol: integer);
  var
    lKind: TModuleKind;
  begin
    if (Result.InterfaceRow = 0) and lHasUnit
      and SameText(aWord, 'interface') then
      Result.InterfaceRow := aRow;
    if not lHeaderIntact then
      Exit;
    if lPending >= 0 then
    begin
      if IsBodyWord(aWord) then
      begin
        lHeaderIntact := False;
        Exit;
      end;
      Result.Modules[lPending].Name := aWord;
      Result.Modules[lPending].NameRow := aRow;
      Result.Modules[lPending].NameCol := aCol;
      lPending := -1;
    end
    else if ModuleKeyword(aWord, lKind) then
      OpenModule(aWord, lKind, aRow, aCol)
    else
      lHeaderIntact := False;
  end;

  procedure HandleWord(const aWord: string; aRow, aCol: integer);
  begin
    if (Result.BeginRow = 0) and SameText(aWord, 'begin') then
      Result.BeginRow := aRow;
    HandleModuleHeader(aWord, aRow, aCol);
    if not lInClause then
    begin
      if SameText(aWord, 'uses') then
      begin
        lInClause := True;
        // An open guard is the clause's own, so the base must not skip past it.
        if lGuard >= 0 then
          lClauseBase := lGuard
        else
          lClauseBase := Length(Result.Guards);
        if Result.UsesRow = 0 then
        begin
          Result.UsesRow := aRow;
          Result.UsesCol := aCol;
        end;
      end;
      Exit;
    end;
    if SameText(aWord, 'in') then
      Exit;
    if (lGuard >= 0) and not lBranchDone then
      AddRef(aWord, aRow, aCol);
  end;

  procedure HandleSemicolon;
  begin
    if not lInClause then
      Exit;
    if lGuard >= 0 then
    begin
      lBranchDone := True;
      Result.Guards[lGuard].EndsClause := True;
    end
    else
      lInClause := False;
  end;

begin
  Result.Guards := nil;
  Result.GuardMentioned := False;
  Result.UsesRow := 0;
  Result.UsesCol := 0;
  Result.Modules := nil;
  Result.HasMode := False;
  Result.HasInclude := False;
  Result.HeaderIncludeRow := 0;
  Result.InterfaceRow := 0;
  Result.BeginRow := 0;
  lHeaderIntact := True;
  lHasUnit := False;
  lPending := -1;
  lDirRow := 0;
  lState := ssNormal;
  lIsDirective := False;
  lInClause := False;
  lInverted := False;
  lBranchDone := False;
  lBody := '';
  lGuard := -1;
  lNest := 0;
  lBranch := 0;
  lClauseBase := 0;
  for lRow := 0 to High(aLines) do
  begin
    lLine := aLines[lRow];
    lLen := Length(lLine);
    lCol := 1;
    while lCol <= lLen do
      case lState of
        ssBrace:
          begin
            lEnd := Pos('}', lLine, lCol);
            if lEnd = 0 then
            begin
              if lIsDirective then
                lBody := lBody + Copy(lLine, lCol, lLen - lCol + 1);
              lCol := lLen + 1;
            end
            else
            begin
              if lIsDirective then
                lBody := lBody + Copy(lLine, lCol, lEnd - lCol);
              lState := ssNormal;
              lCol := lEnd + 1;
              if lIsDirective then
              begin
                lIsDirective := False;
                HandleDirective(lBody);
              end;
            end;
          end;
        ssParen:
          begin
            lEnd := Pos('*)', lLine, lCol);
            if lEnd = 0 then
            begin
              if lIsDirective then
                lBody := lBody + Copy(lLine, lCol, lLen - lCol + 1);
              lCol := lLen + 1;
            end
            else
            begin
              if lIsDirective then
                lBody := lBody + Copy(lLine, lCol, lEnd - lCol);
              lState := ssNormal;
              lCol := lEnd + 2;
              if lIsDirective then
              begin
                lIsDirective := False;
                HandleDirective(lBody);
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
              lBody := '';
              lDirRow := lRow + 1;
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
              lBody := '';
              lDirRow := lRow + 1;
              if lIsDirective then
                Inc(lCol, 3)
              else
                Inc(lCol, 2);
              lState := ssParen;
            end
            else if lLine[lCol] = '''' then
            begin
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
            else if IsIdentStart(lLine[lCol]) then
            begin
              lStart := lCol;
              // A dot continues a unit name only when an identifier follows it.
              while lCol <= lLen do
                if IsIdentChar(lLine[lCol]) then
                  Inc(lCol)
                else if (lLine[lCol] = '.') and (lCol < lLen)
                  and IsIdentStart(lLine[lCol + 1]) then
                  Inc(lCol)
                else
                  Break;
              HandleWord(Copy(lLine, lStart, lCol - lStart), lRow + 1, lStart);
            end
            else
            begin
              if lLine[lCol] = ';' then
                HandleSemicolon;
              Inc(lCol);
            end;
          end;
      end;
    if (lState <> ssNormal) and lIsDirective then
      lBody := lBody + ' ';
  end;
  // A clause the scan never saw terminated yields no comparison.
  if lInClause then
    SetLength(Result.Guards, lClauseBase);
end;


{ ---- pairing }

// True iff the two branches hold corresponding units in some order, which makes
// any positional difference a reordering rather than a defect.
function BranchesCorrespondAsMultisets(const aGuard: TDottedGuard): boolean;
var
  i, j: integer;
  lUsed, lPaired: array of boolean;
begin
  Result := False;
  if Length(aGuard.Dotted) <> Length(aGuard.Plain) then
    Exit;
  SetLength(lUsed, Length(aGuard.Plain));
  SetLength(lPaired, Length(aGuard.Dotted));
  // Exact names first, so a branch listing both X and Api.X keeps its pairing.
  for i := 0 to High(aGuard.Dotted) do
    for j := 0 to High(aGuard.Plain) do
      if not lUsed[j]
        and SameText(aGuard.Dotted[i].Name, aGuard.Plain[j].Name) then
      begin
        lUsed[j] := True;
        lPaired[i] := True;
        Break;
      end;
  for i := 0 to High(aGuard.Dotted) do
  begin
    if lPaired[i] then
      Continue;
    for j := 0 to High(aGuard.Plain) do
      if not lUsed[j]
        and SameText(LastComponent(aGuard.Dotted[i].Name),
        aGuard.Plain[j].Name) then
      begin
        lUsed[j] := True;
        lPaired[i] := True;
        Break;
      end;
    if not lPaired[i] then
      Exit;
  end;
  Result := True;
end;


// True iff a guard's branch lists are trustworthy and are not already known to
// correspond.
function GuardMayReport(const aGuard: TDottedGuard): boolean;
begin
  Result := aGuard.Closed and aGuard.HasElse and not aGuard.Opaque
    and not BranchesCorrespondAsMultisets(aGuard);
end;


// True iff aName has a counterpart in aOpposite, which makes a tail entry a
// duplicate rather than a unit the other branch omits.
function HasCounterpart(const aName: string;
  const aOpposite: TDottedUnitRefArray; aIsDotted: boolean): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to High(aOpposite) do
    if (aIsDotted and Corresponds(aName, aOpposite[i].Name))
      or (not aIsDotted and Corresponds(aOpposite[i].Name, aName)) then
      Exit;
  Result := False;
end;


// How many leading pairs of the two branches the scan can line up.
function SharedLength(const aGuard: TDottedGuard): integer;
begin
  Result := Length(aGuard.Dotted);
  if Length(aGuard.Plain) < Result then
    Result := Length(aGuard.Plain);
end;


// True iff every pair up to SharedLength corresponds, which is what makes the
// remaining tail entries identifiable as the unpaired ones.
function PrefixIsAligned(const aGuard: TDottedGuard): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to SharedLength(aGuard) - 1 do
    if not Corresponds(aGuard.Dotted[i].Name, aGuard.Plain[i].Name) then
    begin
      Result := False;
      Exit;
    end;
end;


// True iff aList holds aName, compared case-insensitively.
function NameIn(const aList: TDottedUnitRefArray; const aName: string): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to High(aList) do
    if SameText(aList[i].Name, aName) then
      Exit;
  Result := False;
end;


{ TRuleDottedUnitsBranchesInconsistent }

procedure TRuleDottedUnitsBranchesInconsistent.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TFpcStyleScan;
  i: integer;

  procedure EmitTail(const aList, aOpposite: TDottedUnitRefArray;
    aFrom: integer; aIsDotted: boolean);
  var
    k: integer;
  begin
    for k := aFrom to High(aList) do
      if not HasCounterpart(aList[k].Name, aOpposite, aIsDotted) then
        aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
          aList[k].Row, aList[k].Col, aList[k].Row,
          aList[k].Col + Length(aList[k].Name) - 1,
          FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
          FMetadata.MessageKey, [aList[k].Name], aList[k].Name);
  end;

begin
  lScan := ScanFpcStyle(aContext.Lines);
  for i := 0 to High(lScan.Guards) do
  begin
    if not GuardMayReport(lScan.Guards[i]) then
      Continue;
    if Length(lScan.Guards[i].Dotted) = Length(lScan.Guards[i].Plain) then
      Continue;
    if not PrefixIsAligned(lScan.Guards[i]) then
      Continue;
    if Length(lScan.Guards[i].Dotted) > SharedLength(lScan.Guards[i]) then
      EmitTail(lScan.Guards[i].Dotted, lScan.Guards[i].Plain,
        SharedLength(lScan.Guards[i]), True)
    else
      EmitTail(lScan.Guards[i].Plain, lScan.Guards[i].Dotted,
        SharedLength(lScan.Guards[i]), False);
  end;
end;


{ TRuleMissingDottedUnitsGuard }

procedure TRuleMissingDottedUnitsGuard.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TFpcStyleScan;
begin
  lScan := ScanFpcStyle(aContext.Lines);
  if lScan.GuardMentioned or (lScan.UsesRow = 0) then
    Exit;
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
    lScan.UsesRow, lScan.UsesCol, lScan.UsesRow, lScan.UsesCol + 3,
    FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
    FMetadata.MessageKey, [], 'uses');
end;


{ TRuleDottedUnitAliasMismatch }

procedure TRuleDottedUnitAliasMismatch.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TFpcStyleScan;
  lGuard: TDottedGuard;
  lRef: TDottedUnitRef;
  lExpected: string;
  i, k: integer;
begin
  lScan := ScanFpcStyle(aContext.Lines);
  for i := 0 to High(lScan.Guards) do
  begin
    lGuard := lScan.Guards[i];
    if not (lGuard.Closed and lGuard.HasElse and not lGuard.Opaque) then
      Continue;
    if Length(lGuard.Dotted) <> Length(lGuard.Plain) then
      Continue;
    for k := 0 to High(lGuard.Dotted) do
    begin
      if not LookupNamespace(lGuard.Plain[k].Name, lExpected) then
        Continue;
      // The alias satisfies the pair from any position in the dotted branch.
      if NameIn(lGuard.Dotted, lExpected) then
        Continue;
      lRef := lGuard.Dotted[k];
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lRef.Row, lRef.Col, lRef.Row, lRef.Col + Length(lRef.Name) - 1,
        FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
        FMetadata.MessageKey,
        [lGuard.Plain[k].Name, lExpected], lRef.Name);
    end;
  end;
end;


{ ---- module-clause primitives }

// The index of the first clause of aScan carrying a name, or -1.
function FirstNamedModule(const aScan: TFpcStyleScan): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to High(aScan.Modules) do
    if aScan.Modules[i].Name <> '' then
      Exit(i);
end;


// The last line aClause's leading comment can occupy: the interface keyword for
// a unit, the first begin for a program or a library, 0 when undelimited.
function CopyrightWindow(const aScan: TFpcStyleScan;
  const aClause: TModuleClause; aLineCount: integer): integer;
begin
  if aClause.Kind = mkUnit then
    Result := aScan.InterfaceRow
  else if aScan.BeginRow > 0 then
    Result := aScan.BeginRow
  else
    Result := aLineCount;
end;


// True iff COPYING.FPC occurs in lines 1..aLast of aLines.
function HeaderCarriesMarker(const aLines: TFpSonarStringArray;
  aLast: integer): boolean;
var
  i: integer;
begin
  Result := False;
  if aLast > Length(aLines) then
    aLast := Length(aLines);
  for i := 1 to aLast do
    if Pos(cCopyrightMarker, UpperCase(aLines[i - 1])) > 0 then
      Exit(True);
end;


// Reports aClause's module keyword, whose spelling is the fingerprint snippet.
procedure EmitAtKeyword(const aRule: TRuleBase; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; const aClause: TModuleClause);
var
  lMeta: TRuleMetadata;
begin
  lMeta := aRule.Metadata;
  aCollector.AddIssue(lMeta.RuleId, aContext.FileName,
    aClause.KeywordRow, aClause.KeywordCol, aClause.KeywordRow,
    aClause.KeywordCol + Length(aClause.Keyword) - 1,
    lMeta.Severity, lMeta.Category, lMeta.DefaultConfidence, lMeta.MessageKey,
    [aClause.Name], aClause.Keyword);
end;


{ TRuleUnitFileNameCaseMismatch }

procedure TRuleUnitFileNameCaseMismatch.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TFpcStyleScan;
  lBase, lFile: string;
  i, lFirst: integer;
begin
  lFile := ExtractFileName(aContext.FileName);
  if lFile = '' then
    Exit;
  lBase := ChangeFileExt(lFile, '');
  lScan := ScanFpcStyle(aContext.Lines);
  lFirst := -1;
  for i := 0 to High(lScan.Modules) do
  begin
    // A .lpr legitimately carries a mixed-case project name.
    if (lScan.Modules[i].Kind <> mkUnit) or (lScan.Modules[i].Name = '') then
      Continue;
    // The ordinary layout, then the generated namespaced mirror.
    if (lBase = LowerCase(lScan.Modules[i].Name))
      or (lBase = lScan.Modules[i].Name) then
      Exit;
    if lFirst < 0 then
      lFirst := i;
  end;
  if lFirst < 0 then
    Exit;
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
    lScan.Modules[lFirst].NameRow, lScan.Modules[lFirst].NameCol,
    lScan.Modules[lFirst].NameRow,
    lScan.Modules[lFirst].NameCol + Length(lScan.Modules[lFirst].Name) - 1,
    FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
    FMetadata.MessageKey, [lFile, lScan.Modules[lFirst].Name],
    lScan.Modules[lFirst].Name);
end;


{ TRuleMissingModeDirective }

procedure TRuleMissingModeDirective.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TFpcStyleScan;
  lFirst: integer;
begin
  lScan := ScanFpcStyle(aContext.Lines);
  if lScan.HasMode or lScan.HasInclude then
    Exit;
  lFirst := FirstNamedModule(lScan);
  if lFirst < 0 then
    Exit;
  EmitAtKeyword(Self, aContext, aCollector, lScan.Modules[lFirst]);
end;


{ TRuleMissingCopyrightHeader }

procedure TRuleMissingCopyrightHeader.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lScan: TFpcStyleScan;
  i, lFirst, lWindow: integer;
begin
  lScan := ScanFpcStyle(aContext.Lines);
  lFirst := -1;
  for i := 0 to High(lScan.Modules) do
  begin
    if lScan.Modules[i].Name = '' then
      Continue;
    if lFirst < 0 then
      lFirst := i;
    lWindow := CopyrightWindow(lScan, lScan.Modules[i], Length(aContext.Lines));
    if HeaderCarriesMarker(aContext.Lines, lWindow) then
      Exit;
    // A window the scan cannot delimit, and one an include precedes, are both
    // absences the rule cannot measure.
    if (lWindow <= 0) or ((lScan.HeaderIncludeRow > 0)
      and (lScan.HeaderIncludeRow <= lWindow)) then
      Exit;
  end;
  if lFirst < 0 then
    Exit;
  EmitAtKeyword(Self, aContext, aCollector, lScan.Modules[lFirst]);
end;


// Emits one issue per use in aUses whose declaration carries aHint.
procedure EmitHintedUses(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  const aUses: TFpSonarHintedUseArray; aHint: TPasMemberHint);

var
  i, lLine: integer;

begin
  for i := 0 to High(aUses) do
  begin
    if not (aHint in aUses[i].Hints) then
      Continue;
    lLine := aContext.Resolver.SourceRow(aUses[i].Node);
    aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lLine, 1, lLine, 1,
      aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
      [aUses[i].Name], '');
  end;
end;


{ TRuleDeprecatedSymbolUsed }

procedure TRuleDeprecatedSymbolUsed.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lUses: TFpSonarHintedUseArray;
  lModuleHints: TPasMemberHints;

begin
  if not aContext.Resolver.TryHintedSymbolUses(lUses, lModuleHints) then
    Exit;
  EmitHintedUses(FMetadata, aContext, aCollector, lUses, hDeprecated);
end;


{ TRulePlatformSymbolUsedInPortableUnit }

procedure TRulePlatformSymbolUsedInPortableUnit.Apply(
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);

var
  lUses: TFpSonarHintedUseArray;
  lModuleHints: TPasMemberHints;

begin
  if not aContext.Resolver.TryHintedSymbolUses(lUses, lModuleHints) then
    Exit;
  if hPlatform in lModuleHints then
    Exit;
  EmitHintedUses(FMetadata, aContext, aCollector, lUses, hPlatform);
end;


{ TRuleExperimentalSymbolUsed }

procedure TRuleExperimentalSymbolUsed.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lUses: TFpSonarHintedUseArray;
  lModuleHints: TPasMemberHints;

begin
  if not aContext.Resolver.TryHintedSymbolUses(lUses, lModuleHints) then
    Exit;
  EmitHintedUses(FMetadata, aContext, aCollector, lUses, hExperimental);
end;


// The number of line endings embedded in aText.
function EmbeddedLineEnds(const aText: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(aText) do
    if (aText[i] = #10)
      or ((aText[i] = #13)
      and ((i = Length(aText)) or (aText[i + 1] <> #10))) then
      Inc(Result);
end;


// True iff aValue occurs in aArr.
function InWordSet(const aValue: string; const aArr: array of string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(aArr) to High(aArr) do
    if aArr[i] = aValue then
      Exit(True);
end;


// True iff comment text aText is a conditional-compilation directive.
function IsConditionalDirective(const aText: string): boolean;
const
  cConditional: array[0..8] of string = ('if', 'ifdef', 'ifndef', 'ifopt',
    'else', 'elseif', 'elsec', 'endif', 'ifend');
var
  i: integer;
begin
  Result := False;
  if (aText = '') or (aText[1] <> '$') then
    Exit;
  i := 2;
  while (i <= Length(aText)) and (aText[i] in ['A'..'Z', 'a'..'z']) do
    Inc(i);
  Result := InWordSet(LowerCase(Copy(aText, 2, i - 2)), cConditional);
end;


type
  // A declaration scope on the member walk's stack.
  TFpcStyleFrameKind = (ffBase, // unit/decl level
    ffStmt, // a begin/asm/try/repeat/case/initialization block
    ffType  // a class/record/object/interface type body
    );

  TFpcStyleFrame = record
    Kind: TFpcStyleFrameKind;
    // The keyword that opened this frame.
    Opener: string;
    // The visibility specifier in force in this type body, '' when none yet.
    CurVis: string;
  end;

  // Undocumented members, each reported at the keyword that declares it.
  TFpcStyleMemberArray = array of TModuleClause;

  TFpcStyleIndexArray = array of integer;

{ Walks this file's own tokens once, tracking type bodies and the visibility
  specifier in force in each, and returns every method (with aWantProperty,
  every property) declared under an explicit public/protected (public/published)
  specifier that carries no documentation comment ending on the line above it. }
function ScanUndocumentedMembers(const aContext: TRuleContext;
  aWantProperty: boolean): TFpcStyleMemberArray;
const
  cSections: array[0..5] of string =
    ('const', 'type', 'var', 'threadvar', 'resourcestring', 'label');
  cRoutines: array[0..4] of string =
    ('procedure', 'function', 'constructor', 'destructor', 'operator');
  cVisibility: array[0..4] of string =
    ('private', 'protected', 'public', 'published', 'automated');
var
  lOwn, lSig: TFpcStyleIndexArray;
  lOwnCount, lSigCount: integer;
  lStack: array of TFpcStyleFrame;
  lParen, lBrack, i: integer;
  lBroken, lSkipVis, lIsVisSpec, lStartsVis, lDeclStart: boolean;
  ltx, lw, lpv, lnx, lVisLow, lCanonVis: string;

  // The effective lexeme of significant token aPos; '' out of range.
  function RawText(aPos: integer): string;
  var
    lTok: TFpSonarToken;
  begin
    Result := '';
    if (aPos < 0) or (aPos >= lSigCount) then
      Exit;
    lTok := aContext.Tokens[lOwn[lSig[aPos]]];
    if lTok.Text <> '' then
      Result := lTok.Text
    else
      Result := lTok.Punct;
  end;

  // The lowercased lexeme of significant token aPos; '' out of range.
  function LowText(aPos: integer): string;
  begin
    Result := LowerCase(RawText(aPos));
  end;

  // True iff significant token aPos is an identifier.
  function IsIdent(aPos: integer): boolean;
  var
    lText: string;
  begin
    Result := False;
    if (aPos < 0) or (aPos >= lSigCount) then
      Exit;
    if aContext.Tokens[lOwn[lSig[aPos]]].IsKeyword then
      Exit;
    lText := aContext.Tokens[lOwn[lSig[aPos]]].Text;
    Result := (lText <> '') and (lText[1] in ['A'..'Z', 'a'..'z', '_']);
  end;

  // Pushes a fresh frame of aKind opened by aOpener.
  procedure PushFrame(aKind: TFpcStyleFrameKind; const aOpener: string);
  begin
    SetLength(lStack, Length(lStack) + 1);
    lStack[High(lStack)].Kind := aKind;
    lStack[High(lStack)].Opener := aOpener;
    lStack[High(lStack)].CurVis := '';
  end;

  // True iff aVis is a specifier the requested member kind is documented under.
  function VisibilityReports(const aVis: string): boolean;
  begin
    if aWantProperty then
      Result := (aVis = 'public') or (aVis = 'published')
    else
      Result := (aVis = 'public') or (aVis = 'protected');
  end;

  { The significant position of the first token of the declaration reported at
    aPos: the start of its attribute run when it carries one. }
  function DeclStart(aPos: integer): integer;
  var
    k, lDepth: integer;
  begin
    Result := aPos;
    k := Result - 1;
    while (k >= 0) and (RawText(k) = ']') do
    begin
      lDepth := 0;
      while k >= 0 do
      begin
        if RawText(k) = ']' then
          Inc(lDepth)
        else if RawText(k) = '[' then
        begin
          Dec(lDepth);
          if lDepth = 0 then
            Break;
        end;
        Dec(k);
      end;
      if k < 0 then
        Exit;
      Result := k;
      Dec(k);
    end;
  end;

  // True iff the token at own position aStart is the first thing on its row.
  function StartsItsLine(aStart: integer): boolean;
  var
    k: integer;
  begin
    Result := True;
    for k := aStart - 1 downto 0 do
      if not aContext.Tokens[lOwn[k]].IsTrivia then
        Exit(aContext.Tokens[lOwn[k]].Row
          < aContext.Tokens[lOwn[aStart]].Row);
  end;

  { True iff a non-directive comment starting its own line ends on the line
    above the token at own position aStart; a directive comment in between is
    transparent, and a conditional directive breaking the chain abstains. }
  function IsDocumented(aStart: integer): boolean;
  var
    k, lWant, lSameRow, lEnd: integer;
    lTok: TFpSonarToken;
    lConditional: boolean;
  begin
    Result := False;
    lConditional := False;
    lSameRow := -1;
    lWant := aContext.Tokens[lOwn[aStart]].Row - 1;
    for k := aStart - 1 downto 0 do
    begin
      lTok := aContext.Tokens[lOwn[k]];
      if lTok.IsComment then
      begin
        lEnd := lTok.Row + EmbeddedLineEnds(lTok.Text);
        // lSameRow admits a second directive sharing the row of the first.
        if (lEnd <> lWant) and (lEnd <> lSameRow) then
        begin
          // An inactive branch emits no token, so its rows cannot chain (INV-2).
          if lConditional or IsConditionalDirective(lTok.Text) then
            Exit(True);
          Exit;
        end;
        if (lTok.Text <> '') and (lTok.Text[1] = '$') then
        begin
          lConditional := lConditional or IsConditionalDirective(lTok.Text);
          lSameRow := lTok.Row;
          lWant := lTok.Row - 1;
        end
        else
          Exit(StartsItsLine(k));
      end
      else if not lTok.IsTrivia then
        Exit;
    end;
  end;

  { Records the member the keyword at significant position aKeyword declares
    and the significant token after aName names, when it is undocumented. }
  procedure AddMember(aKeyword, aName: integer);
  var
    lTok: TFpSonarToken;
  begin
    if not IsIdent(aName + 1) then
      Exit;
    if IsDocumented(lSig[aKeyword])
      or IsDocumented(lSig[DeclStart(aKeyword)]) then
      Exit;
    lTok := aContext.Tokens[lOwn[lSig[aKeyword]]];
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Name := RawText(aName + 1);
    Result[High(Result)].Keyword := lTok.Text;
    Result[High(Result)].KeywordRow := lTok.Row;
    Result[High(Result)].KeywordCol := lTok.Col;
  end;

begin
  SetLength(Result, 0);
  // This file's own tokens only; an include's rows are another file's.
  SetLength(lOwn, Length(aContext.Tokens));
  lOwnCount := 0;
  for i := 0 to High(aContext.Tokens) do
    if aContext.Tokens[i].FileName = aContext.FileName then
    begin
      lOwn[lOwnCount] := i;
      Inc(lOwnCount);
    end;
  SetLength(lOwn, lOwnCount);

  // Significant tokens = non-trivia, non-empty.
  SetLength(lSig, lOwnCount);
  lSigCount := 0;
  for i := 0 to lOwnCount - 1 do
    if (not aContext.Tokens[lOwn[i]].IsTrivia)
      and ((aContext.Tokens[lOwn[i]].Text <> '')
      or (aContext.Tokens[lOwn[i]].Punct <> '')) then
    begin
      lSig[lSigCount] := i;
      Inc(lSigCount);
    end;
  SetLength(lSig, lSigCount);

  // The implicit bottom frame is the unit/decl level.
  SetLength(lStack, 1);
  lStack[0].Kind := ffBase;
  lStack[0].Opener := '';
  lStack[0].CurVis := '';
  lParen := 0;
  lBrack := 0;
  lBroken := False;
  lSkipVis := False;

  for i := 0 to lSigCount - 1 do
  begin
    if lBroken then
      Break;
    ltx := RawText(i);
    lw := '';
    if aContext.Tokens[lOwn[lSig[i]]].IsKeyword then
      lw := LowerCase(aContext.Tokens[lOwn[lSig[i]]].Text);
    // Visibility words are identifiers, not reserved words.
    lVisLow := '';
    if lw = '' then
      lVisLow := LowerCase(ltx);

    if (lParen = 0) and (lBrack = 0)
      and (lStack[High(lStack)].Kind = ffType) then
    begin
      lIsVisSpec := False;
      lStartsVis := False;
      lCanonVis := '';
      if lSkipVis then
      begin
        lSkipVis := False;
        lIsVisSpec := True;
      end
      else
      begin
        lpv := LowText(i - 1);
        // A member declaration starts after the body opener, a ';', an ancestor
        // list, an attribute run, a visibility specifier or a section keyword.
        lDeclStart := (lpv = lStack[High(lStack)].Opener) or (lpv = ';')
          or (lpv = ')') or (lpv = ']')
          or (lpv = 'abstract') or (lpv = 'sealed')
          // A helper body's header ends with 'for <target>'.
          or (LowText(i - 2) = 'for')
          or InWordSet(lpv, cVisibility) or InWordSet(lpv, cSections);
        if lDeclStart then
        begin
          if (lVisLow = 'strict')
            and InWordSet(LowText(i + 1), cVisibility) then
          begin
            lCanonVis := 'strict ' + LowText(i + 1);
            lIsVisSpec := True;
            lStartsVis := True;
            lSkipVis := True;
          end
          else if InWordSet(lVisLow, cVisibility) then
          begin
            lCanonVis := lVisLow;
            lIsVisSpec := True;
            lStartsVis := True;
          end;
        end;
        // The token after a 'class' prefix is that same member, already added.
        if lDeclStart and not lIsVisSpec and (lpv <> 'class')
          and VisibilityReports(lStack[High(lStack)].CurVis) then
        begin
          if aWantProperty then
          begin
            if lw = 'property' then
              AddMember(i, i)
            else if (lw = 'class') and (LowText(i + 1) = 'property') then
              AddMember(i, i + 1);
          end
          else if InWordSet(lw, cRoutines) then
            AddMember(i, i)
          else if ((lw = 'class') or (lw = 'generic'))
            and InWordSet(LowText(i + 1), cRoutines) then
            AddMember(i, i + 1);
        end;
      end;
      if lStartsVis then
        lStack[High(lStack)].CurVis := lCanonVis;
    end;

    if ltx = '(' then
      Inc(lParen)
    else if ltx = ')' then
    begin
      if lParen > 0 then
        Dec(lParen);
    end
    else if ltx = '[' then
      Inc(lBrack)
    else if ltx = ']' then
    begin
      if lBrack > 0 then
        Dec(lBrack);
    end
    else if (lw <> '') and (lParen = 0) then
    begin
      lpv := LowText(i - 1);
      lnx := LowText(i + 1);
      if lw = 'end' then
      begin
        if Length(lStack) > 1 then
          SetLength(lStack, Length(lStack) - 1)
        else if lnx <> '.' then
          lBroken := True;
      end
      else if lw = 'until' then
      begin
        if (Length(lStack) > 1)
          and (lStack[High(lStack)].Kind = ffStmt) then
          SetLength(lStack, Length(lStack) - 1);
      end
      else if (lw = 'begin') or (lw = 'asm') or (lw = 'try')
        or (lw = 'repeat') or (lw = 'initialization')
        or (lw = 'finalization') then
        PushFrame(ffStmt, lw)
      else if lw = 'case' then
      begin
        // A variant record's case is not closed by an end of its own.
        if lStack[High(lStack)].Kind = ffStmt then
          PushFrame(ffStmt, lw);
      end
      else if lw = 'record' then
      begin
        // A record body is never followed by one of these; a constraint is.
        if (lnx <> '>') and (lnx <> ';') and (lnx <> ',') and (lnx <> ')') then
          PushFrame(ffType, lw);
      end
      else if lw = 'object' then
      begin
        // Exclude the 'of object' method-pointer form.
        if lpv <> 'of' then
          PushFrame(ffType, lw);
      end
      else if lw = 'class' then
      begin
        if ((lpv = '=') or (lpv = 'packed') or (lpv = 'bitpacked'))
          and (lnx <> 'of') and (lnx <> ';') then
          PushFrame(ffType, lw);
      end
      else if lw = 'type' then
      begin
        // Only the 'type helper for' form opens a body.
        if (lpv = '=') and (lnx = 'helper') then
          PushFrame(ffType, lw);
      end
      else if (lw = 'interface') or (lw = 'dispinterface') then
      begin
        if (lpv = '=') and (lnx <> ';') then
          PushFrame(ffType, lw);
      end;
    end;
  end;
end;


{ TRulePublicMethodUndocumented }

procedure TRulePublicMethodUndocumented.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lMembers: TFpcStyleMemberArray;
  i: integer;

begin
  lMembers := ScanUndocumentedMembers(aContext, False);
  for i := 0 to High(lMembers) do
    EmitAtKeyword(Self, aContext, aCollector, lMembers[i]);
end;


{ TRulePublicPropertyUndocumented }

procedure TRulePublicPropertyUndocumented.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lMembers: TFpcStyleMemberArray;
  i: integer;

begin
  lMembers := ScanUndocumentedMembers(aContext, True);
  for i := 0 to High(lMembers) do
    EmitAtKeyword(Self, aContext, aCollector, lMembers[i]);
end;


// Creates a per-Apply analyzer over the context's module with the project index.
function MakeAnalyzer(const aContext: TRuleContext): TFpSonarUseAnalyzer;
begin
  Result := MakeUseAnalyzer(aContext.Module, aContext.Resolver,
    aContext.Config.UseTierResolution = utrPrefer);
  Result.ProjectIndex := aContext.ProjectIndex;
end;


{ TRuleInterfaceUsesTooBroad }

procedure TRuleInterfaceUsesTooBroad.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lAnalyzer: TFpSonarUseAnalyzer;
  lClause: TPasUsesClause;
  lUses: TPasUsesUnit;
  lNames: TStringList;
  lName: string;
  i: integer;

begin
  if (aContext.ProjectIndex = nil) or (aContext.Module = nil)
    or (aContext.Module.InterfaceSection = nil) then
    Exit;
  lAnalyzer := MakeAnalyzer(aContext);
  try
    lClause := aContext.Module.InterfaceSection.UsesClause;
    for i := 0 to High(lClause) do
    begin
      lUses := lClause[i];
      lName := lUses.Name;
      if lName = '' then
        Continue;
      // nil => the unit is no project target the index parsed.
      lNames := aContext.ProjectIndex.InterfaceNames(lName);
      if lNames = nil then
        Continue;
      if lAnalyzer.InterfaceReferencesAny(lNames) then
        Continue;
      if aContext.ProjectIndex.UnitHasOperatorOrHelper(lName) then
        Continue;
      if aContext.ProjectIndex.UnitHasInitFinal(lName) then
        Continue;
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lUses.SourceLinenumber, 1, lUses.SourceLinenumber, 1,
        FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
        FMetadata.MessageKey, [lName], lName);
    end;
  finally
    lAnalyzer.Free;
  end;
end;


{ TRuleIOResultNotChecked }

// The +/- of an {$i} switch directive body, #0 when it is no such directive.
function IoSwitchSign(const aBody: string): char;

var
  i: integer;

begin
  Result := #0;
  if not SameText(DirectiveWord(aBody, 1), 'I') then
    Exit;
  i := 1;
  while (i <= Length(aBody)) and not IsIdentStart(aBody[i]) do
    Inc(i);
  while (i <= Length(aBody)) and IsIdentChar(aBody[i]) do
    Inc(i);
  while (i <= Length(aBody)) and (aBody[i] in [' ', #9]) do
    Inc(i);
  if (i <= Length(aBody)) and (aBody[i] in ['-', '+']) then
    Result := aBody[i];
end;


{ True when aRow sits in an {$I-} region of aContext: the last {$i} switch at
  or before it turned the checks off. }
function InIoOffRegion(const aContext: TRuleContext; aRow: integer): boolean;

var
  lSign: char;
  i: integer;

begin
  Result := False;
  for i := 0 to High(aContext.Tokens) do
  begin
    // This file's own tokens only; an include's rows are another file's.
    if not aContext.Tokens[i].IsComment
      or (aContext.Tokens[i].FileName <> aContext.FileName)
      or (aContext.Tokens[i].Row > aRow)
      or (Copy(aContext.Tokens[i].Text, 1, 1) <> '$') then
      Continue;
    lSign := IoSwitchSign(aContext.Tokens[i].Text);
    if lSign <> #0 then
      Result := lSign = '-';
  end;
end;


procedure TRuleIOResultNotChecked.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarIOCheckFindingArray;
  lOk: boolean;
  lRow: integer;
  i: integer;

begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TryIOCheckFindings(lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
  begin
    lRow := aContext.Resolver.SourceRow(lFindings[i].Site);
    // A region closed before the next call leaves that call to check the result.
    if not InIoOffRegion(aContext, lRow)
      or not InIoOffRegion(aContext,
      aContext.Resolver.SourceRow(lFindings[i].Next)) then
      Continue;
    aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, lRow, 1, lRow, 1,
      FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
      FMetadata.MessageKey, [lFindings[i].Name], lFindings[i].Name);
  end;
end;


initialization
  RegisterRule(TRuleDottedUnitsBranchesInconsistent.Create(TRuleMetadata.Make(
    'DottedUnitsBranchesInconsistent', rtTok, rfLineText, sevMajor, itBug,
    cfMedium, False, cKeyBranches).WithDescription(
    'Flags a unit listed in only one branch of an FPC_DOTTEDUNITS guard.')));
  RegisterMessage(cKeyBranches, SDottedUnitsBranchesInconsistent);


  RegisterRule(TRuleMissingDottedUnitsGuard.Create(TRuleMetadata.Make(
    'MissingDottedUnitsGuard', rtTok, rfLineText, sevMinor, itCodeSmell,
    cfMedium, False, cKeyMissingGuard).WithDescription(
    'Flags a uses clause in a file with no FPC_DOTTEDUNITS guard.')));
  RegisterMessage(cKeyMissingGuard, SMissingDottedUnitsGuard);


  RegisterRule(TRuleDottedUnitAliasMismatch.Create(TRuleMetadata.Make(
    'DottedUnitAliasMismatch', rtTok, rfLineText, sevMajor, itBug,
    cfMedium, False, cKeyAlias).WithDescription(
    'Flags a dotted unit name that is not the recorded alias of its non-dotted counterpart.')));
  RegisterMessage(cKeyAlias, SDottedUnitAliasMismatch);


  RegisterRule(TRuleUnitFileNameCaseMismatch.Create(TRuleMetadata.Make(
    'UnitFileNameCaseMismatch', rtTok, rfLineText, sevMinor, itCodeSmell,
    cfHigh, False, cKeyCaseMismatch).WithDescription(
    'Flags a unit file whose name is neither the lowercase form of the unit name nor that name verbatim.')));
  RegisterMessage(cKeyCaseMismatch, SUnitFileNameCaseMismatch);


  RegisterRule(TRuleMissingModeDirective.Create(TRuleMetadata.Make(
    'MissingModeDirective', rtTok, rfLineText, sevMinor, itCodeSmell,
    cfMedium, False, cKeyMissingMode).WithDescription(
    'Flags a module whose file declares no {$mode} directive.')));
  RegisterMessage(cKeyMissingMode, SMissingModeDirective);


  RegisterRule(TRuleMissingCopyrightHeader.Create(TRuleMetadata.Make(
    'MissingCopyrightHeader', rtTok, rfLineText, sevInfo, itCodeSmell,
    cfMedium, False, cKeyMissingCopyright).WithDescription(
    'Flags a module with no COPYING.FPC reference in its leading comment.')));
  RegisterMessage(cKeyMissingCopyright, SMissingCopyrightHeader);


  RegisterRule(TRuleDeprecatedSymbolUsed.Create(TRuleMetadata.Make(
    'DeprecatedSymbolUsed', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, False, cKeyDeprecatedSymbol).WithDescription(
    'Flags a use of a symbol whose declaration carries the deprecated hint modifier.')));
  RegisterMessage(cKeyDeprecatedSymbol, SDeprecatedSymbolUsed);


  RegisterRule(TRulePlatformSymbolUsedInPortableUnit.Create(TRuleMetadata.Make(
    'PlatformSymbolUsedInPortableUnit', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfMedium, False, cKeyPlatformSymbol).WithDescription(
    'Flags a use of a platform-marked symbol in a unit that is not itself marked platform.')));
  RegisterMessage(cKeyPlatformSymbol, SPlatformSymbolUsedInPortableUnit);


  RegisterRule(TRuleExperimentalSymbolUsed.Create(TRuleMetadata.Make(
    'ExperimentalSymbolUsed', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, False, cKeyExperimentalSymbol).WithDescription(
    'Flags a use of a symbol whose declaration carries the experimental hint modifier.')));
  RegisterMessage(cKeyExperimentalSymbol, SExperimentalSymbolUsed);


  RegisterRule(TRulePublicMethodUndocumented.Create(TRuleMetadata.Make(
    'PublicMethodUndocumented', rtTok, rfTokenStream, sevInfo, itCodeSmell,
    cfMedium, False, cKeyMethodUndocumented).WithDescription(
    'Flags a public or protected method with no documentation comment on the line above it.')));
  RegisterMessage(cKeyMethodUndocumented, SPublicMethodUndocumented);


  RegisterRule(TRulePublicPropertyUndocumented.Create(TRuleMetadata.Make(
    'PublicPropertyUndocumented', rtTok, rfTokenStream, sevInfo, itCodeSmell,
    cfMedium, False, cKeyPropertyUndocumented).WithDescription(
    'Flags a public or published property with no documentation comment on the line above it.')));
  RegisterMessage(cKeyPropertyUndocumented, SPublicPropertyUndocumented);


  RegisterRule(TRuleInterfaceUsesTooBroad.Create(TRuleMetadata.Make(
    'InterfaceUsesTooBroad', rtUse, rfAst, sevMinor, itCodeSmell,
    cfMedium, False, cKeyUsesTooBroad).WithDescription(
    'Flags an interface uses entry naming a unit no interface declaration references.')));
  RegisterMessage(cKeyUsesTooBroad, SInterfaceUsesTooBroad);


  // Polarity: absence -- an IOResult read or a call between the two I/O
  // calls satisfies it.
  RegisterRule(TRuleIOResultNotChecked.Create(TRuleMetadata.Make(
    'IOResultNotChecked', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyIOResultNotChecked).WithDescription(
    'Flags an I/O call under {$I-} whose IOResult nothing reads before the '
    + 'next I/O call.')));
  RegisterMessage(cKeyIOResultNotChecked, SIOResultNotChecked);

end.
