{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Token-tier declaration, keyword and punctuation analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Tokens;

{ TOK-tier rules (rtTok / rfTokenStream) over the positioned, token stream:
  keyword casing, section combining, one-declaration-per-line,  punctuation
  semicolon comma hygiene, disabled hint/warning directives, indentation,
  commented-out code, and the NOSONAR / comment / string-literal trackers.
}

{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Ingest, FpSonar.Rules.Consts;

type
  { reports a reserved-word token written with any uppercase letter. }
  TRuleLowercaseKeywords = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a redundant 2nd+ const section header at the same scope. }
  TRuleCombineConstSections = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a redundant 2nd+ type section header at the same scope. }
  TRuleCombineTypeSections = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a redundant 2nd+ var section header at the same scope. }
  TRuleCombineVarSections = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a multi-name field declaration in a class/record/object body. }
  TRuleDeclareFieldsIndividually = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a multi-name var/threadvar declaration. }
  TRuleDeclareVariablesIndividually = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a multi-name parameter group in a routine signature. }
  TRuleDeclareParametersIndividually = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports empty '()' on a routine DECLARATION header (calls deferred). }
  TRuleNoEmptyParenthesesOnRoutines = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a stray ';' that produces an empty statement. }
  TRuleNoStraySemicolons = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a statement-block terminator whose last statement lacks ';'. }
  TRuleNoOmittedSemicolons = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a redundant/trailing comma inside a bracketed list. }
  TRuleNoExtraneousCommas = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a compiler directive that disables hints. }
  TRuleNoDisabledCompilerHints = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a compiler directive that disables warnings. }
  TRuleNoDisabledCompilerWarnings = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports an indented unit-level structure keyword. }
  TRuleNoIndentUnitLevelKeywords = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a misindented class/record visibility specifier. }
  TRuleIndentVisibilitySpecifiers = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a comment that looks like commented-out code (heuristic). }
  TRuleNoCommentedOutCode = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Tracks each inline NOSONAR suppression marker (informational). }
  TRuleTrackNoSonar = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Tracks each comment carrying a TODO/FIXME/HACK/XXX marker. }
  TRuleTrackComments = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Tracks each string/char literal carrying a built-in pattern. }
  TRuleTrackStringLiterals = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports the 2nd (and later) of two consecutive same-level visibility
    sections. }
  TRuleCombineVisibilitySections = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a visibility specifier with no member before the next specifier
    or the type body's end. }
  TRuleRemoveEmptyVisibilitySection = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { reports a var/class-var field section that declares no field. }
  TRuleRemoveEmptyFieldSection = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

const
  // Hardcoded rule-spec defaults (no per-rule param config yet — see header).
  cCheckDeclarations = True;
  cCheckCalls = False;
  cAllowSameModifierGroup = False;

  // TrackNoSonar — track every NOSONAR marker regardless of a trailing reason.
  cRequireReason = False;

  // TrackComments — the built-in, case-sensitive comment markers
  cCommentMarkers: array[0..3] of string = ('TODO', 'FIXME', 'HACK', 'XXX');

  // TrackStringLiterals — the built-in, case-sensitive string-literal pattern.
  cStringLiteralPattern = 'http://';

  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyLowercaseKeywords = 'rule.LowercaseKeywords.message';
  cKeyCombineConst = 'rule.CombineConstSections.message';
  cKeyCombineType = 'rule.CombineTypeSections.message';
  cKeyCombineVar = 'rule.CombineVarSections.message';
  cKeyDeclareFields = 'rule.DeclareFieldsIndividually.message';
  cKeyDeclareVariables = 'rule.DeclareVariablesIndividually.message';
  cKeyDeclareParameters = 'rule.DeclareParametersIndividually.message';
  cKeyNoEmptyParens = 'rule.NoEmptyParenthesesOnRoutines.message';
  cKeyNoStraySemicolons = 'rule.NoStraySemicolons.message';
  cKeyNoOmittedSemicolons = 'rule.NoOmittedSemicolons.message';
  cKeyNoExtraneousCommas = 'rule.NoExtraneousCommas.message';
  cKeyNoDisabledHints = 'rule.NoDisabledCompilerHints.message';
  cKeyNoDisabledWarnings = 'rule.NoDisabledCompilerWarnings.message';
  cKeyNoIndentUnitLevel = 'rule.NoIndentUnitLevelKeywords.message';
  cKeyIndentVisibility = 'rule.IndentVisibilitySpecifiers.message';
  cKeyNoCommentedOutCode = 'rule.NoCommentedOutCode.message';
  cKeyTrackNoSonar = 'rule.TrackNoSonar.message';
  cKeyTrackComments = 'rule.TrackComments.message';
  cKeyTrackStringLiterals = 'rule.TrackStringLiterals.message';
  // Visibility/field-section structure rules.
  cKeyCombineVisibilitySections = 'rule.CombineVisibilitySections.message';
  cKeyRemoveEmptyVisibilitySection = 'rule.RemoveEmptyVisibilitySection.message';
  cKeyRemoveEmptyFieldSection = 'rule.RemoveEmptyFieldSection.message';

  // Well-known '$WARN <target>' classification
  cWarnClassTargets: array[0..7] of string =
    ('SYMBOL_DEPRECATED', 'SYMBOL_PLATFORM', 'SYMBOL_LIBRARY',
    'SYMBOL_EXPERIMENTAL', 'IMPLICIT_VARIANTS', 'NO_RETVAL',
    'COMPARISON_TRUE', 'COMPARISON_FALSE');

type
  { A declaration scope frame on the tracker stack.}
  TFrameKind = (fkBase, // unit/decl level.
    fkStmt, // a begin/asm/try/repeat/case/initialization statement block.
    fkType  // a class/record/object/interface type body.
    );

  TFrame = record
    Kind: TFrameKind;
    Section: string;
    // The keyword that opened this frame (begin/asm/try/repeat/case/record/...)
    Opener: string;
    // The column of this type body's declaration identifier or 0 when unknown
    TypeCol: integer;
    // Per-type-body visibility/field-section state
    CurVis: string;
    // the token index of its specifier (-1 = none);
    LastVisIdx: integer;
    // whether a non-specifier token appeared since LastVisIdx
    MemberSeen: boolean;
    // the token index of an open var/class-var field section (-1 = none)
    FieldSecIdx: integer;
    //  FieldSeen whether a field appeared in it.
    FieldSeen: boolean;
  end;

  // The classifying context of a multi-name declaration (mutually exclusive).
  TDeclContext = (dcField, dcVar, dcParam);

  // A redundant section header to flag (token index into aContext.Tokens).
  TCombineHit = record
    Idx: integer;
    Kind: string;
  end;
  TCombineHitArray = array of TCombineHit;

  // A comma-separated declaration name list plus its classifying context.
  TDeclHit = record
    FirstIdx: integer;
    LastIdx: integer;
    Context: TDeclContext;
  end;
  TDeclHitArray = array of TDeclHit;

  TIntArray = array of integer;

  // The shared structural analysis of one file's token stream.
  TStruct = record
    Combines: TCombineHitArray;
    Declares: TDeclHitArray;
    EmptyParens: TIntArray;
    // the stray ';' tokens (point)
    StraySemis: TIntArray;
    // the terminator keyword tokens (range)
    OmittedSemis: TIntArray;
    // the extraneous ',' tokens (point)
    ExtraCommas: TIntArray;
    // the indented unit-level keyword (range)
    IndentKeywords: TIntArray;
    // the misindented visibility word (range)
    VisibilitySpecs: TIntArray;
    // 2nd+ specifier of a same-level run
    VisCombines: TCombineHitArray;
    // a specifier with no member after it
    EmptyVis: TCombineHitArray;
    // a field section closed with no field
    EmptyFieldSec: TIntArray;
  end;

  { shared structural pass over the significant token stream  }

{ Walks the significant tokens (trivia + EOF dropped) once, returning:
 - the edundant section headers (combine rules),
 - the multi-name declarations (declare rules),
 - the empty routine-declaration parens
 - the stray ';', omitted ';' terminators, extraneous ',' and indented unit-level keywords.
}
function AnalyzeStructure(const aContext: TRuleContext): TStruct;
const
  cSections: array[0..5] of string =
    ('const', 'type', 'var', 'threadvar', 'resourcestring', 'label');
  cRoutines: array[0..4] of string =
    ('procedure', 'function', 'constructor', 'destructor', 'operator');
  cVisibility: array[0..3] of string =
    ('private', 'protected', 'public', 'published');
  cTypeOpeners: array[0..4] of string =
    ('class', 'record', 'object', 'interface', 'dispinterface');
var
  lSig: TIntArray;
  lSigCount: integer;
  lStack: array of TFrame;
  lParen, lBrack, lParamBase, lTypeCol: integer;
  lExpectParams, lInParamList, lBroken, lSkipNextVis: boolean;
  i, j, lCount, lFirstSig, lLastSig, lKwCol: integer;
  lExpectId, lSeenRoutine: boolean;
  lPopKind: TFrameKind;
  ltx, lw, lpv, lnx, lBoundary, lVisLow: string;
  // Visibility/field-section detection.
  lMySkipVis, lIsVisSpec, lStartsVis, lDeclStart: boolean;
  lCanonVis, lpv2: string;

// Effective lexeme of significant token aPos
  function RawText(aPos: integer): string;
  begin
    if (aPos < 0) or (aPos >= lSigCount) then
      Result := ''
    else if aContext.Tokens[lSig[aPos]].Text <> '' then
      Result := aContext.Tokens[lSig[aPos]].Text
    else
      Result := aContext.Tokens[lSig[aPos]].Punct;
  end;

  // Lowercased keyword text of significant token aPos; '' if out of range.
  function LowText(aPos: integer): string;
  begin
    if (aPos < 0) or (aPos >= lSigCount) then
      Result := ''
    else if aContext.Tokens[lSig[aPos]].IsKeyword then
      Result := LowerCase(aContext.Tokens[lSig[aPos]].Text)
    else
      Result := RawText(aPos);
  end;

  // True iff significant token aPos is an identifier.
  function IsIdent(aPos: integer): boolean;
  var
    lText: string;
  begin
    Result := False;
    if (aPos < 0) or (aPos >= lSigCount) then
      Exit;
    if aContext.Tokens[lSig[aPos]].IsKeyword then
      Exit;
    lText := aContext.Tokens[lSig[aPos]].Text;
    if Length(lText) = 0 then
      Exit;
    Result := lText[1] in ['A'..'Z', 'a'..'z', '_'];
  end;

  // True iff aValue is a member of aArr.
  function InSet(const aValue: string; const aArr: array of string): boolean;
  var
    k: integer;
  begin
    Result := False;
    for k := Low(aArr) to High(aArr) do
      if aArr[k] = aValue then
      begin
        Result := True;
        Exit;
      end;
  end;

  // Pushes a fresh frame of aKind opened by aOpener, includes aTypeCol
  procedure PushFrame(aKind: TFrameKind; const aOpener: string;
    aTypeCol: integer = 0);
  begin
    SetLength(lStack, Length(lStack) + 1);
    lStack[High(lStack)].Kind := aKind;
    lStack[High(lStack)].Section := '';
    lStack[High(lStack)].Opener := aOpener;
    lStack[High(lStack)].TypeCol := aTypeCol;
    lStack[High(lStack)].CurVis := '';
    lStack[High(lStack)].LastVisIdx := -1;
    lStack[High(lStack)].MemberSeen := False;
    lStack[High(lStack)].FieldSecIdx := -1;
    lStack[High(lStack)].FieldSeen := False;
  end;

  { True iff significant token aPos ends a statement: an identifier, a closing
    ')' / ']', a nested block 'end', or a number/string literal. }
  function IsStmtEnd(aPos: integer): boolean;
  begin
    Result := False;
    if (aPos < 0) or (aPos >= lSigCount) then
      Exit;
    Result := IsIdent(aPos)
      or (RawText(aPos) = ')') or (RawText(aPos) = ']')
      or (LowText(aPos) = 'end')
      or aContext.Tokens[lSig[aPos]].IsNumber
      or aContext.Tokens[lSig[aPos]].IsString;
  end;

  // Records a redundant section header (suppressed once broken).
  procedure AddCombine(aIdx: integer; const aKind: string);
  begin
    if lBroken then
      Exit;
    SetLength(Result.Combines, Length(Result.Combines) + 1);
    Result.Combines[High(Result.Combines)].Idx := aIdx;
    Result.Combines[High(Result.Combines)].Kind := aKind;
  end;

  // Records a multi-name declaration (suppressed once broken).
  procedure AddDeclare(aFirst, aLast: integer; aContext2: TDeclContext);
  begin
    if lBroken then
      Exit;
    SetLength(Result.Declares, Length(Result.Declares) + 1);
    Result.Declares[High(Result.Declares)].FirstIdx := aFirst;
    Result.Declares[High(Result.Declares)].LastIdx := aLast;
    Result.Declares[High(Result.Declares)].Context := aContext2;
  end;

  // Records an empty routine-declaration paren pair (suppressed once broken).
  procedure AddEmptyParen(aIdx: integer);
  begin
    if lBroken then
      Exit;
    SetLength(Result.EmptyParens, Length(Result.EmptyParens) + 1);
    Result.EmptyParens[High(Result.EmptyParens)] := aIdx;
  end;

  // Records a stray ';' token, suppressed once broken.
  procedure AddStray(aIdx: integer);
  begin
    if lBroken then
      Exit;
    SetLength(Result.StraySemis, Length(Result.StraySemis) + 1);
    Result.StraySemis[High(Result.StraySemis)] := aIdx;
  end;

  // Records a block terminator missing its separator, suppressed once broken.
  procedure AddOmitted(aIdx: integer);
  begin
    if lBroken then
      Exit;
    SetLength(Result.OmittedSemis, Length(Result.OmittedSemis) + 1);
    Result.OmittedSemis[High(Result.OmittedSemis)] := aIdx;
  end;

  // Records an extraneous ',' token, suppressed once broken.
  procedure AddExtraComma(aIdx: integer);
  begin
    if lBroken then
      Exit;
    SetLength(Result.ExtraCommas, Length(Result.ExtraCommas) + 1);
    Result.ExtraCommas[High(Result.ExtraCommas)] := aIdx;
  end;

  // Records an indented unit-level keyword, suppressed once broken.
  procedure AddIndentKw(aIdx: integer);
  begin
    if lBroken then
      Exit;
    SetLength(Result.IndentKeywords, Length(Result.IndentKeywords) + 1);
    Result.IndentKeywords[High(Result.IndentKeywords)] := aIdx;
  end;

  // Records a misindented visibility specifier, suppressed once broken.
  procedure AddVisibilitySpec(aIdx: integer);
  begin
    if lBroken then
      Exit;
    SetLength(Result.VisibilitySpecs, Length(Result.VisibilitySpecs) + 1);
    Result.VisibilitySpecs[High(Result.VisibilitySpecs)] := aIdx;
  end;

  // Records a redundant same-level visibility specifier, suppressed once broken.
  procedure AddVisCombine(aIdx: integer; const aKind: string);
  begin
    if lBroken then
      Exit;
    SetLength(Result.VisCombines, Length(Result.VisCombines) + 1);
    Result.VisCombines[High(Result.VisCombines)].Idx := aIdx;
    Result.VisCombines[High(Result.VisCombines)].Kind := aKind;
  end;

  // Records an empty visibility section's specifier, suppressed once broken.
  procedure AddEmptyVis(aIdx: integer; const aKind: string);
  begin
    if lBroken then
      Exit;
    SetLength(Result.EmptyVis, Length(Result.EmptyVis) + 1);
    Result.EmptyVis[High(Result.EmptyVis)].Idx := aIdx;
    Result.EmptyVis[High(Result.EmptyVis)].Kind := aKind;
  end;

  // Records an empty field section's keyword, suppressed once broken.
  procedure AddEmptyFieldSec(aIdx: integer);
  begin
    if lBroken then
      Exit;
    SetLength(Result.EmptyFieldSec, Length(Result.EmptyFieldSec) + 1);
    Result.EmptyFieldSec[High(Result.EmptyFieldSec)] := aIdx;
  end;

begin
  SetLength(Result.Combines, 0);
  SetLength(Result.Declares, 0);
  SetLength(Result.EmptyParens, 0);
  SetLength(Result.StraySemis, 0);
  SetLength(Result.OmittedSemis, 0);
  SetLength(Result.ExtraCommas, 0);
  SetLength(Result.IndentKeywords, 0);
  SetLength(Result.VisibilitySpecs, 0);
  SetLength(Result.VisCombines, 0);
  SetLength(Result.EmptyVis, 0);
  SetLength(Result.EmptyFieldSec, 0);

  // Significant tokens = non-trivia, non-empty
  lSigCount := 0;
  SetLength(lSig, Length(aContext.Tokens));
  for i := 0 to High(aContext.Tokens) do
    if (not aContext.Tokens[i].IsTrivia)
      and ((aContext.Tokens[i].Text <> '')
      or (aContext.Tokens[i].Punct <> '')) then
    begin
      lSig[lSigCount] := i;
      Inc(lSigCount);
    end;
  SetLength(lSig, lSigCount);

  // The implicit bottom frame is the unit/decl level.
  SetLength(lStack, 1);
  lStack[0].Kind := fkBase;
  lStack[0].Section := '';
  lStack[0].Opener := '';
  lStack[0].TypeCol := 0;
  lStack[0].CurVis := '';
  lStack[0].LastVisIdx := -1;
  lStack[0].MemberSeen := False;
  lStack[0].FieldSecIdx := -1;
  lStack[0].FieldSeen := False;
  lParen := 0;
  lBrack := 0;
  lParamBase := 0;
  lExpectParams := False;
  lInParamList := False;
  lBroken := False;
  lSeenRoutine := False;
  lSkipNextVis := False;
  lMySkipVis := False;

  for i := 0 to lSigCount - 1 do
  begin
    if lBroken then
      Break;
    ltx := RawText(i);
    lw := '';
    if aContext.Tokens[lSig[i]].IsKeyword then
      lw := LowerCase(aContext.Tokens[lSig[i]].Text);

    // detect misindented class/record visibility specifier
    // Note: Visibility words are identifiers (not reserved words)
    lVisLow := '';
    if lw = '' then
      lVisLow := LowerCase(ltx);
    if lSkipNextVis then
      lSkipNextVis := False
    else if (lParen = 0) and (lBrack = 0)
      and (lStack[High(lStack)].Kind = fkType)
      and (lStack[High(lStack)].TypeCol > 0) then
    begin
      lpv := LowText(i - 1);
      if (lVisLow = 'strict') and InSet(LowText(i + 1), cVisibility)
        and ((lpv = lStack[High(lStack)].Opener) or (lpv = ';')) then
      begin
        if aContext.Tokens[lSig[i]].Col <> lStack[High(lStack)].TypeCol then
          AddVisibilitySpec(lSig[i]);
        lSkipNextVis := True;
      end
      else if InSet(lVisLow, cVisibility)
        and ((lpv = lStack[High(lStack)].Opener) or (lpv = ';'))
        and (aContext.Tokens[lSig[i]].Col <> lStack[High(lStack)].TypeCol) then
        AddVisibilitySpec(lSig[i]);
    end;

      { detect visibility/field-section structure
        A parallel branch to the misindent detection above
        Tracks, per open type body, the running visibility section
        and any open var/class-var field section }
    if (lParen = 0) and (lBrack = 0)
      and (lStack[High(lStack)].Kind = fkType) then
    begin
          { lIsVisSpec = this token is part of a visibility specifier
            lStartsVis = this token STARTS a new specifier
            The word after'strict' is part of a specifier but starts nothing. }
      lIsVisSpec := False;
      lStartsVis := False;
      lCanonVis := '';
      if lMySkipVis then
      begin
        lMySkipVis := False;
        lIsVisSpec := True;
      end
      else
      begin
        lpv2 := LowText(i - 1);
        // Declaration-start: after the opener, a ';', a preceding
        // visibility specifier or a section keyword such as 'var'
        lDeclStart := (lpv2 = lStack[High(lStack)].Opener) or (lpv2 = ';')
          or InSet(lpv2, cVisibility) or InSet(lpv2, cSections);
        if lDeclStart then
        begin
          if (lVisLow = 'strict') and InSet(LowText(i + 1), cVisibility) then
          begin
            lCanonVis := 'strict ' + LowText(i + 1);
            lIsVisSpec := True;
            lStartsVis := True;
            lMySkipVis := True;
          end
          else if InSet(lVisLow, cVisibility) then
          begin
            lCanonVis := lVisLow;
            lIsVisSpec := True;
            lStartsVis := True;
          end;
        end;
      end;
      if lStartsVis then
      begin
        // A new visibility specifier ends any open field section.
        if (lStack[High(lStack)].FieldSecIdx >= 0)
          and not lStack[High(lStack)].FieldSeen then
          AddEmptyFieldSec(lStack[High(lStack)].FieldSecIdx);
        lStack[High(lStack)].FieldSecIdx := -1;
        // A repeated same-level specifier
        if lCanonVis = lStack[High(lStack)].CurVis then
          AddVisCombine(lSig[i], lCanonVis);
        // The previous specifier had no member before this one
        if (lStack[High(lStack)].LastVisIdx >= 0)
          and not lStack[High(lStack)].MemberSeen then
          AddEmptyVis(lStack[High(lStack)].LastVisIdx,
            lStack[High(lStack)].CurVis);
        lStack[High(lStack)].CurVis := lCanonVis;
        lStack[High(lStack)].LastVisIdx := lSig[i];
        lStack[High(lStack)].MemberSeen := False;
      end
      else if not lIsVisSpec and (lw <> 'end') then
      begin
        // Any non-specifier declaration token marks the section non-empty.
        lStack[High(lStack)].MemberSeen := True;
        // A 'var' (or 'class var') opens a field section here.
        if lw = 'var' then
        begin
          if (lStack[High(lStack)].FieldSecIdx >= 0)
            and not lStack[High(lStack)].FieldSeen then
            AddEmptyFieldSec(lStack[High(lStack)].FieldSecIdx);
          lStack[High(lStack)].FieldSecIdx := lSig[i];
          lStack[High(lStack)].FieldSeen := False;
        end
        else if (lStack[High(lStack)].FieldSecIdx >= 0) and IsIdent(i) then
          lStack[High(lStack)].FieldSeen := True;
      end;
    end;

    // detect comma-terminated multi-name declaration
    if ltx = ':' then
    begin
      // Walk back over a maximal Ident (',' Ident)* run ending at the colon.
      j := i - 1;
      lCount := 0;
      lFirstSig := -1;
      lLastSig := -1;
      lExpectId := True;
      while j >= 0 do
      begin
        if lExpectId then
        begin
          if IsIdent(j) then
          begin
            if lLastSig < 0 then
              lLastSig := lSig[j];
            lFirstSig := lSig[j];
            Inc(lCount);
            lExpectId := False;
            Dec(j);
          end
          else
            Break;
        end
        else
        begin
          if RawText(j) = ',' then
          begin
            lExpectId := True;
            Dec(j);
          end
          else
            Break;
        end;
      end;
      lBoundary := RawText(j);
      // >= 2 names, not a qualified name (boundary '.'), outside [] indexers.
      if (lCount >= 2) and (lBoundary <> '.') and (lBrack = 0) then
      begin
        if lInParamList and (lParen = lParamBase) then
          AddDeclare(lFirstSig, lLastSig, dcParam)
        else if lParen = 0 then
        begin
          if (lStack[High(lStack)].Kind = fkType)
            and (lStack[High(lStack)].Section <> 'const')
            and (lStack[High(lStack)].Section <> 'type') then
            AddDeclare(lFirstSig, lLastSig, dcField)
          else if (lStack[High(lStack)].Section = 'var')
            or (lStack[High(lStack)].Section = 'threadvar') then
            AddDeclare(lFirstSig, lLastSig, dcVar);
        end;
      end;
      // A return-type colon (no params) disarms the routine-header state.
      if lParen = 0 then
        lExpectParams := False;
    end
    // structural mutation: paren/bracket depth
    else if ltx = '(' then
    begin
      // A routine-header '(' opens the parameter list; detect empty '()'.
      if (lParen = 0) and lExpectParams then
      begin
        if cCheckDeclarations and (LowText(i + 1) = ')') then
          AddEmptyParen(lSig[i]);
        lInParamList := True;
        lParamBase := 1;
        lExpectParams := False;
      end;
      Inc(lParen);
    end
    else if ltx = ')' then
    begin
      if lParen > 0 then
        Dec(lParen);
      if lInParamList and (lParen < lParamBase) then
      begin
        lInParamList := False;
        lParamBase := 0;
      end;
    end
    else if ltx = '[' then
      Inc(lBrack)
    else if ltx = ']' then
    begin
      if lBrack > 0 then
        Dec(lBrack);
    end
    else if ltx = ';' then
    begin
      // A ';' that makes an empty statement
      if (lParen = 0) and (lBrack = 0)
        and (lStack[High(lStack)].Kind = fkStmt)
        and ((RawText(i - 1) = ';') or (LowText(i - 1) = 'begin')) then
        AddStray(lSig[i]);
      if lParen = 0 then
        lExpectParams := False;
    end
    else if ltx = ',' then
    begin
      // A redundant comma inside a bracketed list — an empty element or a trailing comma
      if (lParen > 0) or (lBrack > 0) then
        if (RawText(i - 1) = ',')
          or (RawText(i + 1) = ')') or (RawText(i + 1) = ']') then
          AddExtraComma(lSig[i]);
    end
    // structural mutation: keywords (depth 0 only)
    else if (lw <> '') and (lParen = 0) then
    begin
      lpv := LowText(i - 1);
      lnx := LowText(i + 1);
      // The type-declaration identifier column for a named type body
      lTypeCol := 0;
      if InSet(lw, cTypeOpeners) and (lpv = '=') and IsIdent(i - 2) then
        lTypeCol := aContext.Tokens[lSig[i - 2]].Col;
      // A unit-level structure keyword indented past column 1
      lKwCol := aContext.Tokens[lSig[i]].Col;
      if (lKwCol > 1) and (lStack[High(lStack)].Kind = fkBase) then
      begin
        // Structurally-unambiguous keywords never occur inside a routine.
        if (lw = 'unit') or (lw = 'implementation')
          or (lw = 'initialization') or (lw = 'finalization')
          or (lw = 'uses') or ((lw = 'interface') and (lpv <> '=')) then
          AddIndentKw(lSig[i])
        // Section keywords double as routine-local declarations
        else if (not lSeenRoutine)
          and ((lw = 'type') or (lw = 'const') or (lw = 'var')
          or (lw = 'resourcestring')) then
          AddIndentKw(lSig[i]);
      end;
      if lw = 'end' then
      begin
        // The statement before this 'end' lacks its optional ';'
        if (lStack[High(lStack)].Kind = fkStmt)
          and ((lStack[High(lStack)].Opener = 'begin')
          or (lStack[High(lStack)].Opener = 'try')
          or (lStack[High(lStack)].Opener = 'initialization')
          or (lStack[High(lStack)].Opener = 'finalization'))
          and IsStmtEnd(i - 1) then
          AddOmitted(lSig[i]);
        if Length(lStack) > 1 then
        begin
          lPopKind := lStack[High(lStack)].Kind;
          // Finalize the closing type body:  a trailing empty field
          // section or visibility section ends at the body's 'end'.
          if lPopKind = fkType then
          begin
            if (lStack[High(lStack)].FieldSecIdx >= 0)
              and not lStack[High(lStack)].FieldSeen then
              AddEmptyFieldSec(lStack[High(lStack)].FieldSecIdx);
            if (lStack[High(lStack)].LastVisIdx >= 0)
              and not lStack[High(lStack)].MemberSeen then
              AddEmptyVis(lStack[High(lStack)].LastVisIdx,
                lStack[High(lStack)].CurVis);
          end;
          SetLength(lStack, Length(lStack) - 1);
                  { A begin/asm body that just closed at a declaration scope is
                    an intervening routine between sibling sections: }
          if (lPopKind = fkStmt)
            and (lStack[High(lStack)].Kind in [fkBase, fkType]) then
            lStack[High(lStack)].Section := '';
        end
        else if lnx <> '.' then
          lBroken := True;
      end
      else if lw = 'until' then
      begin
        // The statement before 'until' lacks its optional ';'.
        if (Length(lStack) > 1)
          and (lStack[High(lStack)].Kind = fkStmt) then
        begin
          if (lStack[High(lStack)].Opener = 'repeat')
            and IsStmtEnd(i - 1) then
            AddOmitted(lSig[i]);
          SetLength(lStack, Length(lStack) - 1);
        end;
      end
      else if (lw = 'finally') or (lw = 'except') then
      begin
        // The statement before a try-block's finally/except boundary lacks its optional ';'.
        if (lStack[High(lStack)].Kind = fkStmt)
          and (lStack[High(lStack)].Opener = 'try')
          and IsStmtEnd(i - 1) then
          AddOmitted(lSig[i]);
      end
      else if (lw = 'begin') or (lw = 'asm') or (lw = 'try')
        or (lw = 'repeat') or (lw = 'initialization')
        or (lw = 'finalization') then
        PushFrame(fkStmt, lw)
      else if lw = 'case' then
      begin
        // A statement case is closed by its own end
        if lStack[High(lStack)].Kind = fkStmt then
          PushFrame(fkStmt, lw);
      end
      else if lw = 'record' then
        PushFrame(fkType, lw, lTypeCol)
      else if lw = 'object' then
      begin
        // Exclude the 'of object' method-pointer form.
        if lpv <> 'of' then
          PushFrame(fkType, lw, lTypeCol);
      end
      else if lw = 'class' then
      begin
        // A class body opener follows '='; exclude 'class of' references and 'class;' forward declarations
        if (lpv = '=') and (lnx <> 'of') and (lnx <> ';') then
          PushFrame(fkType, lw, lTypeCol);
      end
      else if (lw = 'interface') or (lw = 'dispinterface') then
      begin
        // A type-body interface follows '='; the unit interface section keyword does not
        if lpv = '=' then
          PushFrame(fkType, lw, lTypeCol)
        else if lStack[High(lStack)].Kind = fkBase then
          lStack[High(lStack)].Section := '';
      end
      else if lw = 'implementation' then
      begin
        // The implementation section is a distinct declaration scope from the interface
        if lStack[High(lStack)].Kind = fkBase then
          lStack[High(lStack)].Section := '';
      end
      else if InSet(lw, cRoutines) then
      begin
        // A routine header breaks the section run and arms the next '(' as a parameter list.
        lStack[High(lStack)].Section := '';
        lExpectParams := True;
        lSeenRoutine := True;
      end
      else if InSet(lw, cSections) then
      begin
        if lStack[High(lStack)].Kind <> fkStmt then
        begin
          // Same kind again at this decl scope => redundant header.
          if lStack[High(lStack)].Section = lw then
            AddCombine(lSig[i], lw);
          lStack[High(lStack)].Section := lw;
        end
        else
          // Inline const/var in a statement block
          lStack[High(lStack)].Section := lw;
      end;
    end;
  end;
end;


{ emission helpers }

// Emits every redundant section header of kind aWant as a range over the keyword.
procedure EmitCombine(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; const aWant: string);
var
  lStruct: TStruct;
  i: integer;
  lTok: TFpSonarToken;
begin
  lStruct := AnalyzeStructure(aContext);
  for i := 0 to High(lStruct.Combines) do
    if lStruct.Combines[i].Kind = aWant then
    begin
      lTok := aContext.Tokens[lStruct.Combines[i].Idx];
      aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lTok.Row, lTok.Col,
        lTok.Row, lTok.Col + Length(lTok.Text) - 1, aMeta.Severity,
        aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey, [],
        LowerCase(lTok.Text));
    end;
end;


{ Emits every multi-name declaration of context aWant as a range from the first
  identifier to the last identifier-before-the-colon.}
procedure EmitDeclare(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aWant: TDeclContext);
var
  lStruct: TStruct;
  i: integer;
  lFirst, lLast: TFpSonarToken;
begin
  lStruct := AnalyzeStructure(aContext);
  for i := 0 to High(lStruct.Declares) do
    if lStruct.Declares[i].Context = aWant then
    begin
      lFirst := aContext.Tokens[lStruct.Declares[i].FirstIdx];
      lLast := aContext.Tokens[lStruct.Declares[i].LastIdx];
      aCollector.AddIssue(aMeta.RuleId, aContext.FileName,
        lFirst.Row, lFirst.Col, lLast.Row, lLast.Col + Length(lLast.Text) - 1,
        aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence,
        aMeta.MessageKey, [], lFirst.Text);
    end;
end;


{ Effective lexeme of a token: its raw Text when filled else the punctuation lexeme }
function EffLex(const aToken: TFpSonarToken): string;
begin
  if aToken.Text <> '' then
    Result := aToken.Text
  else
    Result := aToken.Punct;
end;


{ Emits one issue per token index in aIndices.
  aPoint => a point at the token's  Row:Col
  otherwise a range over the token's lexeme }
procedure EmitIndices(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; const aIndices: TIntArray;
  aPoint: boolean);
var
  i, lEndCol: integer;
  lTok: TFpSonarToken;
  lLex: string;
begin
  for i := 0 to High(aIndices) do
  begin
    lTok := aContext.Tokens[aIndices[i]];
    lLex := EffLex(lTok);
    if aPoint then
      lEndCol := lTok.Col
    else
      lEndCol := lTok.Col + Length(lLex) - 1;
    aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lTok.Row, lTok.Col,
      lTok.Row, lEndCol, aMeta.Severity, aMeta.Category,
      aMeta.DefaultConfidence, aMeta.MessageKey, [], lLex);
  end;
end;


{ Emits one point issue per redundant same-level visibility specifier at
  its token's Row:Col, carrying the canonical visibility keyword as the message arg }

procedure EmitVisCombine(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lStruct: TStruct;
  i: integer;
  lTok: TFpSonarToken;
begin
  lStruct := AnalyzeStructure(aContext);
  for i := 0 to High(lStruct.VisCombines) do
  begin
    lTok := aContext.Tokens[lStruct.VisCombines[i].Idx];
    aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lTok.Row, lTok.Col,
      lTok.Row, lTok.Col, aMeta.Severity, aMeta.Category,
      aMeta.DefaultConfidence, aMeta.MessageKey,
      [lStruct.VisCombines[i].Kind], lStruct.VisCombines[i].Kind);
  end;
end;


{ Emits one point issue per empty visibility section at its specifier token's Row:Col }
procedure EmitEmptyVis(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector);
var
  lStruct: TStruct;
  i: integer;
  lTok: TFpSonarToken;
  lArg: string;
begin
  lStruct := AnalyzeStructure(aContext);
  for i := 0 to High(lStruct.EmptyVis) do
  begin
    lTok := aContext.Tokens[lStruct.EmptyVis[i].Idx];
    // The canonical specifier captured at detection (e.g. 'strict private'),
    lArg := lStruct.EmptyVis[i].Kind;
    aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lTok.Row, lTok.Col,
      lTok.Row, lTok.Col, aMeta.Severity, aMeta.Category,
      aMeta.DefaultConfidence, aMeta.MessageKey, [lArg], lArg);
  end;
end;


{ compiler-directive parsing }

// Splits a comment token's .Text into an upper-cased directive name and the upper-cased trimmed remainder.
function DirectiveParts(const aText: string;
  out aName, aRest: string): boolean;
var
  lBody: string;
  p: integer;
begin
  Result := False;
  aName := '';
  aRest := '';
  if (Length(aText) < 2) or (aText[1] <> '$') then
    Exit;
  lBody := Copy(aText, 2, Length(aText));
  p := 1;
  while (p <= Length(lBody)) and not (lBody[p] in [' ', #9]) do
    Inc(p);
  aName := UpperCase(Copy(lBody, 1, p - 1));
  aRest := UpperCase(Trim(Copy(lBody, p, Length(lBody))));
  Result := aName <> '';
end;


{ True iff aName is on the (currently empty) allowList of permitted directives. }
function DirectiveAllowed(const aName: string): boolean;
begin
  Result := False;
end;


{ Splits a '$WARN' remainder ('<target> <state>') into its upper-cased target and state words.}
procedure SplitWarn(const aRest: string; out aTarget, aState: string);
var
  p: integer;
begin
  p := 1;
  while (p <= Length(aRest)) and not (aRest[p] in [' ', #9]) do
    Inc(p);
  aTarget := Copy(aRest, 1, p - 1);
  aState := Trim(Copy(aRest, p, Length(aRest)));
end;


// True iff aTarget is a known warning-class '$WARN' target
function IsWarnClassTarget(const aTarget: string): boolean;
var
  k: integer;
begin
  Result := False;
  for k := Low(cWarnClassTargets) to High(cWarnClassTargets) do
    if cWarnClassTargets[k] = aTarget then
    begin
      Result := True;
      Exit;
    end;
end;


// True iff aTarget is a known hint-class '$WARN' target
function IsHintClassTarget(const aTarget: string): boolean;
begin
  Result := False;
end;


// Scans the FULL token stream for diagnostic-disabling directives and emits a point at each.
procedure EmitDirectives(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  aWantHints: boolean);
var
  i: integer;
  lTok: TFpSonarToken;
  lName, lRest, lTarget, lState: string;
  lFlag: boolean;
begin
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsComment then
      Continue;
    if not DirectiveParts(lTok.Text, lName, lRest) then
      Continue;
    if DirectiveAllowed(lName) then
      Continue;
    lFlag := False;
    if aWantHints then
    begin
      if (lName = 'HINTS') and (lRest = 'OFF') then
        lFlag := True
      else if lName = 'WARN' then
      begin
        SplitWarn(lRest, lTarget, lState);
        lFlag := (lState = 'OFF') and IsHintClassTarget(lTarget);
      end;
    end
    else
    begin
      if (lName = 'WARNINGS') and (lRest = 'OFF') then
        lFlag := True
      else if lName = 'WARN' then
      begin
        SplitWarn(lRest, lTarget, lState);
        lFlag := (lState = 'OFF') and IsWarnClassTarget(lTarget);
      end;
    end;
    if lFlag then
      aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lTok.Row, lTok.Col,
        lTok.Row, lTok.Col, aMeta.Severity, aMeta.Category,
        aMeta.DefaultConfidence, aMeta.MessageKey, [], lTok.Text);
  end;
end;


{ TRuleLowercaseKeywords }

procedure TRuleLowercaseKeywords.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i: integer;
  lTok: TFpSonarToken;
  lLower: string;
begin
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsKeyword then
      Continue;
    // Keywords are pure ASCII => byte length = column span.
    lLower := LowerCase(lTok.Text);
    if lTok.Text <> lLower then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lTok.Row, lTok.Col, lTok.Row, lTok.Col + Length(lTok.Text) - 1,
        FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
        FMetadata.MessageKey, [lLower], lLower);
  end;
end;


{ TRuleCombineConstSections }

procedure TRuleCombineConstSections.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitCombine(FMetadata, aContext, aCollector, 'const');
end;


{ TRuleCombineTypeSections }

procedure TRuleCombineTypeSections.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitCombine(FMetadata, aContext, aCollector, 'type');
end;


{ TRuleCombineVarSections }

procedure TRuleCombineVarSections.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitCombine(FMetadata, aContext, aCollector, 'var');
end;


{ TRuleDeclareFieldsIndividually }

procedure TRuleDeclareFieldsIndividually.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitDeclare(FMetadata, aContext, aCollector, dcField);
end;


{ TRuleDeclareVariablesIndividually }

procedure TRuleDeclareVariablesIndividually.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitDeclare(FMetadata, aContext, aCollector, dcVar);
end;


{ TRuleDeclareParametersIndividually }

procedure TRuleDeclareParametersIndividually.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  // allowSameModifierGroup is hardcoded False (flag all groups) — see header.
  if cAllowSameModifierGroup then
    Exit;
  EmitDeclare(FMetadata, aContext, aCollector, dcParam);
end;


{ TRuleNoEmptyParenthesesOnRoutines }

procedure TRuleNoEmptyParenthesesOnRoutines.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lStruct: TStruct;
  i: integer;
  lTok: TFpSonarToken;
begin
  // cCheckCalls is hardcoded False; declaration-position '()' only (see header).
  if not cCheckDeclarations then
    Exit;
  lStruct := AnalyzeStructure(aContext);
  for i := 0 to High(lStruct.EmptyParens) do
  begin
    lTok := aContext.Tokens[lStruct.EmptyParens[i]];
    aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
      lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
      FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
      [], '(');
  end;
end;


{ TRuleNoStraySemicolons }

procedure TRuleNoStraySemicolons.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitIndices(FMetadata, aContext, aCollector,
    AnalyzeStructure(aContext).StraySemis, True);
end;


{ TRuleNoOmittedSemicolons }

procedure TRuleNoOmittedSemicolons.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  // Anchored as a RANGE over the terminator keyword (end/until/finally/except).
  EmitIndices(FMetadata, aContext, aCollector,
    AnalyzeStructure(aContext).OmittedSemis, False);
end;


{ TRuleNoExtraneousCommas }

procedure TRuleNoExtraneousCommas.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitIndices(FMetadata, aContext, aCollector,
    AnalyzeStructure(aContext).ExtraCommas, True);
end;


{ TRuleNoDisabledCompilerHints }

procedure TRuleNoDisabledCompilerHints.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitDirectives(FMetadata, aContext, aCollector, True);
end;


{ TRuleNoDisabledCompilerWarnings }

procedure TRuleNoDisabledCompilerWarnings.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitDirectives(FMetadata, aContext, aCollector, False);
end;


{ TRuleNoIndentUnitLevelKeywords }

procedure TRuleNoIndentUnitLevelKeywords.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  // Anchored as a RANGE over the keyword.
  EmitIndices(FMetadata, aContext, aCollector,
    AnalyzeStructure(aContext).IndentKeywords, False);
end;


{ TRuleIndentVisibilitySpecifiers }

procedure TRuleIndentVisibilitySpecifiers.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  // Range over each misindented visibility specifier (anchored on 'strict' for
  // the two-word form); the hit list comes from the shared structural pass.
  EmitIndices(FMetadata, aContext, aCollector,
    AnalyzeStructure(aContext).VisibilitySpecs, False);
end;


{ TRuleNoCommentedOutCode }

procedure TRuleNoCommentedOutCode.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i: integer;
  lTok: TFpSonarToken;
  lBody: string;
begin
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsComment then
      Continue;
    // Never flag a dollar-directive comment nor a NOSONAR-carrying comment.
    if (Length(lTok.Text) > 0) and (lTok.Text[1] = '$') then
      Continue;
    if Pos('NOSONAR', lTok.Text) > 0 then
      Continue;
    // Conservative heuristic: a commented-out assignment shape
    lBody := Trim(lTok.Text);
    if (Length(lBody) > 0) and (lBody[Length(lBody)] = ';')
      and (Pos(':=', lBody) > 0) then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
        FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
        [], lBody);
  end;
end;


{ TRuleTrackNoSonar }

procedure TRuleTrackNoSonar.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i: integer;
  lTok: TFpSonarToken;
  lHasReason: boolean;
  lReason: string;
begin
  // Reuse the NOSONAR scan primitive over each comment token
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsComment then
      Continue;
    if FindNoSonar(lTok.Text, lHasReason, lReason)
      and ((not cRequireReason) or (not lHasReason)) then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
        FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
        [], lTok.Text);
  end;
end;


{ TRuleTrackComments }

procedure TRuleTrackComments.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i, k: integer;
  lTok: TFpSonarToken;
  lMarker: string;
begin
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsComment then
      Continue;
    // Skip a dollar-directive comment (not a prose/code comment).
    if (Length(lTok.Text) > 0) and (lTok.Text[1] = '$') then
      Continue;
    // One issue per comment: the first built-in marker present is reported
    lMarker := '';
    for k := Low(cCommentMarkers) to High(cCommentMarkers) do
      if Pos(cCommentMarkers[k], lTok.Text) > 0 then
      begin
        lMarker := cCommentMarkers[k];
        Break;
      end;
    if lMarker <> '' then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
        FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
        [lMarker], lTok.Text);
  end;
end;


{ TRuleTrackStringLiterals }

procedure TRuleTrackStringLiterals.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  i: integer;
  lTok: TFpSonarToken;
begin
  for i := 0 to High(aContext.Tokens) do
  begin
    lTok := aContext.Tokens[i];
    if not lTok.IsString then
      Continue;
    if Pos(cStringLiteralPattern, lTok.Text) > 0 then
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
        lTok.Row, lTok.Col, lTok.Row, lTok.Col, FMetadata.Severity,
        FMetadata.Category, FMetadata.DefaultConfidence, FMetadata.MessageKey,
        [cStringLiteralPattern], lTok.Text);
  end;
end;


{ TRuleCombineVisibilitySections }

procedure TRuleCombineVisibilitySections.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitVisCombine(FMetadata, aContext, aCollector);
end;


{ TRuleRemoveEmptyVisibilitySection }

procedure TRuleRemoveEmptyVisibilitySection.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  EmitEmptyVis(FMetadata, aContext, aCollector);
end;


{ TRuleRemoveEmptyFieldSection }

procedure TRuleRemoveEmptyFieldSection.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
begin
  // Point at each empty field-section keyword; no message args.
  EmitIndices(FMetadata, aContext, aCollector,
    AnalyzeStructure(aContext).EmptyFieldSec, True);
end;


initialization
  RegisterRule(TRuleLowercaseKeywords.Create(TRuleMetadata.Make(
    'LowercaseKeywords', rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, cKeyLowercaseKeywords).WithDescription(
    'Flags a keyword not written in lowercase.')));
  RegisterMessage(cKeyLowercaseKeywords, SLowercaseKeywords);

  RegisterRule(TRuleCombineConstSections.Create(TRuleMetadata.Make(
    'CombineConstSections', rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, cKeyCombineConst).WithDescription(
    'Flags a const section that can be merged with the preceding one.')));
  RegisterMessage(cKeyCombineConst, SCombineConstSections);

  RegisterRule(TRuleCombineTypeSections.Create(TRuleMetadata.Make(
    'CombineTypeSections', rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, cKeyCombineType).WithDescription(
    'Flags a type section that can be merged with the preceding one.')));
  RegisterMessage(cKeyCombineType, SCombineTypeSections);

  RegisterRule(TRuleCombineVarSections.Create(TRuleMetadata.Make(
    'CombineVarSections', rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, cKeyCombineVar).WithDescription(
    'Flags a var section that can be merged with the preceding one.')));
  RegisterMessage(cKeyCombineVar, SCombineVarSections);

  RegisterRule(TRuleDeclareFieldsIndividually.Create(TRuleMetadata.Make(
    'DeclareFieldsIndividually', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyDeclareFields).WithDescription(
    'Flags multiple fields in one declaration; declare each individually.')));
  RegisterMessage(cKeyDeclareFields, SDeclareFieldsIndividually);

  RegisterRule(TRuleDeclareVariablesIndividually.Create(TRuleMetadata.Make(
    'DeclareVariablesIndividually', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyDeclareVariables).WithDescription(
    'Flags multiple variables in one declaration; declare each individually.')));
  RegisterMessage(cKeyDeclareVariables, SDeclareVariablesIndividually);

  RegisterRule(TRuleDeclareParametersIndividually.Create(TRuleMetadata.Make(
    'DeclareParametersIndividually', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyDeclareParameters).WithDescription(
    'Flags parameters that should be declared in their own group.')));
  RegisterMessage(cKeyDeclareParameters, SDeclareParametersIndividually);

  RegisterRule(TRuleNoEmptyParenthesesOnRoutines.Create(TRuleMetadata.Make(
    'NoEmptyParenthesesOnRoutines', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyNoEmptyParens).WithDescription(
    'Flags empty parentheses on a routine that can be removed.')));
  RegisterMessage(cKeyNoEmptyParens, SNoEmptyParenthesesOnRoutines);

  // Punctuation + directive rules.
  RegisterRule(TRuleNoStraySemicolons.Create(TRuleMetadata.Make(
    'NoStraySemicolons', rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoStraySemicolons).WithDescription(
    'Flags a stray semicolon that can be removed.')));
  RegisterMessage(cKeyNoStraySemicolons, SNoStraySemicolons);

  RegisterRule(TRuleNoOmittedSemicolons.Create(TRuleMetadata.Make(
    'NoOmittedSemicolons', rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoOmittedSemicolons).WithDescription(
    'Flags a missing semicolon before a block terminator.')));
  RegisterMessage(cKeyNoOmittedSemicolons, SNoOmittedSemicolons);

  RegisterRule(TRuleNoExtraneousCommas.Create(TRuleMetadata.Make(
    'NoExtraneousCommas', rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, cKeyNoExtraneousCommas).WithDescription(
    'Flags an extraneous comma that can be removed.')));
  RegisterMessage(cKeyNoExtraneousCommas, SNoExtraneousCommas);

  RegisterRule(TRuleNoDisabledCompilerHints.Create(TRuleMetadata.Make(
    'NoDisabledCompilerHints', rtTok, rfTokenStream, sevMajor, itCodeSmell,
    cfHigh, True, cKeyNoDisabledHints).WithDescription(
    'Flags directives that disable compiler hints.')));
  RegisterMessage(cKeyNoDisabledHints, SNoDisabledCompilerHints);

  RegisterRule(TRuleNoDisabledCompilerWarnings.Create(TRuleMetadata.Make(
    'NoDisabledCompilerWarnings', rtTok, rfTokenStream, sevMajor, itCodeSmell,
    cfHigh, True, cKeyNoDisabledWarnings).WithDescription(
    'Flags directives that disable compiler warnings.')));
  RegisterMessage(cKeyNoDisabledWarnings, SNoDisabledCompilerWarnings);

  RegisterRule(TRuleNoIndentUnitLevelKeywords.Create(TRuleMetadata.Make(
    'NoIndentUnitLevelKeywords', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyNoIndentUnitLevel).WithDescription(
    'Flags a unit-level keyword not starting at column 1.')));
  RegisterMessage(cKeyNoIndentUnitLevel, SNoIndentUnitLevelKeywords);

  // Layout + tracker rules.
  RegisterRule(TRuleIndentVisibilitySpecifiers.Create(TRuleMetadata.Make(
    'IndentVisibilitySpecifiers', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyIndentVisibility).WithDescription(
    'Flags a visibility specifier not indented to the type declaration.')));
  RegisterMessage(cKeyIndentVisibility, SIndentVisibilitySpecifiers);

  RegisterRule(TRuleNoCommentedOutCode.Create(TRuleMetadata.Make(
    'NoCommentedOutCode', rtTok, rfTokenStream, sevMajor, itCodeSmell, cfMedium,
    True, cKeyNoCommentedOutCode).WithDescription(
    'Flags commented-out code that should be removed.')));
  RegisterMessage(cKeyNoCommentedOutCode, SNoCommentedOutCode);

  RegisterRule(TRuleTrackNoSonar.Create(TRuleMetadata.Make(
    'TrackNoSonar', rtTok, rfTokenStream, sevInfo, itCodeSmell, cfHigh,
    True, cKeyTrackNoSonar).WithDescription(
    'Reports where a NOSONAR suppression is used (tracking rule).')));
  RegisterMessage(cKeyTrackNoSonar, STrackNoSonar);

  RegisterRule(TRuleTrackComments.Create(TRuleMetadata.Make(
    'TrackComments', rtTok, rfTokenStream, sevInfo, itCodeSmell, cfHigh,
    True, cKeyTrackComments).WithDescription(
    'Reports comments matching a configured marker (tracking rule).')));
  RegisterMessage(cKeyTrackComments, STrackComments);

  RegisterRule(TRuleTrackStringLiterals.Create(TRuleMetadata.Make(
    'TrackStringLiterals', rtTok, rfTokenStream, sevInfo, itCodeSmell, cfHigh,
    True, cKeyTrackStringLiterals).WithDescription(
    'Reports string literals matching a configured pattern (tracking rule).')));
  RegisterMessage(cKeyTrackStringLiterals, STrackStringLiterals);

  // Visibility/field-section structure rules
  RegisterRule(TRuleCombineVisibilitySections.Create(TRuleMetadata.Make(
    'CombineVisibilitySections', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyCombineVisibilitySections).WithDescription(
    'Flags a visibility section that can be merged with the preceding one of the same kind.')));
  RegisterMessage(cKeyCombineVisibilitySections, SCombineVisibilitySections);

  RegisterRule(TRuleRemoveEmptyVisibilitySection.Create(TRuleMetadata.Make(
    'RemoveEmptyVisibilitySection', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyRemoveEmptyVisibilitySection).WithDescription(
    'Flags an empty visibility section that can be removed.')));
  RegisterMessage(cKeyRemoveEmptyVisibilitySection, SRemoveEmptyVisibilitySection);

  RegisterRule(TRuleRemoveEmptyFieldSection.Create(TRuleMetadata.Make(
    'RemoveEmptyFieldSection', rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, cKeyRemoveEmptyFieldSection).WithDescription(
    'Flags an empty field section that can be removed.')));
  RegisterMessage(cKeyRemoveEmptyFieldSection, SRemoveEmptyFieldSection);

end.
