{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Issue post-processing: fingerprinting, collection, suppression and governance

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Issues;

{ Issue post-processing: everything that happens to an issue after a rule emits
  it, merged from the former FpSonar.Fingerprint, FpSonar.IssueCollector,
  FpSonar.Suppression and FpSonar.Governance (previously FpSonar.Utils). The
  per-issue pipeline is fingerprint -> collect -> suppress -> classify:

    * fingerprint (was FpSonar.Fingerprint): the line-independent
      FNV1a64hex(ruleId #31 path #31 normalize(snippet)), 16-char hex, inline
      FNV-1a-64. Carries no line number; normalize() collapses whitespace/CRLF,
      keeps case. Immutability contract: any change invalidates every committed
      baseline (the golden-value test guards it);
    * collect (was FpSonar.IssueCollector): TFpSonarIssueCollector, the sole
      emission chokepoint — every rule emits via AddIssue, which fingerprints
      from the caller-supplied snippet and stores in insertion order;
      CollectDiagnostic folds a diagnostic into a reserved ParseError/ScanError/
      ResolveError issue;
    * suppress (was FpSonar.Suppression): the inline-NOSONAR scan + marker map
      (fed tokens via IsComment), the config-glob matcher, the
      Suppresses/ConfigGlobSuppresses predicates and the pure
      ApplySuppressions filter;
    * classify (was FpSonar.Governance): ClassifySuppressions assigns one active
      source per issue by the fixed precedence NOSONAR > config-glob > baseline >
      active (mute-in-place, not a drop); ActiveIssues is the hard-drop the gate
      evaluates.

  Interface deps: FpSonar.Types, FpSonar.Config, FpSonar.Baseline and
  FpSonar.Ingest (its token types + IsComment for marker capture, the only
  token-facing part). Deterministic; single-threaded; LCL-free. }

{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types, FpSonar.Config, FpSonar.Baseline, FpSonar.Ingest;

type
  // One captured inline NOSONAR marker: the file + line it sits on, plus the
  // optional trailing reason (the data TrackNoSonar consumes for requireReason).
  TFpSonarNoSonarMarker = record
    FileName: string;
    Line: integer;
    HasReason: boolean;
    Reason: string;
  end;

  TFpSonarNoSonarMarkerArray = array of TFpSonarNoSonarMarker;

  { Accumulates inline NOSONAR markers across a multi-file run (one per run,
    like the IssueCollector). Owns only a value array — trivial lifecycle, no
    per-element heap, 0 unfreed blocks under -gh. }
  TFpSonarSuppressionMap = class
  private
    FMarkers: TFpSonarNoSonarMarkerArray;
  public
    // Scans aTokens for comment tokens carrying a NOSONAR marker and records one
    // marker per hit, keyed by aFileName (the SAME string the engine threads as
    // the issue FileName, NOT token.FileName, so map keys and issue keys match).
    procedure AddFile(const aFileName: string; const aTokens: TFpSonarTokenArray);
    // True iff a marker exists with that file AND line.
    function IsSuppressed(const aFileName: string; aLine: integer): boolean;
    // True iff a NOSONAR marker on aIssue's start line suppresses it: Self is a
    // valid map (nil-safe: a nil map suppresses nothing) AND aIssue is NOT the
    // TrackNoSonar tracker (exempt, so its own on-line info issue survives) AND a
    // marker exists on (FileName, StartLine). The single authority for the
    // NOSONAR predicate (incl. the exemption). Pure; deterministic.
    function Suppresses(const aIssue: TFpSonarIssue): boolean;
    // The captured markers, read-only, in insertion (token) order.
    property Markers: TFpSonarNoSonarMarkerArray read FMarkers;
  end;

// True iff aCommentText contains the upper-case marker token NOSONAR (matched
// CASE-SENSITIVELY). aReason is the trimmed text after the marker (and after an
// optional leading ':'); aHasReason := aReason <> ''. Pure; deterministic.
// aCommentText is expected to be the scanner's comment .Text (delimiters // { }
// (* *) already stripped — see FpSonar.Ingest) so no closing delimiter leaks
// into the reason; this is how AddFile and the TrackNoSonar consumer feed it.
function FindNoSonar(const aCommentText: string; out aHasReason: boolean;
  out aReason: string): boolean;

// A tiny self-contained glob matcher: '*' matches any run of zero-or-more
// characters (INCLUDING path separators), '?' matches exactly one character,
// every other character matches literally. CASE-SENSITIVE. An EMPTY pattern is
// treated as '*' (match anything); '**' is treated the same as '*'. Pure;
// deterministic — deliberately NOT FPC's Masks/maskutils (version-dependent).
function GlobMatch(const aPattern, aText: string): boolean;

// True iff any glob in aGlobs suppresses aIssue: its rule glob matches RuleId AND
// its path glob matches FileName (an omitted pattern is a '*' wildcard). The
// single authority for the config-glob predicate. Pure; deterministic.
function ConfigGlobSuppresses(const aGlobs: TFpSonarSuppressionGlobArray;
  const aIssue: TFpSonarIssue): boolean;

// Returns a NEW issue array in the SAME order, dropping exactly the suppressed
// issues. An issue is suppressed iff (aMap <> nil) and aMap.IsSuppressed(its
// FileName, StartLine), OR any glob in aGlobs matches (rule glob vs RuleId AND
// path glob vs FileName). aMap = nil => only globs apply; empty aGlobs => only
// NOSONAR applies; both empty/nil => an identity copy. No field is mutated.
function ApplySuppressions(const aIssues: TFpSonarIssueArray;
  const aMap: TFpSonarSuppressionMap;
  const aGlobs: TFpSonarSuppressionGlobArray): TFpSonarIssueArray;


{ ===== Fingerprint (was FpSonar.Fingerprint) ===== }

{ Normalizes a code snippet for fingerprinting: trims ends, collapses internal
  whitespace runs to one space, drops CR/LF, preserves case. Carries no line
  number — the source of line-independence. }
function NormalizeSnippet(const aSnippet: string): string;

// Computes the 16-char lowercase-hex FNV-1a-64 fingerprint of
// ruleId + #31 + path + #31 + NormalizeSnippet(snippet).
function ComputeFingerprint(const aRuleId, aPath, aSnippet: string): string;

{ ===== Issue collector (was FpSonar.IssueCollector) ===== }

type
  { The single issue-emission chokepoint. }
  TFpSonarIssueCollector = class
  private
    FIssues: TFpSonarIssueArray;
    function GetCount: integer;
  public
    // Stores aIssue, computing its Fingerprint from ruleId+fileName+aSnippet.
    procedure AddIssue(const aIssue: TFpSonarIssue; const aSnippet: string); overload;
    // Convenience overload: builds the issue from its parts, fingerprints it,
    // appends it, and returns its insertion index. Ergonomics for future rules.
    function AddIssue(const aRuleId, aFileName: string;
      aStartLine, aStartCol, aEndLine, aEndCol: integer;
      aSeverity: TFpSonarSeverity; aType: TFpSonarIssueType;
      aConfidence: TFpSonarConfidence; const aMessageKey: string;
      const aArgs: array of string; const aSnippet: string): integer; overload;
    // Folds a diagnostic into a ParseError/ScanError issue and routes it
    // through AddIssue.
    procedure CollectDiagnostic(const aDiag: TFpSonarDiagnostic);
    // Empties the collector (e.g. between files).
    procedure Clear;
    // The collected issues, read-only, in deterministic insertion order.
    property Issues: TFpSonarIssueArray read FIssues;
    // Number of collected issues.
    property Count: integer read GetCount;
  end;

// Strips a trailing passrc location phrase (" in file ... at line N column M")
// from a diagnostic message so the fold's fingerprint stays line-independent.
function StripPositionPhrase(const aMessage: string): string;

{ ===== Suppression-source governance (was FpSonar.Governance) ===== }

// Returns a NEW issue array in the SAME input order, each record copied whole
// with ONLY SuppressionSource set (Fingerprint and every other field untouched).
// The source is decided by the fixed precedence NOSONAR > config-glob > baseline
// > active. Empty map + empty globs + empty baseline => every issue ssActive
// (identity). Pure; deterministic; single-threaded.
function ClassifySuppressions(const aIssues: TFpSonarIssueArray;
  const aMap: TFpSonarSuppressionMap;
  const aGlobs: TFpSonarSuppressionGlobArray;
  const aBaseline: TFpSonarBaseline): TFpSonarIssueArray;

// Returns a NEW array holding exactly the ssActive issues of aIssues, in order.
// This is the set the quality gate evaluates in mute mode (suppressed issues are
// reported but excluded from the gate). Records copied whole; no field mutated.
function ActiveIssues(const aIssues: TFpSonarIssueArray): TFpSonarIssueArray;

// The lowercase reporting name of a suppression source (active/nosonar/config/
// baseline), analogous to the adapters' SeverityName.
function SuppressionSourceName(aSource: TFpSonarSuppressionSource): string;

// The count of suppressed (non-ssActive) issues in aIssues (for the text footer).
function SuppressedCount(const aIssues: TFpSonarIssueArray): integer;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

  { ===== Fingerprint ===== }

function NormalizeSnippet(const aSnippet: string): string;
var
  i: integer;
  lCh: char;
  lPendingSpace: boolean;
begin
  Result := '';
  lPendingSpace := False;
  for i := 1 to Length(aSnippet) do
  begin
    lCh := aSnippet[i];
      { CR/LF and tabs/spaces are all treated as a single inter-token gap; a run
        of them collapses to one pending space that is only emitted before the
        next real char (so leading/trailing whitespace is trimmed for free). }
    if (lCh = #9) or (lCh = #32) or (lCh = #13) or (lCh = #10) then
      lPendingSpace := True
    else
    begin
      if lPendingSpace and (Result <> '') then
        Result := Result + #32;
      lPendingSpace := False;
      Result := Result + lCh;
    end;
  end;
end;


function ComputeFingerprint(const aRuleId, aPath, aSnippet: string): string;
const
  cOffsetBasis = uint64($CBF29CE484222325);
  cPrime = uint64($00000100000001B3);
var
  lInput: rawbytestring;
  lHash: uint64;
  i: integer;
begin
  // The US separator (#31) cannot occur in identifiers/paths/normalized code,
  // so the three fields are unambiguously delimited.
  lInput := UTF8Encode(aRuleId + #31 + aPath + #31 + NormalizeSnippet(aSnippet));
  lHash := cOffsetBasis;
  for i := 1 to Length(lInput) do
  begin
    lHash := lHash xor uint64(Ord(lInput[i]));
    lHash := lHash * cPrime;
  end;
  Result := LowerCase(IntToHex(lHash, 16));
end;


{ ===== Issue collector ===== }

function StripPositionPhrase(const aMessage: string): string;
var
  lPos: integer;
begin
  Result := aMessage;
  // passrc embeds the location as "... in file <path> at line N column M".
  // Cut from " in file " (removes the volatile path + line/col in one go).
  lPos := Pos(' in file ', Result);
  if lPos > 0 then
    Result := Copy(Result, 1, lPos - 1)
  else
  begin
    // No file phrase: fall back to cutting a trailing " at line ..." phrase.
    lPos := Pos(' at line ', Result);
    if lPos > 0 then
      Result := Copy(Result, 1, lPos - 1);
  end;
  Result := Trim(Result);
end;


function TFpSonarIssueCollector.GetCount: integer;
begin
  Result := Length(FIssues);
end;


procedure TFpSonarIssueCollector.AddIssue(const aIssue: TFpSonarIssue;
  const aSnippet: string);
var
  lIssue: TFpSonarIssue;
begin
  lIssue := aIssue;
  lIssue.Fingerprint := ComputeFingerprint(lIssue.RuleId, lIssue.FileName,
    aSnippet);
  SetLength(FIssues, Length(FIssues) + 1);
  FIssues[High(FIssues)] := lIssue;
end;


function TFpSonarIssueCollector.AddIssue(const aRuleId, aFileName: string;
  aStartLine, aStartCol, aEndLine, aEndCol: integer;
  aSeverity: TFpSonarSeverity; aType: TFpSonarIssueType;
  aConfidence: TFpSonarConfidence; const aMessageKey: string;
  const aArgs: array of string; const aSnippet: string): integer;
var
  lIssue: TFpSonarIssue;
begin
  lIssue := TFpSonarIssue.Make(aRuleId, aFileName, aStartLine, aStartCol, aEndLine,
    aEndCol, aSeverity, aType, aConfidence, aMessageKey, aArgs);
  AddIssue(lIssue, aSnippet);
  Result := High(FIssues);
end;


procedure TFpSonarIssueCollector.CollectDiagnostic(
  const aDiag: TFpSonarDiagnostic);
var
  lRuleId: string;
  lMessageKey: string;
begin
  // Reserved diagnostic RuleIds (bare PascalCase, no prefix).
  case aDiag.Kind of
    dkScanError:
    begin
      lRuleId := 'ScanError';
      lMessageKey := 'rule.ScanError.message';
    end;
    dkResolveError:
    begin
      // A resolution failure folds like ScanError, into the
      // reserved ResolveError RuleId. The message catalog key is seeded by
      // FpSonar.Resolver's initialization (self-contained, like RuleError).
      lRuleId := 'ResolveError';
      lMessageKey := 'rule.ResolveError.message';
    end;
    dkFileNotFound:
    begin
      // The source file could not be opened — a clear, dedicated RuleId instead
      // of a bare "Scan error: <path>" with no reason.
      lRuleId := 'FileNotFound';
      lMessageKey := 'rule.FileNotFound.message';
    end;
    else
      // dkParseError (and any future-defaulted kind).
      lRuleId := 'ParseError';
      lMessageKey := 'rule.ParseError.message';
  end;

  // A parse/scan failure is certain: cfHigh, sevMajor, itCodeSmell (recommended
  // values within the fixed taxonomy). Range = the 1-based diagnostic position
  // (0 preserved when the failure carried none). Snippet = the position-stripped
  // message so the fingerprint stays line-independent.
  AddIssue(lRuleId, aDiag.FileName, aDiag.Row, aDiag.Col, aDiag.Row, aDiag.Col,
    sevMajor, itCodeSmell, cfHigh, lMessageKey, [aDiag.Message],
    StripPositionPhrase(aDiag.Message));
end;


procedure TFpSonarIssueCollector.Clear;
begin
  SetLength(FIssues, 0);
end;


{ ===== Suppression-source governance ===== }

function ClassifySuppressions(const aIssues: TFpSonarIssueArray;
  const aMap: TFpSonarSuppressionMap;
  const aGlobs: TFpSonarSuppressionGlobArray;
  const aBaseline: TFpSonarBaseline): TFpSonarIssueArray;
var
  i: integer;
begin
  SetLength(Result, Length(aIssues));
  for i := 0 to High(aIssues) do
  begin
    // Copy the whole record (Fingerprint included, untouched), then override
    // ONLY SuppressionSource — order is preserved by the linear copy.
    Result[i] := aIssues[i];
    if aMap.Suppresses(aIssues[i]) then
      Result[i].SuppressionSource := ssNoSonar
    else if ConfigGlobSuppresses(aGlobs, aIssues[i]) then
      Result[i].SuppressionSource := ssConfig
    else if aBaseline.Contains(aIssues[i].Fingerprint) then
      Result[i].SuppressionSource := ssBaseline
    else
      Result[i].SuppressionSource := ssActive;
  end;
end;


function ActiveIssues(const aIssues: TFpSonarIssueArray): TFpSonarIssueArray;
var
  i, lCount: integer;
begin
  // ApplySuppressions-shaped: pre-size, copy survivors whole, shrink.
  SetLength(Result, Length(aIssues));
  lCount := 0;
  for i := 0 to High(aIssues) do
    if aIssues[i].SuppressionSource = ssActive then
    begin
      Result[lCount] := aIssues[i];
      Inc(lCount);
    end;
  SetLength(Result, lCount);
end;


function SuppressionSourceName(aSource: TFpSonarSuppressionSource): string;
begin
  case aSource of
    ssNoSonar: Result := 'nosonar';
    ssConfig: Result := 'config';
    ssBaseline: Result := 'baseline';
    else
      Result := 'active';
  end;
end;


function SuppressedCount(const aIssues: TFpSonarIssueArray): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(aIssues) do
    if aIssues[i].SuppressionSource <> ssActive then
      Inc(Result);
end;

const
  // The NoSonar-tracking rule's RuleId — exempt from the NOSONAR-marker
  // branch of ApplySuppressions so its own on-line issue survives the pipeline
  // (SonarQube-aligned). Config-glob suppression still applies to it.
  cNoSonarTrackerRuleId = 'TrackNoSonar';

function FindNoSonar(const aCommentText: string; out aHasReason: boolean;
  out aReason: string): boolean;
const
  cMarker = 'NOSONAR';
var
  lPos: integer;
  lRest: string;
begin
  aHasReason := False;
  aReason := '';
  // Case-sensitive: the marker is the upper-case SonarQube token; "no sonar" or
  // "Nosonar" must NOT suppress.
  lPos := Pos(cMarker, aCommentText);
  Result := lPos > 0;
  if not Result then
    Exit;
  // The trailing text after the marker becomes the reason: strip leading
  // whitespace, then one optional leading ':', then trim. Raw enough for the tracker.
  lRest := Copy(aCommentText, lPos + Length(cMarker), Length(aCommentText));
  lRest := TrimLeft(lRest);
  if (lRest <> '') and (lRest[1] = ':') then
    Delete(lRest, 1, 1);
  aReason := Trim(lRest);
  aHasReason := aReason <> '';
end;


function GlobMatch(const aPattern, aText: string): boolean;
var
  lPattern: string;
  p, t, lStar, lMark: integer;
begin
  // An omitted/empty pattern is a wildcard — this is how an omitted rule/path
  // becomes "match anything".
  lPattern := aPattern;
  if lPattern = '' then
    lPattern := '*';
  // Iterative backtracking matcher (linear in practice; '**' collapses to '*').
  p := 1;
  t := 1;
  lStar := 0;
  lMark := 0;
  while t <= Length(aText) do
  begin
    if (p <= Length(lPattern)) and
      ((lPattern[p] = '?') or (lPattern[p] = aText[t])) then
    begin
      Inc(p);
      Inc(t);
    end
    else if (p <= Length(lPattern)) and (lPattern[p] = '*') then
    begin
      // Remember the star and the text position to backtrack to.
      lStar := p;
      lMark := t;
      Inc(p);
    end
    else if lStar <> 0 then
    begin
      // Backtrack: let the last star swallow one more character.
      p := lStar + 1;
      Inc(lMark);
      t := lMark;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;
  // Trailing stars in the pattern match the empty remainder.
  while (p <= Length(lPattern)) and (lPattern[p] = '*') do
    Inc(p);
  Result := p > Length(lPattern);
end;


function TFpSonarSuppressionMap.Suppresses(const aIssue: TFpSonarIssue): boolean;
begin
  // A marker on the issue's start line (uniform across all RuleIds), EXCEPT the
  // NoSonar tracker, which is exempt so its on-line info issue survives the
  // pipeline. Nil-safe: a nil map (Self=nil) short-circuits to False before any
  // field access, so callers need not guard.
  Result := (Self <> nil)
    and (aIssue.RuleId <> cNoSonarTrackerRuleId)
    and IsSuppressed(aIssue.FileName, aIssue.StartLine);
end;


function ConfigGlobSuppresses(const aGlobs: TFpSonarSuppressionGlobArray;
  const aIssue: TFpSonarIssue): boolean;
var
  j: integer;
begin
  // Rule glob vs RuleId AND path glob vs FileName; first matching glob wins.
  Result := False;
  for j := 0 to High(aGlobs) do
    if GlobMatch(aGlobs[j].RulePattern, aIssue.RuleId) and
      GlobMatch(aGlobs[j].PathPattern, aIssue.FileName) then
    begin
      Result := True;
      Exit;
    end;
end;


function ApplySuppressions(const aIssues: TFpSonarIssueArray;
  const aMap: TFpSonarSuppressionMap;
  const aGlobs: TFpSonarSuppressionGlobArray): TFpSonarIssueArray;
var
  i, lCount: integer;
  lSuppressed: boolean;
begin
  SetLength(Result, Length(aIssues));
  lCount := 0;
  for i := 0 to High(aIssues) do
  begin
    // NOSONAR (incl. the exemption) then config globs — via the
    // single-authority predicates so the composition layer cannot drift
    // from this filter.
    lSuppressed := aMap.Suppresses(aIssues[i]);
    if not lSuppressed then
      lSuppressed := ConfigGlobSuppresses(aGlobs, aIssues[i]);
    if not lSuppressed then
    begin
      // Whole-record copy — fields + Fingerprint untouched; order preserved.
      Result[lCount] := aIssues[i];
      Inc(lCount);
    end;
  end;
  SetLength(Result, lCount);
end;


procedure TFpSonarSuppressionMap.AddFile(const aFileName: string;
  const aTokens: TFpSonarTokenArray);
var
  i: integer;
  lHasReason: boolean;
  lReason: string;
begin
  for i := 0 to High(aTokens) do
    if aTokens[i].IsComment and
      FindNoSonar(aTokens[i].Text, lHasReason, lReason) then
    begin
      SetLength(FMarkers, Length(FMarkers) + 1);
      FMarkers[High(FMarkers)].FileName := aFileName;
      FMarkers[High(FMarkers)].Line := aTokens[i].Row;
      FMarkers[High(FMarkers)].HasReason := lHasReason;
      FMarkers[High(FMarkers)].Reason := lReason;
    end;
end;


function TFpSonarSuppressionMap.IsSuppressed(const aFileName: string;
  aLine: integer): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to High(FMarkers) do
    if (FMarkers[i].Line = aLine) and (FMarkers[i].FileName = aFileName) then
    begin
      Result := True;
      Exit;
    end;
end;

end.
