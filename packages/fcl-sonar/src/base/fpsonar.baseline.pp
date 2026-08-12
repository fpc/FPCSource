{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Snapshot baseline: build, serialize, load, and the new-code filter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Baseline;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  FpSonar.Types;

type
  // One baseline entry: the matching key (Fingerprint) plus informational
  // RuleId/FileName for clean human diffs and governance reuse.
  TFpSonarBaselineEntry = record
    Fingerprint: string;
    RuleId: string;
    FileName: string;
  end;

  TFpSonarBaselineEntryArray = array of TFpSonarBaselineEntry;

  // A built/loaded snapshot: a sorted (fingerprint, ruleId, file),
  // fingerprint-deduped set of entries.
  TFpSonarBaseline = record
    Entries: TFpSonarBaselineEntryArray;
    // Builds a baseline from an issue set: collects each issue's Fingerprint,
    // then sorts + fingerprint-dedupes the entries.
    class function FromIssues(const aIssues: TFpSonarIssueArray): TFpSonarBaseline; static;
    // True iff aFingerprint is present (binary search over the sorted entries).
    function Contains(const aFingerprint: string): boolean;
    // A new issue array keeping ONLY issues whose Fingerprint is NOT in Self.
    function FilterNewCode(const aIssues: TFpSonarIssueArray): TFpSonarIssueArray;
    // Serializes Self to a deterministic, version-stamped JSON string (sorted,
    // deduped entries).
    function ToJSON: string;
    // Parses a baseline snapshot from JSON text INTO Self (sorted + deduped like
    // FromIssues). Returns False + aError on malformed input.
    function LoadFromJSON(const aJsonText: string; out aError: string): boolean;
    // Reads aPath INTO Self and delegates to LoadFromJSON. A missing/unreadable
    // file is False + aError.
    function LoadFromFile(const aPath: string; out aError: string): boolean;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, FpJson.Data, FpJson.Parser;
{$ELSE}
  SysUtils, Classes, fpjson, jsonparser;
{$ENDIF}

const
  // The snapshot format version (bumped only on an incompatible layout change);
  cBaselineFormat = '1';

// Ordinal ordering: fingerprint, then ruleId, then file.
function CompareEntries(const aLeft, aRight: TFpSonarBaselineEntry): integer;
begin
  Result := CompareStr(aLeft.Fingerprint, aRight.Fingerprint);
  if Result = 0 then
    Result := CompareStr(aLeft.RuleId, aRight.RuleId);
  if Result = 0 then
    Result := CompareStr(aLeft.FileName, aRight.FileName);
end;


// In-place quicksort of aEntries[aLo..aHi] by CompareEntries;
procedure SortEntries(var aEntries: TFpSonarBaselineEntryArray; aLo, aHi: integer);
var
  i, j: integer;
  lPivot, lTmp: TFpSonarBaselineEntry;
begin
  if aLo >= aHi then
    Exit;
  i := aLo;
  j := aHi;
  lPivot := aEntries[(aLo + aHi) div 2];
  repeat
    while CompareEntries(aEntries[i], lPivot) < 0 do
      Inc(i);
    while CompareEntries(aEntries[j], lPivot) > 0 do
      Dec(j);
    if i <= j then
    begin
      lTmp := aEntries[i];
      aEntries[i] := aEntries[j];
      aEntries[j] := lTmp;
      Inc(i);
      Dec(j);
    end;
  until i > j;
  SortEntries(aEntries, aLo, j);
  SortEntries(aEntries, i, aHi);
end;


// Sorts aEntries then collapses runs of equal Fingerprint to their first entry
procedure SortAndDedup(var aEntries: TFpSonarBaselineEntryArray);
var
  i, lCount: integer;
begin
  if Length(aEntries) <= 1 then
    Exit;
  SortEntries(aEntries, 0, High(aEntries));
  lCount := 1;
  for i := 1 to High(aEntries) do
    if aEntries[i].Fingerprint <> aEntries[lCount - 1].Fingerprint then
    begin
      aEntries[lCount] := aEntries[i];
      Inc(lCount);
    end;
  SetLength(aEntries, lCount);
end;


class function TFpSonarBaseline.FromIssues(const aIssues: TFpSonarIssueArray): TFpSonarBaseline;
var
  i: integer;
begin
  SetLength(Result.Entries, Length(aIssues));
  for i := 0 to High(aIssues) do
  begin
    Result.Entries[i].Fingerprint := aIssues[i].Fingerprint;
    Result.Entries[i].RuleId := aIssues[i].RuleId;
    Result.Entries[i].FileName := aIssues[i].FileName;
  end;
  SortAndDedup(Result.Entries);
end;


function TFpSonarBaseline.Contains(const aFingerprint: string): boolean;
var
  lLo, lHi, lMid, lCmp: integer;
begin
  // Binary search over the sorted, fingerprint-deduped entries.
  lLo := 0;
  lHi := High(Entries);
  while lLo <= lHi do
  begin
    lMid := (lLo + lHi) div 2;
    lCmp := CompareStr(Entries[lMid].Fingerprint, aFingerprint);
    if lCmp = 0 then
    begin
      Result := True;
      Exit;
    end
    else if lCmp < 0 then
      lLo := lMid + 1
    else
      lHi := lMid - 1;
  end;
  Result := False;
end;


function TFpSonarBaseline.FilterNewCode(const aIssues: TFpSonarIssueArray): TFpSonarIssueArray;
var
  i, lCount: integer;
begin
  SetLength(Result, Length(aIssues));
  lCount := 0;
  for i := 0 to High(aIssues) do
    if not Contains(aIssues[i].Fingerprint) then
    begin
      Result[lCount] := aIssues[i];
      Inc(lCount);
    end;
  SetLength(Result, lCount);
end;


function TFpSonarBaseline.ToJSON: string;
var
  lRoot: TJSONObject;
  lStamp: TJSONObject;
  lIssues: TJSONArray;
  lEntry: TJSONObject;
  i: integer;
begin
  lRoot := TJSONObject.Create;
  try
    // Fixed key order: _fpsonar stamp first, then the issues array.
    lStamp := TJSONObject.Create;
    lStamp.Add('baseline', cBaselineFormat);
    lStamp.Add('version', FpSonarVersion);
    lRoot.Add('_fpsonar', lStamp);

    lIssues := TJSONArray.Create;
    // Entries are already sorted + deduped by FromIssues/LoadFromJSON;
    for i := 0 to High(Entries) do
    begin
      lEntry := TJSONObject.Create;
      lEntry.Add('fingerprint', Entries[i].Fingerprint);
      lEntry.Add('ruleId', Entries[i].RuleId);
      lEntry.Add('file', Entries[i].FileName);
      lIssues.Add(lEntry);
    end;
    lRoot.Add('issues', lIssues);

    Result := lRoot.FormatJSON;
  finally
    lRoot.Free;
  end;
end;


function TFpSonarBaseline.LoadFromJSON(const aJsonText: string;
  out aError: string): boolean;
var
  lData: TJSONData;
  lText: string;
  lRoot: TJSONObject;
  lIssuesData: TJSONData;
  lIssues: TJSONArray;
  lElem: TJSONData;
  lObj: TJSONObject;
  lFp: TJSONData;
  i, lCount: integer;
begin
  Result := False;
  aError := '';
  SetLength(Self.Entries, 0);
  lData := nil;
  // Strip a leading UTF-8 BOM if present (as in the Config loader).
  lText := aJsonText;
  if (Length(lText) >= 3) and (lText[1] = #$EF) and (lText[2] = #$BB)
    and (lText[3] = #$BF) then
    Delete(lText, 1, 3);
  try
    try
      lData := GetJSON(lText);
    except
      on E: Exception do
      begin
        aError := 'invalid baseline JSON: ' + E.Message;
        Exit;
      end;
    end;
    if not (lData is TJSONObject) then
    begin
      aError := 'baseline root must be a JSON object';
      Exit;
    end;
    lRoot := TJSONObject(lData);

    // A missing/empty "issues" array is valid and means "everything is new".
    lIssuesData := lRoot.Find('issues');
    if (lIssuesData <> nil) and (lIssuesData is TJSONArray) then
    begin
      lIssues := TJSONArray(lIssuesData);
      SetLength(Self.Entries, lIssues.Count);
      lCount := 0;
      for i := 0 to lIssues.Count - 1 do
      begin
        lElem := lIssues.Items[i];
        if not (lElem is TJSONObject) then
          Continue;
        lObj := TJSONObject(lElem);
        // Matching reads ONLY "fingerprint"; an entry without one is skipped.
        lFp := lObj.Find('fingerprint');
        if (lFp = nil) or not (lFp.JSONType = jtString) then
          Continue;
        Self.Entries[lCount].Fingerprint := lFp.AsString;
        Self.Entries[lCount].RuleId := lObj.Get('ruleId', '');
        Self.Entries[lCount].FileName := lObj.Get('file', '');
        Inc(lCount);
      end;
      SetLength(Self.Entries, lCount);
      SortAndDedup(Self.Entries);
    end;
    Result := True;
  finally
    lData.Free;
  end;
end;


function TFpSonarBaseline.LoadFromFile(const aPath: string;
  out aError: string): boolean;
var
  lStream: TFileStream;
  lText: string;
begin
  SetLength(Self.Entries, 0);
  aError := '';
  if not FileExists(aPath) then
  begin
    Result := False;
    aError := 'baseline file not found: ' + aPath;
    Exit;
  end;
  // Read raw bytes (no encoding transform) so UTF-8 reaches the parser intact.
  lText := '';
  try
    lStream := TFileStream.Create(aPath, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(lText, lStream.Size);
      if lStream.Size > 0 then
        lStream.ReadBuffer(lText[1], lStream.Size);
    finally
      lStream.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      aError := 'cannot read baseline file: ' + E.Message;
      Exit;
    end;
  end;
  Result := Self.LoadFromJSON(lText, aError);
end;


end.
