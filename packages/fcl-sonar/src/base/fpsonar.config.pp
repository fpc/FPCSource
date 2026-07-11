{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Ruleset and quality-gate policy configuration (JSON)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Config;

(* TFpSonarConfig: the ruleset and policy config

  JSON config schema (`rules.{enabled,severity}` + `gate.*`, plus a `suppressions` array;

  unknown keys are ignored for forward-compat, so keys added later do not break this loader.

    {
      "_fpsonar": { "config": "fpsonar-default", "version": "1" }, // optional stamp; ignored
      "rules": {                       // optional; keys are bare PascalCase RuleIds (verbatim)
        "LowercaseKeywords": { "enabled": false },
        "LineTooLong":       { "severity": "major" }   // info|minor|major|critical|blocker
      },
      "gate": {                        // optional; absent => strict default
        "maxBlocker": 0, "maxCritical": 0,             // -1 = unlimited on that axis
        "maxMajor": -1, "maxMinor": -1, "maxInfo": -1,
        "maxTotal": -1
      },
      "suppressions": [                // optional (1.9); absent => no suppression
        { "rule": "Naming*", "path": "*/legacy/*" }    // *,? globs (case-sensitive)
        // each element: optional string "rule" and/or "path";
        // at least one is required (a both-empty entry = "suppress everything" => hard error).
        // An omitted field is a "*" wildcard.
        // A non-array, a non-object element, or a non-string rule/path is a hard error.
      ]
    }
*)

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  FpSonar.Types;

type
  // The JSON-derived kind of one configured per-rule parameter.
  TFpSonarRuleParamKind = (cpkInt, cpkStr, cpkBool, cpkTargets);

  // One disallow-list entry for a config-driven tracker rule.
  TFpSonarRuleTarget = record
    // Pattern is the glob
    Pattern: string;
    // Message is the optional human wording appended to the issue;
    Message: string;
    //  Severity (valid only when HasSeverity)
    Severity: TFpSonarSeverity;
    // override the rule's default severity for this target
    HasSeverity: boolean;
  end;

  TFpSonarRuleTargetArray = array of TFpSonarRuleTarget;

  // One configured parameter under a rule's 'params' object:
  TFpSonarRuleParam = record
    Key: string;
    Kind: TFpSonarRuleParamKind;
    IntVal: integer;
    StrVal: string;
    BoolVal: boolean;
    Targets: TFpSonarRuleTargetArray;
  end;

  TFpSonarRuleParamArray = array of TFpSonarRuleParam;

  // A per-rule override.
  // The Has* flags distinguish "not specified" from an explicit value.
  // Params contains an optional 'params' object; empty when the rule tunes nothing.
  TFpSonarRuleSetting = record
    RuleId: string;
    HasEnabled: boolean;
    Enabled: boolean;
    HasSeverity: boolean;
    Severity: TFpSonarSeverity;
    Params: TFpSonarRuleParamArray;
  end;

  TFpSonarRuleSettingArray = array of TFpSonarRuleSetting;

  // Quality thresholds.
  TFpSonarGateThresholds = record
    // MaxPerSeverity entry / MaxTotal. -1 = unlimited
    MaxPerSeverity: array[TFpSonarSeverity] of integer;
    // MaxTotal is the MAXIMUM allowed count
    MaxTotal: integer;
    // Evaluates Self against aIssues' (effective) severities: fails on the first
    // per-severity axis exceeded (Blocker..Info), else the total. -1 = unlimited
    // on an axis. The reason names the first exceeded axis.
    function Evaluate(const aIssues: TFpSonarIssueArray): TFpSonarGateOutcome;
  end;

  // A config suppression glob: an issue is suppressed when its RuleId
  // matches RulePattern AND its FileName matches PathPattern. An empty pattern
  // is a "*" wildcard (so a rule-only glob matches any path, and vice versa).
  TFpSonarSuppressionGlob = record
    RulePattern: string;
    PathPattern: string;
  end;

  TFpSonarSuppressionGlobArray = array of TFpSonarSuppressionGlob;

  // The use-tier reference engine selector.
  TFpSonarUseTierResolution = (utrOff,   // the parser-only name-reference engine
    utrPrefer // the resolution-backed engine
    );

  // How TFpSonarConfig.ToJSON renders the ruleset.
  //  cemFull  — every rule listed at its effective setting (the complete,
  //             human-editable template "init-config" writes).
  //  cemDiff  — only what differs from each rule's metadata default (the
  //             minimal, VCS-friendly shape an editor saves).
  TFpSonarConfigEmitMode = (cemFull, cemDiff);

  // The ruleset + gate policy loaded from the JSON --config file.
  TFpSonarConfig = record
    Rules: TFpSonarRuleSettingArray;
    Gate: TFpSonarGateThresholds;
    Suppressions: TFpSonarSuppressionGlobArray;
    // The USE-tier resolution preference. Defaults to utrOff.
    UseTierResolution: TFpSonarUseTierResolution;
    // The built-in default
    class function Default: TFpSonarConfig; static;
    // The effective enable state for aRuleId: an explicit Enabled when the rule
    // has a setting with HasEnabled, else aDefaultEnabled.
    function RuleEnabled(const aRuleId: string; aDefaultEnabled: boolean): boolean;
    // The effective severity for aRuleId: an explicit Severity when the rule
    // has a setting with HasSeverity, else aDefaultSeverity.
    function EffectiveSeverity(const aRuleId: string;
      aDefaultSeverity: TFpSonarSeverity): TFpSonarSeverity;
    // The typed value of the 'aKey' parameter configured for aRuleId, or aDefault
    // when the rule has no such param of the matching kind.
    function RuleParamInt(const aRuleId, aKey: string; aDefault: integer): integer;
    // As RuleParamInt, for a string-valued parameter (e.g. a naming regex pattern).
    function RuleParamStr(const aRuleId, aKey, aDefault: string): string;
    // As RuleParamInt, for a boolean-valued parameter.
    function RuleParamBool(const aRuleId, aKey: string; aDefault: boolean): boolean;
    // The configured 'targets' disallow-list for (aRuleId, aKey), or an empty array.
    function RuleParamTargets(const aRuleId, aKey: string): TFpSonarRuleTargetArray;
    // A new issue array in the same order, each issue's Severity replaced by EffectiveSeverity.
    function ApplyTo(const aIssues: TFpSonarIssueArray): TFpSonarIssueArray;
    // Parses a JSON config text INTO Self, overlaying present keys onto the default config.
    // Returns False + aError on malformed/invalid input.
    function LoadFromJSON(const aJsonText: string; out aError: string): boolean;
    // Reads aPath INTO Self and delegates to LoadFromJSON. A missing/unreadable
    // file is a False + aError.
    function LoadFromFile(const aPath: string; out aError: string): boolean;
    // Serializes Self to fpsonar JSON against the rule set aRules (RuleId-sorted).
    function ToJSON(const aRules: array of TRuleMetadata;
      aMode: TFpSonarConfigEmitMode): string;
    // The complete, human-editable JSON template: 
    function TemplateToJSON(const aRules: array of TRuleMetadata): string;
  private
    // Finds the configured param record for (aRuleId, aKey), or False when absent.
    function FindConfiguredParam(const aRuleId, aKey: string;
      out aParam: TFpSonarRuleParam): boolean;
  end;

// The lowercase enum name for a severity (info/minor/major/critical/blocker).
function SeverityName(aSeverity: TFpSonarSeverity): string;

// Parses a (case-insensitive) severity name; False on an unknown name.
function SeverityFromName(const aName: string; out aSeverity: TFpSonarSeverity): boolean;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, FpJson.Data, FpJson.Parser, FpSonar.Consts;
{$ELSE}
  SysUtils, Classes, fpjson, jsonparser, FpSonar.Consts;
{$ENDIF}

class function TFpSonarConfig.Default: TFpSonarConfig;
var
  lSev: TFpSonarSeverity;
begin
  SetLength(Result.Rules, 0);
  // Unlimited everywhere by default...
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    Result.Gate.MaxPerSeverity[lSev] := -1;
  // ...except the strict, documented CI default: fail on any Blocker or Critical.
  Result.Gate.MaxPerSeverity[sevBlocker] := 0;
  Result.Gate.MaxPerSeverity[sevCritical] := 0;
  Result.Gate.MaxTotal := -1;
  // No suppression by default — every issue flows through untouched.
  SetLength(Result.Suppressions, 0);
  // USE tier defaults to the parser-tier name engine.
  Result.UseTierResolution := utrOff;
end;


function TFpSonarGateThresholds.Evaluate(
  const aIssues: TFpSonarIssueArray): TFpSonarGateOutcome;
var
  lCounts: array[TFpSonarSeverity] of integer;
  lSev: TFpSonarSeverity;
  lTotal, i: integer;

  // True iff this axis has a limit (>= 0) and the count strictly exceeds it.
  function Trips(aCount, aMax: integer): boolean;
  begin
    Result := (aMax >= 0) and (aCount > aMax);
  end;

begin
  Result.Failed := False;
  Result.ExitCode := 0;
  Result.Reason := '';

  // One pass: tally per-severity counts + the total.
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    lCounts[lSev] := 0;
  for i := 0 to High(aIssues) do
    Inc(lCounts[aIssues[i].Severity]);
  lTotal := Length(aIssues);

  // Severities first, in descending severity order (Blocker..Info); the enum is
  // declared ascending, so iterate High downto Low.
  for lSev := High(TFpSonarSeverity) downto Low(TFpSonarSeverity) do
    if Trips(lCounts[lSev], MaxPerSeverity[lSev]) then
    begin
      Result.Failed := True;
      Result.ExitCode := 1;
      Result.Reason := Format(SGateSeverityExceeded,
        [lCounts[lSev], SeverityName(lSev), MaxPerSeverity[lSev]]);
      Exit;
    end;

  // Total last.
  if Trips(lTotal, MaxTotal) then
  begin
    Result.Failed := True;
    Result.ExitCode := 1;
    Result.Reason := Format(SGateTotalExceeded, [lTotal, MaxTotal]);
  end;
end;


function SeverityName(aSeverity: TFpSonarSeverity): string;
begin
  case aSeverity of
    sevInfo: Result := 'info';
    sevMinor: Result := 'minor';
    sevMajor: Result := 'major';
    sevCritical: Result := 'critical';
    sevBlocker: Result := 'blocker';
    else
      Result := 'info';
  end;
end;


function SeverityFromName(const aName: string;
  out aSeverity: TFpSonarSeverity): boolean;
var
  lName: string;
begin
  Result := True;
  lName := LowerCase(Trim(aName));
  if lName = 'info' then
    aSeverity := sevInfo
  else if lName = 'minor' then
    aSeverity := sevMinor
  else if lName = 'major' then
    aSeverity := sevMajor
  else if lName = 'critical' then
    aSeverity := sevCritical
  else if lName = 'blocker' then
    aSeverity := sevBlocker
  else
  begin
    aSeverity := sevInfo;
    Result := False;
  end;
end;


function TFpSonarConfig.RuleEnabled(const aRuleId: string;
  aDefaultEnabled: boolean): boolean;
var
  i: integer;
begin
  Result := aDefaultEnabled;
  for i := 0 to High(Rules) do
    if (Rules[i].RuleId = aRuleId) and Rules[i].HasEnabled then
    begin
      Result := Rules[i].Enabled;
      Exit;
    end;
end;


function TFpSonarConfig.EffectiveSeverity(const aRuleId: string;
  aDefaultSeverity: TFpSonarSeverity): TFpSonarSeverity;
var
  i: integer;
begin
  Result := aDefaultSeverity;
  for i := 0 to High(Rules) do
    if (Rules[i].RuleId = aRuleId) and Rules[i].HasSeverity then
    begin
      Result := Rules[i].Severity;
      Exit;
    end;
end;


function TFpSonarConfig.FindConfiguredParam(const aRuleId, aKey: string;
  out aParam: TFpSonarRuleParam): boolean;
var
  i, j: integer;
begin
  Result := False;
  for i := 0 to High(Rules) do
    if Rules[i].RuleId = aRuleId then
    begin
      for j := 0 to High(Rules[i].Params) do
        if Rules[i].Params[j].Key = aKey then
        begin
          aParam := Rules[i].Params[j];
          Result := True;
          Exit;
        end;
      Exit;
    end;
end;


function TFpSonarConfig.RuleParamInt(const aRuleId, aKey: string;
  aDefault: integer): integer;
var
  lParam: TFpSonarRuleParam;
begin
  Result := aDefault;
  if FindConfiguredParam(aRuleId, aKey, lParam)
    and (lParam.Kind = cpkInt) then
    Result := lParam.IntVal;
end;


function TFpSonarConfig.RuleParamStr(const aRuleId, aKey, aDefault: string): string;
var
  lParam: TFpSonarRuleParam;
begin
  Result := aDefault;
  if FindConfiguredParam(aRuleId, aKey, lParam)
    and (lParam.Kind = cpkStr) then
    Result := lParam.StrVal;
end;


function TFpSonarConfig.RuleParamBool(const aRuleId, aKey: string;
  aDefault: boolean): boolean;
var
  lParam: TFpSonarRuleParam;
begin
  Result := aDefault;
  if FindConfiguredParam(aRuleId, aKey, lParam)
    and (lParam.Kind = cpkBool) then
    Result := lParam.BoolVal;
end;


function TFpSonarConfig.RuleParamTargets(const aRuleId, aKey: string): TFpSonarRuleTargetArray;
var
  lParam: TFpSonarRuleParam;
begin
  SetLength(Result, 0);
  if FindConfiguredParam(aRuleId, aKey, lParam)
    and (lParam.Kind = cpkTargets) then
    Result := lParam.Targets;
end;


function TFpSonarConfig.ApplyTo(const aIssues: TFpSonarIssueArray): TFpSonarIssueArray;
var
  i: integer;
begin
  SetLength(Result, Length(aIssues));
  for i := 0 to High(aIssues) do
  begin
    // Copy the whole record (Fingerprint included, untouched), then override
    // only the severity — order is preserved by the linear copy.
    Result[i] := aIssues[i];
    Result[i].Severity := EffectiveSeverity(aIssues[i].RuleId,
      aIssues[i].Severity);
  end;
end;


// Parses one 'targets' JSON array (the config-driven-tracker list-of-objects) into aTargets.
function ParseTargetsArray(aArr: TJSONArray; const aRuleId, aKey: string;
  out aTargets: TFpSonarRuleTargetArray; out aError: string): boolean;
var
  lElem, lNameData, lMsgData, lSevData: TJSONData;
  lObj: TJSONObject;
  lTarget: TFpSonarRuleTarget;
  lSev: TFpSonarSeverity;
  i: integer;
begin
  Result := False;
  aError := '';
  SetLength(aTargets, 0);
  for i := 0 to aArr.Count - 1 do
  begin
    lElem := aArr.Items[i];
    if not (lElem is TJSONObject) then
    begin
      aError := 'rule "' + aRuleId + '": target in "' + aKey +
        '" must be a JSON object';
      Exit;
    end;
    lObj := TJSONObject(lElem);

    lTarget.Pattern := '';
    lTarget.Message := '';
    lTarget.Severity := sevInfo;
    lTarget.HasSeverity := False;

    // 'name' is canonical; 'glob' is its synonym.
    lNameData := lObj.Find('name');
    if lNameData = nil then
      lNameData := lObj.Find('glob');
    if (lNameData = nil) or (lNameData.JSONType <> jtString)
      or (lNameData.AsString = '') then
    begin
      aError := 'rule "' + aRuleId + '": target in "' + aKey +
        '" must have a "name" or "glob" string';
      Exit;
    end;
    lTarget.Pattern := lNameData.AsString;

    lMsgData := lObj.Find('message');
    if lMsgData <> nil then
    begin
      if lMsgData.JSONType <> jtString then
      begin
        aError := 'rule "' + aRuleId + '": target "message" in "' +
          aKey + '" must be a string';
        Exit;
      end;
      lTarget.Message := lMsgData.AsString;
    end;

    lSevData := lObj.Find('severity');
    if lSevData <> nil then
    begin
      // Guard the type FIRST: AsString on a non-scalar (object/array) raises
      // in fpjson, so it must never be reached for the error text.
      if lSevData.JSONType <> jtString then
      begin
        aError := 'rule "' + aRuleId + '": target "severity" in "' +
          aKey + '" must be a string';
        Exit;
      end;
      if not SeverityFromName(lSevData.AsString, lSev) then
      begin
        aError := 'rule "' + aRuleId + '": target severity "' +
          lSevData.AsString + '" is invalid';
        Exit;
      end;
      lTarget.Severity := lSev;
      lTarget.HasSeverity := True;
    end;

    SetLength(aTargets, Length(aTargets) + 1);
    aTargets[High(aTargets)] := lTarget;
  end;
  Result := True;
end;


// Parses the optional "params" object on one rule into aSetting.Params
function ParseRuleParams(aRuleObj: TJSONObject; var aSetting: TFpSonarRuleSetting;
  out aError: string): boolean;
var
  lParamsData, lValue: TJSONData;
  lParamsObj: TJSONObject;
  lParam: TFpSonarRuleParam;
  i: integer;
begin
  Result := False;
  aError := '';
  SetLength(aSetting.Params, 0);
  lParamsData := aRuleObj.Find('params');
  if lParamsData = nil then
  begin
    Result := True;
    Exit;
  end;
  if not (lParamsData is TJSONObject) then
  begin
    aError := 'rule "' + aSetting.RuleId + '": "params" must be a JSON object';
    Exit;
  end;
  lParamsObj := TJSONObject(lParamsData);
  for i := 0 to lParamsObj.Count - 1 do
  begin
    lValue := lParamsObj.Items[i];
    lParam.Key := lParamsObj.Names[i];
    lParam.IntVal := 0;
    lParam.StrVal := '';
    lParam.BoolVal := False;
    SetLength(lParam.Targets, 0);
    case lValue.JSONType of
      jtNumber:
      begin
        if TJSONNumber(lValue).NumberType = ntFloat then
        begin
          aError := 'rule "' + aSetting.RuleId + '": parameter "' +
            lParam.Key + '" must be an integer, string, boolean, or array';
          Exit;
        end;
        lParam.Kind := cpkInt;
        lParam.IntVal := lValue.AsInteger;
      end;
      jtString:
      begin
        lParam.Kind := cpkStr;
        lParam.StrVal := lValue.AsString;
      end;
      jtBoolean:
      begin
        lParam.Kind := cpkBool;
        lParam.BoolVal := lValue.AsBoolean;
      end;
      jtArray:
      begin
        lParam.Kind := cpkTargets;
        if not ParseTargetsArray(TJSONArray(lValue), aSetting.RuleId,
          lParam.Key, lParam.Targets, aError) then
          Exit;
      end;
      else
        aError := 'rule "' + aSetting.RuleId + '": parameter "' + lParam.Key +
          '" must be an integer, string, boolean, or array';
        Exit;
    end;
    SetLength(aSetting.Params, Length(aSetting.Params) + 1);
    aSetting.Params[High(aSetting.Params)] := lParam;
  end;
  Result := True;
end;


// Overlays the optional "rules" object onto aConfig. Each member key is a bare RuleId.
function OverlayRules(aRoot: TJSONObject; var aConfig: TFpSonarConfig;
  out aError: string): boolean;
var
  lRules: TJSONData;
  lRulesObj: TJSONObject;
  lRuleData, lEnabledData, lSeverityData: TJSONData;
  lRuleObj: TJSONObject;
  lSetting: TFpSonarRuleSetting;
  lSev: TFpSonarSeverity;
  i: integer;
begin
  Result := False;
  aError := '';
  lRules := aRoot.Find('rules');
  if lRules = nil then
  begin
    Result := True;
    Exit;
  end;
  if not (lRules is TJSONObject) then
  begin
    aError := '"rules" must be a JSON object';
    Exit;
  end;
  lRulesObj := TJSONObject(lRules);
  for i := 0 to lRulesObj.Count - 1 do
  begin
    lRuleData := lRulesObj.Items[i];
    if not (lRuleData is TJSONObject) then
    begin
      aError := 'rule "' + lRulesObj.Names[i] + '" must be a JSON object';
      Exit;
    end;
    lRuleObj := TJSONObject(lRuleData);

    lSetting.RuleId := lRulesObj.Names[i];
    lSetting.HasEnabled := False;
    lSetting.Enabled := False;
    lSetting.HasSeverity := False;
    lSetting.Severity := sevInfo;
    SetLength(lSetting.Params, 0);

    lEnabledData := lRuleObj.Find('enabled');
    if lEnabledData <> nil then
    begin
      if lEnabledData.JSONType <> jtBoolean then
      begin
        aError := 'rule "' + lSetting.RuleId +
          '": "enabled" must be a boolean';
        Exit;
      end;
      lSetting.HasEnabled := True;
      lSetting.Enabled := lEnabledData.AsBoolean;
    end;

    lSeverityData := lRuleObj.Find('severity');
    if lSeverityData <> nil then
    begin
      if lSeverityData.JSONType <> jtString then
      begin
        aError := 'rule "' + lSetting.RuleId +
          '": "severity" must be a string';
        Exit;
      end;
      if not SeverityFromName(lSeverityData.AsString, lSev) then
      begin
        aError := 'rule "' + lSetting.RuleId + '": unknown severity "' +
          lSeverityData.AsString + '"';
        Exit;
      end;
      lSetting.HasSeverity := True;
      lSetting.Severity := lSev;
    end;

    if not ParseRuleParams(lRuleObj, lSetting, aError) then
      Exit;

    SetLength(aConfig.Rules, Length(aConfig.Rules) + 1);
    aConfig.Rules[High(aConfig.Rules)] := lSetting;
  end;
  Result := True;
end;


// Reads one integer gate axis from aGateObj into aTarget when present.
// An absentkey is a no-op; a present-but-non-integer value is an error.
function ReadGateInt(aGateObj: TJSONObject; const aKey: string;
  var aTarget: integer; out aError: string): boolean;
var
  lData: TJSONData;
begin
  Result := False;
  aError := '';
  lData := aGateObj.Find(aKey);
  if lData = nil then
  begin
    Result := True;
    Exit;
  end;
  if (lData.JSONType <> jtNumber) or
    (TJSONNumber(lData).NumberType = ntFloat) then
  begin
    aError := 'gate "' + aKey + '" must be an integer';
    Exit;
  end;
  aTarget := lData.AsInteger;
  Result := True;
end;


// Overlays the optional "gate" object onto aConfig's thresholds.
function OverlayGate(aRoot: TJSONObject; var aConfig: TFpSonarConfig;
  out aError: string): boolean;
var
  lGate: TJSONData;
  lGateObj: TJSONObject;
begin
  Result := False;
  aError := '';
  lGate := aRoot.Find('gate');
  if lGate = nil then
  begin
    Result := True;
    Exit;
  end;
  if not (lGate is TJSONObject) then
  begin
    aError := '"gate" must be a JSON object';
    Exit;
  end;
  lGateObj := TJSONObject(lGate);
  if not ReadGateInt(lGateObj, 'maxBlocker',
    aConfig.Gate.MaxPerSeverity[sevBlocker], aError) then Exit;
  if not ReadGateInt(lGateObj, 'maxCritical',
    aConfig.Gate.MaxPerSeverity[sevCritical], aError) then Exit;
  if not ReadGateInt(lGateObj, 'maxMajor',
    aConfig.Gate.MaxPerSeverity[sevMajor], aError) then Exit;
  if not ReadGateInt(lGateObj, 'maxMinor',
    aConfig.Gate.MaxPerSeverity[sevMinor], aError) then Exit;
  if not ReadGateInt(lGateObj, 'maxInfo',
    aConfig.Gate.MaxPerSeverity[sevInfo], aError) then Exit;
  if not ReadGateInt(lGateObj, 'maxTotal', aConfig.Gate.MaxTotal, aError) then
    Exit;
  Result := True;
end;


// Overlays the optional "suppressions" array onto aConfig.
// Each element is an object with optional string "rule"/"path"
function OverlaySuppressions(aRoot: TJSONObject; var aConfig: TFpSonarConfig;
  out aError: string): boolean;
var
  lSupp: TJSONData;
  lArr: TJSONArray;
  lElem: TJSONData;
  lObj: TJSONObject;
  lRuleData, lPathData: TJSONData;
  lGlob: TFpSonarSuppressionGlob;
  i: integer;
begin
  Result := False;
  aError := '';
  lSupp := aRoot.Find('suppressions');
  if lSupp = nil then
  begin
    Result := True;
    Exit;
  end;
  if not (lSupp is TJSONArray) then
  begin
    aError := '"suppressions" must be a JSON array';
    Exit;
  end;
  lArr := TJSONArray(lSupp);
  for i := 0 to lArr.Count - 1 do
  begin
    lElem := lArr.Items[i];
    if not (lElem is TJSONObject) then
    begin
      aError := 'suppression entry ' + IntToStr(i) + ' must be a JSON object';
      Exit;
    end;
    lObj := TJSONObject(lElem);

    lGlob.RulePattern := '';
    lGlob.PathPattern := '';

    lRuleData := lObj.Find('rule');
    if lRuleData <> nil then
    begin
      if lRuleData.JSONType <> jtString then
      begin
        aError := 'suppression entry ' + IntToStr(i) + ': "rule" must be a string';
        Exit;
      end;
      lGlob.RulePattern := lRuleData.AsString;
    end;

    lPathData := lObj.Find('path');
    if lPathData <> nil then
    begin
      if lPathData.JSONType <> jtString then
      begin
        aError := 'suppression entry ' + IntToStr(i) + ': "path" must be a string';
        Exit;
      end;
      lGlob.PathPattern := lPathData.AsString;
    end;

    // An entry with neither rule nor path would suppress everything — reject it.
    if (lRuleData = nil) and (lPathData = nil) then
    begin
      aError := 'suppression entry ' + IntToStr(i) +
        ' must have at least one of "rule" or "path"';
      Exit;
    end;

    SetLength(aConfig.Suppressions, Length(aConfig.Suppressions) + 1);
    aConfig.Suppressions[High(aConfig.Suppressions)] := lGlob;
  end;
  Result := True;
end;


// Overlays the optional top-level "useTier" object onto aConfig.
function OverlayUseTier(aRoot: TJSONObject; var aConfig: TFpSonarConfig;
  out aError: string): boolean;
var
  lUseTier, lResData: TJSONData;
  lUseTierObj: TJSONObject;
  lValue: string;
begin
  Result := False;
  aError := '';
  lUseTier := aRoot.Find('useTier');
  if lUseTier = nil then
  begin
    Result := True;
    Exit;
  end;
  if not (lUseTier is TJSONObject) then
  begin
    aError := '"useTier" must be a JSON object';
    Exit;
  end;
  lUseTierObj := TJSONObject(lUseTier);
  lResData := lUseTierObj.Find('resolution');
  if lResData = nil then
  begin
    Result := True;
    Exit;
  end;
  if lResData.JSONType <> jtString then
  begin
    aError := 'useTier "resolution" must be a string';
    Exit;
  end;
  lValue := LowerCase(Trim(lResData.AsString));
  if lValue = 'off' then
    aConfig.UseTierResolution := utrOff
  else if lValue = 'prefer' then
    aConfig.UseTierResolution := utrPrefer
  else
  begin
    aError := 'useTier "resolution" must be "off" or "prefer"';
    Exit;
  end;
  Result := True;
end;


function TFpSonarConfig.LoadFromJSON(const aJsonText: string;
  out aError: string): boolean;
var
  lData: TJSONData;
  lText: string;
begin
  Result := False;
  aError := '';
  Self := TFpSonarConfig.Default;
  lData := nil;
  // Strip a leading UTF-8 BOM if present.
  lText := aJsonText;
  if (Length(lText) >= 3) and (lText[1] = #$EF) and (lText[2] = #$BB)
    and (lText[3] = #$BF) then
    Delete(lText, 1, 3);
  try
    // Malformed JSON raises EJSONParser/EScannerError — convert to False.
    try
      lData := GetJSON(lText);
    except
      on E: Exception do
      begin
        aError := 'invalid JSON: ' + E.Message;
        Exit;
      end;
    end;
    if not (lData is TJSONObject) then
    begin
      aError := 'config root must be a JSON object';
      Exit;
    end;
    // Overlay onto TFpSonarConfig.Default; unknown keys (e.g. _fpsonar, naming) ignored.
    if not OverlayRules(TJSONObject(lData), Self, aError) then
      Exit;
    if not OverlayGate(TJSONObject(lData), Self, aError) then
      Exit;
    if not OverlaySuppressions(TJSONObject(lData), Self, aError) then
      Exit;
    if not OverlayUseTier(TJSONObject(lData), Self, aError) then
      Exit;
    Result := True;
  finally
    lData.Free;
  end;
end;


function TFpSonarConfig.LoadFromFile(const aPath: string;
  out aError: string): boolean;
var
  lStream: TFileStream;
  lText: string;
begin
  Self := TFpSonarConfig.Default;
  aError := '';
  if not FileExists(aPath) then
  begin
    Result := False;
    aError := 'config file not found: ' + aPath;
    Exit;
  end;
  // Read raw bytes (no encoding transform) so UTF-8 reaches the JSON parser intact.
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
      aError := 'cannot read config file: ' + E.Message;
      Exit;
    end;
  end;
  Result := Self.LoadFromJSON(lText, aError);
end;


function TFpSonarConfig.ToJSON(const aRules: array of TRuleMetadata;
  aMode: TFpSonarConfigEmitMode): string;

  // Case-insensitive RuleId insertion sort, so the emitted file is easy to scan.
  procedure SortByRuleId(var aList: array of TRuleMetadata);
  var
    i, j: integer;
    lTmp: TRuleMetadata;
  begin
    for i := 1 to High(aList) do
    begin
      lTmp := aList[i];
      j := i - 1;
      while (j >= 0) and (CompareText(aList[j].RuleId, lTmp.RuleId) > 0) do
      begin
        aList[j + 1] := aList[j];
        Dec(j);
      end;
      aList[j + 1] := lTmp;
    end;
  end;

  // The override setting for aRuleId in aConfig, or -1 when the rule has none.
  function SettingIndex(const aRuleId: string): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i := 0 to High(Self.Rules) do
      if SameText(Self.Rules[i].RuleId, aRuleId) then
        Exit(i);
  end;

  // The configured parameter aKey under setting aIdx (>=0), or False when unset.
  function FindParam(aIdx: integer; const aKey: string;
    out aParam: TFpSonarRuleParam): boolean;
  var
    j: integer;
  begin
    Result := False;
    if aIdx < 0 then
      Exit;
    for j := 0 to High(Self.Rules[aIdx].Params) do
      if Self.Rules[aIdx].Params[j].Key = aKey then
      begin
        aParam := Self.Rules[aIdx].Params[j];
        Exit(True);
      end;
  end;

  // The inverse of ParseTargetsArray: 'name' canonical, 'message' only when
  // non-empty, 'severity' only when explicitly overridden.
  function TargetsToJSON(const aTargets: TFpSonarRuleTargetArray): TJSONArray;
  var
    k: integer;
    lObj: TJSONObject;
  begin
    Result := TJSONArray.Create;
    for k := 0 to High(aTargets) do
    begin
      lObj := TJSONObject.Create;
      lObj.Add('name', aTargets[k].Pattern);
      if aTargets[k].Message <> '' then
        lObj.Add('message', aTargets[k].Message);
      if aTargets[k].HasSeverity then
        lObj.Add('severity', SeverityName(aTargets[k].Severity));
      Result.Add(lObj);
    end;
  end;

  // Emits parameter aSpec into aParams: cemFull always writes it 
  // cemDiff writes it only when the configured value differs from that default.
  procedure EmitParamValue(aParams: TJSONObject; const aSpec: TRuleParamSpec;
    aIdx: integer);
  var
    lParam: TFpSonarRuleParam;
    lHas: boolean;
    lDefInt: integer;
    lDefBool: boolean;
  begin
    lHas := FindParam(aIdx, aSpec.Name, lParam);
    case aSpec.Kind of
      rpkInt:
      begin
        lDefInt := StrToIntDef(aSpec.DefaultValue, 0);
        if lHas and (lParam.Kind = cpkInt) then
        begin
          if (aMode = cemFull) or (lParam.IntVal <> lDefInt) then
            aParams.Add(aSpec.Name, lParam.IntVal);
        end
        else if aMode = cemFull then
          aParams.Add(aSpec.Name, lDefInt);
      end;
      rpkBool:
      begin
        lDefBool := SameText(aSpec.DefaultValue, 'true');
        if lHas and (lParam.Kind = cpkBool) then
        begin
          if (aMode = cemFull) or (lParam.BoolVal <> lDefBool) then
            aParams.Add(aSpec.Name, lParam.BoolVal);
        end
        else if aMode = cemFull then
          aParams.Add(aSpec.Name, lDefBool);
      end;
      rpkTargets:
      begin
        // The built-in default is an empty disallow-list.
        if lHas and (lParam.Kind = cpkTargets) and (Length(lParam.Targets) > 0) then
          aParams.Add(aSpec.Name, TargetsToJSON(lParam.Targets))
        else if aMode = cemFull then
          aParams.Add(aSpec.Name, TJSONArray.Create);
      end;
    else
      // rpkString and rpkRegex are both a verbatim JSON string.
      if lHas and (lParam.Kind = cpkStr) then
      begin
        if (aMode = cemFull) or (lParam.StrVal <> aSpec.DefaultValue) then
          aParams.Add(aSpec.Name, lParam.StrVal);
      end
      else if aMode = cemFull then
        aParams.Add(aSpec.Name, aSpec.DefaultValue);
    end;
  end;

  // The JSON object for one rule, or nil when aMode=cemDiff and the rule's
  // effective setting matches its metadata default in every field.
  function BuildRule(const aMeta: TRuleMetadata): TJSONObject;
  var
    lIdx, j: integer;
    lEnabled: boolean;
    lSev: TFpSonarSeverity;
    lParams: TJSONObject;
  begin
    Result := TJSONObject.Create;
    lIdx := SettingIndex(aMeta.RuleId);

    lEnabled := aMeta.DefaultEnabled;
    if (lIdx >= 0) and Self.Rules[lIdx].HasEnabled then
      lEnabled := Self.Rules[lIdx].Enabled;
    if (aMode = cemFull) or (lEnabled <> aMeta.DefaultEnabled) then
      Result.Add('enabled', lEnabled);

    lSev := aMeta.Severity;
    if (lIdx >= 0) and Self.Rules[lIdx].HasSeverity then
      lSev := Self.Rules[lIdx].Severity;
    if (aMode = cemFull) or (lSev <> aMeta.Severity) then
      Result.Add('severity', SeverityName(lSev));

    if Length(aMeta.ParamSpecs) > 0 then
    begin
      lParams := TJSONObject.Create;
      for j := 0 to High(aMeta.ParamSpecs) do
        EmitParamValue(lParams, aMeta.ParamSpecs[j], lIdx);
      if (aMode = cemFull) or (lParams.Count > 0) then
        Result.Add('params', lParams)
      else
        lParams.Free;
    end;

    // cemDiff drops an all-default rule entirely.
    if (aMode = cemDiff) and (Result.Count = 0) then
      FreeAndNil(Result);
  end;

var
  lRoot, lStamp, lRules, lRule, lGate, lUseTier, lSupp: TJSONObject;
  lSupps: TJSONArray;
  lSorted: array of TRuleMetadata;
  i: integer;
begin
  SetLength(lSorted, Length(aRules));
  for i := 0 to High(aRules) do
    lSorted[i] := aRules[i];
  SortByRuleId(lSorted);

  lRoot := TJSONObject.Create;
  try
    // Optional provenance stamp; ignored by the loader.
    lStamp := TJSONObject.Create;
    lStamp.Add('config', 'fpsonar-default');
    lStamp.Add('version', '1');
    lRoot.Add('_fpsonar', lStamp);

    // One entry per rule. 
    lRules := TJSONObject.Create;
    for i := 0 to High(lSorted) do
    begin
      if lSorted[i].RuleId = '' then
        Continue;
      lRule := BuildRule(lSorted[i]);
      if lRule <> nil then
        lRules.Add(lSorted[i].RuleId, lRule);
    end;
    lRoot.Add('rules', lRules);

    // Quality-gate thresholds (-1 = unlimited on that axis).
    lGate := TJSONObject.Create;
    lGate.Add('maxBlocker', Self.Gate.MaxPerSeverity[sevBlocker]);
    lGate.Add('maxCritical', Self.Gate.MaxPerSeverity[sevCritical]);
    lGate.Add('maxMajor', Self.Gate.MaxPerSeverity[sevMajor]);
    lGate.Add('maxMinor', Self.Gate.MaxPerSeverity[sevMinor]);
    lGate.Add('maxInfo', Self.Gate.MaxPerSeverity[sevInfo]);
    lGate.Add('maxTotal', Self.Gate.MaxTotal);
    lRoot.Add('gate', lGate);

    // USE-tier reference engine selector.
    lUseTier := TJSONObject.Create;
    if Self.UseTierResolution = utrPrefer then
      lUseTier.Add('resolution', 'prefer')
    else
      lUseTier.Add('resolution', 'off');
    lRoot.Add('useTier', lUseTier);

    // Suppression globs, in authored order.
    lSupps := TJSONArray.Create;
    for i := 0 to High(Self.Suppressions) do
    begin
      lSupp := TJSONObject.Create;
      if Self.Suppressions[i].RulePattern <> '' then
        lSupp.Add('rule', Self.Suppressions[i].RulePattern);
      if Self.Suppressions[i].PathPattern <> '' then
        lSupp.Add('path', Self.Suppressions[i].PathPattern);
      lSupps.Add(lSupp);
    end;
    lRoot.Add('suppressions', lSupps);

    Result := lRoot.FormatJSON;
  finally
    lRoot.Free;
  end;
end;


function TFpSonarConfig.TemplateToJSON(const aRules: array of TRuleMetadata): string;

begin
  Result := Self.ToJSON(aRules, cemFull);
end;


end.
