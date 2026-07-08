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

  // The ruleset + gate policy loaded from the JSON --config file.
  TFpSonarConfig = record
    Rules: TFpSonarRuleSettingArray;
    Gate: TFpSonarGateThresholds;
    Suppressions: TFpSonarSuppressionGlobArray;
    // The USE-tier resolution preference. Defaults to utrOff.
    UseTierResolution: TFpSonarUseTierResolution;
  end;

// The built-in default:
// no rule overrides (empty Rules => every rule keeps its metadata DefaultEnabled + default severity);
// Check fails on any Blocker or Critical and is unlimited for Major/Minor/Info and Total.
function DefaultConfig: TFpSonarConfig;

// The lowercase enum name for a severity (info/minor/major/critical/blocker).
function SeverityName(aSeverity: TFpSonarSeverity): string;

// Parses a (case-insensitive) severity name; False on an unknown name.
function SeverityFromName(const aName: string; out aSeverity: TFpSonarSeverity): boolean;

// The effective enable state for aRuleId: an explicit Enabled when the rule
// has a setting with HasEnabled, else aDefaultEnabled.
function RuleEnabled(const aConfig: TFpSonarConfig; const aRuleId: string;
  aDefaultEnabled: boolean): boolean;

// The effective severity for aRuleId: an explicit Severity when the rule
// has a setting with HasSeverity, else aDefaultSeverity.
function EffectiveSeverity(const aConfig: TFpSonarConfig; const aRuleId: string;
  aDefaultSeverity: TFpSonarSeverity): TFpSonarSeverity;

// The typed value of the 'aKey' parameter configured for aRuleId, or aDefault
// when the rule has no such param of the matching kind.
function RuleParamInt(const aConfig: TFpSonarConfig; const aRuleId, aKey: string;
  aDefault: integer): integer;

// As RuleParamInt, for a string-valued parameter (e.g. a naming regex pattern).
function RuleParamStr(const aConfig: TFpSonarConfig;
  const aRuleId, aKey, aDefault: string): string;

// As RuleParamInt, for a boolean-valued parameter.
function RuleParamBool(const aConfig: TFpSonarConfig; const aRuleId, aKey: string;
  aDefault: boolean): boolean;

// The configured 'targets' disallow-list for (aRuleId, aKey), or an empty array when the rule has no such param.
function RuleParamTargets(const aConfig: TFpSonarConfig;
  const aRuleId, aKey: string): TFpSonarRuleTargetArray;

// Returns a new issue array in the same order, each issue's Severity replaced by EffectiveSeverity.
function ApplyRuleConfig(const aIssues: TFpSonarIssueArray;
  const aConfig: TFpSonarConfig): TFpSonarIssueArray;

// Parses a JSON config text, overlaying present keys onto DefaultConfig.
// Returns False + aError on malformed/invalid input.
function LoadConfigFromJSON(const aJsonText: string; out aConfig: TFpSonarConfig;
  out aError: string): boolean;

// Reads aPath and delegates to LoadConfigFromJSON. A missing/unreadable file is a False + aError.
function LoadConfigFromFile(const aPath: string; out aConfig: TFpSonarConfig;
  out aError: string): boolean;

// Builds a complete, human-editable JSON config template: every rule in aRules
// listed (RuleId-sorted) with its default enabled state and severity, plus the
// quality-gate and use-tier policy from aConfig and an empty suppressions list.
// Intended as a starting point the user copies to a file and edits.
function ConfigTemplateToJSON(const aConfig: TFpSonarConfig;
  const aRules: array of TRuleMetadata): string;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, FpJson.Data, FpJson.Parser;
{$ELSE}
  SysUtils, Classes, fpjson, jsonparser;
{$ENDIF}

function DefaultConfig: TFpSonarConfig;
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


function RuleEnabled(const aConfig: TFpSonarConfig; const aRuleId: string;
  aDefaultEnabled: boolean): boolean;
var
  i: integer;
begin
  Result := aDefaultEnabled;
  for i := 0 to High(aConfig.Rules) do
    if (aConfig.Rules[i].RuleId = aRuleId) and aConfig.Rules[i].HasEnabled then
    begin
      Result := aConfig.Rules[i].Enabled;
      Exit;
    end;
end;


function EffectiveSeverity(const aConfig: TFpSonarConfig; const aRuleId: string;
  aDefaultSeverity: TFpSonarSeverity): TFpSonarSeverity;
var
  i: integer;
begin
  Result := aDefaultSeverity;
  for i := 0 to High(aConfig.Rules) do
    if (aConfig.Rules[i].RuleId = aRuleId) and aConfig.Rules[i].HasSeverity then
    begin
      Result := aConfig.Rules[i].Severity;
      Exit;
    end;
end;


// Finds the configured param record for (aRuleId, aKey), or False when absent.
// The shared lookup behind the three typed RuleParam* accessors.
function FindConfiguredParam(const aConfig: TFpSonarConfig;
  const aRuleId, aKey: string; out aParam: TFpSonarRuleParam): boolean;
var
  i, j: integer;
begin
  Result := False;
  for i := 0 to High(aConfig.Rules) do
    if aConfig.Rules[i].RuleId = aRuleId then
    begin
      for j := 0 to High(aConfig.Rules[i].Params) do
        if aConfig.Rules[i].Params[j].Key = aKey then
        begin
          aParam := aConfig.Rules[i].Params[j];
          Result := True;
          Exit;
        end;
      Exit;
    end;
end;


function RuleParamInt(const aConfig: TFpSonarConfig; const aRuleId, aKey: string;
  aDefault: integer): integer;
var
  lParam: TFpSonarRuleParam;
begin
  Result := aDefault;
  if FindConfiguredParam(aConfig, aRuleId, aKey, lParam)
    and (lParam.Kind = cpkInt) then
    Result := lParam.IntVal;
end;


function RuleParamStr(const aConfig: TFpSonarConfig;
  const aRuleId, aKey, aDefault: string): string;
var
  lParam: TFpSonarRuleParam;
begin
  Result := aDefault;
  if FindConfiguredParam(aConfig, aRuleId, aKey, lParam)
    and (lParam.Kind = cpkStr) then
    Result := lParam.StrVal;
end;


function RuleParamBool(const aConfig: TFpSonarConfig;
  const aRuleId, aKey: string; aDefault: boolean): boolean;
var
  lParam: TFpSonarRuleParam;
begin
  Result := aDefault;
  if FindConfiguredParam(aConfig, aRuleId, aKey, lParam)
    and (lParam.Kind = cpkBool) then
    Result := lParam.BoolVal;
end;


function RuleParamTargets(const aConfig: TFpSonarConfig;
  const aRuleId, aKey: string): TFpSonarRuleTargetArray;
var
  lParam: TFpSonarRuleParam;
begin
  SetLength(Result, 0);
  if FindConfiguredParam(aConfig, aRuleId, aKey, lParam)
    and (lParam.Kind = cpkTargets) then
    Result := lParam.Targets;
end;


function ApplyRuleConfig(const aIssues: TFpSonarIssueArray;
  const aConfig: TFpSonarConfig): TFpSonarIssueArray;
var
  i: integer;
begin
  SetLength(Result, Length(aIssues));
  for i := 0 to High(aIssues) do
  begin
    // Copy the whole record (Fingerprint included, untouched), then override
    // only the severity — order is preserved by the linear copy.
    Result[i] := aIssues[i];
    Result[i].Severity := EffectiveSeverity(aConfig, aIssues[i].RuleId,
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


function LoadConfigFromJSON(const aJsonText: string;
  out aConfig: TFpSonarConfig; out aError: string): boolean;
var
  lData: TJSONData;
  lText: string;
begin
  Result := False;
  aError := '';
  aConfig := DefaultConfig;
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
    // Overlay onto DefaultConfig; unknown keys (e.g. _fpsonar, naming) ignored.
    if not OverlayRules(TJSONObject(lData), aConfig, aError) then
      Exit;
    if not OverlayGate(TJSONObject(lData), aConfig, aError) then
      Exit;
    if not OverlaySuppressions(TJSONObject(lData), aConfig, aError) then
      Exit;
    if not OverlayUseTier(TJSONObject(lData), aConfig, aError) then
      Exit;
    Result := True;
  finally
    lData.Free;
  end;
end;


function LoadConfigFromFile(const aPath: string;
  out aConfig: TFpSonarConfig; out aError: string): boolean;
var
  lStream: TFileStream;
  lText: string;
begin
  aConfig := DefaultConfig;
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
  Result := LoadConfigFromJSON(lText, aConfig, aError);
end;


function ConfigTemplateToJSON(const aConfig: TFpSonarConfig;
  const aRules: array of TRuleMetadata): string;

  // Adds one param's built-in default to aParams, typed per its declared Kind,
  // so the emitted value round-trips to exactly the rule's default.
  procedure EmitParam(aParams: TJSONObject; const aSpec: TRuleParamSpec);
  begin
    case aSpec.Kind of
      rpkInt: aParams.Add(aSpec.Name, StrToIntDef(aSpec.DefaultValue, 0));
      rpkBool: aParams.Add(aSpec.Name, SameText(aSpec.DefaultValue, 'true'));
      // An empty disallow-list; the user adds { "name": ... } entries.
      rpkTargets: aParams.Add(aSpec.Name, TJSONArray.Create);
    else
      // rpkString and rpkRegex are both emitted verbatim as a JSON string.
      aParams.Add(aSpec.Name, aSpec.DefaultValue);
    end;
  end;

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

var
  lRoot, lStamp, lRules, lRule, lParams, lGate, lUseTier: TJSONObject;
  lSorted: array of TRuleMetadata;
  i, j: integer;
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

    // One entry per rule: its default enabled state, severity, and — for a rule
    // that has tunable parameters — a "params" object listing each one at its
    // built-in default, so the file shows every knob the user can turn.
    lRules := TJSONObject.Create;
    for i := 0 to High(lSorted) do
    begin
      if lSorted[i].RuleId = '' then
        Continue;
      lRule := TJSONObject.Create;
      lRule.Add('enabled', lSorted[i].DefaultEnabled);
      lRule.Add('severity', SeverityName(lSorted[i].Severity));
      if Length(lSorted[i].ParamSpecs) > 0 then
      begin
        lParams := TJSONObject.Create;
        for j := 0 to High(lSorted[i].ParamSpecs) do
          EmitParam(lParams, lSorted[i].ParamSpecs[j]);
        lRule.Add('params', lParams);
      end;
      lRules.Add(lSorted[i].RuleId, lRule);
    end;
    lRoot.Add('rules', lRules);

    // Quality-gate thresholds (-1 = unlimited on that axis).
    lGate := TJSONObject.Create;
    lGate.Add('maxBlocker', aConfig.Gate.MaxPerSeverity[sevBlocker]);
    lGate.Add('maxCritical', aConfig.Gate.MaxPerSeverity[sevCritical]);
    lGate.Add('maxMajor', aConfig.Gate.MaxPerSeverity[sevMajor]);
    lGate.Add('maxMinor', aConfig.Gate.MaxPerSeverity[sevMinor]);
    lGate.Add('maxInfo', aConfig.Gate.MaxPerSeverity[sevInfo]);
    lGate.Add('maxTotal', aConfig.Gate.MaxTotal);
    lRoot.Add('gate', lGate);

    // USE-tier reference engine selector.
    lUseTier := TJSONObject.Create;
    if aConfig.UseTierResolution = utrPrefer then
      lUseTier.Add('resolution', 'prefer')
    else
      lUseTier.Add('resolution', 'off');
    lRoot.Add('useTier', lUseTier);

    // No suppression globs by default.
    lRoot.Add('suppressions', TJSONArray.Create);

    Result := lRoot.FormatJSON;
  finally
    lRoot.Free;
  end;
end;


end.
