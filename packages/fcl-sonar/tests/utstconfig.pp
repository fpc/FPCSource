{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the JSON ruleset/gate config loader

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstConfig;

{ TFpSonarConfig tests: the JSON loader (load/override/forward-compat/
  malformed), the pure lookup/transform helpers, and the shipped default-json
  round-trip. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.RuleFramework,
  FpSonar.Rules.Structure, FpSonar.Rules.Naming, FpSonar.Rules.Trackers;

type
  { TFpSonarConfig load/override/gate tests. }
  TConfigTest = class(TTestCase)
  private
    function ConfigPath(const aName: string): string;
    function DefaultJsonPath: string;
    function MakeSynthIssue(const aRuleId: string;
      aSeverity: TFpSonarSeverity; const aFingerprint: string): TFpSonarIssue;
  published
    procedure DefaultConfigGateAndNoOverrides;
    procedure LoadsRuleEnableDisableAndSeverity;
    procedure LoadsGateThresholds;
    procedure ApplyRuleConfigOverridesSeverityKeepsOrderAndFingerprint;
    procedure IgnoresUnknownKeysForwardCompat;
    procedure RejectsMalformedJsonAndBadValues;
    procedure ToleratesLeadingUtf8Bom;
    procedure LoadsSuppressionGlobs;
    procedure RejectsBadSuppressions;
    procedure DefaultJsonFileRoundTrips;
    // Typed per-rule params.
    procedure ParsesNestedParamsObject;
    procedure RuleParamAccessorsReturnConfiguredValues;
    procedure RuleParamAccessorsFallBackToDefault;
    procedure RejectsFloatParamValueAtLoad;
    procedure ValidateAcceptsValidParams;
    procedure ValidateRejectsUnknownParamKey;
    procedure ValidateRejectsWrongType;
    procedure ValidateRejectsBadRegex;
    procedure ValidateIgnoresUnknownRuleId;
    // USE-tier resolution flag stub.
    procedure UseTierResolutionDefaultsOffAndParses;
    procedure RejectsBadUseTierResolution;
    // 'targets' list-of-objects param (config-driven trackers).
    procedure ParsesTargetsArrayAndAccessorReturnsIt;
    procedure RejectsBadTargets;
    procedure ValidateTargetsKindContract;
    // matchUnresolvedByName atomic bool; rides the bool path.
    procedure ParsesMatchUnresolvedByNameBool;
    procedure ValidateRejectsNonBoolMatchUnresolved;
    // ConfigToJSON serializer: round-trip, diff-from-default, full-shape stability.
    procedure SerializerRoundTripsDiffOverModelledFields;
    procedure SerializerDiffOfDefaultConfigIsEmpty;
    procedure SerializerFullShapeByteStable;
  end;


implementation

function TConfigTest.ConfigPath(const aName: string): string;

const
  cThisFile = {$I %FILE%};

var
  lCandidates: array[0..2] of string;
  lDir: string;
  i: Integer;

begin
  lDir := ExtractFilePath(ExpandFileName(cThisFile));
  // tests/core/../../config/<aName>
  lCandidates[0] := lDir + '..' + DirectorySeparator + '..' +
    DirectorySeparator + 'config' + DirectorySeparator + aName;
  lCandidates[1] := ExpandFileName('config' + DirectorySeparator + aName);
  lCandidates[2] := ExtractFilePath(ParamStr(0)) + aName;

  Result := lCandidates[0];
  for i := Low(lCandidates) to High(lCandidates) do
    if FileExists(lCandidates[i]) then
      begin
        Result := lCandidates[i];
        Exit;
      end;
end;


function TConfigTest.DefaultJsonPath: string;

begin
  Result := ConfigPath('fpsonar.default.json');
end;


function TConfigTest.MakeSynthIssue(const aRuleId: string;
  aSeverity: TFpSonarSeverity; const aFingerprint: string): TFpSonarIssue;

begin
  Result := TFpSonarIssue.Make(aRuleId, 'synthetic.pas', 1, 1, 1, 1, aSeverity,
    itCodeSmell, cfHigh, 'rule.' + aRuleId + '.message', []);
  Result.Fingerprint := aFingerprint;
end;


procedure TConfigTest.DefaultConfigGateAndNoOverrides;

var
  lCfg: TFpSonarConfig;

begin
  lCfg := DefaultConfig;
  AssertEquals('blocker max = 0', 0, lCfg.Gate.MaxPerSeverity[sevBlocker]);
  AssertEquals('critical max = 0', 0, lCfg.Gate.MaxPerSeverity[sevCritical]);
  AssertEquals('major unlimited', -1, lCfg.Gate.MaxPerSeverity[sevMajor]);
  AssertEquals('minor unlimited', -1, lCfg.Gate.MaxPerSeverity[sevMinor]);
  AssertEquals('info unlimited', -1, lCfg.Gate.MaxPerSeverity[sevInfo]);
  AssertEquals('total unlimited', -1, lCfg.Gate.MaxTotal);
  AssertEquals('no rule overrides', 0, Length(lCfg.Rules));

  // No setting => the passed default is returned (both directions).
  AssertTrue('default-enabled rule stays enabled',
    RuleEnabled(lCfg, 'AnyRule', True));
  AssertFalse('default-disabled rule stays disabled',
    RuleEnabled(lCfg, 'AnyRule', False));
  AssertTrue('default severity returned unchanged',
    EffectiveSeverity(lCfg, 'AnyRule', sevMajor) = sevMajor);
end;


procedure TConfigTest.LoadsRuleEnableDisableAndSeverity;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  AssertTrue('config loads',
    LoadConfigFromJSON('{"rules":{"FooRule":{"enabled":false},' +
      '"BarRule":{"severity":"blocker"}}}', lCfg, lErr));
  AssertFalse('FooRule explicitly disabled',
    RuleEnabled(lCfg, 'FooRule', True));
  AssertTrue('BarRule severity overridden to blocker',
    EffectiveSeverity(lCfg, 'BarRule', sevMinor) = sevBlocker);
  // An unmentioned rule falls back to its passed defaults.
  AssertTrue('unmentioned rule keeps default enable',
    RuleEnabled(lCfg, 'OtherRule', True));
  AssertTrue('unmentioned rule keeps default severity',
    EffectiveSeverity(lCfg, 'OtherRule', sevMinor) = sevMinor);
  // BarRule has only a severity override => enable falls back to default.
  AssertFalse('BarRule enable falls back to default',
    RuleEnabled(lCfg, 'BarRule', False));
end;


procedure TConfigTest.LoadsGateThresholds;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  AssertTrue('config loads',
    LoadConfigFromJSON('{"gate":{"maxMajor":0,"maxTotal":5}}', lCfg, lErr));
  // The overlay keeps the strict blocker/critical defaults...
  AssertEquals('blocker stays 0', 0, lCfg.Gate.MaxPerSeverity[sevBlocker]);
  AssertEquals('critical stays 0', 0, lCfg.Gate.MaxPerSeverity[sevCritical]);
  // ...and applies the present axes.
  AssertEquals('major -> 0', 0, lCfg.Gate.MaxPerSeverity[sevMajor]);
  AssertEquals('total -> 5', 5, lCfg.Gate.MaxTotal);
  // An axis not mentioned keeps the default.
  AssertEquals('minor stays unlimited', -1, lCfg.Gate.MaxPerSeverity[sevMinor]);
end;


procedure TConfigTest.ApplyRuleConfigOverridesSeverityKeepsOrderAndFingerprint;

var
  lCfg: TFpSonarConfig;
  lErr: string;
  lIssues, lResult: TFpSonarIssueArray;

begin
  SetLength(lIssues, 2);
  lIssues[0] := MakeSynthIssue('BarRule', sevMinor, 'fp-bar-0001');
  lIssues[1] := MakeSynthIssue('ParseError', sevMajor, 'fp-parse-0002');

  AssertTrue('config loads',
    LoadConfigFromJSON('{"rules":{"BarRule":{"severity":"blocker"},' +
      '"ParseError":{"severity":"critical"}}}', lCfg, lErr));

  lResult := ApplyRuleConfig(lIssues, lCfg);

  AssertEquals('same length', 2, Length(lResult));
  // Order preserved + severities overridden.
  AssertEquals('index 0 stays BarRule', 'BarRule', lResult[0].RuleId);
  AssertTrue('BarRule -> blocker', lResult[0].Severity = sevBlocker);
  AssertEquals('index 1 stays ParseError', 'ParseError', lResult[1].RuleId);
  AssertTrue('ParseError -> critical', lResult[1].Severity = sevCritical);
  // Fingerprints untouched (severity is not a fingerprint input).
  AssertEquals('fingerprint 0 untouched', 'fp-bar-0001', lResult[0].Fingerprint);
  AssertEquals('fingerprint 1 untouched', 'fp-parse-0002',
    lResult[1].Fingerprint);
end;


procedure TConfigTest.IgnoresUnknownKeysForwardCompat;

var
  lWith, lWithout: TFpSonarConfig;
  lErr: string;

begin
  // NOTE: 'suppressions' is now a KNOWN, strictly-validated key, so a
  // string-array 'suppressions' would be rejected — use a still-unknown key
  // ('trackers') to keep proving future unknown keys don't break the loader.
  AssertTrue('config with unknown keys loads',
    LoadConfigFromJSON('{"naming":{"x":1},"trackers":["a","b"],' +
      '"_fpsonar":{"config":"x"},"gate":{"maxMajor":3}}', lWith, lErr));
  AssertTrue('config without unknown keys loads',
    LoadConfigFromJSON('{"gate":{"maxMajor":3}}', lWithout, lErr));

  AssertEquals('same major threshold despite unknown keys',
    lWithout.Gate.MaxPerSeverity[sevMajor],
    lWith.Gate.MaxPerSeverity[sevMajor]);
  AssertEquals('same rule count despite unknown keys',
    Length(lWithout.Rules), Length(lWith.Rules));
end;


procedure TConfigTest.RejectsMalformedJsonAndBadValues;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // Malformed JSON => False + non-empty error, no crash.
  AssertFalse('malformed JSON rejected',
    LoadConfigFromJSON('{ not json', lCfg, lErr));
  AssertTrue('malformed JSON has an error message', lErr <> '');

  // A non-object root => False.
  AssertFalse('non-object root rejected',
    LoadConfigFromJSON('[1,2,3]', lCfg, lErr));

  // An unrecognised severity => False.
  AssertFalse('bad severity rejected',
    LoadConfigFromJSON('{"rules":{"X":{"severity":"weird"}}}', lCfg, lErr));

  // A non-integer gate value => False.
  AssertFalse('non-integer gate value rejected',
    LoadConfigFromJSON('{"gate":{"maxMajor":"lots"}}', lCfg, lErr));

  // A non-boolean enabled => False.
  AssertFalse('non-boolean enabled rejected',
    LoadConfigFromJSON('{"rules":{"X":{"enabled":"yes"}}}', lCfg, lErr));
end;


procedure TConfigTest.ToleratesLeadingUtf8Bom;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A UTF-8 BOM (EF BB BF) prepended by some editors must not break the loader.
  AssertTrue('BOM-prefixed config loads clean',
    LoadConfigFromJSON(#$EF#$BB#$BF + '{"gate":{"maxMajor":0}}', lCfg, lErr));
  AssertEquals('gate axis applied past the BOM', 0,
    lCfg.Gate.MaxPerSeverity[sevMajor]);
end;


procedure TConfigTest.LoadsSuppressionGlobs;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  AssertTrue('config with suppressions loads',
    LoadConfigFromJSON('{"suppressions":[{"rule":"Naming*"},' +
      '{"path":"*/gen/*"},{"rule":"LineTooLong","path":"*foo*"}]}', lCfg, lErr));
  AssertEquals('three globs loaded', 3, Length(lCfg.Suppressions));
  // Glob 0: rule only => path is the empty (wildcard) pattern.
  AssertEquals('glob 0 rule', 'Naming*', lCfg.Suppressions[0].RulePattern);
  AssertEquals('glob 0 path omitted', '', lCfg.Suppressions[0].PathPattern);
  // Glob 1: path only => rule is the empty (wildcard) pattern.
  AssertEquals('glob 1 rule omitted', '', lCfg.Suppressions[1].RulePattern);
  AssertEquals('glob 1 path', '*/gen/*', lCfg.Suppressions[1].PathPattern);
  // Glob 2: both present.
  AssertEquals('glob 2 rule', 'LineTooLong', lCfg.Suppressions[2].RulePattern);
  AssertEquals('glob 2 path', '*foo*', lCfg.Suppressions[2].PathPattern);
end;


procedure TConfigTest.RejectsBadSuppressions;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A non-array suppressions => False + non-empty error.
  AssertFalse('non-array suppressions rejected',
    LoadConfigFromJSON('{"suppressions":{}}', lCfg, lErr));
  AssertTrue('non-array has an error message', lErr <> '');

  // A non-string rule => False.
  AssertFalse('non-string rule rejected',
    LoadConfigFromJSON('{"suppressions":[{"rule":123}]}', lCfg, lErr));
  AssertTrue('non-string rule has an error message', lErr <> '');

  // An element with neither rule nor path (= "suppress everything") => False.
  AssertFalse('both-empty entry rejected',
    LoadConfigFromJSON('{"suppressions":[{}]}', lCfg, lErr));
  AssertTrue('both-empty entry has an error message', lErr <> '');
end;


procedure TConfigTest.DefaultJsonFileRoundTrips;

var
  lCfg, lDefault: TFpSonarConfig;
  lErr: string;
  lSev: TFpSonarSeverity;

begin
  AssertTrue('fpsonar.default.json loads clean',
    LoadConfigFromFile('fpsonar.default.json', lCfg, lErr));

  lDefault := DefaultConfig;
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    AssertEquals('gate axis matches DefaultConfig (' + SeverityName(lSev) + ')',
      lDefault.Gate.MaxPerSeverity[lSev], lCfg.Gate.MaxPerSeverity[lSev]);
  AssertEquals('total matches DefaultConfig', lDefault.Gate.MaxTotal,
    lCfg.Gate.MaxTotal);
  AssertEquals('empty rules in default json', 0, Length(lCfg.Rules));
  AssertEquals('empty suppressions in default json', 0,
    Length(lCfg.Suppressions));
end;


procedure TConfigTest.ParsesNestedParamsObject;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // rules.<id>.params is parsed alongside enabled/severity, with each value
  // typed from its JSON kind (integer / string / boolean).
  AssertTrue('params config loads', LoadConfigFromJSON(
    '{"rules":{"R":{"enabled":true,"params":' +
    '{"n":5,"s":"abc","b":true}}}}', lCfg, lErr));
  AssertEquals('one rule setting', 1, Length(lCfg.Rules));
  AssertEquals('three params parsed', 3, Length(lCfg.Rules[0].Params));
end;


procedure TConfigTest.RuleParamAccessorsReturnConfiguredValues;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  AssertTrue('params config loads', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"n":42,"s":"hi","b":true}}}}', lCfg, lErr));
  AssertEquals('int param returned', 42, RuleParamInt(lCfg, 'R', 'n', 7));
  AssertEquals('string param returned', 'hi', RuleParamStr(lCfg, 'R', 's', 'd'));
  AssertTrue('bool param returned', RuleParamBool(lCfg, 'R', 'b', False));
end;


procedure TConfigTest.RuleParamAccessorsFallBackToDefault;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // Constant-as-default discipline: an unconfigured rule, a missing key, an
  // unmatched rule id, AND a kind mismatch all return the passed default.
  lCfg := DefaultConfig;
  AssertEquals('no config => default', 9, RuleParamInt(lCfg, 'R', 'n', 9));

  AssertTrue('params config loads', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"n":42,"s":"hi"}}}}', lCfg, lErr));
  AssertEquals('missing key => default', 7, RuleParamInt(lCfg, 'R', 'gone', 7));
  AssertEquals('other rule => default', 7, RuleParamInt(lCfg, 'Other', 'n', 7));
  // 's' is a string param; the int accessor must not coerce it.
  AssertEquals('kind mismatch => default', 7, RuleParamInt(lCfg, 'R', 's', 7));
  AssertEquals('string fallback', 'd', RuleParamStr(lCfg, 'R', 'gone', 'd'));
  AssertTrue('bool fallback', RuleParamBool(lCfg, 'R', 'gone', True));
end;


procedure TConfigTest.RejectsFloatParamValueAtLoad;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A float is no valid param kind => rejected structurally at load (no registry
  // needed), consistent with the existing strict-value error path.
  AssertFalse('float param value rejected at load', LoadConfigFromJSON(
    '{"rules":{"CyclomaticComplexity":{"params":{"maxComplexity":1.5}}}}',
    lCfg, lErr));
  AssertTrue('float rejection has a message', lErr <> '');
end;


procedure TConfigTest.ValidateAcceptsValidParams;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A known int threshold and a known regex pattern, both well-formed => clean.
  AssertTrue('cfg loads', LoadConfigFromJSON(
    '{"rules":{"CyclomaticComplexity":{"params":{"maxComplexity":20}},' +
    '"ClassNaming":{"params":{"pattern":"^X[A-Za-z0-9]*$"}}}}', lCfg, lErr));
  AssertTrue('valid params accepted',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
  AssertEquals('no error on valid params', '', lErr);
end;


procedure TConfigTest.ValidateRejectsUnknownParamKey;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // An unknown param key for a KNOWN rule is a fail-fast error (typo protection).
  AssertTrue('cfg loads', LoadConfigFromJSON(
    '{"rules":{"CyclomaticComplexity":{"params":{"notAKey":1}}}}', lCfg, lErr));
  AssertFalse('unknown param key rejected',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
  AssertTrue('unknown key has a message', lErr <> '');
end;


procedure TConfigTest.ValidateRejectsWrongType;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A string where the rule declares an int threshold => fail-fast (non-int
  // threshold rejected). The value parses fine structurally; the registry knows
  // the declared kind.
  AssertTrue('cfg loads', LoadConfigFromJSON(
    '{"rules":{"CyclomaticComplexity":{"params":{"maxComplexity":"big"}}}}',
    lCfg, lErr));
  AssertFalse('wrong-typed threshold rejected',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
  AssertTrue('wrong type has a message', lErr <> '');
end;


procedure TConfigTest.ValidateRejectsBadRegex;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // An uncompilable regex on a known pattern param => fail-fast.
  AssertTrue('cfg loads', LoadConfigFromJSON(
    '{"rules":{"ClassNaming":{"params":{"pattern":"["}}}}', lCfg, lErr));
  AssertFalse('uncompilable regex rejected',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
  AssertTrue('bad regex has a message', lErr <> '');
end;


procedure TConfigTest.ValidateIgnoresUnknownRuleId;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // "params strict, rule-ids lenient": a config naming a rule the binary
  // does not know (e.g. a newer rule) must NOT break validation, even with params
  // — forward-compat, exactly as enable/severity already behaves.
  AssertTrue('cfg loads', LoadConfigFromJSON(
    '{"rules":{"SomeFutureRule":{"params":{"whatever":123}}}}', lCfg, lErr));
  AssertTrue('unknown rule id ignored by validation',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
end;


procedure TConfigTest.UseTierResolutionDefaultsOffAndParses;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // Default (no useTier key) is the parser-tier name engine.
  AssertTrue('default config uses utrOff',
    DefaultConfig.UseTierResolution = utrOff);
  AssertTrue('absent useTier keeps default (utrOff)',
    LoadConfigFromJSON('{"gate":{"maxMajor":1}}', lCfg, lErr));
  AssertTrue('absent useTier -> utrOff', lCfg.UseTierResolution = utrOff);
  // Both accepted modes parse (case-insensitive).
  AssertTrue('off parses',
    LoadConfigFromJSON('{"useTier":{"resolution":"off"}}', lCfg, lErr));
  AssertTrue('off -> utrOff', lCfg.UseTierResolution = utrOff);
  AssertTrue('prefer parses',
    LoadConfigFromJSON('{"useTier":{"resolution":"Prefer"}}', lCfg, lErr));
  AssertTrue('prefer -> utrPrefer', lCfg.UseTierResolution = utrPrefer);
end;


procedure TConfigTest.RejectsBadUseTierResolution;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // An explicit value the user passed must be one of the two accepted modes
  // (fail-fast); a non-object useTier / non-string resolution also fails.
  AssertFalse('unknown resolution value rejected',
    LoadConfigFromJSON('{"useTier":{"resolution":"maybe"}}', lCfg, lErr));
  AssertTrue('bad value names an error', lErr <> '');
  AssertFalse('non-string resolution rejected',
    LoadConfigFromJSON('{"useTier":{"resolution":1}}', lCfg, lErr));
  AssertFalse('non-object useTier rejected',
    LoadConfigFromJSON('{"useTier":"off"}', lCfg, lErr));
end;


procedure TConfigTest.ParsesTargetsArrayAndAccessorReturnsIt;

var
  lCfg: TFpSonarConfig;
  lErr: string;
  lTargets: TFpSonarRuleTargetArray;

begin
  // A 'params' value that is a JSON array parses into a typed target list; the
  // accessor returns it. 'name'/'glob' are synonyms; severity is parsed + flagged;
  // an atomic int sibling still parses (the array path is additive, not exclusive).
  AssertTrue('targets config loads', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"targets":[' +
    '{"glob":"*/legacy/*","message":"no legacy","severity":"minor"},' +
    '{"name":"System.MaxInt"}],"other":3}}}}', lCfg, lErr));
  lTargets := RuleParamTargets(lCfg, 'R', 'targets');
  AssertEquals('two targets parsed', 2, Length(lTargets));
  AssertEquals('glob -> pattern', '*/legacy/*', lTargets[0].Pattern);
  AssertEquals('message parsed', 'no legacy', lTargets[0].Message);
  AssertTrue('severity flagged', lTargets[0].HasSeverity);
  AssertTrue('severity parsed', lTargets[0].Severity = sevMinor);
  AssertEquals('name -> pattern', 'System.MaxInt', lTargets[1].Pattern);
  AssertFalse('no severity => unflagged', lTargets[1].HasSeverity);
  // Constant-as-default: an unconfigured rule / key yields an empty list.
  AssertEquals('missing key => empty', 0,
    Length(RuleParamTargets(lCfg, 'R', 'gone')));
  AssertEquals('other rule => empty', 0,
    Length(RuleParamTargets(lCfg, 'Other', 'targets')));
end;


procedure TConfigTest.RejectsBadTargets;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A non-object element, a target missing both name and glob, and an unknown
  // severity name are each a fail-fast load error.
  AssertFalse('non-object target element rejected', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"targets":[1,2]}}}}', lCfg, lErr));
  AssertTrue('non-object element names an error', lErr <> '');
  AssertFalse('target without name/glob rejected', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"targets":[{"message":"x"}]}}}}', lCfg, lErr));
  AssertTrue('missing name/glob names an error', lErr <> '');
  AssertFalse('empty name rejected', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"targets":[{"name":""}]}}}}', lCfg, lErr));
  AssertFalse('unknown severity rejected', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"targets":[{"name":"X","severity":"huge"}]}}}}',
    lCfg, lErr));
  AssertTrue('unknown severity names an error', lErr <> '');
  // A non-string severity (object/array) must fail-fast cleanly, NOT crash the
  // loader on AsString (the error text is built from a non-scalar JSON value).
  AssertFalse('non-string severity rejected', LoadConfigFromJSON(
    '{"rules":{"R":{"params":{"targets":[{"name":"X","severity":{}}]}}}}',
    lCfg, lErr));
  AssertTrue('non-string severity names an error', lErr <> '');
end;


procedure TConfigTest.ValidateTargetsKindContract;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A well-formed targets array on a rule that DECLARES rpkTargets validates clean.
  AssertTrue('targets cfg loads', LoadConfigFromJSON(
    '{"rules":{"DisallowedConstant":{"params":{"targets":' +
    '[{"name":"Foo","severity":"minor"}]}}}}', lCfg, lErr));
  AssertTrue('valid targets accepted: ' + lErr,
    ValidateConfigParams(lCfg, RuleRegistry, lErr));

  // An ARRAY value under an ATOMIC-declared key (CyclomaticComplexity.maxComplexity
  // is rpkInt) is a kind mismatch => fail-fast.
  AssertTrue('array-under-atomic cfg loads', LoadConfigFromJSON(
    '{"rules":{"CyclomaticComplexity":{"params":{"maxComplexity":' +
    '[{"name":"x"}]}}}}', lCfg, lErr));
  AssertFalse('array under atomic key rejected',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));

  // Conversely an ATOMIC value under the rpkTargets 'targets' key => kind mismatch.
  AssertTrue('atomic-under-targets cfg loads', LoadConfigFromJSON(
    '{"rules":{"DisallowedConstant":{"params":{"targets":5}}}}', lCfg, lErr));
  AssertFalse('atomic under targets key rejected',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
end;


procedure TConfigTest.ParsesMatchUnresolvedByNameBool;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // The mode switch is a plain JSON boolean riding the existing bool
  // read-path: it round-trips through RuleParamBool and validates against the
  // registered rpkBool param.
  AssertTrue('matchUnresolvedByName cfg loads', LoadConfigFromJSON(
    '{"rules":{"DisallowedIdentifier":{"params":{' +
    '"matchUnresolvedByName":true}}}}', lCfg, lErr));
  AssertTrue('bool round-trips',
    RuleParamBool(lCfg, 'DisallowedIdentifier', 'matchUnresolvedByName', False));
  AssertFalse('absent => default false',
    RuleParamBool(lCfg, 'DisallowedIdentifier', 'gone', False));
  AssertTrue('valid bool validates against registry',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
end;


procedure TConfigTest.ValidateRejectsNonBoolMatchUnresolved;

var
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // A non-boolean under the rpkBool matchUnresolvedByName key is a kind mismatch =>
  // fail-fast (the exit-2 path).
  AssertTrue('non-bool cfg loads structurally', LoadConfigFromJSON(
    '{"rules":{"DisallowedIdentifier":{"params":{' +
    '"matchUnresolvedByName":"yes"}}}}', lCfg, lErr));
  AssertFalse('non-bool matchUnresolvedByName rejected',
    ValidateConfigParams(lCfg, RuleRegistry, lErr));
  AssertTrue('rejection has a message', lErr <> '');
end;


procedure TConfigTest.SerializerRoundTripsDiffOverModelledFields;

var
  lRules: array of TRuleMetadata;
  lCfg, lBack: TFpSonarConfig;
  lJson, lErr: string;
  lTargets: TFpSonarRuleTargetArray;
  lSetting: TFpSonarRuleSetting;
  lParam: TFpSonarRuleParam;
  lTarget: TFpSonarRuleTarget;

begin
  // Two rules: Alpha tunes an int + regex + targets; Bravo tunes nothing.
  SetLength(lRules, 2);
  lRules[0] := TRuleMetadata.Make('AlphaRule', rtAst, rfAst, sevMinor,
    itCodeSmell, cfHigh, True, '');
  lRules[0].AddParam('minLength', rpkInt, 3);
  lRules[0].AddParam('pattern', rpkRegex, '^[A-Z].*$');
  lRules[0].AddParam('banned', rpkTargets);
  lRules[1] := TRuleMetadata.Make('BravoRule', rtAst, rfAst, sevMajor,
    itCodeSmell, cfHigh, True, '');

  // Edited config: Bravo disabled; Alpha severity->critical, minLength->8,
  // pattern left at default, one banned target added; a gate tweak; a suppression.
  lCfg := DefaultConfig;
  SetLength(lCfg.Rules, 2);

  lSetting.RuleId := 'BravoRule';
  lSetting.HasEnabled := True;
  lSetting.Enabled := False;
  lSetting.HasSeverity := False;
  lSetting.Severity := sevInfo;
  SetLength(lSetting.Params, 0);
  lCfg.Rules[0] := lSetting;

  lSetting.RuleId := 'AlphaRule';
  lSetting.HasEnabled := False;
  lSetting.Enabled := False;
  lSetting.HasSeverity := True;
  lSetting.Severity := sevCritical;
  SetLength(lSetting.Params, 2);
  lParam.Key := 'minLength';
  lParam.Kind := cpkInt;
  lParam.IntVal := 8;
  lParam.StrVal := '';
  lParam.BoolVal := False;
  SetLength(lParam.Targets, 0);
  lSetting.Params[0] := lParam;
  lParam.Key := 'banned';
  lParam.Kind := cpkTargets;
  lParam.IntVal := 0;
  lTarget.Pattern := 'Foo*';
  lTarget.Message := 'no foo';
  lTarget.Severity := sevBlocker;
  lTarget.HasSeverity := True;
  SetLength(lParam.Targets, 1);
  lParam.Targets[0] := lTarget;
  lSetting.Params[1] := lParam;
  lCfg.Rules[1] := lSetting;

  lCfg.Gate.MaxPerSeverity[sevMajor] := 5;

  SetLength(lCfg.Suppressions, 1);
  lCfg.Suppressions[0].RulePattern := 'Naming*';
  lCfg.Suppressions[0].PathPattern := '';

  lJson := ConfigToJSON(lCfg, lRules, cemDiff);
  AssertTrue('diff json reloads: ' + lErr,
    LoadConfigFromJSON(lJson, lBack, lErr));

  // Effective settings survive the round-trip (omitted-at-default keys included).
  AssertTrue('Alpha stays enabled (default true, no override written)',
    RuleEnabled(lBack, 'AlphaRule', True));
  AssertFalse('Bravo disabled', RuleEnabled(lBack, 'BravoRule', True));
  AssertEquals('Alpha severity critical', Ord(sevCritical),
    Ord(EffectiveSeverity(lBack, 'AlphaRule', sevMinor)));
  AssertEquals('Bravo severity falls back to default', Ord(sevMajor),
    Ord(EffectiveSeverity(lBack, 'BravoRule', sevMajor)));
  AssertEquals('Alpha minLength 8', 8,
    RuleParamInt(lBack, 'AlphaRule', 'minLength', 3));
  AssertEquals('Alpha pattern default (unchanged => omitted => fallback)',
    '^[A-Z].*$', RuleParamStr(lBack, 'AlphaRule', 'pattern', '^[A-Z].*$'));
  lTargets := RuleParamTargets(lBack, 'AlphaRule', 'banned');
  AssertEquals('one banned target', 1, Length(lTargets));
  AssertEquals('target pattern', 'Foo*', lTargets[0].Pattern);
  AssertEquals('target message', 'no foo', lTargets[0].Message);
  AssertTrue('target has severity', lTargets[0].HasSeverity);
  AssertEquals('target severity blocker', Ord(sevBlocker),
    Ord(lTargets[0].Severity));
  AssertEquals('gate major 5', 5, lBack.Gate.MaxPerSeverity[sevMajor]);
  AssertEquals('one suppression', 1, Length(lBack.Suppressions));
  AssertEquals('suppression rule', 'Naming*', lBack.Suppressions[0].RulePattern);
  AssertEquals('suppression path wildcard', '',
    lBack.Suppressions[0].PathPattern);
end;


procedure TConfigTest.SerializerDiffOfDefaultConfigIsEmpty;

var
  lRules: array of TRuleMetadata;
  lBack, lDefault: TFpSonarConfig;
  lJson, lErr: string;
  lSev: TFpSonarSeverity;

begin
  SetLength(lRules, 2);
  lRules[0] := TRuleMetadata.Make('AlphaRule', rtAst, rfAst, sevMinor,
    itCodeSmell, cfHigh, True, '');
  lRules[0].AddParam('minLength', rpkInt, 3);
  lRules[1] := TRuleMetadata.Make('BravoRule', rtAst, rfAst, sevMajor,
    itCodeSmell, cfHigh, True, '');

  // Editing nothing writes nothing: rules {}, default gate, no suppressions.
  lJson := ConfigToJSON(DefaultConfig, lRules, cemDiff);
  AssertTrue('empty-edit json reloads: ' + lErr,
    LoadConfigFromJSON(lJson, lBack, lErr));
  AssertEquals('no rule overrides written', 0, Length(lBack.Rules));
  AssertEquals('no suppressions written', 0, Length(lBack.Suppressions));
  lDefault := DefaultConfig;
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    AssertEquals('gate axis default (' + SeverityName(lSev) + ')',
      lDefault.Gate.MaxPerSeverity[lSev], lBack.Gate.MaxPerSeverity[lSev]);
  AssertEquals('gate total default', lDefault.Gate.MaxTotal,
    lBack.Gate.MaxTotal);
end;


procedure TConfigTest.SerializerFullShapeByteStable;

var
  lRules: array of TRuleMetadata;
  lFull, lTemplate, lExpected: string;

begin
  SetLength(lRules, 1);
  lRules[0] := TRuleMetadata.Make('R', rtAst, rfAst, sevMajor,
    itCodeSmell, cfHigh, True, '');

  lExpected :=
    '{' + LineEnding +
    '  "_fpsonar" : {' + LineEnding +
    '    "config" : "fpsonar-default",' + LineEnding +
    '    "version" : "1"' + LineEnding +
    '  },' + LineEnding +
    '  "rules" : {' + LineEnding +
    '    "R" : {' + LineEnding +
    '      "enabled" : true,' + LineEnding +
    '      "severity" : "major"' + LineEnding +
    '    }' + LineEnding +
    '  },' + LineEnding +
    '  "gate" : {' + LineEnding +
    '    "maxBlocker" : 0,' + LineEnding +
    '    "maxCritical" : 0,' + LineEnding +
    '    "maxMajor" : -1,' + LineEnding +
    '    "maxMinor" : -1,' + LineEnding +
    '    "maxInfo" : -1,' + LineEnding +
    '    "maxTotal" : -1' + LineEnding +
    '  },' + LineEnding +
    '  "useTier" : {' + LineEnding +
    '    "resolution" : "off"' + LineEnding +
    '  },' + LineEnding +
    '  "suppressions" : [' + LineEnding +
    '  ]' + LineEnding +
    '}';

  // cemFull is the complete template shape, byte-for-byte.
  lFull := ConfigToJSON(DefaultConfig, lRules, cemFull);
  AssertEquals('cemFull byte-stable format', lExpected, lFull);

  // init-config emits via ConfigTemplateToJSON; it must equal the cemFull shape.
  lTemplate := ConfigTemplateToJSON(DefaultConfig, lRules);
  AssertEquals('ConfigTemplateToJSON == cemFull', lFull, lTemplate);
end;


initialization
  RegisterTest(TConfigTest);

end.
