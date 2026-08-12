{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for rule metadata and the self-registration registry

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRuleRegistry;

{ Rule metadata + self-registration registry tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework;

type
  { A trivial synthetic rule: emits one sentinel issue when applied. Used to
    populate local registries; never touches the global registry. }
  TSynthRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Registry + metadata tests. }
  TRuleRegistryTest = class(TTestCase)
  private
    function ArrayContains(const aArr: TFpSonarStringArray;
      const aValue: string): Boolean;
  published
    procedure MetadataRoundTripThroughRegistry;
    procedure CompletenessPassOnCompleteRules;
    procedure CompletenessFailNamesIncompleteRules;
    procedure DuplicateRuleIdReported;
    procedure ProductionRegistryIsClean;
  end;


implementation

procedure TSynthRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, 1, 1, 1, 1,
    FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
    FMetadata.MessageKey, [], 'synth');
end;


function TRuleRegistryTest.ArrayContains(const aArr: TFpSonarStringArray;
  const aValue: string): Boolean;

var
  i: Integer;

begin
  Result := False;
  for i := 0 to High(aArr) do
    if aArr[i] = aValue then
      begin
        Result := True;
        Exit;
      end;
end;


procedure TRuleRegistryTest.MetadataRoundTripThroughRegistry;

var
  lReg: TRuleRegistry;
  lFound: TRuleBase;
  lMeta: TRuleMetadata;

begin
  lReg := TRuleRegistry.Create;
  try
    lReg.Register(TSynthRule.Create(TRuleMetadata.Make('SynthRT', rtTok,
      rfTokenStream, sevMajor, itBug, cfHigh, True, '')));

    AssertEquals('one rule registered', 1, lReg.Count);
    lFound := lReg.FindById('SynthRT');
    AssertNotNull('FindById returns the registered rule', lFound);

    // The metadata exposed must be exactly what the rule was built with.
    lMeta := lFound.Metadata;
    AssertEquals('RuleId', 'SynthRT', lMeta.RuleId);
    AssertEquals('Tier', Ord(rtTok), Ord(lMeta.Tier));
    AssertEquals('Feed', Ord(rfTokenStream), Ord(lMeta.Feed));
    AssertEquals('Severity', Ord(sevMajor), Ord(lMeta.Severity));
    AssertEquals('Category', Ord(itBug), Ord(lMeta.Category));
    AssertEquals('MessageKey defaulted from RuleId', 'rule.SynthRT.message',
      lMeta.MessageKey);
    AssertTrue('DefaultEnabled', lMeta.DefaultEnabled);

    AssertNull('FindById returns nil for an absent id', lReg.FindById('Nope'));
  finally
    lReg.Free;
  end;
end;


procedure TRuleRegistryTest.CompletenessPassOnCompleteRules;

var
  lReg: TRuleRegistry;

begin
  lReg := TRuleRegistry.Create;
  try
    lReg.Register(TSynthRule.Create(TRuleMetadata.Make('SynthA', rtLex,
      rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, '')));
    lReg.Register(TSynthRule.Create(TRuleMetadata.Make('SynthB', rtAst,
      rfAst, sevMajor, itBug, cfHigh, True, '')));

    AssertEquals('a registry of only complete rules validates clean', 0,
      Length(lReg.Validate));
  finally
    lReg.Free;
  end;
end;


procedure TRuleRegistryTest.CompletenessFailNamesIncompleteRules;

var
  lReg: TRuleRegistry;
  lNoMsg: TRuleMetadata;
  lProblems: TFpSonarStringArray;

begin
  lReg := TRuleRegistry.Create;
  try
    // A rule with an empty RuleId (MessageKey would default but RuleId is the
    // failing field).
    lReg.Register(TSynthRule.Create(TRuleMetadata.Make('', rtLex,
      rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, '')));

    // A rule with a non-empty RuleId but an explicitly emptied MessageKey.
    lNoMsg := TRuleMetadata.Make('NoMsg', rtTok, rfTokenStream, sevMinor,
      itCodeSmell, cfMedium, True, 'placeholder');
    lNoMsg.MessageKey := '';
    lReg.Register(TSynthRule.Create(lNoMsg));

    lProblems := lReg.Validate;
    AssertTrue('incomplete rules make Validate non-empty',
      Length(lProblems) > 0);
    AssertTrue('empty RuleId named', ArrayContains(lProblems, 'empty RuleId'));
    AssertTrue('empty MessageKey named',
      ArrayContains(lProblems, 'NoMsg: empty MessageKey'));
  finally
    lReg.Free;
  end;
end;


procedure TRuleRegistryTest.DuplicateRuleIdReported;

var
  lReg: TRuleRegistry;
  lProblems: TFpSonarStringArray;

begin
  lReg := TRuleRegistry.Create;
  try
    lReg.Register(TSynthRule.Create(TRuleMetadata.Make('Dup', rtLex,
      rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, '')));
    lReg.Register(TSynthRule.Create(TRuleMetadata.Make('Dup', rtTok,
      rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, '')));

    lProblems := lReg.Validate;
    AssertTrue('duplicate RuleId reported',
      ArrayContains(lProblems, 'Dup: duplicate RuleId'));
  finally
    lReg.Free;
  end;
end;


procedure TRuleRegistryTest.ProductionRegistryIsClean;

begin
  // The real CI gate: the global registry every real rule self-registers
  // into must carry no incomplete/duplicate metadata. Empty now, guards later.
  AssertEquals('production registry has no incomplete rules', 0,
    Length(RuleRegistry.Validate));
end;


initialization
  RegisterTest(TRuleRegistryTest);

end.
