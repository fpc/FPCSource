{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Semantic-naming analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.SemNaming;

{ Semantic-naming SEM rules (rtSem / rfResolver):
  consistent name casing across references;
  descendant naming convention. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree,
{$ELSE}
  PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Traversal, FpSonar.Resolver,
  FpSonar.Rules.Consts;

type
  { Flags an identifier use-site whose casing diverges from its declaration. }
  TRuleConsistentNameCasing = class(TRuleBase)
  public
    // Emits one issue per casing-divergent reference, with [used, canonical].
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a class/interface declaration whose name violates the convention
    mandated for descendants of a configured base type. }
  TRuleDescendantNamingConvention = class(TRuleBase)
  public
    // Emits one issue per descendant whose name fails its base's pattern.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Regexpr;
{$ELSE}
  SysUtils, regexpr;
{$ENDIF}

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyConsistentNameCasing = 'rule.ConsistentNameCasing.message';
  cKeyDescendantNamingConvention = 'rule.DescendantNamingConvention.message';

  // The naming patterns for each configured base
  cBaseException = 'Exception';
  cBaseInterface = 'IInterface';
  cPatternException = '^E';
  cPatternInterface = '^I';

// True iff aName matches aPattern. Case-sensitive
function MatchesPattern(const aName, aPattern: string): boolean;
var
  lRe: TRegExpr;
begin
  lRe := TRegExpr.Create;
  try
    lRe.Expression := aPattern;
    Result := lRe.Exec(aName);
  finally
    lRe.Free;
  end;
end;


// The naming pattern for aBase, or '' when the base is not one of the configured conventions.
function PatternForBase(const aBase: string): string;
begin
  if aBase = cBaseException then
    Result := cPatternException
  else if aBase = cBaseInterface then
    Result := cPatternInterface
  else
    Result := '';
end;


// Emits one issue at aLine, column 1 (SEM nodes carry no column), reporting the two message args.
procedure EmitNaming(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aLine: integer;
  const aArg1, aArg2: string);
begin
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, aLine, 1, aLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [aArg1, aArg2], aArg1);
end;


{ TRuleConsistentNameCasing }

procedure TRuleConsistentNameCasing.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lUsed, lCanonical: TFpSonarStringArray;
  i: integer;
begin
  // The wrapper returns every casing-divergent reference
  if not aContext.Resolver.TryInconsistentCasingSites(lNodes, lUsed, lCanonical) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitNaming(FMetadata, aContext, aCollector,
      aContext.Resolver.SourceRow(lNodes[i]), lUsed[i], lCanonical[i]);
end;


{ TRuleDescendantNamingConvention }

procedure TRuleDescendantNamingConvention.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  lBases: TFpSonarStringArray;
  lPattern: string;
  i: integer;
begin
  // The wrapper returns every class/interface that descends a configured base.
  if not aContext.Resolver.TryDescendantNamingViolationSites(lNodes, lBases) then
    Exit;
  for i := 0 to High(lNodes) do
  begin
    lPattern := PatternForBase(lBases[i]);
    if lPattern = '' then
      Continue;
    if not MatchesPattern(lNodes[i].Name, lPattern) then
      EmitNaming(FMetadata, aContext, aCollector,
        aContext.Resolver.SourceRow(lNodes[i]), lNodes[i].Name, lPattern);
  end;
end;


initialization
  RegisterRule(TRuleConsistentNameCasing.Create(TRuleMetadata.Make(
    'ConsistentNameCasing', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyConsistentNameCasing).WithDescription(
    'Flags an identifier spelled with different letter casing than its declaration.')));
  RegisterMessage(cKeyConsistentNameCasing, SConsistentNameCasing);

  RegisterRule(TRuleDescendantNamingConvention.Create(TRuleMetadata.Make(
    'DescendantNamingConvention', rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, cKeyDescendantNamingConvention).WithDescription(
    'Flags a type whose name does not match the convention required by its base type.')));
  RegisterMessage(cKeyDescendantNamingConvention, SDescendantNamingConvention);

end.
