{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Reference and procedural-value semantic analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Refs;

{ Reference / procedural-value SEM rules:
  object-as-interface, nested routine as a procedural value,
  inline var captured by an anonymous method. }

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
  { Flags a concrete object assigned/passed to a COM interface. }
  TRuleNoObjectAsInterface = class(TRuleBase)
  public
    // Emits one issue per object-to-COM-interface coercion site.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a nested routine's address escaping into longer-lived storage. }
  TRuleNoNestedRoutineAsProcValue = class(TRuleBase)
  public
    // Emits one issue per nested-routine-address escape site.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags an anonymous method capturing a narrower-lifetime variable. }
  TRuleNoInlineVarCapturedByAnonMethod = class(TRuleBase)
  public
    // Emits one issue per inline-var / per-iteration-loop-var capture site.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyNoObjectAsInterface = 'rule.NoObjectAsInterface.message';
  cKeyNoNestedRoutineAsProcValue = 'rule.NoNestedRoutineAsProcValue.message';
  cKeyNoInlineVarCapturedByAnonMethod = 'rule.NoInlineVarCapturedByAnonMethod.message';

// Emits one issue at aNode's resolved row, column 1.
procedure EmitRefIssue(const aMeta: TRuleMetadata; const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector; aNode: TPasElement);
var
  lLine: integer;
begin
  lLine := aContext.Resolver.SourceRow(aNode);
  aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lLine, 1, lLine, 1,
    aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence, aMeta.MessageKey,
    [], '');
end;


{ TRuleNoObjectAsInterface }

procedure TRuleNoObjectAsInterface.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  i: integer;
begin
  // The wrapper returns every offending source node (or False to degrade — emit
  // nothing). The whole instance/interface-flavour decision lives behind it.
  if not aContext.Resolver.TryObjectAsComInterfaceSites(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitRefIssue(FMetadata, aContext, aCollector, lNodes[i]);
end;


{ TRuleNoNestedRoutineAsProcValue }

procedure TRuleNoNestedRoutineAsProcValue.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  i: integer;
begin
  // The wrapper returns every escaping '@'-expression (or False to degrade).
  if not aContext.Resolver.TryNestedRoutineAsProcValueSites(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitRefIssue(FMetadata, aContext, aCollector, lNodes[i]);
end;


{ TRuleNoInlineVarCapturedByAnonMethod }

procedure TRuleNoInlineVarCapturedByAnonMethod.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lNodes: TPasElementArray;
  i: integer;
begin
  // The wrapper returns every capturing reference site (or False to degrade).
  if not aContext.Resolver.TryInlineVarCapturedByAnonMethodSites(lNodes) then
    Exit;
  for i := 0 to High(lNodes) do
    EmitRefIssue(FMetadata, aContext, aCollector, lNodes[i]);
end;


initialization
  RegisterRule(TRuleNoObjectAsInterface.Create(TRuleMetadata.Make(
    'NoObjectAsInterface', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyNoObjectAsInterface).WithDescription(
    'Flags assigning an object to a COM interface, which adds reference counting to a manually managed instance.')));
  RegisterMessage(cKeyNoObjectAsInterface, SNoObjectAsInterface);

  RegisterRule(TRuleNoNestedRoutineAsProcValue.Create(TRuleMetadata.Make(
    'NoNestedRoutineAsProcValue', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyNoNestedRoutineAsProcValue).WithDescription(
    'Flags storing the address of a nested routine where it can outlive the enclosing stack frame.')));
  RegisterMessage(cKeyNoNestedRoutineAsProcValue, SNoNestedRoutineAsProcValue);

  RegisterRule(TRuleNoInlineVarCapturedByAnonMethod.Create(TRuleMetadata.Make(
    'NoInlineVarCapturedByAnonMethod', rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, cKeyNoInlineVarCapturedByAnonMethod).WithDescription(
    'Flags an anonymous method capturing a variable whose lifetime is narrower than the closure.')));
  RegisterMessage(cKeyNoInlineVarCapturedByAnonMethod, SNoInlineVarCapturedByAnonMethod);

end.
