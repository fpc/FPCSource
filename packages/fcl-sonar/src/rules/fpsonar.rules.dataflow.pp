{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Data-flow analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.DataFlow;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  Pascal.Tree,
{$ELSE}
  PasTree,
{$ENDIF}
  FpSonar.Types, FpSonar.Issues,
  FpSonar.RuleFramework, FpSonar.Resolver, FpSonar.DataFlow,
  FpSonar.Rules.Consts;

type
  { Flags a read of a simple-typed routine local, or of a program global in the
    main block, that no textually earlier definition of that variable precedes. }
  TRuleUninitializedVariable = class(TRuleBase)
  public
    // Emits one issue per reported use site, with the variable name.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a store to a routine local that every path overwrites before reading
    it (absence: the missing read is the defect). }
  TRuleDeadStore = class(TRuleBase)
  public
    // Emits one issue per overwritten-before-read store, with the local's name.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a read of a routine local that is assigned on one incoming path and
    not on another (absence: the missing definition is the defect). }
  TRuleUninitializedVariableStrict = class(TRuleBase)
  public
    // Emits one issue per maybe-unassigned read, with the local's name.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a store computed from the local's own previous value that no path
    ever reads (absence: the missing read is the defect). }
  TRuleSelfAssignedNeverUsed = class(TRuleBase)
  public
    // Emits one issue per unread self-derived store, with the local's name.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Flags a store to a function result that every returning path overwrites
    before reading it (absence: the missing read is the defect). }
  TRuleResultOverwrittenBeforeExit = class(TRuleBase)
  public
    // Emits one issue per overwritten result store, with the result's name.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  // Dotted message keys (rule.<RuleId>.message), seeded in initialization.
  cKeyUninitializedVariable = 'rule.UninitializedVariable.message';
  cKeyDeadStore = 'rule.DeadStore.message';
  cKeyUninitializedVariableStrict =
    'rule.UninitializedVariableStrict.message';
  cKeySelfAssignedNeverUsed = 'rule.SelfAssignedNeverUsed.message';
  cKeyResultOverwrittenBeforeExit =
    'rule.ResultOverwrittenBeforeExit.message';

// Emits one issue per flow finding of aVerdict, at its site's row.
procedure EmitFlowVerdict(const aMeta: TRuleMetadata;
  const aContext: TRuleContext; const aCollector: TFpSonarIssueCollector;
  aVerdict: TFpSonarFlowVerdict);

var
  lFlow: TFpSonarDataFlow;
  lFindings: TFpSonarFlowFindingArray;
  lOk: boolean;
  lRow, i: integer;

begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    lOk := lFlow.TryFlowFindings(lFindings);
  finally
    lFlow.Free;
  end;
  if not lOk then
    Exit;
  for i := 0 to High(lFindings) do
  begin
    if lFindings[i].Verdict <> aVerdict then
      Continue;
    lRow := aContext.Resolver.SourceRow(lFindings[i].Site);
    aCollector.AddIssue(aMeta.RuleId, aContext.FileName, lRow, 1, lRow, 1,
      aMeta.Severity, aMeta.Category, aMeta.DefaultConfidence,
      aMeta.MessageKey, [lFindings[i].Name], lFindings[i].Name);
  end;
end;


{ TRuleUninitializedVariable }

procedure TRuleUninitializedVariable.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

var
  lFlow: TFpSonarDataFlow;
  lNodes: TPasElementArray;
  lNames: TFpSonarStringArray;
  lRow, i: integer;

begin
  lFlow := TFpSonarDataFlow.Create(aContext.Resolver);
  try
    if not lFlow.TryUninitializedUses(lNodes, lNames) then
      Exit;
    for i := 0 to High(lNodes) do
    begin
      lRow := aContext.Resolver.SourceRow(lNodes[i]);
      aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, lRow, 1,
        lRow, 1, FMetadata.Severity, FMetadata.Category,
        FMetadata.DefaultConfidence, FMetadata.MessageKey, [lNames[i]],
        lNames[i]);
    end;
  finally
    lFlow.Free;
  end;
end;


{ TRuleDeadStore }

procedure TRuleDeadStore.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  EmitFlowVerdict(FMetadata, aContext, aCollector, fvDeadStore);
end;


{ TRuleUninitializedVariableStrict }

procedure TRuleUninitializedVariableStrict.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  EmitFlowVerdict(FMetadata, aContext, aCollector, fvUninitializedStrict);
end;


{ TRuleSelfAssignedNeverUsed }

procedure TRuleSelfAssignedNeverUsed.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  EmitFlowVerdict(FMetadata, aContext, aCollector, fvSelfAssignedNeverUsed);
end;


{ TRuleResultOverwrittenBeforeExit }

procedure TRuleResultOverwrittenBeforeExit.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  EmitFlowVerdict(FMetadata, aContext, aCollector, fvResultOverwritten);
end;


initialization
  // Polarity: absence — an unrecorded definition is read as one that happened.
  RegisterRule(TRuleUninitializedVariable.Create(TRuleMetadata.Make(
    'UninitializedVariable', rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, cKeyUninitializedVariable).WithDescription(
    'Flags a simple-typed routine local or program global read before any '
    + 'definition of it.')));
  RegisterMessage(cKeyUninitializedVariable, SUninitializedVariable);

  // Polarity: absence — an unseen read is read as one that happened.
  RegisterRule(TRuleDeadStore.Create(TRuleMetadata.Make(
    'DeadStore', rtSem, rfResolver, sevMinor, itCodeSmell, cfMedium,
    False, cKeyDeadStore).WithDescription(
    'Flags a store to a simple-typed routine local that every path overwrites '
    + 'before reading it.')));
  RegisterMessage(cKeyDeadStore, SDeadStore);

  // Polarity: absence — an unrecorded definition is read as one that happened.
  RegisterRule(TRuleUninitializedVariableStrict.Create(TRuleMetadata.Make(
    'UninitializedVariableStrict', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyUninitializedVariableStrict).WithDescription(
    'Flags a read of a simple-typed routine local that is assigned on one '
    + 'incoming path and not on another.')));
  RegisterMessage(cKeyUninitializedVariableStrict,
    SUninitializedVariableStrict);

  // Polarity: absence — an unseen read is read as one that happened.
  RegisterRule(TRuleSelfAssignedNeverUsed.Create(TRuleMetadata.Make(
    'SelfAssignedNeverUsed', rtSem, rfResolver, sevMinor, itCodeSmell,
    cfMedium, False, cKeySelfAssignedNeverUsed).WithDescription(
    'Flags a store computed from a routine local''s own previous value that '
    + 'no path ever reads.')));
  RegisterMessage(cKeySelfAssignedNeverUsed, SSelfAssignedNeverUsed);

  // Polarity: absence — an unseen read is read as one that happened.
  RegisterRule(TRuleResultOverwrittenBeforeExit.Create(TRuleMetadata.Make(
    'ResultOverwrittenBeforeExit', rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, cKeyResultOverwrittenBeforeExit).WithDescription(
    'Flags a store to a function result that every returning path overwrites '
    + 'before reading it.')));
  RegisterMessage(cKeyResultOverwrittenBeforeExit,
    SResultOverwrittenBeforeExit);

end.
