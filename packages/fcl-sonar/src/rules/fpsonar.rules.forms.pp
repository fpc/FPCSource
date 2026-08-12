{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    LCL form-file analysis rule

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Forms;

{ LCL form-file rule (rtAst): flags a form unit missing its .lfm form
  resource. }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, Pascal.Tree,
{$ELSE}
  SysUtils, PasTree,
{$ENDIF}
  FpSonar.RuleFramework, FpSonar.Types,
  FpSonar.Traversal, FpSonar.Issues, FpSonar.Config, FpSonar.Rules.Consts;

type
  { Flags a form/frame/datamodule unit with no sibling .lfm. }
  TRuleLfmFormFileExists = class(TRuleBase)
  public
    // Emits one issue at the first form-class declaration when the sibling .lfm
    // is absent; abstains on any non-form unit or uncertain ancestry.
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;


implementation

const
  cKeyLfmFormFileExists = 'rule.LfmFormFileExists.message';
  cDefaultFormBaseTypes = 'TForm,TFrame,TDataModule,TCustomForm';

// Splits a comma-separated list, trimming whitespace and dropping empty entries.
function SplitCommaList(const aValue: string): TStringArray;
var
  lParts: TStringArray;
  lTrimmed: string;
  i: integer;
begin
  SetLength(Result, 0);
  lParts := aValue.Split([',']);
  for i := 0 to High(lParts) do
  begin
    lTrimmed := Trim(lParts[i]);
    if lTrimmed <> '' then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := lTrimmed;
    end;
  end;
end;


// True iff aType is a class whose direct ancestor name matches.
function IsFormDescendant(aType: TPasType;
  const aBaseTypes: array of string): boolean;
var
  lAnc: TPasType;
  i: integer;
begin
  Result := False;
  if not ((aType is TPasClassType) and (TPasClassType(aType).ObjKind = okClass)) then
    Exit;
  lAnc := TPasClassType(aType).AncestorType;
  if (lAnc = nil) or (lAnc.Name = '') then
    Exit;
  for i := Low(aBaseTypes) to High(aBaseTypes) do
    if CompareText(lAnc.Name, aBaseTypes[i]) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;


{ TRuleLfmFormFileExists }

procedure TRuleLfmFormFileExists.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);
var
  lTypes: TPasTypeArray;
  lBaseTypes: TStringArray;
  i, lLine: integer;
  lLfm: string;
begin
  if (aContext.Module = nil) or (aContext.FileName = '') then
    Exit;
  lBaseTypes := SplitCommaList(aContext.Config.RuleParamStr(FMetadata.RuleId, 'formBaseTypes', cDefaultFormBaseTypes));
  lTypes := EnumerateTypes(aContext.Module);
  lLine := 0;
  for i := 0 to High(lTypes) do
    if IsFormDescendant(lTypes[i], lBaseTypes) then
    begin
      lLine := lTypes[i].SourceLinenumber;                // first match wins
      Break;
    end;
  if lLine = 0 then
    Exit;                                                   // not a form unit
  lLfm := ChangeFileExt(aContext.FileName, '.lfm');
  if FileExists(lLfm) then
    Exit;                                                   // resource present
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName,
    lLine, 1, lLine, 1, FMetadata.Severity, FMetadata.Category,
    FMetadata.DefaultConfidence, FMetadata.MessageKey, [], 'lfm-missing');
end;


var
  lMeta: TRuleMetadata;

initialization
  // formBaseTypes declared AFTER TRuleMetadata.Make (which resets ParamSpecs)
  lMeta := TRuleMetadata.Make('LfmFormFileExists', rtAst, rfAst, sevMinor, itCodeSmell,
    cfMedium, True, cKeyLfmFormFileExists);
  lMeta.AddParam('formBaseTypes', rpkString, cDefaultFormBaseTypes);
  lMeta.Description :=
    'Flags a form/frame/datamodule unit that has no sibling .lfm file.';
  RegisterRule(TRuleLfmFormFileExists.Create(lMeta));
  RegisterMessage(cKeyLfmFormFileExists, SLfmFormFileExists);

end.
