{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Precise use-analysis wrapper over the fcl-passrc use analyzer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.UseAnalysis;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Pascal.Tree, Pascal.UseAnalyzer,
{$ELSE}
  Classes, SysUtils, PasTree, pasuseanalyzer,
{$ENDIF}
  FpSonar.Resolver;

type
  { The tolerant use-analysis wrapper: owns one analyzer over a resolver's
    resolved module and exposes the minimal query API. Valid only while that
    build stands — a further BuildFor frees the tree its answers point into. }
  TFpSonarUseAnalysis = class
  private
    FResolver: TFpSonarResolver;
    FAnalyzer: TPasAnalyzer;
    FAnalyzed: boolean;
    FUsable: boolean;
    { Runs the analysis at most once and caches its verdict: True iff the
      module analysed without raising and its use closure was complete. }
    function EnsureAnalyzed: boolean;
  public
    // Binds the wrapper to aResolver; the analysis runs on the first query.
    constructor Create(aResolver: TFpSonarResolver);
    // Frees the analyzer.
    destructor Destroy; override;
    { Tolerant unused-declaration query: True iff the analysis is usable,
      aDeclarations then holding the unused private members, locals and
      parameters — a unit's non-private interface identifiers count as used. }
    function TryUnusedDeclarations(out aDeclarations: TPasElementArray): boolean;
    { Tolerant access-count query: True iff the analysis is usable and it
      recorded an access for aDeclaration; aReads/aWrites are those counts. }
    function TryAccessCounts(aDeclaration: TPasElement;
      out aReads, aWrites: integer): boolean;
    // Tolerant completeness query: True iff the analysis ran on a complete use closure.
    function TryComplete: boolean;
    { Tolerant used-mark query: True iff the analysis is usable, aUsed then
      telling whether the analysis marked aDeclaration as used. }
    function TryDeclarationUsed(aDeclaration: TPasElement;
      out aUsed: boolean): boolean;
  end;

implementation

constructor TFpSonarUseAnalysis.Create(aResolver: TFpSonarResolver);

begin
  inherited Create;
  FResolver := aResolver;
end;


destructor TFpSonarUseAnalysis.Destroy;

begin
  FreeAndNil(FAnalyzer);
  inherited Destroy;
end;


function TFpSonarUseAnalysis.EnsureAnalyzed: boolean;

var
  lModule: TPasModule;

begin
  Result := FUsable;
  if FAnalyzed then
    Exit;
  FAnalyzed := True;
  if (FResolver = nil) or (not FResolver.Succeeded) or (FResolver.Engine = nil) then
    Exit;
  lModule := FResolver.ResolvedModule;
  if lModule = nil then
    Exit;

  try
    FAnalyzer := TPasAnalyzer.Create;
    FAnalyzer.Resolver := FResolver.Engine;
    FAnalyzer.IgnoreImplicitSystemUses := True;
    FAnalyzer.AnalyzeModule(lModule);
    FUsable := FAnalyzer.IsComplete;
    if FUsable then
      FAnalyzer.EmitModuleHints(lModule);
  except
    on E: Exception do
      FUsable := False;
  end;
  Result := FUsable;
end;


function TFpSonarUseAnalysis.TryUnusedDeclarations(
  out aDeclarations: TPasElementArray): boolean;

var
  lUnused: TFPList;
  i: Integer;

begin
  aDeclarations := nil;
  Result := False;
  if not EnsureAnalyzed then
    Exit;

  try
    // The list is caller-owned, the elements it carries are not.
    lUnused := FAnalyzer.GetUnusedElements;
    try
      SetLength(aDeclarations, lUnused.Count);
      for i := 0 to lUnused.Count - 1 do
        aDeclarations[i] := TPasElement(lUnused[i]);
      Result := True;
    finally
      lUnused.Free;
    end;
  except
    on E: Exception do
    begin
      aDeclarations := nil;
      Result := False;
    end;
  end;
end;


function TFpSonarUseAnalysis.TryAccessCounts(aDeclaration: TPasElement;
  out aReads, aWrites: integer): boolean;

var
  lUse: TPAElement;

begin
  aReads := 0;
  aWrites := 0;
  Result := False;
  if aDeclaration = nil then
    Exit;
  if not EnsureAnalyzed then
    Exit;

  try
    lUse := FAnalyzer.FindUsedElement(aDeclaration);
    if lUse = nil then
      Exit;
    aReads := lUse.ReadCount;
    aWrites := lUse.WriteCount;
    Result := True;
  except
    on E: Exception do
    begin
      aReads := 0;
      aWrites := 0;
      Result := False;
    end;
  end;
end;


function TFpSonarUseAnalysis.TryComplete: boolean;

begin
  Result := EnsureAnalyzed;
end;


function TFpSonarUseAnalysis.TryDeclarationUsed(aDeclaration: TPasElement;
  out aUsed: boolean): boolean;

begin
  aUsed := False;
  Result := False;
  if aDeclaration = nil then
    Exit;
  if not EnsureAnalyzed then
    Exit;

  try
    aUsed := FAnalyzer.IsUsed(aDeclaration);
    Result := True;
  except
    on E: Exception do
    begin
      aUsed := False;
      Result := False;
    end;
  end;
end;

end.
