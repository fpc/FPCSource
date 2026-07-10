{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Per-unit fault boundary: scans, parses and resolves one source file without raising

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.SourceFile;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  Pascal.Tree,
{$ELSE}
  Classes, SysUtils,
  PasTree,
{$ENDIF}
  FpSonar.Ingest, FpSonar.Types,
  FpSonar.Resolver;

type
  { Per-unit fault boundary: analyzes one source file, never raising.}
  TFpSonarSourceFile = class
  private
    // Owned
    FScanner: TFpSonarScanner;
    FParser: TFpSonarParser;
    FResolver: TFpSonarResolver;
    FTokens: TFpSonarTokenArray;
    FLines: TFpSonarStringArray;
    FDiagnostics: TFpSonarDiagnosticArray;
    FParseSucceeded: boolean;
    FPpuAutoDetect: boolean;
    FPpuCacheDir: string;
    procedure AddDiagnostic(const aDiag: TFpSonarDiagnostic);
    procedure ReadLines(const aFileName: string);
    function GetModule: TPasModule;
  public
    constructor Create;
    destructor Destroy; override;
    // Analyzes one source file, scanning then parsing through the non-throwing adapter entries.
    procedure Analyze(const aFileName, aCompilerMode: string;
      const aDefines: array of string); overload;
    // As above, additionally scanning the resolver's unit/include search paths
    procedure Analyze(const aFileName, aCompilerMode: string;
      const aDefines, aUnitPaths, aIncludePaths: array of string); overload;
    // As above, additionally opting into the separate real-source preference
    // and selecting the source dialect (dlPas2js enables pas2js parsing).
    procedure Analyze(const aFileName, aCompilerMode: string;
      const aDefines, aUnitPaths, aIncludePaths: array of string;
      aRealRtl: boolean; aTargetPointerSize: integer;
      aDialect: TFpSonarDialect = dlDefault); overload;
    // The token stream from the last Analyze (survives a failed parse).
    property Tokens: TFpSonarTokenArray read FTokens;
    // The raw physical lines from the last Analyze
    property Lines: TFpSonarStringArray read FLines;
    // The parsed AST from the last Analyze, or nil if the parse failed.
    property Module: TPasModule read GetModule;
    // Auto-detect ppudump-from-live-.ppu resolution
    property PpuAutoDetect: boolean read FPpuAutoDetect write FPpuAutoDetect;
    // Persistent ppudump-stub cache directory (--ppu-cache)
    property PpuCacheDir: string read FPpuCacheDir write FPpuCacheDir;
    // The tolerant SEM resolver built by the last Analyze, or nil  when the parse failed.
    property Resolver: TFpSonarResolver read FResolver;
    // Every diagnostic produced by the last Analyze (empty on full success).
    property Diagnostics: TFpSonarDiagnosticArray read FDiagnostics;
    // True iff the last Analyze parsed without a fault.
    property ParseSucceeded: boolean read FParseSucceeded;
  end;


implementation

constructor TFpSonarSourceFile.Create;
begin
  inherited Create;
  FScanner := TFpSonarScanner.Create;
  FParser := TFpSonarParser.Create;
  FParseSucceeded := False;
end;


destructor TFpSonarSourceFile.Destroy;
begin
  FResolver.Free;
  FParser.Free;
  FScanner.Free;
  inherited Destroy;
end;


procedure TFpSonarSourceFile.AddDiagnostic(const aDiag: TFpSonarDiagnostic);
var
  lLen: integer;
begin
  lLen := Length(FDiagnostics);
  SetLength(FDiagnostics, lLen + 1);
  FDiagnostics[lLen] := aDiag;
end;


procedure TFpSonarSourceFile.ReadLines(const aFileName: string);
const
  cBom = #$EF#$BB#$BF;
var
  lStream: TFileStream;
  lContent: string;
  lLen, lStart, i, lCount: integer;
  lLine: string;

  procedure AppendLine(const aLine: string);
  begin
    if Length(FLines) <= lCount then
      SetLength(FLines, (lCount + 1) * 2);
    FLines[lCount] := aLine;
    Inc(lCount);
  end;

begin
  SetLength(FLines, 0);
  lCount := 0;

  // Read the raw bytes independently of the parser so the lines survive a parse
  // failure exactly like the token stream.
  try
    lStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(lContent, lStream.Size);
      if Length(lContent) > 0 then
        lStream.ReadBuffer(lContent[1], Length(lContent));
    finally
      lStream.Free;
    end;
  except
    on E: Exception do
    begin
      SetLength(FLines, 0);
      Exit;
    end;
  end;

  // Strip a leading UTF-8 BOM so column 1 of line 1 is the first real byte
  if (Length(lContent) >= 3) and (Copy(lContent, 1, 3) = cBom) then
    Delete(lContent, 1, 3);

  // Split on LF; strip a single trailing CR per line but keep every other
  // byte so trailing spaces/tabs survive verbatim
  lLen := Length(lContent);
  lStart := 1;
  for i := 1 to lLen do
    if lContent[i] = #10 then
    begin
      lLine := Copy(lContent, lStart, i - lStart);
      if (Length(lLine) > 0) and (lLine[Length(lLine)] = #13) then
        SetLength(lLine, Length(lLine) - 1);
      AppendLine(lLine);
      lStart := i + 1;
    end;
  if lStart <= lLen then
  begin
    lLine := Copy(lContent, lStart, lLen - lStart + 1);
    if (Length(lLine) > 0) and (lLine[Length(lLine)] = #13) then
      SetLength(lLine, Length(lLine) - 1);
    AppendLine(lLine);
  end;

  SetLength(FLines, lCount);
end;


function TFpSonarSourceFile.GetModule: TPasModule;
begin
  Result := FParser.Module;
end;


procedure TFpSonarSourceFile.Analyze(const aFileName, aCompilerMode: string;
  const aDefines: array of string);
begin
  // No extra resolver search paths.
  Analyze(aFileName, aCompilerMode, aDefines, [], []);
end;


procedure TFpSonarSourceFile.Analyze(const aFileName, aCompilerMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string);
begin
  Analyze(aFileName, aCompilerMode, aDefines, aUnitPaths, aIncludePaths, False, 0);
end;


procedure TFpSonarSourceFile.Analyze(const aFileName, aCompilerMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string;
  aRealRtl: boolean; aTargetPointerSize: integer;
  aDialect: TFpSonarDialect = dlDefault);
var
  lDiag: TFpSonarDiagnostic;
  lFileMissing: boolean;
begin
  // Fresh per-call state
  SetLength(FDiagnostics, 0);
  SetLength(FLines, 0);
  FParseSucceeded := False;
  // Drop any resolver from a prior call before rebuilding (reused instance).
  FreeAndNil(FResolver);
  // Dialect drives the pas2js parse-relevant switches on both the LEX token
  // scan and the AST parse (dlDefault is byte-identical).
  FScanner.Dialect := aDialect;
  FParser.Dialect := aDialect;

  // Scan first: the token stream must survive a later parse failure.
  lFileMissing := False;
  if not FScanner.TryScanFile(aFileName, aCompilerMode, aDefines, lDiag) then
  begin
    AddDiagnostic(lDiag);
    lFileMissing := lDiag.Kind = dkFileNotFound;
  end;
  FTokens := FScanner.Tokens;

  // Raw physical lines for the LEX line-text feed.
  ReadLines(aFileName);

  // A file that could not be opened has nothing to parse; the scan already
  // reported it once (dkFileNotFound), so skip the parse to avoid a duplicate
  // diagnostic on the same missing file. Other scan errors (e.g. JavaScript in a
  // pas2js asm block) do NOT skip — the parser often still yields a valid tree.
  if not lFileMissing then
  begin
    // Parse: on failure Module is left nil and a diagnostic recorded
    FParseSucceeded := FParser.TryParseFile(aFileName, aCompilerMode, aDefines,
      lDiag);
    if not FParseSucceeded then
      AddDiagnostic(lDiag);

    // build the tolerant SEM resolver ONLY after a successful bare parse
    if FParseSucceeded then
    begin
      FResolver := TFpSonarResolver.Create;
      FResolver.DependencyInterfaceOnly := True;
      FResolver.IntrinsicConstEval := True;
      FResolver.CondDirectiveEval := True;
      // pas2js has no .ppu, so ppudump auto-detect cannot apply — force it off
      // and rely on real pas2js source (on aUnitPaths) + the synthetic floor.
      FResolver.PpuAutoDetect := FPpuAutoDetect and (aDialect <> dlPas2js);
      FResolver.PpuCacheDir := FPpuCacheDir;
      FResolver.Dialect := aDialect;
      if aTargetPointerSize > 0 then
        FResolver.IntrinsicTargetPointerSize := aTargetPointerSize;
      if aRealRtl then
        FResolver.RealRtlPreferred := True;
      FResolver.BuildFor(aFileName, aCompilerMode, aDefines, aUnitPaths,
        aIncludePaths, lDiag);
    end;
  end;
end;


end.
