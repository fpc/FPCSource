{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Source ingestion: the fcl-passrc scanner and parser adapters (token + AST front-end)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Ingest;


{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  Pascal.Scanner, Pascal.Parser, Pascal.Tree,
{$ELSE}
  Classes, SysUtils,
  PScanner, PParser, PasTree,
{$ENDIF}
  FpSonar.Types;

type
  // A single positioned token. Kind/Text come straight from the scanner; Row
  // and Col are the token START position from CurTokenPos, both 1-based (Col=1
  // is the first column of the line). Trivia tokens (whitespace/line endings)
  // carry an empty Text but an accurate position.
  TFpSonarToken = record
    Kind: TToken;
    Text: string;
    Row: integer;
    Col: integer;
    FileName: string;
    // The classifier methods below are the sole token-kind interface outside
    // this scanner adapter.
    function IsComment: boolean;
    // True iff a numeric-literal token.
    function IsNumber: boolean;
    // True iff a string or char literal token.
    function IsString: boolean;
    // True iff a reserved-word/keyword token. Context directives the scanner
    // returns as identifiers (result, string, out, ...) are NOT keywords.
    function IsKeyword: boolean;
    // True iff the 'begin' reserved word (distinguishes a legacy begin..end unit
    // body from an explicit initialization section, which are AST-identical).
    function IsBegin: boolean;
    // True iff trivia: whitespace, line ending, tab or comment.
    function IsTrivia: boolean;
    // The literal lexeme of a one-/two-/three-char operator or punctuation
    // token ('(' ')' ',' ':' ';' ':=' ...), or '' for any other kind. Symbol
    // tokens carry an empty Text.
    function Punct: string;
  end;

  TFpSonarTokenArray = array of TFpSonarToken;

  { The conditional symbols a scan ended with: the configured defines plus every
    {$define} the file and its includes made, minus every {$undef}. The sole
    define-set interface outside this scanner adapter. }
  TFpSonarDefineSet = record
  private
    FSymbols: TFpSonarStringArray;
    FCaptured: boolean;
  public
    // False when no define set was captured (a failed scan); True otherwise,
    // with aDefined saying whether aSymbol is in it. Case-insensitive.
    function TryIsDefined(const aSymbol: string; out aDefined: boolean): boolean;
  end;

  { Trivia-on scanner adapter over the vendored TPascalScanner. }
  TFpSonarScanner = class
  private
    FTokens: TFpSonarTokenArray;
    FDefines: TFpSonarDefineSet;
    FDialect: TFpSonarDialect;
  public
    // The source dialect; dlPas2js adds the pas2js parse-relevant modeswitches
    // to the scan. Set before ScanFile; default dlDefault (byte-identical).
    property Dialect: TFpSonarDialect read FDialect write FDialect;
    // Scans aFileName under aCompilerMode with the given defines (trivia on),
    // returning every token — including whitespace, line endings and comments —
    // each carrying its start line:col. The result is also kept in Tokens.
    // aIncludePaths + the unit's own directory resolve {$I} includes.
    function ScanFile(const aFileName: string; const aCompilerMode: string;
      const aDefines: array of string;
      const aIncludePaths: array of string): TFpSonarTokenArray;
    // Non-throwing scan entry for the per-unit fault boundary: wraps ScanFile,
    // converting any failure into aDiag (Kind dkScanError) instead of
    // propagating. Returns True on success (aDiag zeroed); False on failure
    // (Tokens holds the best-effort partial stream, possibly empty).
    function TryScanFile(const aFileName: string; const aCompilerMode: string;
      const aDefines: array of string; const aIncludePaths: array of string;
      out aDiag: TFpSonarDiagnostic): boolean;
    // The token sequence produced by the most recent ScanFile call.
    property Tokens: TFpSonarTokenArray read FTokens;
    // The define set the most recent ScanFile call ended with; uncaptured when
    // that scan failed before reaching end of file.
    property Defines: TFpSonarDefineSet read FDefines;
  end;

type
  { Container engine: owns every element the parser creates, so its destructor
    frees the whole parsed tree. CreateElement must call AddOwnedElement. }
  TFpSonarParseEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: string; ASourceLinenumber: integer): TPasElement;
      override;
    function FindElement(const AName: string): TPasElement; override;
  end;

  { Parser adapter and ownership handle. Lifetime contract: this object owns the
    parse engine and therefore the whole AST — free this parser to free the
    tree. The returned TPasModule is valid only while this parser lives, and is
    invalidated by a subsequent ParseFile call (each call owns exactly one tree). }
  TFpSonarParser = class
  private
    FEngine: TFpSonarParseEngine;
    FModule: TPasModule;
    FDialect: TFpSonarDialect;
  public
    // The source dialect; dlPas2js adds the pas2js parse-relevant parser options
    // + modeswitches. Set before ParseFile; default dlDefault (byte-identical).
    property Dialect: TFpSonarDialect read FDialect write FDialect;
    destructor Destroy; override;
    // Parses the unit at aFileName under aCompilerMode with the given defines,
    // returning its pastree TPasModule (also available via Module). Raises
    // EParserError/EScannerError on syntax errors. The tree is owned by this
    // object and freed when this parser is freed.
    function ParseFile(const aFileName: string; const aCompilerMode: string;
      const aDefines: array of string;
      const aIncludePaths: array of string): TPasModule;
    // Non-throwing parse entry for the per-unit fault boundary: wraps
    // ParseFile, converting any failure into aDiag instead of propagating.
    // Returns True on success (Module valid, aDiag zeroed); False on failure
    // (Module nil, aDiag describes the failure).
    function TryParseFile(const aFileName: string; const aCompilerMode: string;
      const aDefines: array of string; const aIncludePaths: array of string;
      out aDiag: TFpSonarDiagnostic): boolean;
    // The module parsed by the most recent ParseFile call (nil before that).
    property Module: TPasModule read FModule;
  end;

type
  { Plain-typed conditional-directive query. aFuncName is the lowercased
    cond-function ('declared'/'sizeof'); aArgument is the bare identifier/type
    name; aGenArity is the generic-parameter count parsed from the optional
    '<...>' (-1 when there is no '<'). On True, aValue holds the decided result
    ('1'/'0' for declared, the byte size for sizeof); False fails the unit. }
  TFpSonarCondEvalQuery = function(const aFuncName, aArgument: string;
    aGenArity: integer; out aValue: string): boolean of object;

  { Keep-alive handle for a resolved parse: its scanner, parser and file
    resolver must outlive the parse call. The resolver wrapper owns a list of
    these and frees them (parsers first) at teardown. }
  TFpSonarResolvedParse = class
  private
    FFileResolver: TFileResolver;
    FScanner: TPascalScanner;
    FParser: TPasParser;
    // The resolver-backed declared()/sizeof() evaluator; nil in the default
    // synthetic-preferred mode.
    FCondEvalQuery: TFpSonarCondEvalQuery;
    // The scanner OnEvalFunction handler: answers only declared()/sizeof() and
    // fails the unit for any other cond-function.
    function EvalCondFunction(Sender: TCondDirectiveEvaluator;
      Name, Param: string; out Value: string): boolean;
  public
    // Frees parser (which detaches it from the engine), then scanner, then the
    // file-resolver — none of which TPasParser owns.
    destructor Destroy; override;
  end;

  // Resolved parse entry for the SEM tier's cross-unit resolver. Drives
  // TPasParser with a caller-supplied TPasTreeContainer engine so resolution
  // runs inline during the parse, fed the include search paths and defines.
  // The implicit "uses System" is kept so System aliases bind via the wrapper's
  // FindUnit.

  // The created scanner/parser/file-resolver are wrapped in a
  // TFpSonarResolvedParse appended to aOwned before the parse, so the caller
  // owns them even if the parse raises. On a syntax error it catches
  // EParserError, fills aDiag (dkParseError) and returns False; a resolution
  // failure propagates to the resolver wrapper, which tags it dkResolveError.

  // aImplicitUses lists extra unit names to append to the parser's implicit-uses
  // chain, after the always-present 'System'. Empty leaves the chain at 'System'.

// aCondEvalQuery is the real-RTL cond-directive evaluator. When assigned, the
// scanner's OnEvalFunction is wired to it and value-macro mode (-Sm) is enabled.
// Nil leaves OnEvalFunction unassigned and macros off.
function ParseResolvedKeepAlive(aEngine: TPasTreeContainer;
  const aFileName, aCompilerMode: string;
  const aDefines, aIncludePaths, aImplicitUses: array of string;
  aCondEvalQuery: TFpSonarCondEvalQuery; aDialect: TFpSonarDialect;
  aOwned: TFPList; out aModule: TPasModule;
  out aDiag: TFpSonarDiagnostic): boolean;

implementation

const
  // pas2js parse-relevant PARSER options: the syntax-enabling subset of
  // fppas2js.po_Pas2js, WITHOUT the four resolver-semantic members
  // (po_Resolver, po_ResolveStandardTypes, po_StopOnUnitInterface,
  // po_CheckDirectiveRTTI) — a parse dialect must not switch on full resolution.
  Pas2jsParserOptions: TPOptions =
    [po_AsyncProcs, po_ExtConstWithoutExpr, po_AsmWhole];
  // Mirror of fppas2js.msAllPas2jsModeSwitches MINUS the mode selectors
  // (msDelphi/msObjfpc — the configured compiler mode owns those). Inlined
  // literally rather than importing pastojs, which fcl-sonar does not depend on.
  Pas2jsModeSwitches: TModeSwitches = [
    msClass, msResult, msRepeatForward, msInitFinal, msOut, msDefaultPara,
    msProperty, msExcept, msDefaultUnicodestring, msCBlocks,
    msFunctionReferences, msAnonymousFunctions, msNestedComment, msAutoDeref,
    msHintDirective, msAdvancedRecords, msExternalClass, msTypeHelpers,
    msArrayOperators, msPrefixedAttributes, msOmitRTTI, msMultiHelpers,
    msImplicitFunctionSpec, msMultiLineStrings, msDelphiMultiLineStrings];

// Adds the pas2js parse-relevant modeswitches to aScanner for dlPas2js; a no-op
// for dlDefault. Uses ReadOnlyModeSwitches so the switches survive an in-source
// {$mode} directive. Call after SetCompilerMode.
procedure ApplyDialectModeSwitches(aScanner: TPascalScanner;
  aDialect: TFpSonarDialect);
begin
  if aDialect = dlPas2js then
    aScanner.ReadOnlyModeSwitches := aScanner.ReadOnlyModeSwitches +
      Pas2jsModeSwitches;
end;


// Adds the pas2js parse-relevant parser options to aParser for dlPas2js; a no-op
// for dlDefault (byte-identical).
procedure ApplyDialectParserOptions(aParser: TPasParser;
  aDialect: TFpSonarDialect);
begin
  if aDialect = dlPas2js then
    aParser.Options := aParser.Options + Pas2jsParserOptions;
end;


function TFpSonarToken.IsComment: boolean;
begin
  Result := Kind = tkComment;
end;


function TFpSonarToken.IsNumber: boolean;
begin
  Result := Kind = tkNumber;
end;


function TFpSonarToken.IsString: boolean;
begin
  Result := Kind in [tkString, tkStringMultiLine, tkChar];
end;


function TFpSonarToken.IsKeyword: boolean;
begin
  Result := Kind in [tkabsolute..tkxor];
end;


function TFpSonarToken.IsBegin: boolean;
begin
  Result := Kind = tkbegin;
end;


function TFpSonarToken.IsTrivia: boolean;
begin
  Result := Kind in [tkWhitespace, tkComment, tkLineEnding, tkTab];
end;


function TFpSonarToken.Punct: string;
begin
  // The contiguous one-/two-/three-char symbol block of TToken, before the
  // reserved words; TokenInfos maps each to its source lexeme.
  if Kind in [tkBraceOpen..tkDotDotDot] then
    Result := TokenInfos[Kind]
  else
    Result := '';
end;


function TFpSonarDefineSet.TryIsDefined(const aSymbol: string;
  out aDefined: boolean): boolean;
var
  i: integer;
begin
  aDefined := False;
  Result := FCaptured;
  if not Result then
    Exit;
  for i := 0 to High(FSymbols) do
    if SameText(FSymbols[i], aSymbol) then
    begin
      aDefined := True;
      Exit;
    end;
end;


function TFpSonarScanner.ScanFile(const aFileName: string;
  const aCompilerMode: string; const aDefines: array of string;
  const aIncludePaths: array of string): TFpSonarTokenArray;
var
  lResolver: TFileResolver;
  lScanner: TPascalScanner;
  lToken: TToken;
  lCount: integer;
  i: integer;
begin
  SetLength(FTokens, 0);
  SetLength(FDefines.FSymbols, 0);
  FDefines.FCaptured := False;
  lCount := 0;
  // Nil first so the finally clause frees only what was actually constructed,
  // even if a constructor below raises (Free is a no-op on nil).
  lResolver := nil;
  lScanner := nil;
  try
    lResolver := TFileResolver.Create;
    // {$I} resolves relative to the unit's own directory first, then the
    // configured include paths — without this an in-tree include aborts the scan.
    lResolver.BaseDirectory := ExtractFilePath(aFileName);
    for i := Low(aIncludePaths) to High(aIncludePaths) do
      if aIncludePaths[i] <> '' then
        lResolver.AddIncludePath(aIncludePaths[i]);
    lScanner := TPascalScanner.Create(lResolver);
    // TRIVIA ON: surface whitespace, line endings and comments as real tokens.
    lScanner.SkipWhiteSpace := False;
    lScanner.SkipComments := False;
    // Resources hold nothing a linter inspects; skipping {$R} handling keeps a
    // missing/unsupported resource from aborting the whole unit's scan.
    lScanner.Options := lScanner.Options + [po_DisableResources];
    lScanner.SetCompilerMode(aCompilerMode);
    ApplyDialectModeSwitches(lScanner, FDialect);
    for i := Low(aDefines) to High(aDefines) do
      if aDefines[i] <> '' then
        lScanner.AddDefine(aDefines[i]);
    lScanner.OpenFile(aFileName);
    // Drive the scanner to EOF, collecting one TFpSonarToken per token. The
    // final tkEOF token is included so callers can detect end-of-stream.
    repeat
      lToken := lScanner.FetchToken;
      if Length(FTokens) <= lCount then
        SetLength(FTokens, (lCount + 1) * 2);
      FTokens[lCount].Kind := lToken;
      FTokens[lCount].Text := lScanner.CurTokenString;
      FTokens[lCount].Row := lScanner.CurTokenPos.Row;
      FTokens[lCount].Col := lScanner.CurTokenPos.Column;
      FTokens[lCount].FileName := lScanner.CurTokenPos.FileName;
      Inc(lCount);
      { pas2js asm blocks contain literal JavaScript: read the block as
        non-Pascal text until 'end', like the parser's po_AsmWhole option. }
      if (FDialect = dlPas2js) and (lToken = tkasm) then
        repeat
          lToken := lScanner.ReadNonPascalTillEndToken(True);
          if Length(FTokens) <= lCount then
            SetLength(FTokens, (lCount + 1) * 2);
          FTokens[lCount].Kind := lToken;
          FTokens[lCount].Text := lScanner.CurTokenString;
          FTokens[lCount].Row := lScanner.CurTokenPos.Row;
          FTokens[lCount].Col := lScanner.CurTokenPos.Column;
          FTokens[lCount].FileName := lScanner.CurTokenPos.FileName;
          Inc(lCount);
        until (lToken = tkEOF) or (lToken = tkEnd);
    until lToken = tkEOF;
    SetLength(FTokens, lCount);
    // The union IsDefined itself answers from: plain defines and macro names.
    SetLength(FDefines.FSymbols, lScanner.Defines.Count + lScanner.Macros.Count);
    for i := 0 to lScanner.Defines.Count - 1 do
      FDefines.FSymbols[i] := lScanner.Defines[i];
    for i := 0 to lScanner.Macros.Count - 1 do
      FDefines.FSymbols[lScanner.Defines.Count + i] := lScanner.Macros[i];
    FDefines.FCaptured := True;
  finally
    lScanner.Free;
    lResolver.Free;
  end;
  Result := FTokens;
end;


function TFpSonarScanner.TryScanFile(const aFileName: string;
  const aCompilerMode: string; const aDefines: array of string;
  const aIncludePaths: array of string;
  out aDiag: TFpSonarDiagnostic): boolean;
begin
  // Success contract: zeroed diagnostic. Reset up front for the happy path.
  aDiag.FileName := '';
  aDiag.Row := 0;
  aDiag.Col := 0;
  aDiag.Kind := dkScanError;
  aDiag.Message := '';
  Result := True;
  try
    ScanFile(aFileName, aCompilerMode, aDefines, aIncludePaths);
  except
    // A missing/unopenable file is the resolver's own EFileNotFoundError: relay
    // it as a dedicated dkFileNotFound.
    on E: EFileNotFoundError do
    begin
      aDiag.FileName := aFileName;
      aDiag.Row := 0;
      aDiag.Col := 0;
      aDiag.Kind := dkFileNotFound;
      aDiag.Message := aFileName;
      Result := False;
    end;
    // Scanner errors carry no typed position — anchor to the file at 0:0 and
    // keep whatever partial token stream ScanFile collected before failing.
    on E: Exception do
    begin
      aDiag.FileName := aFileName;
      aDiag.Row := 0;
      aDiag.Col := 0;
      aDiag.Kind := dkScanError;
      aDiag.Message := E.Message;
      Result := False;
    end;
  end;
end;

function TFpSonarParseEngine.CreateElement(AClass: TPTreeElement;
  const AName: string; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: string; ASourceLinenumber: integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
  // Register with the container so Destroy frees it. The parser never calls
  // AddOwnedElement itself.
  AddOwnedElement(Result);
end;


function TFpSonarParseEngine.FindElement(const AName: string): TPasElement;
begin
  Result := nil;
end;


destructor TFpSonarParser.Destroy;
begin
  // Freeing the engine frees every owned element (the whole AST).
  FEngine.Free;
  inherited Destroy;
end;


function TFpSonarParser.ParseFile(const aFileName: string;
  const aCompilerMode: string; const aDefines: array of string;
  const aIncludePaths: array of string): TPasModule;
var
  lResolver: TFileResolver;
  lScanner: TPascalScanner;
  lParser: TPasParser;
  i: integer;
begin
  FModule := nil;
  // Each ParseFile owns exactly one tree: drop any tree from a prior call so
  // repeated calls do not silently accumulate ASTs in a single engine.
  FreeAndNil(FEngine);
  FEngine := TFpSonarParseEngine.Create;
  // Nil first so the finally clause frees only what was actually constructed,
  // even if a constructor below raises (Free is a no-op on nil).
  lResolver := nil;
  lScanner := nil;
  lParser := nil;
  try
    lResolver := TFileResolver.Create;
    // {$I} resolves relative to the unit's own directory first, then the
    // configured include paths — without this an in-tree include aborts the parse.
    lResolver.BaseDirectory := ExtractFilePath(aFileName);
    for i := Low(aIncludePaths) to High(aIncludePaths) do
      if aIncludePaths[i] <> '' then
        lResolver.AddIncludePath(aIncludePaths[i]);
    lScanner := TPascalScanner.Create(lResolver);
    lParser := TPasParser.Create(lScanner, lResolver, FEngine);
    // Set the parser Options before SetCompilerMode: assigning
    // TPasParser.Options resets the scanner's CurrentModeSwitches on a
    // po_delphi transition. po_DisableResources skips {$R} handling.
    lParser.Options := lParser.Options + [po_ArrayRangeExpr, po_DisableResources];
    ApplyDialectParserOptions(lParser, FDialect);
    lScanner.SetCompilerMode(aCompilerMode);
    ApplyDialectModeSwitches(lScanner, FDialect);
    for i := Low(aDefines) to High(aDefines) do
      if aDefines[i] <> '' then
        lScanner.AddDefine(aDefines[i]);
    lScanner.OpenFile(aFileName);
    // Raises EParserError on any syntax error -> propagates to the caller.
    lParser.ParseMain(FModule);
  finally
    // TPasParser does not own the scanner/resolver (mirrors the smoke test);
    // free them explicitly. The engine (and AST) survive — owned by Self.
    lParser.Free;
    lScanner.Free;
    lResolver.Free;
  end;
  Result := FModule;
end;


function TFpSonarParser.TryParseFile(const aFileName: string;
  const aCompilerMode: string; const aDefines: array of string;
  const aIncludePaths: array of string;
  out aDiag: TFpSonarDiagnostic): boolean;
begin
  // Success contract: zeroed diagnostic, valid Module. Reset up front so the
  // happy path leaves a clean record.
  aDiag.FileName := '';
  aDiag.Row := 0;
  aDiag.Col := 0;
  aDiag.Kind := dkParseError;
  aDiag.Message := '';
  Result := True;
  try
    ParseFile(aFileName, aCompilerMode, aDefines, aIncludePaths);
  except
    // A missing/unopenable file (resolver's EFileNotFoundError) — surface it as
    // dkFileNotFound, matching TryScanFile, rather than a bare "Parse error".
    // Checked before EParserError: this is not a syntax failure.
    on E: EFileNotFoundError do
    begin
      aDiag.FileName := aFileName;
      aDiag.Row := 0;
      aDiag.Col := 0;
      aDiag.Kind := dkFileNotFound;
      aDiag.Message := aFileName;
      Result := False;
    end;
    // The typed parser failure (scanner errors are re-raised as this one via
    // po_KeepScannerError) — read its position straight from the exception.
    on E: EParserError do
    begin
      aDiag.FileName := E.Filename;
      aDiag.Row := E.Row;
      aDiag.Col := E.Column;
      aDiag.Kind := dkParseError;
      aDiag.Message := E.Message;
      Result := False;
    end;
    // Any other failure carries no typed position: anchor it to the file at 0:0.
    on E: Exception do
    begin
      aDiag.FileName := aFileName;
      aDiag.Row := 0;
      aDiag.Col := 0;
      aDiag.Kind := dkParseError;
      aDiag.Message := E.Message;
      Result := False;
    end;
  end;
  // On failure leave Module nil — the partial tree is engine-owned and freed
  // when this parser is freed; ParseFile already nilled it on entry.
  if not Result then
    FModule := nil;
end;


destructor TFpSonarResolvedParse.Destroy;
begin
  // Parser FIRST: its destructor nils Engine.CurrentParser (-> resolver Clear),
  // which is exactly why this bundle had to outlive the parse. Then scanner,
  // then the file-resolver (TPasParser owns none of them).
  FParser.Free;
  FScanner.Free;
  FFileResolver.Free;
  inherited Destroy;
end;


function TFpSonarResolvedParse.EvalCondFunction(Sender: TCondDirectiveEvaluator;
  Name, Param: string; out Value: string): boolean;
var
  lFunc, lArg: string;
  lArity, lLt, i: integer;
begin
  // Default to the "unknown / cannot answer" outcome: False makes the vendored
  // scanner raise its "function expected" error, degrading the unit rather than
  // fabricating a conditional branch (degrade rather than fabricate).
  Value := '0';
  Result := False;
  if not Assigned(FCondEvalQuery) then
    Exit;
  lFunc := LowerCase(Name);
  // Only declared() and sizeof() are answered; every other cond-function fails
  // the unit.
  if (lFunc <> 'declared') and (lFunc <> 'sizeof') then
    Exit;
  // Parse the optional generic arity from the '<...>' suffix, mirroring the
  // reference compiler: the comma count inside the brackets + 1 is the parameter
  // count; no '<' means a non-generic reference (-1 = "any").
  lArg := Trim(Param);
  lArity := -1;
  lLt := Pos('<', lArg);
  if lLt > 0 then
  begin
    lArity := 1;
    for i := lLt + 1 to Length(lArg) do
      if lArg[i] = ',' then
        Inc(lArity);
    lArg := Trim(Copy(lArg, 1, lLt - 1));
  end;
  Result := FCondEvalQuery(lFunc, lArg, lArity, Value);
end;


function ParseResolvedKeepAlive(aEngine: TPasTreeContainer;
  const aFileName, aCompilerMode: string;
  const aDefines, aIncludePaths, aImplicitUses: array of string;
  aCondEvalQuery: TFpSonarCondEvalQuery; aDialect: TFpSonarDialect;
  aOwned: TFPList; out aModule: TPasModule;
  out aDiag: TFpSonarDiagnostic): boolean;
var
  lParse: TFpSonarResolvedParse;
  i: integer;
begin
  // Success contract mirrors TryParseFile: zeroed diagnostic on the happy path.
  aDiag.FileName := '';
  aDiag.Row := 0;
  aDiag.Col := 0;
  aDiag.Kind := dkParseError;
  aDiag.Message := '';
  aModule := nil;
  Result := True;
  // Own the bundle up front so an EPasResolve (or anything) raised mid-parse
  // leaves no leak — the caller frees aOwned at teardown.
  lParse := TFpSonarResolvedParse.Create;
  aOwned.Add(lParse);
  try
    lParse.FFileResolver := TFileResolver.Create;
    // The unit's own directory + the configured include paths resolve {$I}
    // includes; cross-unit unit lookup is the resolver wrapper's FindUnit.
    lParse.FFileResolver.BaseDirectory := ExtractFilePath(aFileName);
    for i := Low(aIncludePaths) to High(aIncludePaths) do
      if aIncludePaths[i] <> '' then
        lParse.FFileResolver.AddIncludePath(aIncludePaths[i]);
    lParse.FScanner := TPascalScanner.Create(lParse.FFileResolver);
    // The CALLER's engine (a TPasResolver subclass) drives resolution inline;
    // creating the parser with it sets the engine's CurrentParser.
    lParse.FParser := TPasParser.Create(lParse.FScanner, lParse.FFileResolver,
      aEngine);
    // Implicit "uses System" is KEPT: the wrapper's FindUnit binds System
    // (synthetic) so System aliases resolve on real units. Any extra
    // implicit-uses (e.g. 'objpas') are appended AFTER System.
    for i := Low(aImplicitUses) to High(aImplicitUses) do
      if aImplicitUses[i] <> '' then
        lParse.FParser.ImplicitUses.Add(aImplicitUses[i]);
    // Set the parser Options BEFORE SetCompilerMode: assigning
    // TPasParser.Options pushes them into the scanner, whose setter resets
    // CurrentModeSwitches on a po_delphi transition.
    lParse.FParser.Options := lParse.FParser.Options +
      [po_ArrayRangeExpr, po_DisableResources];
    ApplyDialectParserOptions(lParse.FParser, aDialect);
    lParse.FScanner.SetCompilerMode(aCompilerMode);
    ApplyDialectModeSwitches(lParse.FScanner, aDialect);
    for i := Low(aDefines) to High(aDefines) do
      if aDefines[i] <> '' then
        lParse.FScanner.AddDefine(aDefines[i]);
    // Real-RTL cond-directive evaluator (opt-in): wire the scanner's
    // declared()/sizeof() hook and enable value-macro mode (bsMacro = -Sm).
    lParse.FCondEvalQuery := aCondEvalQuery;
    if Assigned(aCondEvalQuery) then
    begin
      lParse.FScanner.OnEvalFunction := @lParse.EvalCondFunction;
      lParse.FScanner.CurrentBoolSwitches :=
        lParse.FScanner.CurrentBoolSwitches + [bsMacro];
    end;
    lParse.FScanner.OpenFile(aFileName);
    // EParserError on a syntax error; EPasResolve (a plain Exception, NOT an
    // EParserError) inline on a resolution failure — the latter propagates.
    lParse.FParser.ParseMain(aModule);
  except
    // ONLY genuine syntax errors are folded here; anything else (a resolution
    // failure, IO) propagates to the resolver wrapper for dkResolveError
    // tagging. This keeps the resolver-engine dependency out of this unit.
    on E: EParserError do
    begin
      aDiag.FileName := E.Filename;
      aDiag.Row := E.Row;
      aDiag.Col := E.Column;
      aDiag.Kind := dkParseError;
      aDiag.Message := E.Message;
      aModule := nil;
      Result := False;
    end;
  end;
end;

end.
