{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by the Free Pascal development team

    Sample HTTP server application

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

{ $DEFINE USEGNUTLS}
{ $DEFINE USEMICROHTTP} // Note, this must match what is defined in fpsimpleserver

program simpleserver;

{$IFDEF USEMICROHTTP}
{$UNDEF USEGNUTLS}
{$ENDIF}

uses
{$IFDEF UNIX}
  cwstring,
  cthreads,
{$ENDIF}
{$IFNDEF USEMICROHTTP}
{$ifdef USEGNUTLS}
  gnutlssockets,
{$else}
  opensslsockets,
{$endif}
{$ENDIF}
  sysutils, classes, types,
  httpdefs, fpwebfile,
  markdown.parser, markdown.elements, markdown.processors, markdown.htmlrender,
  fphttp2server,
  fpmkunit,
  fpsimpleserver;

Type
  { TMarkdownFileModule renders Markdown (.md) files to HTML on the fly. All other
    files are served verbatim by the inherited implementation. When the requested
    file is absent it falls back to a Markdown source: a directory request resolves
    to index.md/README.md, and a missing file.html resolves to file.md. It is
    installed (via TSimpleFileModule.DefaultSimpleFileModuleClass) only when
    -M/--markdown is given, so the fcl-md dependency stays in the binary. }
  TMarkdownFileModule = Class(TSimpleFileModule)
  Protected
    // Serve .md files as rendered HTML; delegate everything else to inherited.
    procedure SendFile(const aFileName: String; aResponse: TResponse); override;
    // For directory requests with no index page, fall back to index.md/README.md.
    function GetRequestFileName(const aRequest: TRequest): String; override;
  end;

  { THTTPApplication adds opt-in HTTP/2 (-2/--http2) and Markdown serving
    (-M/--markdown) on top of the reusable fpsimpleserver unit. The unit itself
    stays HTTP/2- and Markdown-agnostic; the dependencies on fphttp2server and
    fcl-md live only here, in the binary. }
  THTTPApplication = Class(TFPSimpleServerApplication)
  Private
    FHTTP2 : Boolean;
    FMarkdown : Boolean;
    FDirListing : Boolean;
    FH2Handler : TFPHTTP2Handler;
  Protected
    procedure GetValidOptions(out aShort: String; out aLong: TStringDynArray); override;
    procedure ProcessOptions; override;
    procedure ConfigureServer; override;
    procedure WriteOptions; override;
  Public
    destructor Destroy; override;
  end;

procedure TMarkdownFileModule.SendFile(const aFileName: String; aResponse: TResponse);

Var
  lMD : TStringList;
  lParser : TMarkdownParser;
  lDoc : TMarkdownDocument;
  lRenderer : TMarkdownHTMLRenderer;

begin
  if not SameText(ExtractFileExt(aFileName),'.md') then
    begin
    inherited SendFile(aFileName,aResponse);
    Exit;
    end;
  lMD:=TStringList.Create;
  lParser:=Nil;
  lDoc:=Nil;
  lRenderer:=Nil;
  try
    lMD.LoadFromFile(aFileName);
    lParser:=TMarkdownParser.Create(Nil);
    lDoc:=lParser.Parse(lMD);
    lRenderer:=TMarkdownHTMLRenderer.Create(Nil);
    lRenderer.Options:=[hoEnvelope,hoHead];
    lRenderer.Title:=ChangeFileExt(ExtractFileName(aFileName),'');
    aResponse.Content:=lRenderer.RenderHTML(lDoc);
    aResponse.ContentType:='text/html; charset=utf-8';
    aResponse.ContentLength:=Length(aResponse.Content);
    aResponse.SendContent;
  finally
    lRenderer.Free;
    lDoc.Free;
    lParser.Free;
    lMD.Free;
  end;
end;


function TMarkdownFileModule.GetRequestFileName(const aRequest: TRequest): String;

  function MapExists(const aName : String) : Boolean;
  begin
    Result:=(aName<>'') and FileExists(MapFileName(aName));
  end;

Var
  lRaw,lDir,lMarkdown : String;

begin
  // Inherited appends IndexPageName for directory requests (when index is enabled).
  Result:=inherited GetRequestFileName(aRequest);
  // The requested file exists: serve it as-is.
  if MapExists(Result) then
    Exit;
  lRaw:=aRequest.PathInfo;
  if lRaw='' then
    lRaw:=aRequest.URI;
  // Directory request whose index page is missing: fall back to a markdown
  // index, index.md first, then README.md.
  if (IndexPageName<>'') and ((lRaw='') or (lRaw[Length(lRaw)]='/')) then
    begin
    lDir:=Copy(Result,1,Length(Result)-Length(IndexPageName));
    if MapExists(lDir+'index.md') then
      Result:=lDir+'index.md'
    else if MapExists(lDir+'README.md') then
      Result:=lDir+'README.md';
    Exit;
    end;
  // A missing .html file: serve the matching .md file when it is present.
  if SameText(ExtractFileExt(Result),'.html') then
    begin
    lMarkdown:=ChangeFileExt(Result,'.md');
    if MapExists(lMarkdown) then
      Result:=lMarkdown;
    end;
end;


procedure THTTPApplication.GetValidOptions(out aShort: String; out aLong: TStringDynArray);
begin
  inherited GetValidOptions(aShort, aLong);
  aShort := aShort + '2MD';
  SetLength(aLong, Length(aLong)+3);
  aLong[High(aLong)-2] := 'http2';
  aLong[High(aLong)-1] := 'markdown';
  aLong[High(aLong)] := 'dir-listing';
end;

procedure THTTPApplication.ProcessOptions;
begin
  inherited ProcessOptions;
  FHTTP2 := HasOption('2','http2');
  FMarkdown := HasOption('M','markdown');
  FDirListing := HasOption('D','dir-listing');
end;

procedure THTTPApplication.ConfigureServer;
begin
  inherited ConfigureServer;
  // Install the Markdown-aware file module before routes are registered.
  if FMarkdown then
    TSimpleFileModule.DefaultSimpleFileModuleClass:=TMarkdownFileModule;
  // Generate a directory listing when a directory has no index page.
  TFPCustomFileModule.AllowDirectoryListing:=FDirListing;
  if not FHTTP2 then
    Exit;
  // Attach an HTTP/2 handler to the embedded server and activate it BEFORE the
  // accept loop starts. Cleartext serves prior-knowledge/h2c; with --ssl the
  // handler advertises ALPN 'h2,http/1.1' and serves negotiated h2 over TLS.
  FH2Handler := TFPHTTP2Handler.Create(Self);
  FH2Handler.WebServer := HTTPHandler.HTTPServer;
  FH2Handler.Active := True;
end;

procedure THTTPApplication.WriteOptions;
begin
  inherited WriteOptions;
  Writeln('-2 --http2            Enable HTTP/2 (cleartext prior-knowledge/h2c, or TLS-ALPN with --ssl).');
  Writeln('-D --dir-listing      Generate a directory listing when a directory has no index page.');
  Writeln('-M --markdown         Render .md files as HTML. Missing file.html falls back to file.md,');
  Writeln('                      and directories fall back to index.md/README.md.');
end;

destructor THTTPApplication.Destroy;
begin
  // Deactivate the handler (unregisters the seam, restores ALPN) before teardown.
  if Assigned(FH2Handler) then
    FH2Handler.Active := False;
  FreeAndNil(FH2Handler);
  inherited Destroy;
end;

Var
  Application : THTTPApplication;

begin
  Application:=THTTPApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
