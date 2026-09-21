{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Writes an SVG document to a PDF file.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program svg2pdf;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

uses
{$IF defined(UNIX) and defined(UNICODERTL)}
  // A program of the unicode RTL converts its own strings.
  {$IFDEF FPC_DOTTEDUNITS}
     UnixApi.CWString,
  {$ELSE FPC_DOTTEDUNITS}
     cwstring,
  {$ENDIF FPC_DOTTEDUNITS}
{$ENDIF}
{$IFDEF FPC_DOTTEDUNITS}
     System.SysUtils, System.Classes, Fcl.CustApp,
{$ELSE FPC_DOTTEDUNITS}
     sysutils, classes, custapp,
{$ENDIF FPC_DOTTEDUNITS}
     fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.pdf,
     fpsvg.soft, fpsvg.freetype, fpsvg.fonts.provider;

type

  { TSVGToPDFApplication }

  TSVGToPDFApplication = class(TCustomApplication)
  private
    FInput, FOutput, FTitle: String;
    FWidth, FHeight: Integer;
    FScale: Double;
    FStretch, FQuiet: Boolean;
    FFonts: ISVGFontProvider;
    FImages: TSVGFileImageResolver;
    FDocuments: TSVGFileDocumentResolver;
    function ReadOptions: Boolean;
    procedure Say(const aLine: String);
  protected
    procedure DoRun; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp;
  end;

// Reads a WxH pair. False when the text is not one.
function TryParseSize(const aText: String; out aWidth, aHeight: Integer): Boolean;

var
  lAt: Integer;

begin
  Result := False;
  lAt := Pos('x', LowerCase(aText));
  if lAt <= 1 then
    Exit;
  Result := TryStrToInt(Copy(aText, 1, lAt - 1), aWidth)
        and TryStrToInt(Copy(aText, lAt + 1, Length(aText) - lAt), aHeight)
        and (aWidth > 0) and (aHeight > 0);
end;


constructor TSVGToPDFApplication.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  StopOnException := True;
  FScale := 1;
  // The provider of the platform knows the fonts of the system and what
  // covers a character no wanted face holds.
  FFonts := SVGPlatformFontProvider;
  FImages := TSVGFileImageResolver.Create('');
  FDocuments := TSVGFileDocumentResolver.Create('');
end;


destructor TSVGToPDFApplication.Destroy;

begin
  FFonts := nil;
  FImages.Free;
  FDocuments.Free;
  inherited Destroy;
end;


procedure TSVGToPDFApplication.Say(const aLine: String);

begin
  if not FQuiet then
    WriteLn(aLine);
end;


function TSVGToPDFApplication.ReadOptions: Boolean;

var
  lError, lSize: String;
  lCode: Integer;
  lNames: array of String;

begin
  Result := False;
  lError := CheckOptions('hqs:z:t:S',
    ['help', 'quiet', 'size:', 'scale:', 'title:', 'stretch']);
  if lError <> '' then
    begin
    WriteLn('svg2pdf: ', lError);
    Exit;
    end;
  if HasOption('h', 'help') or (ParamCount = 0) then
    begin
    WriteHelp;
    Exit;
    end;
  FQuiet := HasOption('q', 'quiet');
  FStretch := HasOption('S', 'stretch');
  FTitle := GetOptionValue('t', 'title');
  if HasOption('s', 'size') then
    begin
    lSize := GetOptionValue('s', 'size');
    if not TryParseSize(lSize, FWidth, FHeight) then
      begin
      WriteLn('svg2pdf: ', lSize, ' is not a size of the form 640x480.');
      Exit;
      end;
    end;
  if HasOption('z', 'scale') then
    begin
    Val(Trim(GetOptionValue('z', 'scale')), FScale, lCode);
    if (lCode <> 0) or (FScale <= 0) then
      begin
      WriteLn('svg2pdf: the scale must be a number above zero.');
      Exit;
      end;
    end;
  lNames := GetNonOptions('hqs:z:t:S',
    ['help', 'quiet', 'size:', 'scale:', 'title:', 'stretch']);
  if Length(lNames) < 2 then
    begin
    WriteLn('svg2pdf: name a document to read and a file to write.');
    Exit;
    end;
  FInput := lNames[0];
  FOutput := lNames[1];
  Result := True;
end;


procedure TSVGToPDFApplication.WriteHelp;

begin
  WriteLn('svg2pdf writes an SVG document to a PDF file.');
  WriteLn;
  WriteLn('  svg2pdf [options] document.svg page.pdf');
  WriteLn;
  WriteLn('  --size=WxH       size of the page in user units');
  WriteLn('  --stretch        fill that size instead of fitting the document in it');
  WriteLn('  --scale=N        PDF points per user unit, one by default');
  WriteLn('  --title=TEXT     title written into the document');
  WriteLn('  --quiet          say nothing about what was written');
  WriteLn('  --help           this text');
end;


procedure TSVGToPDFApplication.DoRun;

var
  lDocument: TSVGDocument;
  lRenderer: TSVGRenderer;
  lBackend: TSVGPDFBackend;

begin
  Terminate;
  if not ReadOptions then
    Exit;
  if not FileExists(FInput) then
    begin
    WriteLn('svg2pdf: ', FInput, ' is not there.');
    ExitCode := 1;
    Exit;
    end;
  lDocument := nil;
  lRenderer := nil;
  lBackend := nil;
  try
    lDocument := ReadSVGFile(FInput);
    lRenderer := TSVGRenderer.Create;
    lRenderer.Fonts := FFonts;
    lRenderer.Images := FImages;
    lRenderer.Documents := FDocuments;
    lBackend := TSVGPDFBackend.Create;
    lBackend.Scale := FScale;
    if FTitle <> '' then
      lBackend.Document.Infos.Title := FTitle;
    if (FWidth <= 0) or (FHeight <= 0) then
      lRenderer.Render(lDocument, lBackend)
    else if FStretch then
      lRenderer.RenderToSize(lDocument, lBackend, FWidth, FHeight)
    else
      lRenderer.RenderToFit(lDocument, lBackend, FWidth, FHeight);
    if lBackend.Page = nil then
      begin
      WriteLn('svg2pdf: ', FInput, ' asks for no size and holds no');
      WriteLn('viewBox; give one with --size.');
      ExitCode := 1;
      Exit;
      end;
    lBackend.SaveToFile(FOutput);
    Say(Format('svg2pdf: wrote %s.', [FOutput]));
  finally
    lBackend.Free;
    lRenderer.Free;
    lDocument.Free;
  end;
end;


var
  Application: TSVGToPDFApplication;

begin
  Application := TSVGToPDFApplication.Create(nil);
  Application.Title := 'svg2pdf';
  Application.Run;
  Application.Free;
end.
