{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Renders an SVG document to a raster image of a size you choose.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program drawsvg;

{$mode objfpc}{$H+}

uses sysutils, classes, custapp, fpimage,
     fpreadpng, fpreadjpeg, fpreadgif, fpreadbmp, fpreadpnm, fpreadtga,
     fpreadxpm,
     fpwritepng, fpwritejpeg, fpwritebmp, fpwritepnm, fpwritetga,
     fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.soft,
     fpsvg.freetype, fpsvg.fonts.support,
{$IFDEF DARWIN}
     fpsvg.coretext,
{$ENDIF}
{$IFDEF WINDOWS}
     fpsvg.gdi,
{$ENDIF}
     fpsvg.fonts.provider;

type
  TDrawSVGApplication = class(TCustomApplication)
  private
    FInput, FOutput, FView: String;
    FWidth, FHeight: Integer;
    FBackground: TSVGColor;
    FHasBackground: Boolean;
    FQuiet, FStretch: Boolean;
    FEngine: String;
    FFonts: ISVGFontProvider;
    FFreeType: TSVGFreeTypeProvider;
{$IFDEF DARWIN}
    FCoreText: TSVGCoreTextProvider;
{$ENDIF}
{$IFDEF WINDOWS}
    FGDI: TSVGGDIProvider;
{$ENDIF}
    FImages: TSVGFileImageResolver;
    FDocuments: TSVGFileDocumentResolver;
    FFontFiles: TSVGFileFontResolver;
    // Reports a font file that a document needs but could not be found.
    procedure FontFileNeeded(aSender: TObject; const aURL, aBaseURI: String; var aFileName: String);
    function ReadOptions: Boolean;
    procedure Log(const aLine: String);
    // Composites the surface onto the background colour. The alpha is lost.
    procedure FlattenOnto(aImage: TFPCustomImage);
    // True when the format of a file name does not support an alpha channel.
    function FormatWantsAGround(const aFileName: String): Boolean;
    // A writer of the given class, with alpha support on/off.
    function MakeWriter(aClass: TFPCustomImageWriterClass; aKeepAlpha: Boolean): TFPCustomImageWriter;
  protected
    procedure DoRun; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp;
  end;

// Reads a WxH pair. False when the text is not correctly formatted.
function TryParseSize(const aText: String; out aWidth, aHeight: Integer): Boolean;

var
  lCross: Integer;

begin
  Result := False;
  lCross := Pos('x', LowerCase(aText));
  if lCross = 0 then
    Exit;
  aWidth := StrToIntDef(Copy(aText, 1, lCross - 1), 0);
  aHeight := StrToIntDef(Copy(aText, lCross + 1, Length(aText)), 0);
  Result := (aWidth > 0) and (aHeight > 0);
end;


// The views a document declares, separated by commas.
function ViewNames(aDocument: TSVGDocument): String;

var
  lViews: TSVGViewArray;
  I: Integer;

begin
  Result := '';
  lViews := SVGViewsOf(aDocument);
  for I := 0 to High(lViews) do
    begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + lViews[I].Name;
    end;
end;


{ TDrawSVGApplication }

constructor TDrawSVGApplication.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  StopOnException := True;
  FBackground := TSVGColor.FromBytes(255, 255, 255, 255);
end;


destructor TDrawSVGApplication.Destroy;

begin
  FImages := nil;
  FDocuments := nil;
  FFontFiles := nil;
  FFonts := nil;
  FreeAndNil(FFreeType);
{$IFDEF DARWIN}
  FreeAndNil(FCoreText);
{$ENDIF}
{$IFDEF WINDOWS}
  FreeAndNil(FGDI);
{$ENDIF}
  inherited Destroy;
end;


procedure TDrawSVGApplication.Log(const aLine: String);

begin
  if not FQuiet then
    WriteLn(aLine);
end;


procedure TDrawSVGApplication.FontFileNeeded(aSender: TObject;
  const aURL, aBaseURI: String; var aFileName: String);

begin
  Log('  the font ' + aURL + ' is nowhere to be found');
end;


procedure TDrawSVGApplication.WriteHelp;

begin
  WriteLn('drawsvg renders an SVG document to a raster image.');
  WriteLn;
  WriteLn('Usage:  drawsvg [options] <document.svg>');
  WriteLn;
  WriteLn('-h --help           this text');
  WriteLn('-s --size=WxH       pixel size to render at overrides the natural size of the document');
  WriteLn('-o --output=FILE    where to write; Default: the document extension changed to .png.');
  WriteLn('                    The extension determines the format: png, jpg, bmp, ppm or tga');
  WriteLn('-b --background=C   paint onto this colour first, by name or as #rrggbb.');
  WriteLn('                    White by default if no a format with no alpha channel is specified.');
  WriteLn('-f --fonts=DIR      add a directory of fonts; repeatable. Default: system font directories');
  WriteLn('-p --font-path=DIR  look here for document-specified a font files.');
  WriteLn('-e --font-engine=E  How to draw text: freetype, coretext (mac only) gdi (Windows only');
  WriteLn('                    Platform default is used otherwise');
  WriteLn('-n --no-fonts       render no text');
  WriteLn('-c --no-coverage    Do not search for a replacement font in case a glyph is not found in used font.');
  WriteLn('-v --view=NAME      Frame the drawing by the given view name. Allows # and svgView(...)');
  WriteLn('-S --stretch        Stretch to given size. Default is to keep document shape and centre');
  WriteLn('-q --quiet          Only write errors');
  WriteLn;
  WriteLn('Exit status is 0 when the image was written and 1 on error');
end;


function TDrawSVGApplication.ReadOptions: Boolean;

var
  lPaths: TStringArray;
  I: Integer;
  lRest: TStringArray;
  lCoverage: ISVGFontCoverage;

begin
  Result := False;
  lRest := GetNonOptions('hs:o:b:f:p:e:ncvSq', ['help', 'size:', 'output:', 'background:',
    'fonts:', 'font-path:', 'font-engine:', 'view:', 'no-fonts',
    'no-coverage', 'stretch', 'quiet']);
  if Length(lRest) <> 1 then
    begin
    WriteLn('drawsvg: one document to render.');
    Exit;
    end;
  FInput := lRest[0];
  if not FileExists(FInput) then
    begin
    WriteLn('drawsvg: ', FInput, ' is not there.');
    Exit;
    end;
  FQuiet := HasOption('q','quiet');
  FStretch := HasOption('S','stretch');
  FView := GetOptionValue('v','view');
  if HasOption('s','size') and not TryParseSize(GetOptionValue('s','size'),
                                            FWidth, FHeight) then
    begin
    WriteLn('drawsvg: --size takes a pair like 512x512.');
    Exit;
    end;
  FOutput := GetOptionValue('o','output');
  if FOutput = '' then
    FOutput := ChangeFileExt(FInput, '.png');
  FHasBackground := HasOption('b','background');
  if FHasBackground
     and not FBackground.TryParse(GetOptionValue('b','background')) then
    begin
    WriteLn('drawsvg: ', GetOptionValue('b','background'), ' is not a colour.');
    Exit;
    end;
  FEngine := LowerCase(Trim(GetOptionValue('e','font-engine')));
  if FEngine = '' then
    FEngine := LowerCase(SVGPlatformFontEngine);
  if FEngine = 'core text' then
    FEngine := 'coretext';
  if (FEngine <> 'freetype') and (FEngine <> 'coretext')
     and (FEngine <> 'gdi') then
    begin
    WriteLn('drawsvg: --font-engine takes freetype, coretext or gdi.');
    Exit;
    end;
{$IFNDEF DARWIN}
  if FEngine = 'coretext' then
    begin
    WriteLn('drawsvg: Core Text is macOS only.');
    Exit;
    end;
{$ENDIF}
{$IFNDEF WINDOWS}
  if FEngine = 'gdi' then
    begin
    WriteLn('drawsvg: GDI is Windows only.');
    Exit;
    end;
{$ENDIF}
  lPaths := GetOptionValues('f', 'fonts');
{$IFDEF DARWIN}
  if FEngine = 'coretext' then
    begin
    FCoreText := TSVGCoreTextProvider.Create;
    FFonts := FCoreText;
    if not HasOption('no-fonts') then
      for I := 0 to High(lPaths) do
        FCoreText.AddFontPath(lPaths[I]);
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if FEngine = 'gdi' then
    begin
    FGDI := TSVGGDIProvider.Create;
    FFonts := FGDI;
    if not HasOption('no-fonts') then
      for I := 0 to High(lPaths) do
        FGDI.AddFontPath(lPaths[I]);
    end;
{$ENDIF}
  if FEngine = 'freetype' then
    begin
    FFreeType := TSVGFreeTypeProvider.Create;
    FFonts := FFreeType;
    if not HasOption('n','no-fonts') then
      begin
      for I := 0 to High(lPaths) do
        FFreeType.AddFontPath(lPaths[I]);
      if Length(lPaths) = 0 then
        FFreeType.AddSystemFonts;
      if not HasOption('c','no-coverage') then
        begin
        lCoverage := SVGPlatformCoverage;
        if lCoverage <> nil then
          FFreeType.Coverage := lCoverage
        else
          Log('nothing here answers what covers a character, so one no'
            + ' wanted face holds will draw as an empty box.');
        end;
      end;
    end;
  if HasOption('n','no-fonts') then
    FFonts := nil;
  FImages := TSVGFileImageResolver.Create(ExtractFilePath(FInput));
  FDocuments := TSVGFileDocumentResolver.Create(ExtractFilePath(FInput));
  FFontFiles := TSVGFileFontResolver.Create(ExtractFilePath(FInput));
  FFontFiles.SearchPath := GetOptionValue('p','font-path');
  FFontFiles.OnFontFileNeeded := @FontFileNeeded;
  Result := True;
end;


function TDrawSVGApplication.FormatWantsAGround(
  const aFileName: String): Boolean;

var
  lExtension: String;

begin
  lExtension := LowerCase(ExtractFileExt(aFileName));
  Result := (lExtension = '.jpg') or (lExtension = '.jpeg')
         or (lExtension = '.bmp') or (lExtension = '.ppm')
         or (lExtension = '.pnm');
end;


function TDrawSVGApplication.MakeWriter(aClass: TFPCustomImageWriterClass;
  aKeepAlpha: Boolean): TFPCustomImageWriter;

begin
  Result := aClass.Create;
  if Result is TFPWriterPNG then
    begin
    TFPWriterPNG(Result).WordSized := False;
    // A PNG writer drops the alpha channel unless it is asked to keep it.
    // Every transparent pixel would then become opaque black.
    if aKeepAlpha then
      TFPWriterPNG(Result).UseAlpha := True;
    end;
end;


procedure TDrawSVGApplication.FlattenOnto(aImage: TFPCustomImage);

var
  X, Y: Integer;
  lSource, lResult: TSVGColor;
  lAlpha: Cardinal;

begin
  for Y := 0 to aImage.Height - 1 do
    for X := 0 to aImage.Width - 1 do
      begin
      lSource := TSVGColor(aImage.Colors[X, Y]);
      lAlpha := lSource.Alpha;
      if lAlpha = 65535 then
        Continue;
      lResult.Red := (lSource.Red * lAlpha
        + FBackground.Red * (65535 - lAlpha)) div 65535;
      lResult.Green := (lSource.Green * lAlpha
        + FBackground.Green * (65535 - lAlpha)) div 65535;
      lResult.Blue := (lSource.Blue * lAlpha
        + FBackground.Blue * (65535 - lAlpha)) div 65535;
      lResult.Alpha := 65535;
      aImage.Colors[X, Y] := TFPColor(lResult);
      end;
end;


procedure TDrawSVGApplication.DoRun;

var
  lDocument: TSVGDocument;
  lRenderer: TSVGRenderer;
  lBackend: TSVGSoftBackend;
  lWriter: TFPCustomImageWriterClass;
  lWriterObject: TFPCustomImageWriter;
  lView: TSVGView;
  lFlattened: Boolean;

begin
  Terminate;
  if HasOption('h', 'help') or (ParamCount = 0) then
    begin
    WriteHelp;
    Exit;
    end;
  if not ReadOptions then
    begin
    ExitCode := 1;
    Exit;
    end;
  lWriter := TFPCustomImage.FindWriterFromFileName(FOutput);
  if lWriter = nil then
    begin
    WriteLn('drawsvg: nothing here writes ', ExtractFileExt(FOutput), '.');
    ExitCode := 1;
    Exit;
    end;
  lDocument := nil;
  lRenderer := nil;
  lBackend := nil;
  lView := TSVGView.None;
  try
    try
      lDocument := ReadSVGFile(FInput);
      lRenderer := TSVGRenderer.Create;
      if (FView <> '') and not SVGViewOfFragment(lDocument, FView, lView) then
        begin
        WriteLn('drawsvg: ', FInput, ' declares no view ', FView, '.');
        if ViewNames(lDocument) <> '' then
          WriteLn('It declares: ', ViewNames(lDocument), '.');
        ExitCode := 1;
        Exit;
        end;
      lRenderer.View := lView;
      lRenderer.Fonts := FFonts;
      lRenderer.Images := FImages;
      lRenderer.Documents := FDocuments;
      lRenderer.FontFiles := FFontFiles;
      lBackend := TSVGSoftBackend.Create;
      if (FWidth <= 0) or (FHeight <= 0) then
        lRenderer.Render(lDocument, lBackend)
      else if FStretch then
        lRenderer.RenderToSize(lDocument, lBackend, FWidth, FHeight)
      else
        lRenderer.RenderToFit(lDocument, lBackend, FWidth, FHeight);
      if lBackend.Image = nil then
        begin
        WriteLn('drawsvg: ', FInput, ' asks for no size and holds no');
        WriteLn('viewBox; give one with --size.');
        ExitCode := 1;
        Exit;
        end;
      lFlattened := FHasBackground or FormatWantsAGround(FOutput);
      if lFlattened then
        FlattenOnto(lBackend.Image);
      lWriterObject := MakeWriter(lWriter, not lFlattened);
      try
        lBackend.Image.SaveToFile(FOutput, lWriterObject);
      finally
        lWriterObject.Free;
      end;
      Log(Format('%s: %d by %d, %d elements, %d fonts, written to %s',
        [ExtractFileName(FInput), lBackend.Image.Width,
         lBackend.Image.Height, lRenderer.ElementCount,
         lRenderer.FontsLoaded, FOutput]));
    except
      on E: Exception do
        begin
        WriteLn('drawsvg: ', FInput, ': ', E.Message);
        ExitCode := 1;
        end;
    end;
  finally
    lRenderer.Free;
    lBackend.Free;
    lDocument.Free;
  end;
end;


var
  Application: TDrawSVGApplication;

begin
  Application := TDrawSVGApplication.Create(nil);
  try
    Application.Title := 'drawsvg';
    Application.Run;
  finally
    Application.Free;
  end;
end.
