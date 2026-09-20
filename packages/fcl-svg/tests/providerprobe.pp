{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Draws text with the font provider of the platform and nothing else.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program providerprobe;

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
     System.SysUtils, FpImage, fpsvg.types, fpsvg.dom, fpsvg.read,
     fpsvg.render, fpsvg.soft, fpsvg.fonts.provider;
{$ELSE FPC_DOTTEDUNITS}
     sysutils, fpimage, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render,
     fpsvg.soft, fpsvg.fonts.provider;
{$ENDIF FPC_DOTTEDUNITS}

const
  Source = '<svg xmlns="http://www.w3.org/2000/svg" width="200" '
    + 'height="60" viewBox="0 0 200 60">'
    + '<text x="10" y="40" font-family="sans-serif" font-size="30" '
    + 'fill="#000000">Hamburg</text></svg>';

var
  Document: TSVGDocument;
  Renderer: TSVGRenderer;
  Backend: TSVGSoftBackend;
  Painted: Integer;
  X, Y: Integer;
  Colour: TFPColor;

begin
  WriteLn('provider: ', SVGPlatformFontEngine);
  if SVGPlatformFontProvider = nil then
    begin
    WriteLn('  NO PROVIDER. Nothing can be drawn.');
    Halt(1);
    end;
  Document := nil;
  Renderer := nil;
  Backend := nil;
  try
    Document := ReadSVGString(Source);
    Renderer := TSVGRenderer.Create;
    Renderer.Fonts := SVGPlatformFontProvider;
    Backend := TSVGSoftBackend.Create;
    Renderer.Render(Document, Backend);
    Painted := 0;
    for Y := 0 to Backend.Image.Height - 1 do
      for X := 0 to Backend.Image.Width - 1 do
        begin
        Colour := Backend.Image.Colors[X, Y];
        if Colour.Alpha > 0 then
          Inc(Painted);
        end;
    WriteLn(Format('drawn: %d of the %d pixels were painted',
      [Painted, Backend.Image.Width * Backend.Image.Height]));
    if Painted = 0 then
      begin
      WriteLn('The text drew nothing, so the provider gave no outline.');
      Halt(2);
      end;
    WriteLn('The platform provider draws text.');
  finally
    Backend.Free;
    Renderer.Free;
    Document.Free;
  end;
end.
