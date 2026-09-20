{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Reports what this system offers for a character no wanted face covers.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fontprobe;

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
     System.SysUtils, fpsvg.types, fpsvg.freetype, fpsvg.fonts.support;
{$ELSE FPC_DOTTEDUNITS}
     sysutils, fpsvg.types, fpsvg.freetype, fpsvg.fonts.support;
{$ENDIF FPC_DOTTEDUNITS}

type
  TProbeCase = record
    CodePoint : Cardinal;
    Script    : String;
  end;

const
  Cases: array[0..9] of TProbeCase = (
    (CodePoint: $0041; Script: 'latin A'),
    (CodePoint: $00E9; Script: 'latin e acute'),
    (CodePoint: $0416; Script: 'cyrillic Zhe'),
    (CodePoint: $03A9; Script: 'greek Omega'),
    (CodePoint: $20AC; Script: 'euro sign'),
    (CodePoint: $2603; Script: 'snowman'),
    (CodePoint: $05D0; Script: 'hebrew Alef'),
    (CodePoint: $0627; Script: 'arabic Alef'),
    (CodePoint: $65E5; Script: 'japanese Nichi'),
    (CodePoint: $4E2D; Script: 'chinese Zhong'));
  WantedFamily = 'DejaVu Sans, sans-serif';
  ProbeSize = 20;

var
  Provider: TSVGFreeTypeProvider;
  Source: ISVGFontCoverage;
  Wanted: ISVGFont;
  Covered, Needed, Failed: Integer;


// Reports on one character. False when a cover was needed and the face
// that came back still had no glyph for it.
function Probe(const aCase: TProbeCase): Boolean;

var
  lRequest: TSVGFontRequest;
  lFile: String;
  lCover: ISVGFont;

begin
  Result := True;
  lRequest := TSVGFontRequest.Create(WantedFamily, ProbeSize);
  Write(Format('  U+%4.4X  %-16s ', [aCase.CodePoint, aCase.Script]));
  if (Wanted <> nil) and (Wanted.GetGlyphIndex(aCase.CodePoint) <> 0) then
    begin
    WriteLn('the wanted face has it');
    Exit;
    end;
  Inc(Needed);
  lFile := Source.CoverFor(aCase.CodePoint, lRequest);
  if lFile = '' then
    begin
    WriteLn('NO FILE RETURNED');
    Exit(False);
    end;
  if not FileExists(lFile) then
    begin
    WriteLn('returned ', lFile, ' WHICH IS NOT THERE');
    Exit(False);
    end;
  // Returning a file name is only half the job. The face must also open
  // and really hold the character, which is what the renderer needs.
  lCover := Provider.ResolveCover(aCase.CodePoint, lRequest);
  if lCover = nil then
    begin
    WriteLn('returned ', ExtractFileName(lFile), ' WHICH WOULD NOT OPEN');
    Exit(False);
    end;
  if lCover.GetGlyphIndex(aCase.CodePoint) = 0 then
    begin
    WriteLn('returned ', ExtractFileName(lFile),
      ' WHICH HAS NO GLYPH FOR IT');
    Exit(False);
    end;
  WriteLn('covered by ', ExtractFileName(lFile), ' as "',
    lCover.GetFontName, '"');
  Inc(Covered);
end;


var
  I: Integer;
  lRequest: TSVGFontRequest;

begin
  Provider := TSVGFreeTypeProvider.Create;
  try
    WriteLn('freetype: ', BoolToStr(Provider.Available, 'yes', 'no'),
      ', ', Provider.AddSystemFonts, ' font files recorded');
    if not Provider.Available then
      begin
      WriteLn('Without freetype nothing can be drawn, so nothing is probed.');
      Halt(1);
      end;
    Source := SVGPlatformCoverage;
    WriteLn('coverage: ', SVGPlatformCoverageName);
    if Source = nil then
      begin
      if SVGHasPlatformCoverage then
        WriteLn('  NOT AVAILABLE on this system.')
      else
        WriteLn('  this platform has no coverage source.');
      WriteLn('Nothing further can be probed.');
      Halt(1);
      end;
    Provider.Coverage := Source;
    lRequest := TSVGFontRequest.Create(WantedFamily, ProbeSize);
    Wanted := Provider.ResolveFont(lRequest);
    if Wanted = nil then
      WriteLn('  nothing resolves for "', WantedFamily,
        '", so every character is put to the coverage source')
    else
      WriteLn('  "', WantedFamily, '" resolves to "', Wanted.GetFontName,
        '"');
    WriteLn;
    Covered := 0;
    Needed := 0;
    Failed := 0;
    for I := 0 to High(Cases) do
      if not Probe(Cases[I]) then
        Inc(Failed);
    WriteLn;
    WriteLn(Format('%d of the %d characters the wanted face lacks were '
      + 'covered, %d were not.', [Covered, Needed, Failed]));
    if Needed = 0 then
      WriteLn('The wanted face covered everything, so the source was never'
        + ' put to work. Try a system with fewer fonts installed.')
    else if Covered = 0 then
      begin
      WriteLn('The source returned nothing usable for any of them, so it'
        + ' is not working.');
      Halt(2);
      end;
    if Failed > 0 then
      Halt(3);
  finally
    Source := nil;
    if Provider <> nil then
      Provider.Coverage := nil;
    Wanted := nil;
    Provider.Free;
  end;
end.
